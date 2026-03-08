;;; helpers-spec.el --- Shared test infrastructure for scope validation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Shared test infrastructure for scope validation testing.
;; Provides:
;; 1. Custom matchers for validation results
;; 2. Mock utilities for gptel sessions and processes
;; 3. Test fixture creation helpers
;; 4. Common setup/teardown functions
;;
;; Usage:
;;   (require 'helpers-spec)
;;   (describe "My test suite"
;;     (before-each (helpers-spec-setup-session))
;;     (after-each (helpers-spec-teardown-session))
;;     (it "validates command"
;;       (expect (helpers-spec-validate-command "ls")
;;               :to-be-validation-success)))

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;;; Custom Matchers

(buttercup-define-matcher :to-be-validation-success (result)
  "Match validation result indicating success.
Result should be a plist with :status :success."
  (let ((status (plist-get result :status)))
    (if (eq status :success)
        t
      (cons nil (format "Expected validation success but got status: %S" status)))))

(buttercup-define-matcher :to-be-validation-error (result expected-error-type)
  "Match validation result with specific error type.
Result should be a plist with :status :error and :error matching EXPECTED-ERROR-TYPE."
  (let ((status (plist-get result :status))
        (error-type (plist-get result :error)))
    (if (and (eq status :error)
             (equal error-type expected-error-type))
        t
      (cons nil (format "Expected error %S but got status: %S, error: %S"
                        expected-error-type status error-type)))))

(buttercup-define-matcher :to-have-file-operation (result operation path)
  "Match that semantic result contains file operation.
Checks if RESULT has a file operation with OPERATION type and PATH."
  (let* ((file-ops (plist-get result :file-operations))
         (matching-op (cl-find-if
                       (lambda (op)
                         (and (eq (plist-get op :operation) operation)
                              (string= (plist-get op :path) path)))
                       file-ops)))
    (if matching-op
        t
      (cons nil (format "Expected file operation %S %S but operations were: %S"
                        operation path file-ops)))))

(buttercup-define-matcher :to-have-cloud-auth (result provider)
  "Match that semantic result contains cloud authentication.
Checks if RESULT has cloud auth detection for PROVIDER."
  (let* ((cloud-auth (plist-get result :cloud-auth))
         (detected-provider (plist-get cloud-auth :provider)))
    (if (eq detected-provider provider)
        t
      (cons nil (format "Expected cloud auth for %S but got: %S"
                        provider cloud-auth)))))

(buttercup-define-matcher :to-have-parse-coverage (result expected-ratio)
  "Match that parse coverage ratio meets or exceeds EXPECTED-RATIO.
EXPECTED-RATIO should be a float between 0.0 and 1.0."
  (let* ((coverage (plist-get result :coverage))
         (ratio (plist-get coverage :ratio)))
    (if (and ratio (>= ratio expected-ratio))
        t
      (cons nil (format "Expected coverage >= %.2f but got: %.2f"
                        expected-ratio (or ratio 0.0))))))

;;; Mock Session Management

(defvar helpers-spec--mock-session-dir nil
  "Temporary directory for mock gptel session.")

(defvar helpers-spec--mock-session-buffer nil
  "Buffer used for mock gptel session.")

(defvar helpers-spec--original-session-fn nil
  "Original value of jf/gptel-session--current-session-dir.")

(defun helpers-spec-setup-session ()
  "Set up a mock gptel session for testing.
Creates temporary directory and buffer, mocks session detection."
  ;; Create temp session directory
  (setq helpers-spec--mock-session-dir
        (make-temp-file "gptel-test-session-" t))

  ;; Create mock session buffer
  (setq helpers-spec--mock-session-buffer
        (generate-new-buffer "*gptel-test-session*"))

  ;; Mock session directory detection
  (when (fboundp 'jf/gptel-session--current-session-dir)
    (setq helpers-spec--original-session-fn
          (symbol-function 'jf/gptel-session--current-session-dir))
    (fset 'jf/gptel-session--current-session-dir
          (lambda () helpers-spec--mock-session-dir)))

  ;; Return session info for test use
  (list :dir helpers-spec--mock-session-dir
        :buffer helpers-spec--mock-session-buffer))

(defun helpers-spec-teardown-session ()
  "Tear down mock gptel session.
Removes temporary directory and buffer, restores original functions."
  ;; Restore original session function
  (when helpers-spec--original-session-fn
    (fset 'jf/gptel-session--current-session-dir
          helpers-spec--original-session-fn)
    (setq helpers-spec--original-session-fn nil))

  ;; Clean up mock buffer
  (when (and helpers-spec--mock-session-buffer
             (buffer-live-p helpers-spec--mock-session-buffer))
    (kill-buffer helpers-spec--mock-session-buffer)
    (setq helpers-spec--mock-session-buffer nil))

  ;; Clean up temp directory
  (when helpers-spec--mock-session-dir
    (when (file-exists-p helpers-spec--mock-session-dir)
      (delete-directory helpers-spec--mock-session-dir t))
    (setq helpers-spec--mock-session-dir nil)))

;;; Mock Process Management

(defvar helpers-spec--mock-process-output nil
  "Mock output for process execution.")

(defvar helpers-spec--mock-process-exit-code 0
  "Mock exit code for process execution.")

(defun helpers-spec-mock-process-output (output &optional exit-code)
  "Mock process execution to return OUTPUT with EXIT-CODE.
If EXIT-CODE is nil, defaults to 0 (success)."
  (setq helpers-spec--mock-process-output output)
  (setq helpers-spec--mock-process-exit-code (or exit-code 0)))

(defun helpers-spec-reset-process-mocks ()
  "Reset all process mocking state."
  (setq helpers-spec--mock-process-output nil)
  (setq helpers-spec--mock-process-exit-code 0))

;;; Fixture Creation Helpers

(defun helpers-spec-make-scope-yml (content)
  "Create temporary scope.yml file with CONTENT.
Returns path to the created file."
  (let ((scope-file (make-temp-file "scope-" nil ".yml")))
    (with-temp-file scope-file
      (insert content))
    scope-file))

(defun helpers-spec-make-minimal-scope ()
  "Create minimal valid scope.yml for testing.
Returns path to the created file."
  (helpers-spec-make-scope-yml
   "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute:
    - \"/workspace/scripts/**\"
  modify:
    - \"/workspace/config/**\"
  deny:
    - \"/etc/**\"

bash_tools:
  categories:
    read_only:
      commands:
        - ls
        - cat
    safe_write:
      commands:
        - mkdir
        - touch
  deny:
    - rm
    - sudo

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))

(defun helpers-spec-make-scope-with-cloud-deny ()
  "Create scope.yml with cloud auth denied.
Returns path to the created file."
  (helpers-spec-make-scope-yml
   "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"

bash_tools:
  categories:
    read_only:
      commands: [ls, cat]
  deny: [rm]

cloud:
  auth_detection: \"deny\"

security:
  enforce_parse_complete: true
"))

(defun helpers-spec-make-scope-with-allowed-providers (providers)
  "Create scope.yml with specific allowed cloud PROVIDERS.
PROVIDERS should be a list like (:aws :gcp).
Returns path to the created file."
  (let ((providers-yaml
         (mapconcat (lambda (p) (format "    - %s" (substring (symbol-name p) 1)))
                    providers "\n")))
    (helpers-spec-make-scope-yml
     (format "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"

bash_tools:
  categories:
    read_only:
      commands: [ls, cat]
  deny: [rm]

cloud:
  auth_detection: \"warn\"
  allowed_providers:
%s

security:
  enforce_parse_complete: true
" providers-yaml))))

;;; Validation Helper Functions

(defun helpers-spec-validate-command (command &optional scope-file)
  "Validate COMMAND using SCOPE-FILE.
If SCOPE-FILE is nil, uses minimal scope.
Returns validation result plist."
  (let ((scope (or scope-file (helpers-spec-make-minimal-scope))))
    (unwind-protect
        (progn
          ;; This is a placeholder - actual validation would call
          ;; jf/gptel-scope--validate-bash-command or similar
          ;; For now, return a mock result for test infrastructure
          (list :status :success
                :command command
                :scope scope))
      (when (and scope (not scope-file))
        (delete-file scope)))))

(defun helpers-spec-parse-command (command)
  "Parse COMMAND and return semantic extraction result.
Returns plist with :status, :ast, :semantics, :coverage."
  ;; This is a placeholder - actual parsing would call bash-parser
  ;; For now, return a mock result for test infrastructure
  (list :status :success
        :command command
        :ast '(:type pipeline)
        :semantics '()
        :coverage '(:ratio 1.0)))

;;; Assertion Helpers

(defun helpers-spec-assert-validation-success (result)
  "Assert that validation RESULT indicates success.
Throws error if validation failed."
  (unless (eq (plist-get result :status) :success)
    (error "Expected validation success but got: %S" result)))

(defun helpers-spec-assert-validation-error (result expected-error)
  "Assert that validation RESULT has EXPECTED-ERROR.
Throws error if validation succeeded or has different error."
  (unless (eq (plist-get result :status) :error)
    (error "Expected validation error but got success"))
  (unless (equal (plist-get result :error) expected-error)
    (error "Expected error %S but got %S"
           expected-error (plist-get result :error))))

;;; Provide

(provide 'helpers-spec)

;;; helpers-spec.el ends here
