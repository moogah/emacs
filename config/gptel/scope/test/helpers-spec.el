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
(require 'jf-gptel-scope-yaml)

;; Load contract validation for mock self-validation
;; config/gptel/scope/test/ -> config/test/contracts/ (3 levels up, then test/contracts)
(let ((contracts-dir (expand-file-name "../../../test/contracts/"
                                       (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path contracts-dir))
(require 'contract-core)
(require 'contract-bash-parser)
(contract--register-buttercup-matcher)

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
                              (string= (plist-get op :file) path)))
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
    - \"/workspace\"
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
  deny: [rm]

cloud:
  auth_detection: \"warn\"
  allowed_providers:
%s

security:
  enforce_parse_complete: true
" providers-yaml))))

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

;;; Bash-Parser Mocking Helpers

(defvar helpers-spec--mock-bash-parse-result nil
  "Mock result for jf/bash-parse calls.")

(defvar helpers-spec--mock-bash-semantics-result nil
  "Mock result for jf/bash-extract-semantics calls.")

(defvar helpers-spec--original-bash-parse-fn nil
  "Original value of jf/bash-parse.")

(defvar helpers-spec--original-bash-semantics-fn nil
  "Original value of jf/bash-extract-semantics.")

(defun helpers-spec--mock-parse-result (command commands parse-complete)
  "Mock jf/bash-parse results with contract self-validation.
COMMAND is the input command string.
COMMANDS is the list of all extracted command names (strings).
PARSE-COMPLETE is a boolean flag indicating parse completeness.

Returns a mock plist with :success, :all-commands, :parse-complete.
:all-commands is a list of command plists with :command-name field."
  (let* ((all-commands (mapcar (lambda (cmd-name)
                                 (list :command-name cmd-name))
                               commands))
         (result (list :success t
                       :all-commands all-commands
                       :parse-complete parse-complete
                       :tokens '()
                       :command command)))
    (when-let ((error (contract/bash-parse-result--validate result)))
      (error "Mock parse-result violates contract: %s" error))
    result))

(defun helpers-spec--mock-semantics (file-ops cloud-auth coverage)
  "Mock jf/bash-extract-semantics results with contract self-validation.
FILE-OPS is a list of file operation plists.
CLOUD-AUTH is a cloud auth plist (or nil).  Note: scope consumer expects a flat
  plist here (:provider :aws :command \"cmd\"), not a list-of-plists as bash-parser
  produces.  This is a known consumer-side gap (scope should car the domain value).
COVERAGE is a plist with coverage statistics.

Returns a mock plist with :domains as alist matching bash-parser output."
  (let* ((domains (delq nil
                        (list (cons :filesystem file-ops)
                              (when cloud-auth
                                (cons :authentication cloud-auth)))))
         (result (list :domains domains
                       :coverage coverage
                       :parse-complete t)))
    ;; Validate file-ops individually (contract-enforced)
    (dolist (op file-ops)
      (when-let ((error (contract/bash-file-op--validate op)))
        (error "Mock file-op in semantics violates contract: %s" error)))
    ;; Validate top-level structure (alist domains, required keys)
    (when-let ((error (contract--validate-plist result
                        '((:domains listp)
                          (:coverage listp)
                          (:parse-complete booleanp)))))
      (error "Mock semantics violates contract: %s" error))
    ;; Validate domains is an alist (not plist)
    (when domains
      (when-let ((error (contract/bash-domains--validate domains)))
        (error "Mock semantics domains violates contract: %s" error)))
    result))

(defun helpers-spec--make-file-op (operation path &rest props)
  "Build file operation plist with contract self-validation.
OPERATION is a keyword (:read, :write, :execute, :modify).
PATH is the file path string.
PROPS are additional properties as plist (e.g., :command \"cat\" :source :positional-arg).

Returns file-op plist in bash-parser format: (:file PATH :operation OP :command CMD
 :confidence CONF :source SRC)."
  (let ((result (append (list :file path
                              :operation operation
                              :confidence (or (plist-get props :confidence) :high)
                              :source (or (plist-get props :source) :positional-arg)
                              :command (or (plist-get props :command) "unknown"))
                        props)))
    (when-let ((error (contract/bash-file-op--validate result)))
      (error "Mock file-op violates contract: %s" error))
    result))

(defun helpers-spec-setup-bash-mocks ()
  "Set up bash-parser mocking infrastructure.
Saves original functions and installs mocks."
  ;; Save originals if not already saved
  (when (and (fboundp 'jf/bash-parse)
             (not helpers-spec--original-bash-parse-fn))
    (setq helpers-spec--original-bash-parse-fn
          (symbol-function 'jf/bash-parse)))
  (when (and (fboundp 'jf/bash-extract-semantics)
             (not helpers-spec--original-bash-semantics-fn))
    (setq helpers-spec--original-bash-semantics-fn
          (symbol-function 'jf/bash-extract-semantics)))

  ;; Install mocks
  (when (fboundp 'jf/bash-parse)
    (fset 'jf/bash-parse
          (lambda (_command)
            (or helpers-spec--mock-bash-parse-result
                (list :success t :all-commands '() :parse-complete t)))))
  (when (fboundp 'jf/bash-extract-semantics)
    (fset 'jf/bash-extract-semantics
          (lambda (_parsed)
            (or helpers-spec--mock-bash-semantics-result
                (list :domains '() :coverage '(:ratio 1.0) :parse-complete t))))))

(defun helpers-spec-teardown-bash-mocks ()
  "Tear down bash-parser mocking infrastructure.
Restores original functions."
  ;; Restore originals
  (when helpers-spec--original-bash-parse-fn
    (fset 'jf/bash-parse helpers-spec--original-bash-parse-fn)
    (setq helpers-spec--original-bash-parse-fn nil))
  (when helpers-spec--original-bash-semantics-fn
    (fset 'jf/bash-extract-semantics helpers-spec--original-bash-semantics-fn)
    (setq helpers-spec--original-bash-semantics-fn nil))

  ;; Clear mock state
  (setq helpers-spec--mock-bash-parse-result nil)
  (setq helpers-spec--mock-bash-semantics-result nil))

(defun helpers-spec-mock-bash-parse (command commands parse-complete)
  "Set mock result for next jf/bash-parse call.
COMMAND, COMMANDS, PARSE-COMPLETE are passed to helpers-spec--mock-parse-result."
  (setq helpers-spec--mock-bash-parse-result
        (helpers-spec--mock-parse-result command commands parse-complete)))

(defun helpers-spec-mock-bash-semantics (file-ops cloud-auth coverage)
  "Set mock result for next jf/bash-extract-semantics call.
FILE-OPS, CLOUD-AUTH, COVERAGE are passed to helpers-spec--mock-semantics."
  (setq helpers-spec--mock-bash-semantics-result
        (helpers-spec--mock-semantics file-ops cloud-auth coverage)))

;;; Scope Configuration Builders

(defun helpers-spec--convert-vectors-to-lists (obj)
  "Recursively convert vectors to lists in OBJ.
Also converts YAML boolean keywords to Emacs booleans:
- :true -> t
- :false -> nil"
  (cond
   ;; Convert YAML boolean keywords to Emacs booleans
   ((eq obj :true) t)
   ((eq obj :false) nil)
   ((vectorp obj)
    (mapcar #'helpers-spec--convert-vectors-to-lists (append obj nil)))
   ((and (listp obj) (not (null obj)))
    (if (keywordp (car obj))
        ;; It's a plist, process keys and values
        (let ((result nil))
          (while obj
            (push (car obj) result)  ; key
            (push (helpers-spec--convert-vectors-to-lists (cadr obj)) result)  ; value
            (setq obj (cddr obj)))
          (nreverse result))
      ;; It's a regular list
      (mapcar #'helpers-spec--convert-vectors-to-lists obj)))
   (t obj)))

(defun helpers-spec-load-scope-config (scope-file)
  "Load scope configuration from SCOPE-FILE.
Returns scope-config plist ready for validation functions."
  (jf/gptel-scope-yaml--load-schema scope-file))

(defun helpers-spec--scope-with-paths (read write execute modify deny)
  "Build scope.yml with operation-specific paths.
READ, WRITE, EXECUTE, MODIFY, DENY are lists of path patterns.

Returns formatted YAML string."
  (let ((format-paths (lambda (paths)
                        (if paths
                            (mapconcat (lambda (p) (format "    - \"%s\"" p))
                                       paths "\n")
                          "    []"))))
    (format "paths:
  read:
%s
  write:
%s
  execute:
%s
  modify:
%s
  deny:
%s

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"
            (funcall format-paths read)
            (funcall format-paths write)
            (funcall format-paths execute)
            (funcall format-paths modify)
            (funcall format-paths deny))))

(defun helpers-spec--scope-with-cloud (auth-detection allowed-providers)
  "Build scope.yml with cloud config.
AUTH-DETECTION is a string (\"allow\", \"warn\", or \"deny\").
ALLOWED-PROVIDERS is a list of provider keywords (:aws, :gcp, etc.).

Returns formatted YAML string."
  (let ((providers-yaml
         (if allowed-providers
             (format "  allowed_providers:\n%s"
                     (mapconcat (lambda (p)
                                  (format "    - %s"
                                          (substring (symbol-name p) 1)))
                                allowed-providers "\n"))
           "")))
    (format "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"

bash_tools:
  deny: []

cloud:
  auth_detection: \"%s\"
%s

security:
  enforce_parse_complete: true
"
            auth-detection
            providers-yaml)))

;;; Additional Custom Matchers

(buttercup-define-matcher :to-have-warning (result)
  "Match that result contains a warning.
Result should be a plist with :status :warning."
  (let ((status (plist-get result :status)))
    (if (eq status :warning)
        t
      (cons nil (format "Expected warning but got status: %S" status)))))

(buttercup-define-matcher :to-be-denied-command (result)
  "Match command_denied error in result.
Result should have :error :command_denied."
  (let ((error-type (plist-get result :error)))
    (if (eq error-type :command_denied)
        t
      (cons nil (format "Expected :command_denied but got error: %S" error-type)))))

(buttercup-define-matcher :to-be-path-out-of-scope (result)
  "Match path_out_of_scope error in result.
Result should have :error :path_out_of_scope."
  (let ((error-type (plist-get result :error)))
    (if (eq error-type :path_out_of_scope)
        t
      (cons nil (format "Expected :path_out_of_scope but got error: %S" error-type)))))

;;; Violation-Info Fixture Factory

(defun helpers-spec--make-violation-info (tool error-code &rest props)
  "Build a realistic violation-info plist via the real build-violation-info.
TOOL is the tool name string (e.g., \"read_file\", \"run_bash_command\").
ERROR-CODE is the machine-readable error string (e.g., \"not-in-scope\",
\"path_out_of_scope\", \"command_denied\").
PROPS are keyword arguments:
  :path       - File path (for path-based errors)
  :command    - Command string (for command errors)
  :provider   - Cloud provider (for cloud auth errors)
  :resource   - Explicit resource override
  :operation  - Operation keyword (:read, :write, etc.)
  :metadata   - File metadata plist
  :message    - Explicit human-readable message override

Constructs a validator-format plist and passes it through the real
jf/gptel-scope--build-violation-info, so the output always matches
what production code produces.  The only logic owned by this factory
is generating a realistic :message when one is not provided."
  (let* ((path (plist-get props :path))
         (command (plist-get props :command))
         (provider (plist-get props :provider))
         (operation (plist-get props :operation))
         (metadata (plist-get props :metadata))
         (explicit-resource (plist-get props :resource))
         (explicit-message (plist-get props :message))
         ;; Generate a human-readable message if not provided
         (message (or explicit-message
                      (helpers-spec--generate-violation-message
                       error-code path command provider operation)))
         ;; Real validators always include :resource as a fallback.
         ;; build-violation-info's default case uses (or :resource :path),
         ;; so we derive :resource from contextual fields when not explicit.
         (resource (or explicit-resource command path provider))
         ;; Derive validation-type from tool name. The only bash-validated
         ;; tool is run_bash_command; everything else flows through the
         ;; filesystem validator and uses 'path.
         (validation-type (if (string= tool "run_bash_command") 'bash 'path))
         ;; Build validator-format input plist. The real validation
         ;; entrypoint tags results with :validation-type; this factory
         ;; reproduces that so build-violation-info reads it directly.
         (validator-plist
          (append
           (list :error error-code
                 :message message
                 :operation operation
                 :metadata metadata
                 :resource resource
                 :validation-type validation-type)
           (when path (list :path path))
           (when command (list :command command))
           (when provider (list :provider provider)))))
    (jf/gptel-scope--build-violation-info validator-plist tool)))

(defun helpers-spec--generate-violation-message (error-code path command provider operation)
  "Generate a realistic human-readable message for ERROR-CODE.
PATH, COMMAND, PROVIDER, OPERATION provide context for the message."
  (pcase error-code
    ("path_out_of_scope"
     (format "Path not in %s scope: %s" (or operation "read") (or path "unknown")))
    ("path_denied"
     (format "Path denied by scope: %s" (or path "unknown")))
    ("not-in-scope"
     (format "Path not in %s scope: %s" (or operation "read") (or path "unknown")))
    ("denied-pattern"
     (format "Path denied by scope: %s" (or path "unknown")))
    ("command_denied"
     (format "Command '%s' is in deny list" (or command "unknown")))
    ("command-not-allowed"
     (format "No bash_tools configuration found. Command denied: %s"
             (or command "unknown")))
    ("cloud_auth_denied"
     (format "Cloud authentication denied: %s (%s provider)"
             (or command "unknown") (or provider "unknown")))
    ("not-in-org-roam-patterns"
     (format "Pattern not in org-roam configuration: %s"
             (or path command "unknown")))
    ("unknown-org-roam-tool"
     (format "Unknown org-roam tool: %s" (or command "unknown")))
    (_ (format "Validation failed: %s" error-code))))

;;; Provide

(provide 'helpers-spec)

;;; helpers-spec.el ends here
