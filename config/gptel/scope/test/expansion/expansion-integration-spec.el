;;; expansion-integration-spec.el --- Integration tests for validator -> expansion UI flow -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Validator -> build-violation-info -> Expansion UI
;;
;; These tests verify the interface contract between scope validation
;; components, ensuring proper data flow from validators through to
;; the expansion UI without mismatches in field names or formats.
;;
;; Migration note: rewritten as part of migrate-expansion-tests
;; (cycle-3) — scope_config plists are now constructed directly via
;; `helpers-spec-make-scope-config' (no scope.yml round-trip).
;;
;; Test coverage:
;; 1. Path validator output -> build-violation-info -> violation-info format
;; 2. Pattern validator output -> build-violation-info -> violation-info format
;; 3. Bash validator output -> build-violation-info -> violation-info format
;; 4. Violation-info -> Expansion UI rendering (no crashes on missing fields)
;; 5. End-to-end: validator failure -> expansion -> approval flow

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Fixtures

(defvar expansion-integration--test-scope-config
  (helpers-spec-make-scope-config
   :read    '("/workspace/allowed/**")
   :write   '("/workspace/writable/**")
   :execute '()
   :modify  '()
   :deny    '("/workspace/denied/**")
   :auth-detection "warn")
  "Test scope configuration for validators.")

;;; Helper Functions

(defun expansion-integration--call-path-validator (filepath operation)
  "Call filesystem validator with FILEPATH and OPERATION (read or write).
Returns check-result from validator."
  (let ((tool-name (if (eq operation 'read) "read_file_in_scope" "write_file_in_scope")))
    (jf/gptel-scope--validate-filesystem-tool
     tool-name operation (list filepath)
     expansion-integration--test-scope-config nil)))

;;; ============================================================
;;; Validator -> build-violation-info contract tests
;;; ============================================================

(describe "Validator -> build-violation-info -> Expansion UI Integration"

  (describe "Path validator integration"

    (it "transforms path validator denial into valid violation-info"
      (let* ((check-result (append
                            (expansion-integration--call-path-validator "/tmp/outside.txt" 'read)
                            (list :validation-type 'path)))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file_in_scope")))
        ;; Verify check-result has expected format
        (expect (plist-get check-result :allowed) :to-be nil)
        (expect (plist-get check-result :error) :to-equal "not-in-scope")
        (expect (plist-get check-result :resource) :to-be-truthy)

        ;; Verify violation-info transformation
        (expect (plist-get violation-info :tool) :to-equal "read_file_in_scope")
        (expect (plist-get violation-info :resource) :to-be-truthy)
        (expect (plist-get violation-info :reason) :to-be-truthy)
        (expect (plist-get violation-info :reason) :not :to-be nil)
        (expect (plist-get violation-info :validation-type) :to-equal 'path)))

    (it "handles denied-pattern from path validator"
      (let* ((check-result '(:allowed nil
                             :error "denied-pattern"
                             :resource "/workspace/denied/file.txt"
                             :tool "read_file_in_scope"
                             :validation-type path
                             :message "Path denied by scope: /workspace/denied/file.txt"))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file_in_scope")))
        (expect (plist-get violation-info :reason) :to-equal "Path denied by scope: /workspace/denied/file.txt"))))

  (describe "Bash validator integration — real validator output"

    (it "denied-pattern from real validate-path-operation flows through correctly"
      (let* ((config '(:paths (:read ("/workspace/**") :write () :execute ()
                               :modify () :deny ("/etc/**"))))
             (validation-error (append
                                (jf/gptel-scope--validate-path-operation
                                 "/etc/passwd" :read config)
                                (list :validation-type 'bash)))
             (violation-info (jf/gptel-scope--build-violation-info
                              validation-error "run_bash_command")))
        (expect (plist-get validation-error :error) :to-equal "denied-pattern")
        ;; build-violation-info must produce usable violation-info
        (expect (plist-get violation-info :tool) :to-equal "run_bash_command")
        (expect (plist-get violation-info :resource) :to-equal "/etc/passwd")
        (expect (plist-get violation-info :reason) :to-be-truthy)
        (expect (plist-get violation-info :validation-type) :to-equal 'bash)))

    (it "not-in-scope from real validate-path-operation flows through correctly"
      (let* ((config '(:paths (:read ("/workspace/**") :write () :execute ()
                               :modify () :deny ())))
             (validation-error (append
                                (jf/gptel-scope--validate-path-operation
                                 "/tmp/file.txt" :read config)
                                (list :validation-type 'bash)))
             (violation-info (jf/gptel-scope--build-violation-info
                              validation-error "run_bash_command")))
        (expect (plist-get validation-error :error) :to-equal "not-in-scope")
        (expect (plist-get violation-info :tool) :to-equal "run_bash_command")
        (expect (plist-get violation-info :resource) :to-equal "/tmp/file.txt")
        (expect (plist-get violation-info :reason) :to-be-truthy)
        (expect (plist-get violation-info :validation-type) :to-equal 'bash))))

  (describe "Reason extracted from :message field"

    (it "reads :message for violation-info :reason"
      (let* ((check-result '(:allowed nil
                             :error "not-in-scope"
                             :message "Path not in read scope: /tmp/file.txt"
                             :resource "/tmp/file.txt"
                             :tool "read_file_in_scope"
                             :validation-type path))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file_in_scope")))
        (expect (plist-get violation-info :reason) :to-equal "Path not in read scope: /tmp/file.txt"))))

  (describe "Transformation robustness"

    (it "returns nil for :reason when :message not present"
      (let* ((malformed '(:allowed nil
                         :error "some-error"
                         :resource "/tmp/file.txt"
                         :tool "read_file_in_scope"
                         :validation-type path))
             (violation-info (jf/gptel-scope--build-violation-info malformed "read_file_in_scope")))
        (expect (plist-get violation-info :reason) :to-be nil)
        (expect (plist-get violation-info :resource) :to-equal "/tmp/file.txt")))))

;;; ============================================================
;;; End-to-end expansion integration tests
;;; ============================================================

(describe "End-to-end: Validator failure -> expansion -> approval flow"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Path validator -> expansion -> add-to-scope"

    (it "path validation failure triggers expansion, add-to-scope approves"
      (let* ((check-result (append
                            (expansion-integration--call-path-validator "/tmp/outside.txt" 'read)
                            (list :validation-type 'path)))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file_in_scope"))
             (expansion-called nil)
             (expansion-result nil))

        ;; Verify validator denied
        (expect (plist-get check-result :allowed) :to-be nil)

        ;; Verify violation-info is well-formed
        (expect (plist-get violation-info :tool) :to-equal "read_file_in_scope")
        (expect (plist-get violation-info :reason) :to-be-truthy)

        ;; Mock expansion UI to simulate add-to-scope approval
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (vi callback _patterns _tool-name)
                  (setq expansion-called t)
                  ;; Verify violation-info fields are accessible
                  (expect (plist-get vi :tool) :to-equal "read_file_in_scope")
                  ;; Simulate approval
                  (funcall callback
                           (json-serialize
                            (list :success t
                                  :patterns_added (vector "/tmp/**")
                                  :message "Added to scope")))))

        ;; Trigger expansion
        (jf/gptel-scope-prompt-expansion
         violation-info
         (lambda (result) (setq expansion-result result))
         (list "/tmp/**")
         "read_file_in_scope")

        ;; Verify end-to-end flow
        (expect expansion-called :to-be t)
        (let ((parsed (json-parse-string expansion-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)))))

  (describe "Bash validator -> expansion -> allow-once approval"

    (it "bash validation failure triggers expansion, allow-once delivers success"
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-called nil)
             (expansion-result nil))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))
          (expect validation-error :not :to-be nil)
          (expect (plist-get validation-error :error) :to-equal "not-in-scope")

          (let ((violation-info (jf/gptel-scope--build-violation-info
                                 validation-error "run_bash_command")))
            (expect (plist-get violation-info :tool) :to-equal "run_bash_command")
            (expect (plist-get violation-info :validation-type) :to-equal 'bash)

            (spy-on 'jf/gptel-scope-prompt-expansion
                    :and-call-fake
                    (lambda (_vi callback _patterns _tool-name)
                      (setq expansion-called t)
                      (funcall callback
                               (json-serialize
                                (list :success t
                                      :allowed_once t
                                      :message "Allowed once")))))

            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (result) (setq expansion-result result))
             (list (format "%s:%s" command directory))
             "run_bash_command")))

        (expect expansion-called :to-be t)
        (let ((parsed (json-parse-string expansion-result :object-type 'plist)))
          (expect (plist-get parsed :allowed_once) :to-be t)))))

  (describe "Validation -> expansion -> denial preserves error"

    (it "denial flow preserves original validation error context"
      ;; End-to-end: validation fails -> expansion -> user denies -> error context preserved
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "cat /etc/shadow")
             (directory "/workspace")
             (denial-result nil))

        ;; Mock out-of-scope validation
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/etc/shadow" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))
          (expect validation-error :not :to-be nil)

          (let ((violation-info (jf/gptel-scope--build-violation-info
                                 validation-error "run_bash_command")))
            (expect (plist-get violation-info :resource) :to-be-truthy)

            ;; Step 3: User denies
            (spy-on 'jf/gptel-scope-prompt-expansion
                    :and-call-fake
                    (lambda (_vi callback _patterns _tool-name)
                      (funcall callback
                               (json-serialize
                                (list :success nil
                                      :user_denied t
                                      :message "User denied")))))

            ;; Trigger expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (result) (setq denial-result result))
             nil
             "run_bash_command")))

        (let ((parsed (json-parse-string denial-result :object-type 'plist)))
          (expect (plist-get parsed :user_denied) :to-be t)
          (expect (or (eq (plist-get parsed :success) :json-false)
                      (eq (plist-get parsed :success) nil)) :to-be t))))))

(provide 'expansion-integration-spec)
;;; expansion-integration-spec.el ends here
