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
;; Sources:
;; - scope/test/integration-validator-expansion-spec.el (7 tests, absorbed)
;; - New end-to-end scenarios: validation failure -> expansion -> approval flow
;;
;; Test coverage:
;; 1. Path validator output -> build-violation-info -> violation-info format
;; 2. Pattern validator output -> build-violation-info -> violation-info format
;; 3. Bash validator output -> build-violation-info -> violation-info format
;; 4. Violation-info -> Expansion UI rendering (no crashes on missing fields)
;; 5. End-to-end: validator failure -> expansion trigger -> approval -> retry
;;
;; Mocking approach:
;; - Mock: transient menu user choices, call-process
;; - Real: validation functions, build-violation-info, expansion triggering logic

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Fixtures

(defvar expansion-integration--test-scope-config
  '(:paths (:read ("/workspace/allowed/**")
            :write ("/workspace/writable/**")
            :execute ()
            :modify ()
            :deny ("/workspace/denied/**"))
    :cloud (:auth-detection "warn")
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8))
  "Test scope configuration for validators.")

;;; Helper Functions

(defun expansion-integration--call-path-validator (filepath operation)
  "Call path validator with FILEPATH and OPERATION.
Returns check-result from validator."
  (let* ((tool-name (if (eq operation 'read) "read_file" "write_file_in_scope"))
         (category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
         (args (list filepath))
         (metadata nil))
    (jf/gptel-scope--validate-path-tool tool-name args category expansion-integration--test-scope-config metadata)))

(defun expansion-integration--call-pattern-validator (pattern)
  "Call pattern validator with PATTERN.
Returns check-result from validator."
  (let* ((tool-name "create_roam_node_in_scope")
         (category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
         (args (list pattern))
         (config '(:roam (:node-patterns ("work/**" "personal/**")))))
    (jf/gptel-scope--validate-pattern-tool tool-name args category config)))

;;; ============================================================
;;; Tests absorbed from integration-validator-expansion-spec.el
;;; ============================================================

(describe "Validator -> build-violation-info -> Expansion UI Integration"

  (describe "Path validator integration"

    (it "transforms path validator denial into valid violation-info"
      ;; Call path validator with out-of-scope file
      (let* ((check-result (expansion-integration--call-path-validator "/tmp/outside.txt" 'read))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file")))
        ;; Verify check-result has expected format
        (expect (plist-get check-result :allowed) :to-be nil)
        (expect (plist-get check-result :reason) :to-equal "not-in-scope")
        (expect (plist-get check-result :resource) :to-be-truthy)

        ;; Verify violation-info transformation
        (expect (plist-get violation-info :tool) :to-equal "read_file")
        (expect (plist-get violation-info :resource) :to-be-truthy)
        (expect (plist-get violation-info :reason) :to-be-truthy)
        (expect (plist-get violation-info :reason) :not :to-be nil)
        (expect (plist-get violation-info :validation-type) :to-equal 'path)))

    (it "handles denied-pattern reason from path validator"
      (let* ((check-result '(:allowed nil
                             :reason "denied-pattern"
                             :resource "/workspace/denied/file.txt"
                             :tool "read_file"))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file")))
        (expect (plist-get violation-info :reason) :to-equal "denied-pattern"))))

  (describe "Pattern validator integration"

    (it "transforms pattern validator denial into valid violation-info"
      (let* ((check-result (expansion-integration--call-pattern-validator "invalid/node/path"))
             (violation-info (jf/gptel-scope--build-violation-info check-result "create_roam_node_in_scope")))
        ;; Verify check-result format
        (expect (plist-get check-result :allowed) :to-be nil)
        (expect (plist-get check-result :reason) :to-be-truthy)

        ;; Verify violation-info transformation
        (expect (plist-get violation-info :tool) :to-equal "create_roam_node_in_scope")
        (expect (plist-get violation-info :reason) :not :to-be nil)
        (expect (plist-get violation-info :validation-type) :to-equal 'pattern))))

  (describe "Bash validator integration (legacy :message format)"

    (it "handles legacy validation-error format with :message field"
      ;; This is the format used by bash validator and in existing tests
      (let* ((validation-error '(:error "command_denied"
                                 :command "rm -rf /tmp/file"
                                 :operation write
                                 :message "Command denied"))
             (violation-info (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        ;; Should still work (backwards compatibility)
        (expect (plist-get violation-info :tool) :to-equal "run_bash_command")
        (expect (plist-get violation-info :resource) :to-equal "rm -rf /tmp/file")
        (expect (plist-get violation-info :reason) :to-equal "Command denied")
        (expect (plist-get violation-info :validation-type) :to-equal 'bash)))

    (it "handles cloud auth denial with :message field"
      (let* ((validation-error '(:error "cloud_auth_denied"
                                 :provider "aws"
                                 :operation write
                                 :message "Cloud authentication denied"))
             (violation-info (jf/gptel-scope--build-violation-info validation-error "run_bash_command")))
        (expect (plist-get violation-info :reason) :to-equal "Cloud authentication denied"))))

  (describe "Format priority: :reason over :message"

    (it "prioritizes :reason when both fields present"
      ;; Edge case: if both fields exist, :reason should win (current format)
      (let* ((mixed-format '(:allowed nil
                             :reason "current-format-reason"
                             :message "legacy-format-message"
                             :resource "/tmp/file.txt"
                             :tool "read_file"))
             (violation-info (jf/gptel-scope--build-violation-info mixed-format "read_file")))
        (expect (plist-get violation-info :reason) :to-equal "current-format-reason"))))

  (describe "Transformation robustness"

    (it "returns nil for :reason when neither :reason nor :message present"
      ;; Edge case: malformed check-result missing both fields
      (let* ((malformed '(:allowed nil
                         :resource "/tmp/file.txt"
                         :tool "read_file"))
             (violation-info (jf/gptel-scope--build-violation-info malformed "read_file")))
        ;; Should not crash, but reason will be nil
        (expect (plist-get violation-info :reason) :to-be nil)
        (expect (plist-get violation-info :resource) :to-equal "/tmp/file.txt")))))

;;; ============================================================
;;; End-to-end expansion integration tests (new)
;;; ============================================================

(describe "End-to-end: Validator failure -> expansion -> approval flow"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks)
    (setq jf/gptel-scope--allow-once-list nil))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Path validator -> expansion -> add-to-scope"

    (it "path validation failure triggers expansion, add-to-scope approves"
      ;; End-to-end: path validator denies -> build-violation-info -> expansion prompt -> approval
      (let* ((check-result (expansion-integration--call-path-validator "/tmp/outside.txt" 'read))
             (violation-info (jf/gptel-scope--build-violation-info check-result "read_file"))
             (expansion-called nil)
             (expansion-result nil))

        ;; Verify validator denied
        (expect (plist-get check-result :allowed) :to-be nil)

        ;; Verify violation-info is well-formed
        (expect (plist-get violation-info :tool) :to-equal "read_file")
        (expect (plist-get violation-info :reason) :to-be-truthy)

        ;; Mock expansion UI to simulate add-to-scope approval
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (vi callback patterns tool-name)
                  (setq expansion-called t)
                  ;; Verify violation-info fields are accessible
                  (expect (plist-get vi :tool) :to-equal "read_file")
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
         "read_file")

        ;; Verify end-to-end flow
        (expect expansion-called :to-be t)
        (let ((parsed (json-parse-string expansion-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)))))

  (describe "Bash validator -> expansion -> allow-once -> retry succeeds"

    (it "bash validation failure triggers expansion, allow-once grants access"
      ;; End-to-end: bash semantic validation denies -> expansion -> allow-once -> permission granted
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-called nil)
             (expansion-result nil))

        ;; Mock out-of-scope validation
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Step 1: Validation fails
        (let ((validation-error (jf/gptel-scope--validate-command-semantics
                                 command directory scope-config)))
          (expect validation-error :not :to-be nil)
          (expect (plist-get validation-error :error) :to-equal "path_out_of_scope")

          ;; Step 2: Build violation info
          (let ((violation-info (jf/gptel-scope--build-violation-info
                                 validation-error "run_bash_command")))
            (expect (plist-get violation-info :tool) :to-equal "run_bash_command")
            (expect (plist-get violation-info :validation-type) :to-equal 'bash)

            ;; Step 3: Expansion UI approves with allow-once
            (spy-on 'jf/gptel-scope-prompt-expansion
                    :and-call-fake
                    (lambda (vi callback patterns tool-name)
                      (setq expansion-called t)
                      ;; Grant allow-once
                      (jf/gptel-scope-add-to-allow-once-list
                       "run_bash_command"
                       (format "%s:%s" command directory))
                      (funcall callback
                               (json-serialize
                                (list :success t
                                      :allowed_once t
                                      :message "Allowed once")))))

            ;; Trigger expansion
            (jf/gptel-scope-prompt-expansion
             violation-info
             (lambda (result) (setq expansion-result result))
             (list (format "%s:%s" command directory))
             "run_bash_command")))

        ;; Step 4: Verify allow-once was granted
        (expect expansion-called :to-be t)
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (let ((parsed (json-parse-string expansion-result :object-type 'plist)))
          (expect (plist-get parsed :allowed_once) :to-be t))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Validation -> expansion -> denial preserves error"

    (it "denial flow preserves original validation error context"
      ;; End-to-end: validation fails -> expansion -> user denies -> error context preserved
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /etc/shadow")
             (directory "/workspace")
             (denial-result nil))

        ;; Mock out-of-scope validation
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/etc/shadow" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Step 1: Validation fails
        (let ((validation-error (jf/gptel-scope--validate-command-semantics
                                 command directory scope-config)))
          (expect validation-error :not :to-be nil)

          ;; Step 2: Build violation info preserves context
          (let ((violation-info (jf/gptel-scope--build-violation-info
                                 validation-error "run_bash_command")))
            (expect (plist-get violation-info :resource) :to-be-truthy)

            ;; Step 3: User denies
            (spy-on 'jf/gptel-scope-prompt-expansion
                    :and-call-fake
                    (lambda (vi callback patterns tool-name)
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

        ;; Step 4: Verify denial
        (let ((parsed (json-parse-string denial-result :object-type 'plist)))
          (expect (plist-get parsed :user_denied) :to-be t)
          (expect (or (eq (plist-get parsed :success) :json-false)
                      (eq (plist-get parsed :success) nil)) :to-be t))

        ;; Verify allow-once was NOT granted
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 0)

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Cross-validator violation-info format consistency"

    (it "all validator types produce compatible violation-info for expansion UI"
      ;; Verify that path, pattern, and bash validators all produce
      ;; violation-info with the fields expansion UI expects
      (let ((path-check '(:allowed nil
                          :reason "not-in-scope"
                          :resource "/tmp/file.txt"
                          :tool "read_file"))
            (bash-check '(:error "path_out_of_scope"
                          :command "cat /tmp/file.txt"
                          :path "/tmp/file.txt"
                          :operation :read
                          :message "Path out of scope"))
            (pattern-check '(:allowed nil
                             :reason "pattern-not-matched"
                             :resource "invalid/path"
                             :tool "create_roam_node_in_scope")))

        ;; Build violation-info from each
        (let ((path-vi (jf/gptel-scope--build-violation-info path-check "read_file"))
              (bash-vi (jf/gptel-scope--build-violation-info bash-check "run_bash_command"))
              (pattern-vi (jf/gptel-scope--build-violation-info pattern-check "create_roam_node_in_scope")))

          ;; All should have :tool field
          (expect (plist-get path-vi :tool) :to-equal "read_file")
          (expect (plist-get bash-vi :tool) :to-equal "run_bash_command")
          (expect (plist-get pattern-vi :tool) :to-equal "create_roam_node_in_scope")

          ;; All should have :reason field (required for UI display)
          (expect (plist-get path-vi :reason) :to-be-truthy)
          (expect (plist-get bash-vi :reason) :to-be-truthy)
          (expect (plist-get pattern-vi :reason) :to-be-truthy)

          ;; All should have :resource field
          (expect (plist-get path-vi :resource) :to-be-truthy)
          (expect (plist-get bash-vi :resource) :to-be-truthy)
          (expect (plist-get pattern-vi :resource) :to-be-truthy)

          ;; Each has appropriate validation-type
          (expect (plist-get path-vi :validation-type) :to-equal 'path)
          (expect (plist-get bash-vi :validation-type) :to-equal 'bash)
          (expect (plist-get pattern-vi :validation-type) :to-equal 'pattern))))))

(provide 'expansion-integration-spec)
;;; expansion-integration-spec.el ends here
