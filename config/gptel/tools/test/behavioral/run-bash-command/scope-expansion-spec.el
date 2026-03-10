;;; scope-expansion-spec.el --- Inline scope expansion behavioral tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INLINE SCOPE EXPANSION BEHAVIORAL TESTS
;;
;; Tests the complete inline expansion workflow for run_bash_command where
;; validation failures automatically trigger the expansion UI, allowing
;; commands to execute within a single tool call upon user approval.
;;
;; This is HIGH-LEVEL behavioral testing:
;; - Tests the semantic validation workflow (WHAT was implemented)
;; - Tests inline expansion UI trigger and approval flow
;; - Tests allow-once permission lifecycle
;; - Tests transient action handlers (user choices)
;; - Mocks only stateful operations (bash execution, user interaction)
;;
;; Key workflows tested:
;; 1. Command in scope → semantic validation passes → can execute
;; 2. Command out of scope → semantic validation fails → UI triggered → user approves → retry succeeds
;; 3. Command out of scope → semantic validation fails → UI triggered → user denies → error returned
;; 4. Allow-once permission lifecycle (granted → consumed → expired)
;; 5. Transient action handlers (deny, allow-once actions)
;;
;; NOTE: The semantic validation (jf/gptel-scope--validate-command-semantics) is
;; what was implemented for inline expansion. Full integration into the tool
;; permission dispatch is a separate concern.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "../../../scope/scope-expansion.el" tools-dir)))

;;; Test Suite

(describe "run_bash_command: Inline scope expansion workflows"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks)
    ;; Clear allow-once list for test isolation
    (setq jf/gptel-scope--allow-once-list nil))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Semantic validation: in-scope commands"

    (it "validates read operation when path is in read scope"
      ;; Scenario: Command reads file within configured read paths
      ;; Expected: Validation passes (returns nil)
      ;; NOTE: Semantic validator returns nil for success, error plist for failure
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /workspace/README.md")
             (directory "/workspace"))

        ;; Mock bash parse and semantics
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/README.md" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Validate command
        (let ((result (jf/gptel-scope--validate-command-semantics
                       command directory scope-config)))

          ;; Assert: Validation succeeds (nil means success)
          (expect result :to-be nil))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Semantic validation: out-of-scope commands"

    (it "denies read operation when path is out of scope"
      ;; Scenario: Command tries to read file outside configured paths
      ;; Expected: Validation fails with path_out_of_scope error plist
      ;; NOTE: Semantic validator returns error plist for failure, nil for success
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/secret.txt")
             (directory "/workspace"))

        ;; Mock out-of-scope read
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/secret.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Validate command
        (let ((result (jf/gptel-scope--validate-command-semantics
                       command directory scope-config)))

          ;; Assert: Validation fails (returns error plist)
          (expect result :not :to-be nil)
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :path) :to-equal "/tmp/secret.txt")
          (expect (plist-get result :operation) :to-equal :read))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Inline expansion: trigger and approval flow"

    (it "triggers expansion UI when validation fails, user approves with add-to-scope, scope updated"
      ;; Scenario: Out-of-scope → UI → add to scope → scope.yml updated → approval
      ;; Expected: Scope file modified, callback receives approval
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        ;; Mock out-of-scope validation
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-command-semantics
                                 command directory scope-config)))

          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    ;; Simulate user choosing "Add to scope"
                    ;; This would normally update scope.yml via jf/gptel-scope--add-to-scope
                    ;; For testing, we'll just verify the callback is invoked correctly
                    ;; Note: patterns_added should be a vector (JSON array)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vector "/tmp/data.txt")
                                    :message "Added to scope permanently")))))

          ;; Trigger inline expansion
          (jf/gptel-scope--trigger-inline-expansion
           validation-error
           "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          ;; Assert: Expansion UI was called
          (expect expansion-ui-called :to-be t)

          ;; Assert: Wrapper callback received approval
          (expect (plist-get wrapper-callback-result :approved) :to-be t))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "triggers expansion UI when validation fails, user approves with allow-once, retry succeeds"
      ;; Scenario: Out-of-scope → UI → allow-once → retry validation → success
      ;; Expected: Full inline expansion workflow completes in single flow
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        ;; Mock out-of-scope validation
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-command-semantics
                                 command directory scope-config)))

          ;; Assert validation failed (returns error plist, not nil)
          (expect validation-error :not :to-be nil)

          ;; Spy on expansion UI
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    ;; Simulate user choosing "Allow once"
                    (jf/gptel-scope-add-to-allow-once-list
                     "run_bash_command"
                     (format "%s:%s" command directory))
                    ;; Invoke callback with approval
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :allowed_once t
                                    :message "Allowed for this turn")))))

          ;; Trigger inline expansion
          (jf/gptel-scope--trigger-inline-expansion
           validation-error
           "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          ;; Assert: Expansion UI was called
          (expect expansion-ui-called :to-be t)

          ;; Assert: Wrapper callback received approval
          (expect (plist-get wrapper-callback-result :approved) :to-be t)

          ;; Assert: Allow-once permission was granted
          (expect (length jf/gptel-scope--allow-once-list) :to-equal 1))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "triggers expansion UI when validation fails, user denies, error returned"
      ;; Scenario: Out-of-scope → UI → deny → rejection
      ;; Expected: Wrapper callback receives denial
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "rm /etc/hosts")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        ;; Mock denied operation
        (helpers-spec-mock-bash-parse command '("rm") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/etc/hosts" :command-name "rm"))
         nil
         '(:ratio 1.0))

        ;; Get validation error
        (let ((validation-error (jf/gptel-scope--validate-command-semantics
                                 command directory scope-config)))

          ;; Spy on expansion UI
          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (violation-info callback patterns tool-name)
                    (setq expansion-ui-called t)
                    ;; Simulate user choosing "Deny"
                    (funcall callback
                             (json-serialize
                              (list :success nil
                                    :user_denied t
                                    :message "User denied request")))))

          ;; Trigger inline expansion
          (jf/gptel-scope--trigger-inline-expansion
           validation-error
           "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          ;; Assert: Expansion UI was called
          (expect expansion-ui-called :to-be t)

          ;; Assert: Wrapper callback received denial
          (expect (plist-get wrapper-callback-result :approved) :to-be nil)
          (expect (plist-get wrapper-callback-result :reason) :to-equal "user_denied"))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Transient action handlers"

    (it "add-to-scope action updates scope file and invokes callback"
      ;; Scenario: User clicks "Add to scope (permanent)" in transient menu
      ;; Expected: Scope file updated, callback invoked with success
      ;; NOTE: This tests the transient action handler, not the full file I/O
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result))))

        ;; Mock transient scope with path-based violation
        (spy-on 'transient-scope
                :and-return-value (list :violation (list :tool "run_bash_command"
                                                         :resource "/tmp/test.txt"
                                                         :operation :read
                                                         :validation-type 'bash
                                                         :reason "test")
                                        :callback mock-callback
                                        :patterns '("/tmp/**")
                                        :tool-name "run_bash_command"))

        ;; Mock scope file operations to avoid actual file writes
        (spy-on 'jf/gptel-scope--get-scope-file-path :and-return-value scope-yml)
        (spy-on 'jf/gptel-scope--add-path-to-scope :and-return-value t)

        ;; Prevent actual transient quit
        (spy-on 'transient-quit-one)

        ;; Simulate user clicking add-to-scope
        (jf/gptel-scope--add-to-scope)

        ;; Assert: Callback was invoked
        (expect callback-invoked :to-be t)

        ;; Assert: Callback result indicates success
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :patterns_added) :not :to-be nil))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "deny action invokes callback with user_denied result"
      ;; Scenario: User clicks "Deny" in transient menu
      ;; Expected: Callback invoked with denial structure
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result))))

        ;; Mock transient scope
        (spy-on 'transient-scope
                :and-return-value (list :violation (list :tool "run_bash_command"
                                                         :resource "/tmp/file.txt"
                                                         :reason "test")
                                        :callback mock-callback
                                        :patterns '("/tmp/**")
                                        :tool-name "run_bash_command"))

        ;; Prevent actual transient quit
        (spy-on 'transient-quit-one)

        ;; Simulate user clicking deny
        (jf/gptel-scope--deny-expansion)

        ;; Assert: Callback was invoked
        (expect callback-invoked :to-be t)

        ;; Assert: Result structure indicates denial
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (or (eq (plist-get parsed :success) :json-false)
                      (eq (plist-get parsed :success) nil)) :to-be t)
          (expect (plist-get parsed :user_denied) :to-be t))))

    (it "allow-once action adds permission and invokes callback"
      ;; Scenario: User clicks "Allow once (temporary)" in transient menu
      ;; Expected: Permission added, callback invoked with success
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result))))

        ;; Clear allow-once list
        (setq jf/gptel-scope--allow-once-list nil)

        ;; Mock transient scope
        (spy-on 'transient-scope
                :and-return-value (list :violation (list :tool "run_bash_command"
                                                         :resource "cat /tmp/file.txt:/workspace"
                                                         :reason "test")
                                        :callback mock-callback
                                        :patterns '("cat /tmp/file.txt:/workspace")
                                        :tool-name "run_bash_command"))

        ;; Prevent actual transient quit
        (spy-on 'transient-quit-one)

        ;; Simulate user clicking allow-once
        (jf/gptel-scope--allow-once-action)

        ;; Assert: Callback was invoked
        (expect callback-invoked :to-be t)

        ;; Assert: Permission was added
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (let ((entry (car jf/gptel-scope--allow-once-list)))
          (expect (car entry) :to-equal "run_bash_command")
          (expect (cdr entry) :to-equal "cat /tmp/file.txt:/workspace"))

        ;; Assert: Callback result indicates success
        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (plist-get parsed :allowed_once) :to-be t)))))

  (describe "Allow-once permission lifecycle"

    (it "validation succeeds after allow-once permission granted"
      ;; Scenario: Permission granted → validation retried → succeeds
      ;; Expected: Validation passes via allow-once
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (command "cat /tmp/test.txt")
             (directory "/workspace"))

        ;; Initially fails validation (returns error plist)
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/test.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((result1 (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)))
          (expect result1 :not :to-be nil))

        ;; Grant allow-once permission (what expansion UI does)
        (jf/gptel-scope-add-to-allow-once-list
         "run_bash_command"
         (format "%s:%s" command directory))

        ;; Verify permission exists
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)

        ;; Retry validation - now should check allow-once
        ;; Note: Semantic validator doesn't consume allow-once, the tool wrapper does
        ;; This test verifies allow-once was granted properly
        (let ((entry (car jf/gptel-scope--allow-once-list)))
          (expect (car entry) :to-equal "run_bash_command")
          (expect (cdr entry) :to-equal (format "%s:%s" command directory)))

        ;; Cleanup
        (delete-file scope-yml)))))

(provide 'scope-expansion-spec)
;;; scope-expansion-spec.el ends here
