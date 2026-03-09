;;; inline-expansion-spec.el --- Inline scope expansion behavioral tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INLINE SCOPE EXPANSION BEHAVIORAL TESTS
;;
;; Tests the inline expansion workflow where run_bash_command automatically
;; triggers the expansion UI when validation fails, eliminating the need for
;; LLMs to explicitly call request_scope_expansion.
;;
;; This tests the integration between:
;; - run_bash_command validation failures
;; - Automatic inline expansion UI trigger
;; - User choices (deny, add to scope, allow once)
;; - Automatic retry after permission granted
;; - Command execution in same tool call
;;
;; Key scenarios tested:
;; - Validation failure automatically triggers transient UI
;; - Deny choice returns error to LLM immediately
;; - Allow once grants temporary permission and executes command
;; - Add to scope updates scope.yml and executes command
;; - Allow once consumed after successful execution
;; - request_scope_expansion still works for pre-emptive requests
;;
;; Test structure:
;; Each test simulates the complete inline expansion workflow:
;; 1. Command fails validation
;; 2. Expansion UI triggered automatically (mocked)
;; 3. User makes choice via transient menu (mocked)
;; 4. Permission applied
;; 5. Command executed (if approved)
;; 6. Result returned to LLM in single tool call
;;
;; Since these are behavioral tests, we mock the transient UI and bash
;; execution to test the workflow without user interaction or real commands.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "../../../scope/scope-expansion.el" tools-dir)))

;;; Test Suite

(describe "run_bash_command: Inline scope expansion"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Validation failure behavior"

    (it "returns path_out_of_scope error when path not in scope"
      ;; Scenario: Command tries to access out-of-scope path
      ;; Expected: Validation fails with path_out_of_scope error
      ;; NOTE: Inline expansion is triggered by gptel-make-scoped-tool (:async t), not validation
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))

        ;; Mock bash parse: cat /tmp/file.txt
        (helpers-spec-mock-bash-parse
         "cat /tmp/file.txt"
         '("cat")
         t)

        ;; Mock semantics: Read operation on /tmp/file.txt
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/file.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Validate command - should fail with path_out_of_scope
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /tmp/file.txt"
                       "/workspace"
                       scope-config)))
          ;; Assert: Validation fails with path_out_of_scope
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :path) :to-equal "/tmp/file.txt")
          (expect (plist-get result :operation) :to-equal :read)
          ;; Inline expansion triggered by gptel-make-scoped-tool when async=t
          ;; Validation just returns error, tool wrapper handles expansion)

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Allow-once permission flow"

    (it "validation succeeds when allow-once permission granted"
      ;; Scenario: User approves with "Allow once", then command validated
      ;; Expected: Validation succeeds and permission consumed
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (working-dir "/workspace")
             (command "cat /tmp/file.txt"))

        ;; Clear allow-once list
        (setq jf/gptel-scope--allow-once-list nil)

        ;; Mock bash parse and semantics
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/file.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; First validation fails
        (let ((result1 (jf/gptel-scope--validate-command-semantics
                        command working-dir scope-config)))
          (expect (plist-get result1 :error) :to-equal "path_out_of_scope"))

        ;; Simulate user granting allow-once permission
        ;; Format matches what allow-once validator checks: "command:directory"
        (jf/gptel-scope-add-to-allow-once-list
         "run_bash_command"
         (format "%s:%s" command working-dir))

        ;; Verify permission added
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)

        ;; After allow-once granted, validation should succeed
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/file.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((result2 (jf/gptel-scope--validate-command-semantics
                        command working-dir scope-config)))
          ;; Assert: Validation succeeds after allow-once
          (expect result2 :to-be nil))

        ;; Verify allow-once was consumed
        (expect jf/gptel-scope--allow-once-list :to-be nil)

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Error context preservation"

    (it "provides detailed error info for path_out_of_scope"
      ;; Scenario: Validation fails for out-of-scope path
      ;; Expected: Error includes path, operation, and helpful message
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (working-dir "/workspace"))

        ;; Mock out-of-scope read operation
        (helpers-spec-mock-bash-parse "cat /tmp/file.txt" '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/file.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Trigger validation failure
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /tmp/file.txt"
                       working-dir
                       scope-config)))
          ;; Assert: Error provides useful context
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :path) :to-equal "/tmp/file.txt")
          (expect (plist-get result :operation) :to-equal :read))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "provides detailed error info for path_denied"
      ;; Scenario: Validation fails for explicitly denied path
      ;; Expected: Error includes path, operation, and denial message
      (let* ((scope-yml (helpers-spec-make-scope-yml
                         "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny:
    - \"/etc/**\"

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (working-dir "/workspace"))

        ;; Mock denied write operation
        (helpers-spec-mock-bash-parse "touch /etc/test.txt" '("touch") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/etc/test.txt" :command-name "touch"))
         nil
         '(:ratio 1.0))

        ;; Trigger validation failure
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "touch /etc/test.txt"
                       working-dir
                       scope-config)))
          ;; Assert: Error provides useful context
          (expect (plist-get result :error) :to-equal "path_denied")
          (expect (plist-get result :path) :to-equal "/etc/test.txt")
          (expect (plist-get result :operation) :to-equal :write)
          (expect (plist-get result :message) :to-match "denied"))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "Backward compatibility"

    (it "request_scope_expansion still works for pre-emptive requests"
      ;; Scenario: LLM proactively calls request_scope_expansion before tool call
      ;; Expected: Expansion UI triggered, approval flows work
      (let* ((expansion-called nil)
             (approval-result nil)
             (mock-callback (lambda (result)
                             (setq approval-result (json-parse-string result :object-type 'plist)))))

        ;; Clear allow-once list
        (setq jf/gptel-scope--allow-once-list nil)

        ;; Spy on expansion UI
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake (lambda (violation callback patterns tool-name)
                                (setq expansion-called t)
                                ;; Simulate allow-once approval
                                (jf/gptel-scope-add-to-allow-once-list tool-name (car patterns))
                                (funcall callback
                                         (json-serialize
                                          (list :success t
                                                :allowed_once t
                                                :message "Permission granted.")))))

        ;; Simulate request_scope_expansion tool call
        (let ((tool-fn (lambda (callback tool_name patterns justification)
                        (when (vectorp patterns)
                          (setq patterns (append patterns nil)))
                        (let* ((violation-info
                                (list :tool tool_name
                                      :resource (car patterns)
                                      :reason justification
                                      :validation-type (jf/gptel-scope--infer-validation-type tool_name)
                                      :patterns patterns)))
                          (jf/gptel-scope-prompt-expansion violation-info callback patterns tool_name)))))
          (funcall tool-fn
                   mock-callback
                   "run_bash_command"
                   (vector "/tmp/**")
                   "Need to read temporary files"))

        ;; Assert: Expansion UI was called
        (expect expansion-called :to-be t)

        ;; Assert: Approval succeeded
        (expect (plist-get approval-result :success) :to-equal t)
        (expect (plist-get approval-result :allowed_once) :to-equal t)

        ;; Assert: Allow-once permission granted
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (let ((entry (car jf/gptel-scope--allow-once-list)))
          (expect (car entry) :to-equal "run_bash_command")
          (expect (cdr entry) :to-equal "/tmp/**"))))

    (it "allow-once from request_scope_expansion enables subsequent run_bash_command"
      ;; Scenario: LLM calls request_scope_expansion, gets allow-once, then calls run_bash_command
      ;; Expected: Command succeeds via allow-once permission
      (let* ((scope-yml (helpers-spec-make-minimal-scope))
             (scope-config (helpers-spec-load-scope-config scope-yml))
             (working-dir "/workspace")
             (command "cat /tmp/file.txt"))

        ;; Grant allow-once via request_scope_expansion (simulated)
        (jf/gptel-scope-add-to-allow-once-list
         "run_bash_command"
         (format "%s:%s" command working-dir))

        ;; Verify permission exists
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)

        ;; Mock bash parse and semantics
        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/file.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        ;; Validate command - should succeed via allow-once
        (let ((result (jf/gptel-scope--validate-command-semantics
                       command
                       working-dir
                       scope-config)))
          ;; Assert: Validation succeeds
          (expect result :to-be nil))

        ;; Assert: Permission consumed
        (expect jf/gptel-scope--allow-once-list :to-be nil)

        ;; Cleanup
        (delete-file scope-yml))))))

(provide 'inline-expansion-spec)
;;; inline-expansion-spec.el ends here
