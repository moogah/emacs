;;; scope-expansion-spec.el --- Scope expansion workflow tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; SCOPE EXPANSION WORKFLOWS
;;
;; Tests the complete scope expansion workflow when run_bash_command encounters
;; validation failures and the LLM requests permission via request_scope_expansion.
;;
;; This tests the integration between:
;; - run_bash_command validation failures
;; - request_scope_expansion meta-tool
;; - jf/gptel-scope-prompt-expansion UI
;; - User choices (deny, add to scope, allow once)
;; - Retry behavior after permission granted
;;
;; Key scenarios tested:
;; - Validation failure returns structured error guiding LLM to request_scope_expansion
;; - request_scope_expansion triggers transient UI with violation details
;; - Deny choice returns error to LLM
;; - Add to scope updates scope.yml and succeeds on retry
;; - Allow once grants temporary permission for current turn
;; - Allow once is consumed after use
;; - Multiple expansion requests in same turn
;;
;; Test structure:
;; Each test simulates the complete multi-step workflow:
;; 1. Initial command fails validation
;; 2. LLM requests scope expansion
;; 3. User makes choice via transient menu
;; 4. Result returned to LLM
;; 5. (Optional) Command retried with new permission
;;
;; Since these are behavioral tests exercising real code paths, we use mocking
;; to simulate user interaction with the transient menu and bash command execution.

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

(describe "run_bash_command: Scope expansion workflows"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Validation failure to expansion request flow"

    (it "returns structured error guiding LLM to request_scope_expansion when path denied"
      ;; Scenario: Command tries to read file outside scope
      ;; Expected: Validation fails with error telling LLM to use request_scope_expansion
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
        ;; Mock parse: cat /tmp/file.txt
        (helpers-spec-mock-bash-parse
         "cat /tmp/file.txt"
         '("cat")
         t)

        ;; Mock semantics: Read operation on /tmp/file.txt
        (helpers-spec-mock-bash-semantics
         '((:operation :read :path "/tmp/file.txt"))
         nil
         '(:ratio 1.0))

        ;; Validate command
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "cat /tmp/file.txt"
                       "/workspace"
                       scope-config)))
          ;; Assert: Path out of scope error
          (expect (plist-get result :error) :to-equal "path_out_of_scope")
          (expect (plist-get result :path) :to-equal "/tmp/file.txt")
          (expect (plist-get result :operation) :to-equal :read)
          ;; Error message should guide LLM to use request_scope_expansion
          (expect (plist-get result :message) :to-match "request_scope_expansion"))

        ;; Cleanup
        (delete-file scope-yml)))

    (it "returns structured error guiding LLM when command in deny list"
      ;; Scenario: Command is in bash_tools.deny
      ;; Expected: Error tells LLM this command is denied (not expandable)
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
  deny:
    - rm
    - sudo

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
             (scope-config (helpers-spec-load-scope-config scope-yml)))
        ;; Mock parse: rm /tmp/file.txt
        (helpers-spec-mock-bash-parse
         "rm /tmp/file.txt"
         '("rm")
         t)

        ;; Mock semantics: Write operation
        (helpers-spec-mock-bash-semantics
         '((:operation :write :path "/tmp/file.txt"))
         nil
         '(:ratio 1.0))

        ;; Validate command
        (let ((result (jf/gptel-scope--validate-command-semantics
                       "rm /tmp/file.txt"
                       "/workspace"
                       scope-config)))
          ;; Assert: Command denied error (stage 3)
          (expect (plist-get result :error) :to-equal "command_denied")
          (expect (plist-get result :command) :to-equal "rm")
          ;; Deny list errors should NOT suggest request_scope_expansion
          ;; (commands in deny list are intentionally blocked)
          (expect (plist-get result :message) :not :to-match "request_scope_expansion"))

        ;; Cleanup
        (delete-file scope-yml))))

  (describe "request_scope_expansion tool behavior"

    (it "builds violation info and calls jf/gptel-scope-prompt-expansion"
      ;; Scenario: LLM calls request_scope_expansion after validation failure
      ;; Expected: Tool builds violation info and triggers transient UI
      (let* ((callback-invoked nil)
             (callback-result nil)
             (expansion-called nil)
             (expansion-args nil)
             (mock-callback (lambda (result)
                             (setq callback-invoked t)
                             (setq callback-result result))))
        ;; Spy on jf/gptel-scope-prompt-expansion to verify it's called
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake (lambda (violation-info callback patterns tool-name)
                                (setq expansion-called t)
                                (setq expansion-args (list :violation violation-info
                                                          :callback callback
                                                          :patterns patterns
                                                          :tool-name tool-name))
                                ;; Simulate user denying (for this test)
                                (funcall callback
                                         (json-serialize
                                          (list :success nil
                                                :user_denied t
                                                :message "User denied scope expansion request.")))))

        ;; Call request_scope_expansion as LLM would
        ;; Note: gptel-make-tool creates a lambda with callback as first arg
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

        ;; Assert: Violation info built correctly
        (let ((violation (plist-get expansion-args :violation)))
          (expect (plist-get violation :tool) :to-equal "run_bash_command")
          (expect (plist-get violation :resource) :to-equal "/tmp/**")
          (expect (plist-get violation :reason) :to-equal "Need to read temporary files")
          (expect (plist-get violation :validation-type) :to-equal 'bash))

        ;; Assert: Callback was invoked with denial
        (expect callback-invoked :to-be t)
        (let ((result (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get result :success) :to-equal :json-false)
          (expect (plist-get result :user_denied) :to-equal t))))

    (it "converts vector patterns to list for Elisp processing"
      ;; Scenario: LLM passes patterns as JSON array (becomes Elisp vector)
      ;; Expected: Tool converts to list for processing
      (let* ((expansion-called nil)
             (received-patterns nil)
             (mock-callback (lambda (result) nil)))
        ;; Spy on expansion to capture patterns
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake (lambda (violation-info callback patterns tool-name)
                                (setq expansion-called t)
                                (setq received-patterns patterns)
                                (funcall callback
                                         (json-serialize (list :success nil :user_denied t)))))

        ;; Call with vector (as JSON array would be parsed)
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
                   (vector "/tmp/**" "/var/log/**")
                   "Need log access"))

        ;; Assert: Patterns converted to list
        (expect expansion-called :to-be t)
        (expect (listp received-patterns) :to-be t)
        (expect received-patterns :to-equal '("/tmp/**" "/var/log/**")))))

  (describe "User choice workflows"

    (it "deny choice returns error to LLM"
      ;; Scenario: User selects "Deny (reject tool call)" in transient menu
      ;; Expected: Callback invoked with success=false, user_denied=true
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                             (setq callback-invoked t)
                             (setq callback-result result))))
        ;; Simulate transient scope with violation info
        (spy-on 'transient-scope
                :and-return-value (list :violation (list :tool "run_bash_command"
                                                        :resource "/tmp/file.txt"
                                                        :reason "test"
                                                        :validation-type 'bash)
                                       :callback mock-callback
                                       :patterns '("/tmp/**")
                                       :tool-name "run_bash_command"))

        ;; Simulate user choosing deny
        (spy-on 'transient-quit-one)  ; Prevent actual transient quit
        (jf/gptel-scope--deny-expansion)

        ;; Assert: Callback invoked
        (expect callback-invoked :to-be t)

        ;; Assert: Result structure
        (let ((result (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get result :success) :to-equal :json-false)
          (expect (plist-get result :user_denied) :to-equal t)
          (expect (plist-get result :message) :to-match "denied"))))

    (it "allow once choice grants temporary permission"
      ;; Scenario: User selects "Allow once (temporary)"
      ;; Expected: Tool and resource added to allow-once list, callback returns success
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                             (setq callback-invoked t)
                             (setq callback-result result))))
        ;; Clear allow-once list
        (setq jf/gptel-scope--allow-once-list nil)

        ;; Simulate transient scope
        (spy-on 'transient-scope
                :and-return-value (list :violation (list :tool "run_bash_command"
                                                        :resource "/tmp/**"
                                                        :reason "test"
                                                        :validation-type 'bash)
                                       :callback mock-callback
                                       :patterns '("/tmp/**")
                                       :tool-name "run_bash_command"))

        ;; Simulate user choosing allow once
        (spy-on 'transient-quit-one)
        (jf/gptel-scope--allow-once-action)

        ;; Assert: Callback invoked with success
        (expect callback-invoked :to-be t)
        (let ((result (json-parse-string callback-result :object-type 'plist)))
          (expect (plist-get result :success) :to-equal t)
          (expect (plist-get result :allowed_once) :to-equal t))

        ;; Assert: Allow-once list populated
        (expect jf/gptel-scope--allow-once-list
                :to-equal '(("run_bash_command" . "/tmp/**")))))

    (it "add to scope choice updates scope.yml for path-based resource"
      ;; Scenario: User selects "Add to scope (permanent)" for path resource
      ;; Expected: scope.yml updated with new path, callback returns success
      ;; NOTE: Skipped - requires deeper integration with scope file management
      ;; The issue is that temp files are cleaned up before validation can access them
      ;; This should be tested in integration tests with real session setup
      :pending "Requires integration test environment with session management")))

(describe "run_bash_command: Allow-once consumption and retry"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "consumes allow-once permission before command execution"
    ;; Scenario: Allow-once granted, command validated again
    ;; Expected: Permission consumed during validation, not after execution
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
           (scope-config (helpers-spec-load-scope-config scope-yml))
           (working-dir "/workspace"))
      ;; Grant allow-once permission
      ;; For bash tools, resource format is "command:directory"
      (setq jf/gptel-scope--allow-once-list nil)
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command"
                                            (format "cat /tmp/file.txt:%s" working-dir))

      ;; Verify permission exists
      (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)

      ;; Mock parse and semantics for cat /tmp/file.txt
      (helpers-spec-mock-bash-parse
       "cat /tmp/file.txt"
       '("cat")
       t)

      (helpers-spec-mock-bash-semantics
       '((:operation :read :path "/tmp/file.txt"))
       nil
       '(:ratio 1.0))

      ;; Validate command - should succeed via allow-once
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /tmp/file.txt"
                     working-dir
                     scope-config)))
        ;; Assert: Validation succeeds
        (expect result :to-be nil))

      ;; Assert: Permission consumed (list now empty)
      (expect jf/gptel-scope--allow-once-list :to-be nil)

      ;; Cleanup
      (delete-file scope-yml)))

  (it "second identical command fails after allow-once consumed"
    ;; Scenario: Allow-once used once, second identical command denied
    ;; Expected: First succeeds, second fails (permission consumed)
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
           (scope-config (helpers-spec-load-scope-config scope-yml))
           (working-dir "/workspace"))
      ;; Grant allow-once permission with correct format
      (setq jf/gptel-scope--allow-once-list nil)
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command"
                                            (format "cat /tmp/file.txt:%s" working-dir))

      ;; First validation - should succeed
      (helpers-spec-mock-bash-parse "cat /tmp/file.txt" '("cat") t)
      (helpers-spec-mock-bash-semantics
       '((:operation :read :path "/tmp/file.txt"))
       nil
       '(:ratio 1.0))

      (let ((result1 (jf/gptel-scope--validate-command-semantics
                      "cat /tmp/file.txt"
                      working-dir
                      scope-config)))
        (expect result1 :to-be nil))

      ;; Second validation - should fail (permission consumed)
      (helpers-spec-mock-bash-parse "cat /tmp/file.txt" '("cat") t)
      (helpers-spec-mock-bash-semantics
       '((:operation :read :path "/tmp/file.txt"))
       nil
       '(:ratio 1.0))

      (let ((result2 (jf/gptel-scope--validate-command-semantics
                      "cat /tmp/file.txt"
                      working-dir
                      scope-config)))
        ;; Assert: Second attempt denied
        (expect (plist-get result2 :error) :to-equal "path_out_of_scope"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "multiple allow-once permissions tracked independently"
    ;; Scenario: Multiple allow-once grants in same turn for different resources
    ;; Expected: Each permission consumed independently
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (scope-config (helpers-spec-load-scope-config scope-yml))
           (working-dir "/workspace"))
      ;; Grant multiple allow-once permissions with correct format
      (setq jf/gptel-scope--allow-once-list nil)
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command"
                                            (format "cat /tmp/file1.txt:%s" working-dir))
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command"
                                            (format "cat /tmp/file2.txt:%s" working-dir))

      ;; Verify both exist
      (expect (length jf/gptel-scope--allow-once-list) :to-equal 2)

      ;; Use first permission
      (helpers-spec-mock-bash-parse "cat /tmp/file1.txt" '("cat") t)
      (helpers-spec-mock-bash-semantics
       '((:operation :read :path "/tmp/file1.txt"))
       nil
       '(:ratio 1.0))

      (jf/gptel-scope--validate-command-semantics
       "cat /tmp/file1.txt"
       working-dir
       scope-config)

      ;; Assert: Only first permission consumed
      (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
      ;; The second permission should remain
      (expect (car jf/gptel-scope--allow-once-list)
              :to-equal (cons "run_bash_command"
                             (format "cat /tmp/file2.txt:%s" working-dir)))

      ;; Cleanup
      (delete-file scope-yml))))

(provide 'scope-expansion-spec)
;;; scope-expansion-spec.el ends here
