;;; e2e-async-tool-expansion-spec.el --- End-to-end async tool with inline expansion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; END-TO-END INTEGRATION TEST: Async Tool with Inline Expansion
;;
;; This test exercises the COMPLETE flow from async tool call through
;; inline expansion and back to tool execution, using minimal mocking.
;;
;; WHAT THIS TESTS:
;; 1. Real async tool created with gptel-make-scoped-tool macro
;; 2. Complete callback chain: tool → validation → expansion UI → retry → execution
;; 3. Config reload before retry validation
;; 4. Lexical binding closure preservation through callback chain
;; 5. Final tool execution after approval
;;
;; WHAT THIS MOCKS:
;; 1. File I/O (scope.yml read/write) - no disk access
;; 2. Transient UI - auto-approval without user interaction
;; 3. Gptel-make-tool - simplified tool creation (no gptel dependency)
;;
;; TEST PROCESS:
;; 1. Create test demonstrating bug (fails with expected error)
;; 2. Run test to confirm expected failure
;; 3. Apply fix to implementation
;; 4. Run test again to confirm it passes

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)

;;; Test State

(defvar test-scope-yml-content nil
  "In-memory representation of scope.yml content.")

(defvar test-scope-file-path "/fake/project/scope.yml"
  "Fake path to scope.yml for testing.")

(defvar test-tool-executed nil
  "Flag indicating if the tool body was executed.")

(defvar test-tool-result nil
  "Stores the result returned by the tool body.")

(defvar test-final-callback-result nil
  "Stores the final result passed to gptel callback.")

(defvar test-callback-invocation-count 0
  "Count how many times the final callback was invoked.")

;;; Mock Functions

(defvar test-scope-config-plist nil
  "Direct plist representation of scope config for testing.")

(defun test-load-scope-config ()
  "Mock config loader that returns test-scope-config-plist."
  test-scope-config-plist)

(defun test-mock-expansion-ui-approve (violation-info expansion-callback patterns tool-name)
  "Mock expansion UI that auto-approves and updates scope config.
This simulates the user clicking 'Add to scope' which:
1. Updates scope.yml (simulated here by updating test-scope-config-plist)
2. Invokes expansion-callback with success JSON"
  ;; Simulate scope.yml update (add path to write section)
  (let* ((resource (plist-get violation-info :resource))
         (current-write-paths (plist-get test-scope-config-plist :paths-write))
         (updated-write-paths (append current-write-paths (list resource))))
    ;; Update in-memory config
    (setq test-scope-config-plist
          (plist-put test-scope-config-plist :paths-write updated-write-paths)))

  ;; Invoke callback with success JSON (as real UI handler does)
  (funcall expansion-callback
           (json-serialize (list :success t
                                :patterns_added (vconcat patterns)
                                :message "Test: Auto-approved and scope updated"))))

(defun test-reset-state ()
  "Reset all test state variables."
  (setq test-scope-config-plist
        '(:paths-read ("/workspace/**")
          :paths-write ("/workspace/allowed/**")
          :paths-deny ()))
  (setq test-tool-executed nil)
  (setq test-tool-result nil)
  (setq test-final-callback-result nil)
  (setq test-callback-invocation-count 0))

;;; Mock gptel-make-tool (simplified - no gptel dependency)

(defmacro test-gptel-make-tool (&rest args)
  "Simplified gptel-make-tool for testing.
Extracts :function and returns it directly."
  (let ((function-arg (plist-get args :function)))
    function-arg))

;; Alias for compatibility with macro
(defalias 'gptel-make-tool 'test-gptel-make-tool)

;;; Test Suite

(describe "E2E: Async Tool with Inline Expansion"

  (before-each
    (test-reset-state)

    ;; Mock file I/O
    (spy-on 'jf/gptel-scope--get-scope-file-path
            :and-return-value test-scope-file-path)
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'file-writable-p :and-return-value t)

    ;; Mock scope config loader
    (spy-on 'jf/gptel-scope--load-config
            :and-call-fake #'test-load-scope-config)

    ;; Mock expansion UI
    (spy-on 'jf/gptel-scope-prompt-expansion
            :and-call-fake #'test-mock-expansion-ui-approve))

  (describe "Complete flow: validation failure → approval → retry → execution"

    (it "FAILS when config not reloaded before retry (demonstrates bug)"
      ;; Create a real async tool using the macro
      ;; Use real tool name from jf/gptel-scope--tool-categories
      (let* ((test-tool
              (gptel-make-scoped-tool
               "write_file_in_scope"
               "Test tool for writing files"
               (list '(:name "filepath" :type string :description "File to write")
                     '(:name "content" :type string :description "Content to write"))
               "filesystem"
               :async
               ;; Tool body - should only execute if validation passes
               (progn
                 (setq test-tool-executed t)
                 (setq test-tool-result (list :success t
                                             :filepath filepath
                                             :content content
                                             :message "File written successfully"))
                 test-tool-result)))

             ;; Test filepath that's initially NOT in scope
             (test-filepath "/workspace/newfile.txt")

             ;; Mock gptel callback
             (gptel-callback
              (lambda (result-json)
                (setq test-callback-invocation-count (1+ test-callback-invocation-count))
                (setq test-final-callback-result
                      (json-parse-string result-json :object-type 'plist)))))

        ;; Verify initial state: filepath NOT in write scope
        (let ((initial-config (test-load-scope-config)))
          (expect (member test-filepath (plist-get initial-config :paths-write))
                  :to-be nil))

        ;; Call the async tool
        ;; Expected flow:
        ;; 1. Validation fails (path not in scope)
        ;; 2. Inline expansion triggered
        ;; 3. Mock UI auto-approves and updates scope.yml
        ;; 4. Retry validation (should reload config here!)
        ;; 5. If config reloaded: validation passes, tool executes
        ;; 6. If config NOT reloaded: validation fails again, tool never executes
        (funcall test-tool gptel-callback test-filepath "test content")

        ;; Verify scope.yml was updated (by mock UI)
        (let ((updated-config (test-load-scope-config)))
          (expect (member test-filepath (plist-get updated-config :paths-write))
                  :not :to-be nil))

        ;; CRITICAL ASSERTIONS:

        ;; The callback should be invoked exactly once
        (expect test-callback-invocation-count :to-equal 1)

        ;; The tool body should have executed (only happens if retry succeeds)
        ;; THIS WILL FAIL if implementation doesn't reload config
        (expect test-tool-executed :to-be t
                "Tool body should execute after approval and config reload")

        ;; The final result should be a success
        (expect (plist-get test-final-callback-result :success) :to-be t
                "Final callback result should indicate success")

        ;; The result should contain the tool's response (not a validation error)
        (expect (plist-get test-final-callback-result :filepath) :to-equal test-filepath
                "Final result should contain tool response, not error")
        (expect (plist-get test-final-callback-result :message)
                :to-equal "File written successfully"
                "Final result should contain tool success message")))

    (it "handles denial correctly (user clicks deny)"
      ;; Override expansion UI to deny instead of approve
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info expansion-callback patterns tool-name)
                ;; Simulate user denial
                (funcall expansion-callback
                         (json-serialize (list :success nil
                                              :user_denied t
                                              :message "User denied")))))

      (let* ((test-tool
              (gptel-make-scoped-tool
               "write_file_in_scope"
               "Test tool"
               (list '(:name "filepath" :type string))
               "filesystem"
               :async
               (progn
                 (setq test-tool-executed t)
                 (list :success t))))

             (test-filepath "/workspace/denied.txt")

             (gptel-callback
              (lambda (result-json)
                (setq test-final-callback-result
                      (json-parse-string result-json :object-type 'plist)))))

        ;; Call tool
        (funcall test-tool gptel-callback test-filepath)

        ;; Tool body should NOT execute
        (expect test-tool-executed :to-be nil)

        ;; Final result should be an error
        (expect (plist-get test-final-callback-result :success) :to-be nil)
        (expect (plist-get test-final-callback-result :error) :to-be-truthy)))

    (it "handles allow-once correctly (temporary permission)"
      ;; Override expansion UI to grant allow-once
      (spy-on 'jf/gptel-scope-prompt-expansion
              :and-call-fake
              (lambda (violation-info expansion-callback patterns tool-name)
                ;; Simulate allow-once (doesn't update scope.yml)
                (funcall expansion-callback
                         (json-serialize (list :success t
                                              :allowed_once t
                                              :message "Allowed once")))))

      (let* ((test-tool
              (gptel-make-scoped-tool
               "write_file_in_scope"
               "Test tool"
               (list '(:name "filepath" :type string))
               "filesystem"
               :async
               (progn
                 (setq test-tool-executed t)
                 (list :success t :message "Executed"))))

             (test-filepath "/workspace/once.txt")

             (gptel-callback
              (lambda (result-json)
                (setq test-final-callback-result
                      (json-parse-string result-json :object-type 'plist)))))

        ;; Call tool
        (funcall test-tool gptel-callback test-filepath)

        ;; Tool body SHOULD execute (allow-once grants permission)
        ;; NOTE: This test will also FAIL if config reload bug exists,
        ;; because allow-once needs the retry path to work too
        (expect test-tool-executed :to-be t)

        ;; Final result should be success
        (expect (plist-get test-final-callback-result :success) :to-be t)

        ;; Scope.yml should NOT be updated (allow-once is temporary)
        (let ((config (test-load-scope-config)))
          (expect (member test-filepath (plist-get config :paths-write))
                  :to-be nil))))))

(describe "E2E: Lexical binding closure preservation"

  (before-each
    (test-reset-state)

    ;; Setup mocks
    (spy-on 'jf/gptel-scope--get-scope-file-path
            :and-return-value test-scope-file-path)
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'file-writable-p :and-return-value t)
    (spy-on 'jf/gptel-scope--load-config
            :and-call-fake #'test-load-scope-config)
    (spy-on 'jf/gptel-scope-prompt-expansion
            :and-call-fake #'test-mock-expansion-ui-approve))

  (it "preserves callback closure through async callback chain"
    (let* ((callback-captured nil)
           (test-tool
            (gptel-make-scoped-tool
             "write_file_in_scope"
             "Test tool"
             (list '(:name "filepath" :type string))
             "filesystem"
             :async
             (list :success t :filepath filepath)))

           (test-filepath "/workspace/closure-test.txt")

           ;; The critical test: this callback should be preserved through
           ;; the entire chain: tool → validation → UI → retry → execution
           (gptel-callback
            (lambda (result-json)
              (setq callback-captured result-json))))

      ;; Call tool
      (funcall test-tool gptel-callback test-filepath)

      ;; Verify callback was invoked (closure preserved)
      (expect callback-captured :not :to-be nil)

      ;; Verify it's valid JSON (no error thrown)
      (when callback-captured
        (json-parse-string callback-captured :object-type 'plist)))))

(provide 'e2e-async-tool-expansion-spec)
;;; e2e-async-tool-expansion-spec.el ends here
