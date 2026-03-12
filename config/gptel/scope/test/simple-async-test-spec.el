;;; simple-async-test-spec.el --- Simplified async tool test -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; SIMPLE TEST: Focus on the core bug without complex mocking

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)

;;; Test State

(defvar simple-test-config nil
  "Simple test config.")

(defvar simple-test-ui-called nil
  "Flag: was UI called?")

(defvar simple-test-expansion-callback nil
  "Captured expansion callback.")

(defvar simple-test-tool-executed nil
  "Flag: was tool body executed?")

(defvar simple-test-final-result nil
  "Final result from gptel callback.")

;;; Mocks

(defun simple-test-load-config ()
  "Return simple-test-config."
  (message "DEBUG: Loading config, paths-write=%S" (plist-get simple-test-config :paths-write))
  simple-test-config)

(defun simple-test-mock-ui (violation-info expansion-callback patterns tool-name)
  "Mock UI that captures callback and auto-approves."
  (message "DEBUG: Mock UI called, violation=%S" violation-info)
  (setq simple-test-ui-called t)
  (setq simple-test-expansion-callback expansion-callback)

  ;; Simulate user approval: update config FIRST
  (let* ((resource (plist-get violation-info :resource))
         (current-paths (plist-get simple-test-config :paths-write))
         (updated-paths (append current-paths (list resource))))
    (message "DEBUG: Updating config from %S to %S" current-paths updated-paths)
    (setq simple-test-config (plist-put simple-test-config :paths-write updated-paths)))

  ;; Then invoke callback with success
  (message "DEBUG: Invoking expansion callback with success")
  (funcall expansion-callback
           (json-serialize (list :success t
                                :patterns_added (vconcat patterns)
                                :message "Approved"))))

;;; Test Suite

(describe "Simple Async Tool Test"

  (before-each
    (setq simple-test-config '(:paths-read ("/workspace/**")
                               :paths-write ("/workspace/allowed/**")
                               :paths-deny ()))
    (setq simple-test-ui-called nil)
    (setq simple-test-expansion-callback nil)
    (setq simple-test-tool-executed nil)
    (setq simple-test-final-result nil)

    ;; Mock config loader
    (spy-on 'jf/gptel-scope--load-config
            :and-call-fake #'simple-test-load-config)

    ;; Mock expansion UI
    (spy-on 'jf/gptel-scope-prompt-expansion
            :and-call-fake #'simple-test-mock-ui))

  (it "calls UI when validation fails"
    ;; Create simple test tool using REAL tool name from categories
    (let* ((test-tool
            (gptel-make-scoped-tool
             "write_file_in_scope"  ; Use real tool name from jf/gptel-scope--tool-categories
             "Test"
             (list '(:name "filepath" :type string))
             "filesystem"
             :async
             (progn
               (message "DEBUG: Tool body executing for %s" filepath)
               (setq simple-test-tool-executed t)
               (list :success t :filepath filepath))))

           (test-filepath "/workspace/newfile.txt")

           (gptel-callback
            (lambda (result-json)
              (message "DEBUG: Final callback invoked with: %s" result-json)
              (setq simple-test-final-result (json-parse-string result-json :object-type 'plist)))))

      ;; Verify initial state
      (expect (member test-filepath (plist-get simple-test-config :paths-write)) :to-be nil)

      ;; Call tool
      (message "DEBUG: Calling tool with filepath=%s" test-filepath)
      (funcall (gptel-tool-function test-tool) gptel-callback test-filepath)

      ;; Check what happened
      (message "DEBUG: After tool call:")
      (message "  UI called: %s" simple-test-ui-called)
      (message "  Tool executed: %s" simple-test-tool-executed)
      (message "  Config paths-write: %S" (plist-get simple-test-config :paths-write))
      (message "  Final result: %S" simple-test-final-result)

      ;; Assertions
      (expect simple-test-ui-called :to-be t)

      (expect (member test-filepath (plist-get simple-test-config :paths-write)) :not :to-be nil)

      (expect simple-test-tool-executed :to-be t))))

(provide 'simple-async-test-spec)
;;; simple-async-test-spec.el ends here
