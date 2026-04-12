;;; simple-async-test-spec.el --- Simplified async tool test -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; SIMPLE TEST: Focus on the core bug without complex mocking

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)
(require 'jf-gptel-scope-validation)
(require 'jf-gptel-scope-tool-wrapper)
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
  (message "DEBUG: Loading config, paths-write=%S"
           (plist-get (plist-get simple-test-config :paths) :write))
  simple-test-config)

(defun simple-test-mock-ui (violation-info expansion-callback patterns tool-name)
  "Mock UI that captures callback and auto-approves."
  (message "DEBUG: Mock UI called, violation=%S" violation-info)
  (setq simple-test-ui-called t)
  (setq simple-test-expansion-callback expansion-callback)

  ;; Simulate user approval: update nested paths.write FIRST
  (let* ((resource (plist-get violation-info :resource))
         (paths (plist-get simple-test-config :paths))
         (current-write (plist-get paths :write))
         (updated-write (append current-write (list resource)))
         (updated-paths (plist-put paths :write updated-write)))
    (message "DEBUG: Updating config write from %S to %S" current-write updated-write)
    (setq simple-test-config (plist-put simple-test-config :paths updated-paths)))

  ;; Then invoke callback with success
  (message "DEBUG: Invoking expansion callback with success")
  (funcall expansion-callback
           (json-serialize (list :success t
                                :patterns_added (vconcat patterns)
                                :message "Approved"))))

;;; Test Suite

(describe "Simple Async Tool Test"

  (before-each
    (setq simple-test-config
          '(:paths (:read ("/workspace/**")
                    :write ("/workspace/allowed/**")
                    :execute ()
                    :modify ()
                    :deny ())))
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
             "write_file_in_scope"  ; Use real tool name
             "Test"
             (list '(:name "filepath" :type string))
             :operation write
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
      (expect (member test-filepath
                      (plist-get (plist-get simple-test-config :paths) :write))
              :to-be nil)

      ;; Call tool
      (message "DEBUG: Calling tool with filepath=%s" test-filepath)
      (funcall (gptel-tool-function test-tool) gptel-callback test-filepath)

      ;; Check what happened
      (message "DEBUG: After tool call:")
      (message "  UI called: %s" simple-test-ui-called)
      (message "  Tool executed: %s" simple-test-tool-executed)
      (message "  Config paths.write: %S"
               (plist-get (plist-get simple-test-config :paths) :write))
      (message "  Final result: %S" simple-test-final-result)

      ;; Assertions
      (expect simple-test-ui-called :to-be t)

      (expect (member test-filepath
                      (plist-get (plist-get simple-test-config :paths) :write))
              :not :to-be nil)

      (expect simple-test-tool-executed :to-be t))))

(provide 'simple-async-test-spec)
;;; simple-async-test-spec.el ends here
