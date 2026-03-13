;;; run-bash-command-spec.el --- Behavioral tests for run_bash_command tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; BEHAVIORAL TESTS: run_bash_command
;;
;; Tests verify the actual tool function behavior, not helper functions in isolation.
;;
;; The tool function (lambda (callback command) ...) should:
;; 1. Run jf/gptel-scope--validate-command-semantics for semantic validation
;; 2. On validation success: execute the command, invoke callback with result
;; 3. On validation failure: call trigger-inline-expansion (show expansion UI)
;; 4. After user approval: execute the command, invoke callback with result
;; 5. After user denial: invoke callback with error (no command execution)
;;
;; Mocked functions (boundary with external systems):
;; - jf/gptel-scope--load-config: Returns mock config (scope.yml loading)
;; - jf/gptel-scope--validate-command-semantics: Returns nil or error plist
;; - jf/gptel-bash--execute-command: Returns mock execution results
;; - jf/gptel-scope--trigger-inline-expansion: Simulates user choice

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'tool-test-helpers-spec (expand-file-name "helpers-spec.el" test-dir))
  ;; Load scope-core for validators and tool infrastructure
  (require 'jf-gptel-scope-core (expand-file-name "../scope/scope-core.el" tools-dir))
  ;; Load scope-expansion for inline expansion functions
  (require 'jf-gptel-scope-expansion (expand-file-name "../scope/scope-expansion.el" tools-dir))
  ;; Load scope-shell-tools which registers run_bash_command
  (require 'jf-gptel-scope-shell-tools (expand-file-name "../scope/scope-shell-tools.el" tools-dir)))

;;; Test Helpers

(defun run-bash-spec--get-tool-fn ()
  "Get the registered run_bash_command tool function from gptel--known-tools."
  (when-let* ((bash-tools (alist-get "bash" gptel--known-tools nil nil #'equal))
              (tool (alist-get "run_bash_command" bash-tools nil nil #'equal)))
    (gptel-tool-function tool)))

(defun run-bash-spec--validation-error (&optional error-type message path)
  "Build a semantic validation error plist (as returned by validate-command-semantics)."
  (list :error (or error-type "path_out_of_scope")
        :message (or message "Path not in scope")
        :path (or path "/denied/path")
        :operation :read))

;;; Test Suite

(describe "run_bash_command: behavioral contract"

  (describe "tool infrastructure"

    (it "run_bash_command is registered in gptel--known-tools under 'bash' category"
      (let* ((bash-tools (alist-get "bash" gptel--known-tools nil nil #'equal))
             (tool (alist-get "run_bash_command" bash-tools nil nil #'equal)))
        (expect tool :not :to-be nil)
        (expect (gptel-tool-async tool) :to-be t)))

    (it "tool function takes (callback command) arguments"
      (let ((tool-fn (run-bash-spec--get-tool-fn)))
        (expect tool-fn :not :to-be nil))))

  (describe "on semantic validation success"

    (before-each
      (spy-on 'jf/gptel-scope--load-config
              :and-return-value (tool-test--scope-config-minimal))
      (spy-on 'jf/gptel-scope--validate-command-semantics
              :and-return-value nil)  ; nil = validation passed
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value (tool-test--execution-result "file1.txt\nfile2.txt" 0)))

    (it "calls jf/gptel-bash--execute-command with command"
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "ls -la")
        (expect 'jf/gptel-bash--execute-command :to-have-been-called)))

    (it "invokes callback with success JSON"
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (received-result nil)
             (callback (lambda (result-json)
                         (setq received-result (json-parse-string result-json :object-type 'plist)))))
        (funcall tool-fn callback "ls -la")
        (expect (plist-get received-result :success) :to-be t)
        (expect (plist-get received-result :output) :to-equal "file1.txt\nfile2.txt")
        (expect (plist-get received-result :exit_code) :to-equal 0)))

    (it "does NOT call trigger-inline-expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "ls -la")
        (expect 'jf/gptel-scope--trigger-inline-expansion :not :to-have-been-called)))

    (it "passes non-zero exit code through with success: false"
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value (tool-test--execution-error "not found" 1))
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (received-result nil)
             (callback (lambda (result-json)
                         (setq received-result (json-parse-string result-json :object-type 'plist)))))
        (funcall tool-fn callback "grep missing")
        (expect (plist-get received-result :success) :to-be nil)
        (expect (plist-get received-result :exit_code) :to-equal 1))))

  (describe "on semantic validation failure"

    (before-each
      (spy-on 'jf/gptel-scope--load-config
              :and-return-value (tool-test--scope-config-minimal))
      (spy-on 'jf/gptel-scope--validate-command-semantics
              :and-return-value (run-bash-spec--validation-error
                                 "path_out_of_scope" "Path not in scope" "/etc/passwd")))

    (it "calls jf/gptel-scope--trigger-inline-expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "cat /etc/passwd")
        (expect 'jf/gptel-scope--trigger-inline-expansion :to-have-been-called)))

    (it "passes tool name 'run_bash_command' to trigger-inline-expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "cat /etc/passwd")
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
          (expect (nth 1 call-args) :to-equal "run_bash_command"))))

    (it "passes validation error to trigger-inline-expansion"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "cat /etc/passwd")
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
          (expect (plist-get (nth 0 call-args) :error) :to-equal "path_out_of_scope"))))

    (it "does NOT call jf/gptel-bash--execute-command before user decision"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)  ; intercept, don't call wrapper
      (spy-on 'jf/gptel-bash--execute-command)
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "cat /etc/passwd")
        (expect 'jf/gptel-bash--execute-command :not :to-have-been-called))))

  (describe "after user approves via expansion UI"

    (before-each
      (spy-on 'jf/gptel-scope--load-config
              :and-return-value (tool-test--scope-config-minimal))
      (spy-on 'jf/gptel-scope--validate-command-semantics
              :and-return-value (run-bash-spec--validation-error))
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value (tool-test--execution-result "personal" 0))
      ;; Mock expansion UI: immediately invokes wrapper-callback with approval
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_validation-error _tool-name wrapper-callback)
                (funcall wrapper-callback (list :approved t)))))

    (it "executes the command after approval"
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "cat ~/.machine-role")
        (expect 'jf/gptel-bash--execute-command :to-have-been-called)))

    (it "invokes callback with command output after approval"
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (received-result nil)
             (callback (lambda (result-json)
                         (setq received-result (json-parse-string result-json :object-type 'plist)))))
        (funcall tool-fn callback "cat ~/.machine-role")
        (expect (plist-get received-result :success) :to-be t)
        (expect (plist-get received-result :output) :to-equal "personal"))))

  (describe "after user denies via expansion UI"

    (before-each
      (spy-on 'jf/gptel-scope--load-config
              :and-return-value (tool-test--scope-config-minimal))
      (spy-on 'jf/gptel-scope--validate-command-semantics
              :and-return-value (run-bash-spec--validation-error
                                 "path_out_of_scope" "Path not in scope" "/etc/shadow"))
      ;; Mock expansion UI: immediately invokes wrapper-callback with denial
      (spy-on 'jf/gptel-scope--trigger-inline-expansion
              :and-call-fake
              (lambda (_validation-error _tool-name wrapper-callback)
                (funcall wrapper-callback (list :approved nil :reason "user_denied")))))

    (it "does NOT execute the command after denial"
      (spy-on 'jf/gptel-bash--execute-command)
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (callback (lambda (_result) nil)))
        (funcall tool-fn callback "cat /etc/shadow")
        (expect 'jf/gptel-bash--execute-command :not :to-have-been-called)))

    (it "invokes callback with error response after denial"
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (received-result nil)
             (callback (lambda (result-json)
                         (setq received-result (json-parse-string result-json :object-type 'plist)))))
        (funcall tool-fn callback "cat /etc/shadow")
        (expect (plist-get received-result :success) :to-be nil)
        (expect (plist-get received-result :error) :to-equal "path_out_of_scope"))))

  (describe "error handling"

    (it "invokes callback with tool_exception on unexpected error"
      (spy-on 'jf/gptel-scope--load-config
              :and-call-fake (lambda () (error "Simulated config error")))
      (let* ((tool-fn (run-bash-spec--get-tool-fn))
             (received-result nil)
             (callback (lambda (result-json)
                         (setq received-result (json-parse-string result-json :object-type 'plist)))))
        (funcall tool-fn callback "ls")
        (expect (plist-get received-result :success) :to-be nil)
        (expect (plist-get received-result :error) :to-equal "tool_exception")))))

(provide 'run-bash-command-spec)

;;; run-bash-command-spec.el ends here
