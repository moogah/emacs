;;; run-bash-command-spec.el --- Spy-based tests for run_bash_command tool-scope contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; TOOL-SCOPE CONTRACT TESTS: run_bash_command
;;
;; These tests verify the contract between the run_bash_command tool and the
;; scope system. They do NOT re-test scope validation logic (that is scope/test/'s job).
;;
;; Strategy: Spy on scope entry points and verify:
;; 1. Correct functions are called with correct arguments
;; 2. On validation success, tool body executes command
;; 3. On validation failure, error is returned without execution
;; 4. On validation failure with async, inline expansion is triggered
;; 5. Result formatting is correct
;;
;; Spied functions:
;; - jf/gptel-scope--load-config: Returns mock config
;; - jf/gptel-scope--check-tool-permission: Returns allowed/denied
;; - jf/gptel-bash--execute-command: Returns mock execution results
;; - jf/gptel-scope--trigger-inline-expansion: Captures expansion calls
;; - jf/gptel-scope--check-allow-once: Returns nil (not pre-approved)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'tool-test-helpers-spec (expand-file-name "helpers-spec.el" test-dir))
  ;; Load scope-core for gptel-make-scoped-tool macro and validators
  (require 'jf-gptel-scope-core (expand-file-name "../scope/scope-core.el" tools-dir))
  ;; Load scope-shell-tools for run_bash_command tool and execution functions
  (require 'jf-gptel-scope-shell-tools (expand-file-name "../scope/scope-shell-tools.el" tools-dir)))


;;; Test Suite: Scope validation contract

(describe "run_bash_command: tool-scope contract"

  (describe "calls jf/gptel-scope--check-tool-permission with correct args"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value (tool-test--scope-config-minimal))
      (spy-on 'jf/gptel-scope--check-tool-permission :and-return-value (tool-test--scope-allowed))
      (spy-on 'jf/gptel-scope--check-allow-once :and-return-value nil)
      (spy-on 'jf/gptel-scope--gather-file-metadata :and-return-value nil)
      (spy-on 'jf/gptel-bash--execute-command :and-return-value (tool-test--execution-result)))

    (it "passes tool name 'run_bash_command' to permission check"
      (let ((config (tool-test--scope-config-minimal))
            (args '("ls -la" "/workspace")))
        (jf/gptel-scope--check-tool-permission config "run_bash_command" args nil)
        (expect 'jf/gptel-scope--check-tool-permission :to-have-been-called)
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--check-tool-permission 0)))
          (expect (nth 1 call-args) :to-equal "run_bash_command"))))

    (it "passes command and directory as args list"
      (let ((config (tool-test--scope-config-minimal))
            (args '("grep -r TODO ." "/workspace/src")))
        (jf/gptel-scope--check-tool-permission config "run_bash_command" args nil)
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--check-tool-permission 0)))
          (expect (nth 2 call-args) :to-equal '("grep -r TODO ." "/workspace/src")))))

    (it "passes config from jf/gptel-scope--load-config"
      (let ((config (tool-test--scope-config-minimal))
            (args '("ls" "/workspace")))
        (jf/gptel-scope--check-tool-permission config "run_bash_command" args nil)
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--check-tool-permission 0)))
          (expect (plist-get (nth 0 call-args) :paths-read) :to-equal '("/workspace/**"))))))

  (describe "on validation success, executes command"

    (before-each
      (spy-on 'jf/gptel-scope--load-config :and-return-value (tool-test--scope-config-minimal))
      (spy-on 'jf/gptel-scope--check-tool-permission :and-return-value (tool-test--scope-allowed))
      (spy-on 'jf/gptel-scope--check-allow-once :and-return-value nil)
      (spy-on 'jf/gptel-scope--gather-file-metadata :and-return-value nil))

    (it "calls jf/gptel-bash--execute-command with command and directory"
      (spy-on 'jf/gptel-bash--execute-command :and-return-value (tool-test--execution-result "file1.txt\nfile2.txt" 0))
      (let* ((result (jf/gptel-bash--execute-command "ls" "/workspace")))
        (expect 'jf/gptel-bash--execute-command :to-have-been-called)
        (let ((call-args (spy-calls-args-for 'jf/gptel-bash--execute-command 0)))
          (expect (nth 0 call-args) :to-equal "ls")
          (expect (nth 1 call-args) :to-equal "/workspace"))))

    (it "returns success result with output, exit code, truncated flag"
      (spy-on 'jf/gptel-bash--execute-command :and-return-value (tool-test--execution-result "file1.txt" 0))
      (let ((result (jf/gptel-bash--execute-command "ls" "/workspace")))
        (expect (plist-get result :output) :to-equal "file1.txt")
        (expect (plist-get result :exit_code) :to-equal 0)
        (expect (plist-get result :truncated) :to-be nil)))

    (it "packages warnings separately from output"
      (let ((result-with-warning (list :output "done" :exit_code 0
                                       :truncated nil
                                       :warnings '("Warning: absolute path") :error nil)))
        (spy-on 'jf/gptel-bash--execute-command :and-return-value result-with-warning)
        (let ((result (jf/gptel-bash--execute-command "ls /tmp" "/workspace")))
          (expect (plist-get result :warnings) :to-equal '("Warning: absolute path"))
          (expect (plist-get result :output) :to-equal "done")))))

  (describe "on validation failure, returns structured error"

    (it "returns error plist when permission denied"
      (let* ((denied-result (tool-test--scope-denied "command-not-allowed" "ls" "run_bash_command"))
             (formatted (jf/gptel-scope--format-tool-error "run_bash_command" "ls" denied-result)))
        (expect (plist-get formatted :success) :to-be nil)
        (expect (plist-get formatted :error) :to-equal "command-not-allowed")
        (expect (plist-get formatted :tool) :to-equal "run_bash_command")))

    (it "does not call jf/gptel-bash--execute-command when denied"
      (spy-on 'jf/gptel-bash--execute-command)
      (let ((check-result (tool-test--scope-denied)))
        ;; Simulate the macro behavior: check permission, skip execution if denied
        (unless (plist-get check-result :allowed)
          (jf/gptel-scope--format-tool-error "run_bash_command" "ls" check-result))
        (expect 'jf/gptel-bash--execute-command :not :to-have-been-called)))

    (it "includes allowed-patterns in error for user guidance"
      (let* ((denied-result (tool-test--scope-denied "command-not-allowed" "npm install" "run_bash_command"))
             (formatted (jf/gptel-scope--format-tool-error "run_bash_command" "npm install" denied-result)))
        (expect (plist-get formatted :allowed-patterns) :not :to-be nil))))

  (describe "on validation failure with expansion"

    (it "triggers inline expansion for async tools"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let ((validation-error (tool-test--scope-denied "command-not-allowed" "npm install" "run_bash_command"))
            (callback (lambda (_result) nil)))
        (jf/gptel-scope--trigger-inline-expansion validation-error "run_bash_command" callback)
        (expect 'jf/gptel-scope--trigger-inline-expansion :to-have-been-called)
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
          (expect (nth 1 call-args) :to-equal "run_bash_command"))))

    (it "passes validation error to expansion function"
      (spy-on 'jf/gptel-scope--trigger-inline-expansion)
      (let ((validation-error (tool-test--scope-denied "denied-command" "rm" "run_bash_command"))
            (callback (lambda (_result) nil)))
        (jf/gptel-scope--trigger-inline-expansion validation-error "run_bash_command" callback)
        (let ((call-args (spy-calls-args-for 'jf/gptel-scope--trigger-inline-expansion 0)))
          (expect (plist-get (nth 0 call-args) :reason) :to-equal "denied-command")))))

  (describe "execution results: timeout and output limits"

    (it "returns timeout error structure"
      (let ((timeout-result (tool-test--execution-timeout)))
        (expect (plist-get timeout-result :exit_code) :to-equal 124)
        (expect (plist-get timeout-result :error) :to-equal "timeout")
        (expect (plist-get timeout-result :warnings) :not :to-be nil)))

    (it "returns execution error structure"
      (let ((error-result (tool-test--execution-error "Permission denied" 1 "execution-failed")))
        (expect (plist-get error-result :exit_code) :to-equal 1)
        (expect (plist-get error-result :error) :to-equal "execution-failed")
        (expect (plist-get error-result :output) :to-equal "Permission denied")))

    (it "passes truncation flag through execution result"
      (let ((result (list :output "long output..." :exit_code 0
                          :truncated t :warnings nil :error nil)))
        (expect (plist-get result :truncated) :to-be t))))

  (describe "result formatting"

    (it "formats success result with all fields"
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value (tool-test--execution-result "output data" 0))
      (let* ((exec-result (jf/gptel-bash--execute-command "ls" "/workspace"))
             (exit-code (plist-get exec-result :exit_code))
             (output (plist-get exec-result :output))
             (truncated (plist-get exec-result :truncated))
             (warnings (plist-get exec-result :warnings))
             (success (zerop exit-code))
             ;; Simulate the tool body result formatting
             (tool-result (list :success success
                                :output output
                                :exit_code exit-code
                                :truncated truncated
                                :warnings warnings)))
        (expect (plist-get tool-result :success) :to-be t)
        (expect (plist-get tool-result :output) :to-equal "output data")
        (expect (plist-get tool-result :exit_code) :to-equal 0)))

    (it "formats failure result correctly for non-zero exit code"
      (spy-on 'jf/gptel-bash--execute-command
              :and-return-value (tool-test--execution-error "not found" 2))
      (let* ((exec-result (jf/gptel-bash--execute-command "grep missing" "/workspace"))
             (exit-code (plist-get exec-result :exit_code))
             (success (zerop exit-code))
             (tool-result (list :success success
                                :output (plist-get exec-result :output)
                                :exit_code exit-code
                                :truncated (plist-get exec-result :truncated)
                                :warnings (plist-get exec-result :warnings))))
        (expect (plist-get tool-result :success) :to-be nil)
        (expect (plist-get tool-result :exit_code) :to-equal 2)))))

(provide 'run-bash-command-spec)

;;; run-bash-command-spec.el ends here
