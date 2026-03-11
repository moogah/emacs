;;; command-execution-spec.el --- Integration tests for command execution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Command execution with process mocking
;;
;; Tests jf/gptel-bash--execute-command which handles:
;; - Process execution with timeout
;; - Output truncation for large results
;; - Absolute path warnings
;; - Non-zero exit codes
;; - Execution error handling
;;
;; Test organization:
;; 1. Successful execution (1 test)
;; 2. Timeout handling (1 test)
;; 3. Output truncation (1 test)
;; 4. Absolute path warnings (2 tests)
;; 5. Non-zero exit codes (1 test)
;; 6. Execution errors (1 test)
;; 7. Working directory (1 test)
;;
;; Total: 8 tests
;;
;; These tests mock call-process to avoid actual command execution.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Helper Functions

(defun test-exec--mock-call-process (exit-code output)
  "Mock call-process to return EXIT-CODE and OUTPUT.
Sets up spy that inserts OUTPUT into buffer and returns EXIT-CODE."
  (spy-on 'call-process
          :and-call-fake
          (lambda (program &optional infile buffer display &rest args)
            ;; Insert mock output into buffer (buffer is 't' for current buffer)
            (when (eq buffer t)
              (insert output))
            ;; Return exit code
            exit-code)))

(defun test-exec--mock-timeout ()
  "Mock with-timeout to trigger timeout path.
Sets up call-process to signal timeout error."
  (spy-on 'call-process
          :and-call-fake
          (lambda (&rest args)
            (signal 'timeout "Command timed out"))))

;;; Command Execution Tests

(describe "jf/gptel-bash--execute-command"

  (describe "successful execution"
    (it "returns output and zero exit code"
      (test-exec--mock-call-process 0 "file1.txt\nfile2.txt\n")
      (let ((result (jf/gptel-bash--execute-command "ls" "/workspace")))
        (expect (plist-get result :output) :to-equal "file1.txt\nfile2.txt\n")
        (expect (plist-get result :exit_code) :to-equal 0)
        (expect (plist-get result :truncated) :to-be nil)
        (expect (plist-get result :warnings) :to-be nil)
        (expect (plist-get result :error) :to-be nil))))

  (describe "timeout handling"
    (xit "returns timeout error when command exceeds timeout"
      ;; Mock timeout signal
      (test-exec--mock-timeout)
      (let ((result (jf/gptel-bash--execute-command "sleep 100" "/workspace")))
        (expect (plist-get result :error) :to-equal "timeout")
        ;; exit_code 124 is standard timeout convention
        (expect (plist-get result :exit_code) :to-equal 124)
        (expect (plist-get result :output) :to-match "timed out"))))

  (describe "output truncation"
    (it "truncates output over 10000 chars and sets truncated flag"
      (let ((long-output (make-string 15000 ?x)))
        (test-exec--mock-call-process 0 long-output)
        (let ((result (jf/gptel-bash--execute-command "cat huge-file" "/workspace")))
          (expect (plist-get result :truncated) :to-be t)
          ;; Output should be truncated with message
          (expect (length (plist-get result :output)) :to-be-less-than 12000)
          (expect (plist-get result :output) :to-match "Output truncated")
          (expect (plist-get result :output) :to-match "15000 chars")))))

  (describe "absolute path warnings"
    (xit "warns when command contains absolute paths"
      (test-exec--mock-call-process 0 "content")
      (let ((result (jf/gptel-bash--execute-command "grep pattern /etc/passwd" "/workspace")))
        (expect (plist-get result :warnings) :to-have-length-of 1)
        (expect (car (plist-get result :warnings)) :to-match "absolute path")
        (expect (car (plist-get result :warnings)) :to-match "/etc/passwd")))

    (xit "does not warn for relative paths"
      (test-exec--mock-call-process 0 "content")
      (let ((result (jf/gptel-bash--execute-command "grep pattern ./file.txt" "/workspace")))
        (expect (plist-get result :warnings) :to-be nil))))

  (describe "non-zero exit codes"
    (it "captures non-zero exit code but still returns output"
      (test-exec--mock-call-process 1 "Error: file not found\n")
      (let ((result (jf/gptel-bash--execute-command "cat missing.txt" "/workspace")))
        (expect (plist-get result :exit_code) :to-equal 1)
        (expect (plist-get result :output) :to-equal "Error: file not found\n")
        ;; Non-zero exit is not an "error" field (command ran successfully)
        (expect (plist-get result :error) :to-be nil))))

  (describe "execution errors"
    (it "catches and returns execution errors"
      (spy-on 'call-process :and-throw-error '(error "Process failed"))
      (let ((result (jf/gptel-bash--execute-command "invalid" "/workspace")))
        (expect (plist-get result :error) :to-equal "execution-failed")
        (expect (plist-get result :output) :to-match "execution failed")
        (expect (plist-get result :output) :to-match "Process failed"))))

  (describe "working directory"
    (xit "executes command in specified directory"
      (let ((captured-dir nil))
        ;; Spy on call-process to capture default-directory
        (spy-on 'call-process
                :and-call-fake
                (lambda (&rest args)
                  (setq captured-dir default-directory)
                  0))
        (jf/gptel-bash--execute-command "pwd" "/tmp")
        ;; Verify call-process was called with /tmp as default-directory
        (expect captured-dir :to-equal "/tmp/")))))

(provide 'command-execution-spec)

;;; command-execution-spec.el ends here
