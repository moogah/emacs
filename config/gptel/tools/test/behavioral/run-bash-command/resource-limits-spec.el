;;; resource-limits-spec.el --- Timeout and resource limits tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; TIMEOUT AND RESOURCE LIMITS (Execution Constraints)
;;
;; Tests the timeout and resource limit enforcement during command execution.
;; This validates that bash commands respect temporal constraints and output
;; size limits to prevent resource exhaustion.
;;
;; Key behaviors tested:
;; - Commands complete successfully within timeout window
;; - Timeout enforced for long-running commands (30-second limit)
;; - Long-running commands generate appropriate warnings
;; - Output returned fully when within size limit (10,000 chars)
;; - Output truncated when exceeding 10,000 char limit
;; - Truncation notice suggests filtering techniques (head, grep, tail)
;; - Non-zero exit codes captured correctly
;; - Execution errors handled gracefully
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates timeout, resource, or output handling behavior.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Test Suite

(describe "run_bash_command: Timeout and resource limits"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "allows command that completes within timeout"
    ;; Test: Command completes within timeout
    ;; Command: 'ls /workspace'
    ;; Mock call-process: Return immediately with exit-code 0
    ;; Assert: :success t, :timeout nil
    (spy-on 'call-process
            :and-call-fake
            (lambda (program &optional infile buffer display &rest args)
              (when (eq buffer t)
                (insert "file1.txt\nfile2.txt\n"))
              0)) ; Return success exit code immediately

    (let ((result (jf/gptel-bash--execute-command "ls /workspace" "/workspace")))
      (expect (plist-get result :exit_code) :to-equal 0)
      (expect (plist-get result :error) :to-be nil)
      (expect (plist-get result :output) :to-equal "file1.txt\nfile2.txt\n")))

  (it "enforces timeout for command exceeding 30 seconds"
    ;; Test: Command exceeds timeout (30 seconds)
    ;; Command: 'sleep 60'
    ;; Mock run-with-timer to fire timeout immediately
    ;; Assert: :error 'timeout', :exit_code 124
    ;; Assert: :output contains 'timed out after 30 seconds'
    (spy-on 'run-with-timer
            :and-call-fake
            (lambda (secs repeat function &rest args)
              ;; Fire the timeout function immediately
              (apply function args)
              ;; Return a dummy timer object
              'dummy-timer))

    ;; Mock call-process to simulate a command that would run too long
    (spy-on 'call-process
            :and-call-fake
            (lambda (&rest args)
              ;; This should not be reached because timeout fires first
              0))

    ;; Execute command - timeout should trigger immediately
    (let ((result (jf/gptel-bash--execute-command "sleep 60" "/workspace")))
      (expect (plist-get result :error) :to-equal "timeout")
      (expect (plist-get result :exit_code) :to-equal 124)
      (expect (plist-get result :output) :to-match "timed out after 30 seconds")))

  (it "warns when long-running command is terminated"
    ;; Test: Long-running command terminated
    ;; Command: 'find / -name "*"'
    ;; Mock run-with-timer to trigger timeout immediately
    ;; Assert: :error 'timeout'
    ;; Assert: :warnings suggests using filters to reduce execution time
    (spy-on 'run-with-timer
            :and-call-fake
            (lambda (secs repeat function &rest args)
              (apply function args)
              'dummy-timer))

    (spy-on 'call-process
            :and-call-fake
            (lambda (&rest args)
              0))

    (let ((result (jf/gptel-bash--execute-command "find / -name \"*\"" "/workspace")))
      (expect (plist-get result :error) :to-equal "timeout")
      (expect (plist-get result :warnings) :not :to-be nil)
      (expect (car (plist-get result :warnings)) :to-match "more specific filters")))

  (it "returns full output when within limit"
    ;; Test: Output within limit returned fully
    ;; Command: 'cat small-file.txt'
    ;; Mock call-process: Return output of 1000 chars
    ;; Assert: :truncated nil, full output returned
    (let ((small-output (make-string 1000 ?a)))
      (spy-on 'call-process
              :and-call-fake
              (lambda (program &optional infile buffer display &rest args)
                (when (eq buffer t)
                  (insert small-output))
                0))

      (let ((result (jf/gptel-bash--execute-command "cat small-file.txt" "/workspace")))
        (expect (plist-get result :truncated) :to-be nil)
        (expect (plist-get result :output) :to-equal small-output)
        (expect (length (plist-get result :output)) :to-equal 1000))))

  (it "truncates output exceeding 10,000 chars limit"
    ;; Test: Output exceeding limit truncated (10,000 chars)
    ;; Command: 'cat huge-file.txt'
    ;; Mock call-process: Return output of 15,000 chars
    ;; Assert: :truncated t
    ;; Assert: Output length <= 10,000 chars
    ;; Assert: :output contains truncation notice with original size
    (let ((huge-output (make-string 15000 ?b)))
      (spy-on 'call-process
              :and-call-fake
              (lambda (program &optional infile buffer display &rest args)
                (when (eq buffer t)
                  (insert huge-output))
                0))

      (let ((result (jf/gptel-bash--execute-command "cat huge-file.txt" "/workspace")))
        (expect (plist-get result :truncated) :to-be t)
        ;; Output should be truncated with message appended
        (expect (length (plist-get result :output)) :to-be-less-than 12000)
        (expect (plist-get result :output) :to-match "Output truncated")
        (expect (plist-get result :output) :to-match "15000 chars"))))

  (it "suggests filters in truncation notice"
    ;; Test: Truncation notice suggests filters
    ;; Command: 'ls -R /workspace'
    ;; Mock call-process: Return huge output
    ;; Assert: Truncation notice mentions 'head', 'grep', 'tail' for narrowing results
    (let ((huge-output (make-string 12000 ?c)))
      (spy-on 'call-process
              :and-call-fake
              (lambda (program &optional infile buffer display &rest args)
                (when (eq buffer t)
                  (insert huge-output))
                0))

      (let ((result (jf/gptel-bash--execute-command "ls -R /workspace" "/workspace")))
        (expect (plist-get result :truncated) :to-be t)
        (expect (plist-get result :output) :to-match "head")
        (expect (plist-get result :output) :to-match "grep")
        (expect (plist-get result :output) :to-match "tail"))))

  (it "captures non-zero exit code"
    ;; Test: Non-zero exit code captured
    ;; Command: 'cat missing-file.txt'
    ;; Mock call-process: Return exit-code 1, output 'No such file'
    ;; Assert: :success nil, :exit_code 1
    ;; Assert: :output contains error message
    (spy-on 'call-process
            :and-call-fake
            (lambda (program &optional infile buffer display &rest args)
              (when (eq buffer t)
                (insert "cat: missing-file.txt: No such file or directory\n"))
              1)) ; Return error exit code

    (let ((result (jf/gptel-bash--execute-command "cat missing-file.txt" "/workspace")))
      (expect (plist-get result :exit_code) :to-equal 1)
      (expect (plist-get result :output) :to-match "No such file")
      (expect (plist-get result :error) :to-be nil))) ; Non-zero exit is not an error field

  (it "handles execution errors gracefully"
    ;; Test: Execution error handled
    ;; Command: 'invalid-command'
    ;; Mock call-process: Throw error 'Process failed'
    ;; Assert: :error 'execution-failed'
    ;; Assert: :output contains 'execution failed'
    (spy-on 'call-process
            :and-throw-error '(error "Process failed to start"))

    (let ((result (jf/gptel-bash--execute-command "invalid-command" "/workspace")))
      (expect (plist-get result :error) :to-equal "execution-failed")
      (expect (plist-get result :output) :to-match "execution failed")
      (expect (plist-get result :output) :to-match "Process failed"))))

(provide 'resource-limits-spec)
;;; resource-limits-spec.el ends here
