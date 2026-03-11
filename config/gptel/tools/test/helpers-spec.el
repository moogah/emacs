;;; helpers-spec.el --- Lightweight test helpers for tool-level tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Lightweight test infrastructure for tool-level spy-based tests.
;; These helpers support testing the CONTRACT between tools and the scope system,
;; NOT the scope validation logic itself (that lives in scope/test/).
;;
;; Provides:
;; 1. Mock scope validation response builders (success/failure)
;; 2. Mock process execution result builders
;; 3. Minimal session setup/teardown

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;;; Scope Validation Response Builders

(defun tool-test--scope-allowed ()
  "Build a scope validation success response.
Matches the plist format returned by `jf/gptel-scope--check-tool-permission'."
  (list :allowed t))

(defun tool-test--scope-denied (&optional reason resource tool)
  "Build a scope validation failure response.
REASON defaults to \"path_out_of_scope\".
RESOURCE is the denied resource path.
TOOL is the tool name.
Matches the plist format returned by `jf/gptel-scope--check-tool-permission'."
  (list :allowed nil
        :reason (or reason "path_out_of_scope")
        :resource (or resource "/denied/path")
        :tool (or tool "unknown_tool")
        :allowed-patterns '("/workspace/**")))

(defun tool-test--scope-config-minimal ()
  "Build a minimal scope config plist for testing.
This is what `jf/gptel-scope--load-config' would return."
  (list :paths-read '("/workspace/**")
        :paths-write '("/workspace/**")
        :paths-deny '("/etc/**")
        :bash-tools (list :categories (list :read-only (list :commands '("ls" "cat" "grep"))
                                            :safe-write (list :commands '("mkdir" "touch"))
                                            :dangerous (list :commands '()))
                          :deny '("rm" "sudo"))))

;;; Process Execution Result Builders

(defun tool-test--execution-result (&optional output exit-code)
  "Build a mock command execution result.
OUTPUT defaults to \"mock output\".
EXIT-CODE defaults to 0.
Matches the plist format returned by `jf/gptel-bash--execute-command'."
  (list :output (or output "mock output")
        :exit_code (or exit-code 0)
        :truncated nil
        :warnings nil
        :error nil))

(defun tool-test--execution-error (&optional output exit-code error-type)
  "Build a mock command execution error result.
OUTPUT defaults to \"Command failed\".
EXIT-CODE defaults to 1.
ERROR-TYPE defaults to \"execution-failed\"."
  (list :output (or output "Command failed")
        :exit_code (or exit-code 1)
        :truncated nil
        :warnings nil
        :error (or error-type "execution-failed")))

(defun tool-test--execution-timeout ()
  "Build a mock command timeout result."
  (list :output "Command execution timed out after 30 seconds."
        :exit_code 124
        :truncated nil
        :warnings '("Command timed out")
        :error "timeout"))

;;; Validation Pipeline Result Builders

(defun tool-test--validation-success ()
  "Build a semantic validation pipeline success result (nil = no error)."
  nil)

(defun tool-test--validation-error (error-type &optional message)
  "Build a semantic validation pipeline error result.
ERROR-TYPE is the error string (e.g., \"command_denied\", \"path_out_of_scope\").
MESSAGE is an optional human-readable message."
  (list :error error-type
        :message (or message (format "Validation failed: %s" error-type))))

;;; Session Setup/Teardown

(defvar tool-test--temp-dir nil
  "Temporary directory for tool test session.")

(defun tool-test-setup ()
  "Set up minimal test environment for tool tests.
Creates a temp directory for session simulation."
  (setq tool-test--temp-dir (make-temp-file "tool-test-" t)))

(defun tool-test-teardown ()
  "Tear down tool test environment."
  (when (and tool-test--temp-dir (file-exists-p tool-test--temp-dir))
    (delete-directory tool-test--temp-dir t))
  (setq tool-test--temp-dir nil))

;;; Provide

(provide 'tool-test-helpers-spec)

;;; helpers-spec.el ends here
