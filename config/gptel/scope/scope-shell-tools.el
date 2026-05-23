;;; scope-shell-tools.el --- GPTEL Scope Shell and Meta Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Tool definitions for run_bash_command and request_scope_expansion.
;; Validation logic is in scope-validation.el.

;;; Code:

;; Dependencies


;; [[file:scope-shell-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-tool-wrapper)
(require 'jf-gptel-scope-validation)
;; Dependencies:1 ends here

;; Constants


;; [[file:scope-shell-tools.org::*Constants][Constants:1]]
(defconst jf/gptel-bash--max-output-chars 10000
  "Maximum characters in command output before truncation.")

(defconst jf/gptel-bash--command-timeout 30
  "Timeout in seconds for bash command execution.")
;; Constants:1 ends here

;; Check Absolute Paths


;; [[file:scope-shell-tools.org::*Check Absolute Paths][Check Absolute Paths:1]]
(defun jf/gptel-bash--check-absolute-paths (command)
  "Check if COMMAND contains absolute paths.
Returns warning string if found, nil otherwise."
  (when (string-match "/[[:alnum:]_/-]+" command)
    "Warning: Command contains absolute path arguments. Directory scope may not protect these paths."))
;; Check Absolute Paths:1 ends here

;; Execute Command


;; [[file:scope-shell-tools.org::*Execute Command][Execute Command:1]]
(defun jf/gptel-bash--execute-command (command directory)
  "Execute COMMAND in DIRECTORY with timeout and output truncation."
  (cl-block jf/gptel-bash--execute-command
    (let* ((default-directory (file-truename (expand-file-name directory)))
           (output nil)
           (exit-code nil)
           (truncated nil)
           (warnings nil)
           (error-type nil)
           (max-output-chars jf/gptel-bash--max-output-chars))

      (condition-case err
          (with-timeout (jf/gptel-bash--command-timeout
                         (progn
                           (setq error-type "timeout")
                           (setq exit-code 124)
                           (setq output (format "Command execution timed out after %d seconds."
                                                jf/gptel-bash--command-timeout))
                           (setq warnings (list "Command timed out"))))
            (setq output
                  (with-temp-buffer
                    (setq exit-code
                          (call-process shell-file-name nil t nil
                                        shell-command-switch command))
                    (buffer-string))))
        (error
         (cl-return-from jf/gptel-bash--execute-command
           (list :output (format "Command execution failed: %s" (error-message-string err))
                 :exit_code 1
                 :error "execution-failed"
                 :truncated nil
                 :warnings nil))))

      ;; Truncate output if too long
      (let ((original-length (length output)))
        (when (> original-length max-output-chars)
          (setq truncated t)
          (setq output
                (concat (substring output 0 max-output-chars)
                        (format "\n\n[Output truncated at %d chars. Total: %d chars.]"
                                max-output-chars original-length)))))

      ;; Check for warnings
      (let ((path-warning (jf/gptel-bash--check-absolute-paths command)))
        (when path-warning
          (setq warnings (vector path-warning))))

      (list :output output
            :exit_code exit-code
            :truncated truncated
            :warnings warnings
            :error error-type))))
;; Execute Command:1 ends here

;; run_bash_command Tool


;; [[file:scope-shell-tools.org::*run_bash_command Tool][run_bash_command Tool:1]]
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command with semantic scope validation.

Commands are validated semantically — file operations extracted from the command
are checked against scope paths (read/write/execute/modify/deny).

Commands with zero file operations (version checks, help flags) are allowed automatically.

Relative paths in the command are resolved from the session's current working
directory (default-directory), which is set from the session context.

Security features:
- 30-second timeout
- Output truncation at 10,000 chars
- Seven-stage semantic validation pipeline
- Deny list blocks dangerous commands

Examples:
  run_bash_command('ls -la')
  run_bash_command('grep -r TODO . | head -20')
  run_bash_command('git log --oneline -10')"

 (list '(:name "command"
         :type string
         :description "Shell command to execute (pipes and redirects allowed)"))

 ;; No :operation — file operations are extracted from the command by
 ;; the bash semantic pipeline at validation time.

 (let* ((result (jf/gptel-bash--execute-command command default-directory))
        (exit-code (plist-get result :exit_code))
        (output (plist-get result :output))
        (truncated (plist-get result :truncated))
        (warnings (plist-get result :warnings))
        (success (zerop exit-code)))
   (list :success success
         :output output
         :exit_code exit-code
         :truncated truncated
         :warnings warnings)))
;; run_bash_command Tool:1 ends here

;; request_scope_expansion Tool

;; LLM uses this tool to explicitly request user approval for expanding scope.


;; [[file:scope-shell-tools.org::*request_scope_expansion Tool][request_scope_expansion Tool:1]]
(gptel-make-tool
 :name "request_scope_expansion"
 :async t
 :description "Request user approval to expand scope with new patterns.

Displays interactive menu with 3 options:
1. Deny - Reject the expansion request
2. Add to scope - Permanently add patterns to the session's `:PROPERTIES:` drawer in `session.org`
3. Allow once - Temporarily allow for current turn only

Returns:
- success: true if approved (add-to-scope or allow-once)
- success: false if denied
- allowed_once: true if temporary permission granted
- patterns_added: list of patterns if permanently added"
 :args (list '(:name "tool_name"
               :type string
               :description "Tool name (e.g., 'read_file_in_scope', 'write_file_in_scope')")
             '(:name "patterns"
               :type array
               :items (:type string)
               :description "Patterns to add (e.g., [\"/tmp/**\"] for files)")
             '(:name "justification"
               :type string
               :description "Explain why this access is needed."))
 :category "scope"
 :function
 (lambda (callback tool_name patterns justification)
   (when (vectorp patterns)
     (setq patterns (append patterns nil)))
   (let* ((violation-info
           (list :tool tool_name
                 :resource (car patterns)
                 :reason justification
                 :validation-type 'path
                 :patterns patterns)))
     (jf/gptel-scope-prompt-expansion violation-info callback patterns tool_name))))
;; request_scope_expansion Tool:1 ends here

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
