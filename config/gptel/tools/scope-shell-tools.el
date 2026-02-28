;; File Header


;; [[file:scope-shell-tools.org::*File Header][File Header:1]]
;;; scope-shell-tools.el --- GPTEL Scope Shell and Meta Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Scope-aware shell execution and meta tools for v3.0 preset-based permission management.

;;; Code:
;; File Header:1 ends here

;; Dependencies


;; [[file:scope-shell-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)  ; For jf/gptel-scope-prompt-expansion
;; Dependencies:1 ends here

;; Implementation


;; [[file:scope-shell-tools.org::*Implementation][Implementation:1]]
(defun jf/gptel-bash--parse-command (cmd-string)
  "Extract base command from CMD-STRING.
Handles pipes, redirects, command substitution.
Examples:
  'grep foo | head' → 'grep'
  'ls -la > output.txt' → 'ls'
  'echo $(date)' → 'echo'"
  (let* ((trimmed (string-trim cmd-string))
         ;; Split on shell metacharacters
         (parts (split-string trimmed "[ |><;&]+" t))
         (base (car parts)))
    base))
;; Implementation:1 ends here

;; Check Absolute Paths

;; Check if command contains absolute paths that bypass directory scoping.


;; [[file:scope-shell-tools.org::*Check Absolute Paths][Check Absolute Paths:1]]
(defun jf/gptel-bash--check-absolute-paths (command)
  "Check if COMMAND contains absolute paths.
Returns warning string if found, nil otherwise."
  (when (string-match "/[[:alnum:]_/-]+" command)
    "Warning: Command contains absolute path arguments. Directory scope may not protect these paths."))
;; Check Absolute Paths:1 ends here

;; Execute Command

;; Execute command in directory with timeout and output truncation.


;; [[file:scope-shell-tools.org::*Execute Command][Execute Command:1]]
(defun jf/gptel-bash--execute-command (command directory)
  "Execute COMMAND in DIRECTORY with timeout and output truncation.
Returns (:output OUTPUT :exit_code CODE :truncated BOOL :warnings LIST :error ERROR-TYPE).
Warnings are returned in a separate field to allow LLM to distinguish them from command output.
Error types: timeout, execution-failed."
  (let* ((default-directory (file-truename (expand-file-name directory)))
         (output nil)
         (exit-code nil)
         (truncated nil)
         (warnings nil)
         (error-type nil)
         (max-output-chars 10000))

    (condition-case err
        (with-timeout (30  ; 30-second timeout
                       (progn
                         (setq error-type "timeout")
                         (setq exit-code 124)  ; GNU timeout exit code
                         (setq output "Command execution timed out after 30 seconds. Output may be incomplete.")
                         (setq warnings (list "Command timed out - use more specific filters to reduce execution time"))))
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
                      (format "\n\n[Output truncated at %d chars. Total: %d chars. Use filters like 'head', 'grep', or 'tail' to narrow results.]"
                              max-output-chars original-length)))))

    ;; Check for warnings (collect in list, don't modify output)
    (let ((path-warning (jf/gptel-bash--check-absolute-paths command)))
      (when path-warning
        (setq warnings (list path-warning))))

    (list :output output
          :exit_code exit-code
          :truncated truncated
          :warnings warnings
          :error error-type)))
;; Execute Command:1 ends here

;; Tool Implementation


;; [[file:scope-shell-tools.org::*Tool Implementation][Tool Implementation:1]]
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command in specified directory with scope validation.

Commands are categorized:
- read_only: ls, grep, find, cat, head, tail, wc, file, git log, git show, git diff
- safe_write: mkdir, touch, echo, git add, git commit
- dangerous: (empty by default, requires explicit approval)

Directory must be in scope for command category:
- read_only commands: directory must match paths.read OR paths.write
- safe_write commands: directory must match paths.write

Shell composition allowed (pipes, redirects, command substitution), but only base command is validated.

Security features:
- 30-second timeout
- Output truncation at 10,000 chars
- Warnings for absolute paths in arguments
- Deny list blocks dangerous commands (rm, rmdir, mv, cp, ln, scp, rsync, chmod, sudo, crontab, iptables, systemctl, useradd, shutdown, reboot, halt, poweroff, etc.)

Examples:
  run_bash_command('ls -la', '/Users/jefffarr/emacs')
  run_bash_command('grep -r TODO . | head -20', '/Users/jefffarr/projects/myapp')
  run_bash_command('git log --oneline -10', '/Users/jefffarr/emacs')
  run_bash_command('mkdir scratch', '/tmp')"

 (list '(:name "command"
         :type string
         :description "Shell command to execute (pipes and redirects allowed)")
       '(:name "directory"
         :type string
         :description "Working directory (must be in scope for command category)"))

 "bash"

 ;; Tool body - executed only if validation passes
 (let* ((result (jf/gptel-bash--execute-command command directory))
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
;; Tool Implementation:1 ends here

;; Request Scope Expansion Tool (Meta Tool, v3.0 Async)

;; LLM uses this tool to explicitly request user approval for expanding scope using transient menu.


;; [[file:scope-shell-tools.org::*Request Scope Expansion Tool (Meta Tool, v3.0 Async)][Request Scope Expansion Tool (Meta Tool, v3.0 Async):1]]
(gptel-make-tool
 :name "request_scope_expansion"
 :async t  ; MUST be async for transient menu
 :description "Request user approval to expand scope with new patterns.

Displays interactive menu with 3 options:
1. Deny - Reject the expansion request
2. Add to scope - Permanently add patterns to scope.yml
3. Allow once - Temporarily allow for current turn only

The user will see:
- Which tool needs access
- What patterns/resources you want to add
- Your justification for why access is needed

Returns:
- success: true if approved (add-to-scope or allow-once)
- success: false if denied
- allowed_once: true if temporary permission granted
- patterns_added: list of patterns if permanently added"
 :args (list '(:name "tool_name"
               :type string
               :description "Tool name (e.g., 'read_file', 'write_file_in_scope')")
             '(:name "patterns"
               :type array
               :items (:type string)
               :description "Patterns to add (e.g., [\"/tmp/**\"] for files, [\"npm\"] for shell)")
             '(:name "justification"
               :type string
               :description "Explain why this access is needed. Be specific."))
 :category "scope"
 :function
 (lambda (callback tool_name patterns justification)  ; callback first!
   ;; Convert patterns from vector to list
   (when (vectorp patterns)
     (setq patterns (append patterns nil)))

   ;; Build violation info for transient menu
   (let* ((violation-info
           (list :tool tool_name
                 :resource (car patterns)  ; First pattern as resource
                 :reason justification
                 :validation-type (jf/gptel-scope--infer-validation-type tool_name)
                 :patterns patterns)))

     ;; Show transient menu - pass callback, patterns, tool_name directly in scope
     (jf/gptel-scope-prompt-expansion violation-info callback patterns tool_name))))
;; Request Scope Expansion Tool (Meta Tool, v3.0 Async):1 ends here

;; Helper: Infer Validation Type

;; Helper to infer validation type from tool name using tool categories.


;; [[file:scope-shell-tools.org::*Helper: Infer Validation Type][Helper: Infer Validation Type:1]]
(defun jf/gptel-scope--infer-validation-type (tool-name)
  "Infer validation type from TOOL-NAME using tool categories."
  (let ((category (cdr (assoc tool-name jf/gptel-scope--tool-categories))))
    (plist-get category :validation)))
;; Helper: Infer Validation Type:1 ends here

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
