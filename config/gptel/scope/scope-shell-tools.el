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
(require 'jf-gptel-scope-expansion)  ; For jf/gptel-scope--prompt-expansion
;; Dependencies:1 ends here

;; Run Approved Command Tool (Scope-Aware, v3.0)

;; Execute shell command if allowed by scope plan using v3.0 scoped-tool macro.


;; [[file:scope-shell-tools.org::*Run Approved Command Tool (Scope-Aware, v3.0)][Run Approved Command Tool (Scope-Aware, v3.0):1]]
(gptel-make-scoped-tool
 "run_approved_command"
 "Execute shell command if allowed by scope plan.

Checks command against shell_commands.allow and shell_commands.deny in scope.yml.
Allow list: Exact command name match (e.g., 'ls' allows 'ls -la')
Deny list: Substring match (e.g., 'rm -rf' blocks any command containing 'rm -rf')

Deny patterns override allow patterns for security.
Use request_scope_expansion to add commands to allow list."

 (list '(:name "command"
         :type string
         :description "Shell command to execute"))

 "shell"

 ;; Tool body - executed only if validation passes
 (condition-case err
     (let* ((output (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process shell-file-name nil t nil
                                   shell-command-switch command))))
            (exit-code 0))  ; Simplified - proper exit code requires process handle
       (list :success t
             :output output
             :exit_code exit-code
             :message "Command executed"))
   (error
    (list :success nil
          :error "command_failed"
          :message (format "Command execution failed: %s" (error-message-string err))))))
;; Run Approved Command Tool (Scope-Aware, v3.0):1 ends here

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
               :description "Tool name (e.g., 'read_file', 'write_file_in_scope', 'run_approved_command')")
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
     (jf/gptel-scope--prompt-expansion violation-info callback patterns tool_name))))
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
