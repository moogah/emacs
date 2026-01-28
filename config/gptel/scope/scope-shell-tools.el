;; Dependencies


;; [[file:scope-shell-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Run Approved Command Tool (Scope-Aware)

;; Execute shell command if allowed by scope plan.


;; [[file:scope-shell-tools.org::*Run Approved Command Tool (Scope-Aware)][Run Approved Command Tool (Scope-Aware):1]]
(gptel-make-tool
 :name "run_approved_command"
 :description "Execute shell command if allowed by scope plan.
Checks command against allow_commands (whitelist) and deny_commands (blacklist).

Allow patterns: Exact command name match (e.g., 'ls' allows 'ls -la')
Deny patterns: Substring match (e.g., 'rm -rf' blocks any command containing 'rm -rf')

Deny patterns override allow patterns for security.

Returns command output on success or scope violation error if not approved.
Use request_scope_expansion to ask user to add command to allowed list."
 :args (list '(:name "command"
               :type string
               :description "Shell command to execute"))
 :category "shell"
 :function
 (lambda (command)
   (let* ((plan (jf/gptel-scope--load-plan)))

     (unless plan
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")))

     ;; Check scope
     (let ((check-result (jf/gptel-scope--check-shell-command plan command)))
       (if (plist-get check-result :allowed)
           ;; Allowed - execute command
           (condition-case err
               (let* ((output (with-output-to-string
                               (with-current-buffer standard-output
                                 (call-process shell-file-name nil t nil
                                             shell-command-switch command))))
                      (exit-code (process-exit-status
                                 (get-buffer-process (current-buffer)))))
                 (list :success t
                       :output output
                       :exit_code (or exit-code 0)
                       :message "Command executed"))
             (error
              (list :success nil
                    :error "command_failed"
                    :message (format "Command execution failed: %s" (error-message-string err)))))

         ;; Denied
         (list :success nil
               :error "scope_violation"
               :resource_type "shell"
               :operation "exec"
               :resource command
               :allowed_commands (plist-get check-result :allowed_commands)
               :deny_commands (plist-get check-result :deny_commands)
               :message (format "Command '%s' not in approved command list. Allowed: %s. Use request_scope_expansion to ask user for approval."
                              command
                              (if (plist-get check-result :allowed_commands)
                                  (string-join (plist-get check-result :allowed_commands) ", ")
                                "none"))))))))
;; Run Approved Command Tool (Scope-Aware):1 ends here

;; Request Scope Expansion Tool (Meta Tool)

;; LLM uses this tool to explicitly request user approval for expanding scope.


;; [[file:scope-shell-tools.org::*Request Scope Expansion Tool (Meta Tool)][Request Scope Expansion Tool (Meta Tool):1]]
(gptel-make-tool
 :name "request_scope_expansion"
 :description "Request user approval to expand scope plan with new patterns for a specific tool.

Use this when a tool operation is denied and you need user permission.
Displays approval dialog to user explaining why access is needed.

The user will see:
- Which specific tool needs access (write_file_in_scope, edit_file_in_scope, etc.)
- Patterns you want to add
- Your justification for why access is needed

Returns success with patterns_added if user approves, or user_denied if rejected.

IMPORTANT: Always explain to the user WHY you need the expanded access in the justification.
Be specific about what you're trying to accomplish.

Available tool names:
- write_file_in_scope
- edit_file_in_scope
- create_roam_node_in_scope
- add_roam_tags_in_scope
- link_roam_nodes_in_scope
- run_approved_command"
 :args (list '(:name "tool_name"
               :type string
               :description "Specific tool name (e.g., 'write_file_in_scope', 'edit_file_in_scope', 'run_approved_command')")
             '(:name "patterns"
               :type array
               :items (:type string)
               :description "List of patterns to add (e.g., [\"/tmp/**\"] for filesystem tools, [\"tag:gptel\"] for org_roam, [\"npm\"] for shell)")
             '(:name "justification"
               :type string
               :description "Explain to user why this access is needed. Be specific about the task you're trying to accomplish."))
 :category "scope"
 :function
 (lambda (tool_name patterns justification)
   ;; Convert patterns from vector to list if needed (JSON arrays come as vectors)
   (when (vectorp patterns)
     (setq patterns (append patterns nil)))

   (let* ((plan (jf/gptel-scope--load-plan)))

     (unless plan
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")))

     ;; Show approval dialog to user
     (let ((approved
            (yes-or-no-p
             (format "═══════════════════════════════════════
SCOPE EXPANSION REQUEST
═══════════════════════════════════════

Tool: %s

Patterns to add:
  %s

Justification:
  %s

═══════════════════════════════════════
Approve this scope expansion? "
                     tool_name
                     (mapconcat #'identity patterns "\n  ")
                     justification))))

       (if approved
           ;; User approved - add patterns to the specific tool (v2.0 tool-level format)
           (progn
             (let* ((tools-data (or (plist-get plan :tools) nil))
                    (tool-key (intern (concat ":" tool_name)))
                    (tool-config (or (plist-get tools-data tool-key) nil))
                    (existing (plist-get tool-config :patterns))
                    (updated (append existing patterns)))

               ;; Ensure tool is enabled and has the new patterns
               (setq tool-config (plist-put tool-config :allowed t))
               (setq tool-config (plist-put tool-config :patterns updated))
               ;; Preserve deny_patterns if they exist
               (unless (plist-member tool-config :deny_patterns)
                 (setq tool-config (plist-put tool-config :deny_patterns nil)))
               (setq tools-data (plist-put tools-data tool-key tool-config))

               ;; Update plan with modified tools data (capture return value!)
               (setq plan (plist-put plan :tools tools-data)))

             ;; Save updated plan
             (jf/gptel-scope--save-plan plan)

             (list :success t
                   :patterns_added patterns
                   :message (format "Scope expanded. Added %d pattern(s) to %s"
                                  (length patterns) tool_name)))

         ;; User denied
         (list :success nil
               :user_denied t
               :message "User denied scope expansion request. Try a different approach or explain why the access is important."))))))
;; Request Scope Expansion Tool (Meta Tool):1 ends here

;; Inspect Scope Plan Tool (Meta Tool)

;; LLM uses this tool to view current scope plan and understand permissions before requesting expansion.


;; [[file:scope-shell-tools.org::*Inspect Scope Plan Tool (Meta Tool)][Inspect Scope Plan Tool (Meta Tool):1]]
(gptel-make-tool
 :name "inspect_scope_plan"
 :description "Inspect the current session's scope plan to understand tool permissions.

Returns information about:
- Which tools are enabled (allowed: true)
- What patterns each tool accepts
- What deny patterns exist
- Default policy

Use this BEFORE requesting scope expansion to:
- Check if your desired operation is already allowed
- Understand what patterns are currently approved
- Make informed scope expansion requests
- Suggest operations that fit within existing scope"
 :args (list)
 :category "scope"
 :function
 (lambda ()
   (let* ((plan (jf/gptel-scope--load-plan)))
     (if (not plan)
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")
       ;; Format plan as readable structure
       (let* ((tools-data (plist-get plan :tools))
              (formatted-tools
               (cl-loop for (tool-key tool-config) on tools-data by #'cddr
                        when (keywordp tool-key)
                        collect (list
                                 :tool (substring (symbol-name tool-key) 1)
                                 :enabled (plist-get tool-config :allowed)
                                 :patterns (plist-get tool-config :patterns)
                                 :deny_patterns (plist-get tool-config :deny_patterns)))))
         (list :success t
               :session_id (plist-get plan :session_id)
               :default_policy (plist-get plan :default_policy)
               :tools formatted-tools
               :message "Scope plan retrieved successfully. Check which tools are enabled and what patterns are allowed."))))))
;; Inspect Scope Plan Tool (Meta Tool):1 ends here

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
