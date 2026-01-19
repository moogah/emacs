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
   (let* ((session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

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
 :description "Request user approval to expand scope plan with new patterns.

Use this when operations are denied by scope and you need user permission.
Displays approval dialog to user explaining why access is needed.

The user will see:
- Resource type (filesystem, org_roam, shell)
- Patterns you want to add
- Your justification for why access is needed

Returns success with patterns_added if user approves, or user_denied if rejected.

IMPORTANT: Always explain to the user WHY you need the expanded access in the justification.
Be specific about what you're trying to accomplish."
 :args (list '(:name "resource_type"
               :type string
               :description "Type: 'filesystem', 'org_roam', or 'shell'")
             '(:name "patterns"
               :type array
               :items (:type string)
               :description "List of patterns to add to scope (e.g., [\"/tmp/**\"] for filesystem, [\"tag:gptel\"] for org_roam, [\"npm\"] for shell)")
             '(:name "justification"
               :type string
               :description "Explain to user why this access is needed. Be specific about the task you're trying to accomplish."))
 :category "scope"
 :function
 (lambda (resource-type patterns justification)
   ;; Convert patterns from vector to list if needed (JSON arrays come as vectors)
   (when (vectorp patterns)
     (setq patterns (append patterns nil)))

   (let* ((session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

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

Resource type: %s

Patterns to add:
  %s

Justification:
  %s

═══════════════════════════════════════
Approve this scope expansion? "
                     resource-type
                     (mapconcat #'identity patterns "\n  ")
                     justification))))

       (if approved
           ;; User approved - add patterns to plan
           (progn
             (pcase resource-type
               ("filesystem"
                (let* ((fs-data (plist-get plan :filesystem))
                       (existing (plist-get fs-data :write))
                       (updated (append existing patterns)))
                  (plist-put fs-data :write updated)
                  (plist-put plan :filesystem fs-data)))
               ("org_roam"
                (let* ((roam-data (plist-get plan :org_roam))
                       (existing (plist-get roam-data :write))
                       (updated (append existing patterns)))
                  (plist-put roam-data :write updated)
                  (plist-put plan :org_roam roam-data)))
               ("shell"
                (let* ((shell-data (plist-get plan :shell))
                       (existing (plist-get shell-data :allow_commands))
                       (updated (append existing patterns)))
                  (plist-put shell-data :allow_commands updated)
                  (plist-put plan :shell shell-data))))

             ;; Save updated plan
             (jf/gptel-scope--save-plan session-id plan)

             (list :success t
                   :patterns_added patterns
                   :message (format "Scope expanded. Added %d pattern(s) to %s"
                                  (length patterns) resource-type)))

         ;; User denied
         (list :success nil
               :user_denied t
               :message "User denied scope expansion request. Try a different approach or explain why the access is important."))))))
;; Request Scope Expansion Tool (Meta Tool):1 ends here

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
