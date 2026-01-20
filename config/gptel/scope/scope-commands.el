;; Dependencies


;; [[file:scope-commands.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Deny-All Template (Secure Default)

;; Nothing allowed by default. User must explicitly add patterns.


;; [[file:scope-commands.org::*Deny-All Template (Secure Default)][Deny-All Template (Secure Default):1]]
(defun jf/gptel-scope--template-deny-all (session-id)
  "Secure default template - nothing allowed, user must add patterns.
Recommended for new sessions where you want explicit control."
  (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
default_policy: deny

filesystem:
  write: []
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"
    - \"**/node_modules/**\"
    - \"**/.env\"

org_roam:
  write: []
  link: []

shell:
  allow_commands: []
  deny_commands:
    - \"rm -rf\"
    - \"sudo\"
    - \"chmod\"
    - \"chown\"
"
          session-id
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
;; Deny-All Template (Secure Default):1 ends here

;; Codebase Read Template

;; Read operations allowed, limited safe shell commands for exploration.


;; [[file:scope-commands.org::*Codebase Read Template][Codebase Read Template:1]]
(defun jf/gptel-scope--template-codebase-read (session-id)
  "Read-only exploration template.
Allows safe shell commands for code exploration (ls, find, grep, git log).
No write operations allowed - LLM must request permission."
  (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
default_policy: deny

filesystem:
  write: []
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"
    - \"**/node_modules/**\"
    - \"**/.env\"

org_roam:
  write: []
  link: []

shell:
  allow_commands:
    - \"ls\"
    - \"find\"
    - \"grep\"
    - \"git status\"
    - \"git log\"
    - \"git diff\"
    - \"cat\"
    - \"head\"
    - \"tail\"
  deny_commands:
    - \"rm\"
    - \"sudo\"
    - \"chmod\"
    - \"chown\"
    - \"mv\"
    - \"cp\"
"
          session-id
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
;; Codebase Read Template:1 ends here

;; Org-Roam Safe Template

;; Allow org-roam operations in gptel/ subdirectory with gptel tag.


;; [[file:scope-commands.org::*Org-Roam Safe Template][Org-Roam Safe Template:1]]
(defun jf/gptel-scope--template-org-roam-safe (session-id)
  "Org-roam safe template.
Allows creating and linking nodes in gptel/ subdirectory with gptel tag.
Safe for LLM-generated notes that are clearly separated from personal notes."
  (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
default_policy: deny

filesystem:
  write:
    - \"%s/gptel/**/*.org\"
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"

org_roam:
  write:
    - \"subdirectory:gptel/**\"
    - \"tag:gptel\"
  link:
    - \"node_id:*\"

shell:
  allow_commands:
    - \"ls\"
    - \"find\"
    - \"grep\"
  deny_commands:
    - \"rm\"
    - \"sudo\"
"
          session-id
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")
          (format-time-string "%Y-%m-%dT%H:%M:%SZ")
          org-roam-directory))
;; Org-Roam Safe Template:1 ends here

;; Permissive Template

;; Broad access for trusted workflows. Use with caution.


;; [[file:scope-commands.org::*Permissive Template][Permissive Template:1]]
(defun jf/gptel-scope--template-permissive (session-id)
  "Permissive template - broad access.
USE WITH CAUTION: Allows wide filesystem access and many shell commands.
Only use when you trust the LLM workflow and want minimal restrictions."
  (let ((project-root (or (projectile-project-root) default-directory)))
    (format "version: \"1.0\"
session_id: \"%s\"
created: \"%s\"
updated: \"%s\"
default_policy: deny

filesystem:
  write:
    - \"%s/**\"
    - \"/tmp/**\"
  deny:
    - \"**/.git/**\"
    - \"**/runtime/**\"
    - \"**/node_modules/**\"
    - \"**/.env\"

org_roam:
  write:
    - \"subdirectory:**\"
    - \"tag:*\"
  link:
    - \"node_id:*\"

shell:
  allow_commands:
    - \"ls\"
    - \"find\"
    - \"grep\"
    - \"git status\"
    - \"git log\"
    - \"git diff\"
    - \"cat\"
    - \"head\"
    - \"tail\"
    - \"npm\"
    - \"node\"
    - \"python\"
  deny_commands:
    - \"rm -rf\"
    - \"sudo\"
"
            session-id
            (format-time-string "%Y-%m-%dT%H:%M:%SZ")
            (format-time-string "%Y-%m-%dT%H:%M:%SZ")
            project-root)))
;; Permissive Template:1 ends here

;; Generate Scope Plan YAML

;; Helper function to generate scope plan YAML string without user interaction.
;; Used for auto-initialization and programmatic plan creation.


;; [[file:scope-commands.org::*Generate Scope Plan YAML][Generate Scope Plan YAML:1]]
(defun jf/gptel--generate-scope-plan-yaml (session-id &optional template)
  "Generate scope plan YAML for SESSION-ID using TEMPLATE.
TEMPLATE can be deny-all, codebase-read, org-roam-safe, or permissive.
Defaults to deny-all.

This is an internal helper used for auto-initialization of session scope plans.
Returns YAML string directly without writing to file."
  (let* ((template (or template "deny-all"))
         (template-fn (intern (format "jf/gptel-scope--template-%s" template))))
    (unless (fboundp template-fn)
      (error "Unknown scope template: %s" template))
    (funcall template-fn session-id)))
;; Generate Scope Plan YAML:1 ends here

;; Initialize Scope Plan

;; Create new scope plan for current gptel session.


;; [[file:scope-commands.org::*Initialize Scope Plan][Initialize Scope Plan:1]]
(defun jf/gptel-scope-init-plan (&optional template)
  "Create scope plan for current gptel session.
Requires being in gptel buffer, auto-initializes session if needed.

TEMPLATE: Symbol for template to use (deny-all, codebase-read, org-roam-safe, permissive)
         If nil, prompts user to select template."
  (interactive
   (list (intern (completing-read "Template: "
                                  '("deny-all" "codebase-read" "org-roam-safe" "permissive")
                                  nil t nil nil "deny-all"))))
  (unless gptel-mode
    (user-error "Not in a gptel buffer. Start one with M-x gptel first"))

  ;; Auto-initialize session if needed
  (when (and (not jf/gptel--session-id) jf/gptel-autosave-enabled)
    (message "Initializing new session...")
    (jf/gptel--initialize-session))

  (unless jf/gptel--session-id
    (user-error "No session ID. Enable autosave with M-x jf/gptel-toggle-autosave"))

  (let* ((session-id jf/gptel--session-id)
         (session-data (jf/gptel--get-session-data session-id))
         (session-dir (plist-get session-data :directory))
         (plan-file (expand-file-name "scope-plan.yml" session-dir))
         (template-fn (intern (format "jf/gptel-scope--template-%s" template)))
         (plan-content (funcall template-fn session-id)))

    ;; Check if plan already exists
    (when (file-exists-p plan-file)
      (unless (yes-or-no-p "Scope plan already exists. Overwrite? ")
        (user-error "Cancelled")))

    ;; Write plan file
    (with-temp-file plan-file
      (insert plan-content))

    ;; Open for editing
    (find-file plan-file)
    (message "Scope plan created: %s (Edit and save to customize)" plan-file)))
;; Initialize Scope Plan:1 ends here

;; Edit Scope Plan

;; Open existing scope plan for editing.


;; [[file:scope-commands.org::*Edit Scope Plan][Edit Scope Plan:1]]
(defun jf/gptel-scope-edit-plan (&optional session-id)
  "Open scope plan in editor.
Works from anywhere - prompts for session if not in gptel buffer.

SESSION-ID: Optional session ID. If nil, uses current session or prompts."
  (interactive)
  (let* ((session-id (or session-id
                        (and (boundp 'jf/gptel--session-id) jf/gptel--session-id)
                        (let ((sessions (jf/gptel--list-all-sessions)))
                          (when sessions
                            (completing-read "Select session: " sessions nil t)))))
         (session-data (when session-id (jf/gptel--get-session-data session-id)))
         (session-dir (when session-data (plist-get session-data :directory)))
         (plan-file (when session-dir (expand-file-name "scope-plan.yml" session-dir))))

    (unless session-id
      (user-error "No session selected or found"))

    (unless (file-exists-p plan-file)
      (if (yes-or-no-p (format "No plan found for session %s. Create one? " session-id))
          (let ((template (intern (completing-read "Template: "
                                                  '("deny-all" "codebase-read" "org-roam-safe" "permissive")
                                                  nil t nil nil "deny-all"))))
            (with-temp-file plan-file
              (insert (funcall (intern (format "jf/gptel-scope--template-%s" template)) session-id)))
            (find-file plan-file)
            (message "Scope plan created: %s" plan-file))
        (user-error "Cancelled")))

    (find-file plan-file)))
;; Edit Scope Plan:1 ends here

;; List Session Helpers (For Completion)

;; Helper functions to list sessions for command completion.


;; [[file:scope-commands.org::*List Session Helpers (For Completion)][List Session Helpers (For Completion):1]]
(defun jf/gptel--list-all-sessions ()
  "List all gptel session IDs (active and archived).
Returns list of session ID strings."
  (let ((session-dir (expand-file-name "~/gptel-sessions"))
        (sessions nil))
    (when (file-directory-p session-dir)
      (dolist (entry (directory-files session-dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"))
        (when (file-directory-p (expand-file-name entry session-dir))
          (push entry sessions))))
    (nreverse sessions)))
;; List Session Helpers (For Completion):1 ends here

;; Provide Feature


;; [[file:scope-commands.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-commands)
;;; scope-commands.el ends here
;; Provide Feature:1 ends here
