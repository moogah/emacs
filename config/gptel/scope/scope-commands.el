;; Dependencies


;; [[file:scope-commands.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Project Selection

;; Interactive selection of 0 or more projectile projects.


;; [[file:scope-commands.org::*Project Selection][Project Selection:1]]
(defun jf/gptel--select-projects ()
  "Interactively select 0 or more projectile projects.
Returns list of absolute project root paths."
  (let ((projects (projectile-known-projects)))
    (if (null projects)
        (progn
          (message "No known projectile projects found")
          nil)
      (let* ((selection (completing-read-multiple
                        "Select projects (RET when done): "
                        projects
                        nil t)))
        (when selection
          (mapcar #'expand-file-name selection))))))
;; Project Selection:1 ends here

;; Project Display Name

;; Extract display name from project root path.


;; [[file:scope-commands.org::*Project Display Name][Project Display Name:1]]
(defun jf/gptel--project-display-name (project-root)
  "Extract display name from PROJECT-ROOT path.
Uses projectile-project-name if available, else basename."
  (condition-case nil
      (let ((default-directory project-root))
        (projectile-project-name))
    (error (file-name-nondirectory (directory-file-name project-root)))))
;; Project Display Name:1 ends here

;; Get Activity Org-File from Scope Plan

;; Extract activity org-file path from scope-plan.yml.


;; [[file:scope-commands.org::*Get Activity Org-File from Scope Plan][Get Activity Org-File from Scope Plan:1]]
(defun jf/gptel-scope--get-activity-org-file (scope-plan-file)
  "Extract activity_org_file path from SCOPE-PLAN-FILE.
Returns the org-roam document path if present, nil otherwise."
  (when (file-exists-p scope-plan-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents scope-plan-file)
          (goto-char (point-min))
          (when (re-search-forward "^activity_org_file: \"\\([^\"]+\\)\"" nil t)
            (match-string 1)))
      (error
       (message "Warning: Failed to parse activity_org_file from %s: %s"
                scope-plan-file (error-message-string err))
       nil))))
;; Get Activity Org-File from Scope Plan:1 ends here

;; Parse Preset Tools

;; Extract tool list and settings from preset file.


;; [[file:scope-commands.org::*Parse Preset Tools][Parse Preset Tools:1]]
(defun jf/gptel-scope--parse-preset-tools (branch-dir)
  "Extract tools and their settings from preset file in BRANCH-DIR.

Reads preset.md or preset.org from BRANCH-DIR, parses YAML frontmatter
or org properties to extract :tools field.

Returns alist of (tool-name . allowed-bool), or nil if:
- Preset file doesn't exist
- File has no tools field
- Error parsing file

Supports formats:
- Map: {tool1: {allowed: true}, tool2: {allowed: false}}
- List (legacy): [\"tool1\", \"tool2\"] - defaults to allowed: true"
  (when branch-dir
    (let* ((md-file (expand-file-name "preset.md" branch-dir))
           (org-file (expand-file-name "preset.org" branch-dir))
           (preset-file (cond
                         ((file-exists-p md-file) md-file)
                         ((file-exists-p org-file) org-file)
                         (t nil))))
      (when preset-file
        (condition-case err
            (progn
              (require 'gptel-agent nil t)
              (when (fboundp 'gptel-agent-read-file)
                (let* ((preset-data (gptel-agent-read-file preset-file '((dummy . dummy))))
                       (preset-plist (cdr preset-data))
                       (tools (plist-get preset-plist :tools)))
                  (cond
                   ;; Map format: {tool1: {allowed: true}, tool2: {allowed: false}}
                   ;; Parsed as plist: (:tool1 (:allowed t) :tool2 (:allowed nil))
                   ((and (listp tools) (keywordp (car tools)))
                    (let ((result nil))
                      (while tools
                        (let* ((tool-key (pop tools))
                               (tool-props (pop tools))
                               (tool-name (substring (symbol-name tool-key) 1)) ; Remove leading :
                               (allowed (if (listp tool-props)
                                           (plist-get tool-props :allowed)
                                         t))) ; Default to true if no props
                          (push (cons tool-name allowed) result)))
                      (nreverse result)))

                   ;; List of strings (legacy) - default all to allowed: true
                   ((and (listp tools) (stringp (car tools)))
                    (mapcar (lambda (tool) (cons tool t)) tools))

                   ;; Space-separated string (from org properties) - default to allowed: true
                   ((stringp tools)
                    (mapcar (lambda (tool) (cons tool t)) (split-string tools)))

                   ;; Nil or unknown format
                   (t nil)))))
          (error
           (message "Warning: Failed to parse preset tools from %s: %s"
                    preset-file (error-message-string err))
           nil))))))
;; Parse Preset Tools:1 ends here

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
          (let* ((template (intern (completing-read "Template: "
                                                   '("deny-all" "codebase-read" "org-roam-safe"
                                                     "permissive" "project-aware")
                                                   nil t nil nil "deny-all")))
                 (template-args (when (eq template 'project-aware)
                                 (list (jf/gptel--select-projects)))))
            (with-temp-file plan-file
              (insert (apply (intern (format "jf/gptel-scope--template-%s" template))
                           session-id template-args)))
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
