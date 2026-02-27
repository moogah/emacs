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

;; Parse Preset Tools

;; Extract tool list from the preset registry (=gptel--known-presets=).

;; Reads the preset name from =metadata.yml= in the branch directory, then
;; looks up the preset's =:tools= list from the in-memory registry.


;; [[file:scope-commands.org::*Parse Preset Tools][Parse Preset Tools:1]]
(defun jf/gptel-scope--parse-preset-tools (branch-dir)
  "Extract tools from the preset registered for the session in BRANCH-DIR.

Reads the preset name from metadata.yml in BRANCH-DIR, then looks up the
preset's :tools list from `gptel--known-presets'.

Returns alist of (tool-name . t) for each tool, or nil if:
- BRANCH-DIR is nil
- metadata.yml doesn't exist or has no preset field
- Preset not found in `gptel--known-presets'
- Preset has no :tools"
  (when branch-dir
    (condition-case err
        (let* ((metadata-file (expand-file-name jf/gptel-session--metadata-file branch-dir))
               (preset-name
                (when (file-exists-p metadata-file)
                  (with-temp-buffer
                    (insert-file-contents metadata-file)
                    (goto-char (point-min))
                    (when (re-search-forward "^preset: \"\\([^\"]+\\)\"" nil t)
                      (match-string 1))))))
          (when preset-name
            (let* ((preset-plist (alist-get preset-name gptel--known-presets
                                           nil nil #'string=))
                   (tools (plist-get preset-plist :tools)))
              (when (listp tools)
                (mapcar (lambda (tool)
                          (cons (if (symbolp tool) (symbol-name tool) tool) t))
                        tools)))))
      (error
       (message "Warning: Failed to parse preset tools for %s: %s"
                branch-dir (error-message-string err))
       nil))))
;; Parse Preset Tools:1 ends here

;; List All Sessions

;; Helper function to list sessions for command completion.


;; [[file:scope-commands.org::*List All Sessions][List All Sessions:1]]
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
;; List All Sessions:1 ends here

;; Provide Feature


;; [[file:scope-commands.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-commands)
;;; scope-commands.el ends here
;; Provide Feature:1 ends here
