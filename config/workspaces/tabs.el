;;; tabs.el --- Workspaces tab-bar integration -*- lexical-binding: t; -*-

(require 'tab-bar)
(require 'workspace-data-model)

(defvar workspace--registry (make-hash-table :test 'equal)
  "WORKSPACE-NAME → workspace plist.
Mutated by all `workspace-*' commands.  The registry is the in-memory
source of truth; persistence (later task) walks it to produce the
on-disk form.")

(defun workspace--registered-names ()
  "Return the list of workspace names currently in the registry."
  (let (names)
    (maphash (lambda (k _v) (push k names)) workspace--registry)
    (nreverse names)))

(defun workspace--frame-current-tab ()
  "Return the live current-tab alist from the selected frame's `tabs' parameter.
Distinct from `tab-bar--current-tab', which returns a fresh copy."
  (assq 'current-tab (frame-parameter nil 'tabs)))

(defun workspace--tab-name (tab)
  "Return the `name' slot of TAB, or nil."
  (cdr (assq 'name tab)))

(defun workspace--tab-workspace-name (tab)
  "Return the workspace name associated with TAB, or nil.

A tab is considered owned by workspaces if its `name' matches a key
in `workspace--registry'."
  (let ((name (workspace--tab-name tab)))
    (when (and name (gethash name workspace--registry))
      name)))

(defun workspace--current-name ()
  "Return the workspace name of the currently selected tab, or nil."
  (workspace--tab-workspace-name (workspace--frame-current-tab)))

(defun workspace--tab-for (name)
  "Return the live tab alist whose `name' is NAME and is in the registry."
  (seq-find (lambda (tab)
              (and (equal (workspace--tab-name tab) name)
                   (gethash name workspace--registry)))
            (frame-parameter nil 'tabs)))

(defun workspace--tab-index-for (name)
  "Return the 1-based index of the tab whose `name' is NAME (workspace).
Returns nil if no such tab exists."
  (let ((tabs (frame-parameter nil 'tabs))
        (i 1)
        result)
    (while (and tabs (not result))
      (when (and (equal (workspace--tab-name (car tabs)) name)
                 (gethash name workspace--registry))
        (setq result i))
      (setq tabs (cdr tabs)
            i (1+ i)))
    result))

(defun workspace-default-home-builder (_workspace-name)
  "Default `workspace-home-builder': show a single window on *scratch*."
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;; CYCLE-1 PLACEHOLDER: this helper synthesizes a HOME path so that
;; `workspace--make' (whose signature now requires HOME) has something
;; to anchor to.  Cycle 3's `workspace-new-default-path' task replaces
;; this with the proper `workspaces-default-parent-directory'
;; defcustom + scaffold pipeline (which also creates the directory on
;; disk).  Until then, hardcode a sensible default so workspace-new
;; produces a valid :home and the branch stays buildable.  See task
;; `wire-home-into-callsites'.
(defun wire-home-into-callsites--synthesize-home (name)
  "Synthesize a placeholder home path for NAME.
This is a CYCLE-1 PLACEHOLDER replaced by cycle-3's
`workspace-new-default-path' task (which introduces the
`workspaces-default-parent-directory' defcustom + scaffold pipeline)."
  (file-name-as-directory
   (expand-file-name
    name
    (expand-file-name "emacs-workspaces" (or (getenv "HOME") "~")))))

(defun workspace-new (name)
  "Create a new workspace named NAME (or select it if it already exists).

A new tab is created and renamed to NAME; the registry entry is
inserted before the home builder runs so the new workspace is
recognized by `workspace--current-name' from the builder onward.
The home builder runs in the context of the new tab; buffers it
displays become workspace members via the buffer-membership module."
  (interactive "sWorkspace name: ")
  (let ((existing-tab (workspace--tab-for name)))
    (if existing-tab
        (progn
          (tab-bar-select-tab (workspace--tab-index-for name))
          (gethash name workspace--registry))
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)
      ;; Register BEFORE running the home builder so the new tab is
      ;; recognized as owned by workspaces from this point on.
      (let ((ws (workspace--make
                 name
                 (wire-home-into-callsites--synthesize-home name))))
        (puthash name ws workspace--registry))
      (when (and (boundp 'workspace-home-builder)
                 (functionp workspace-home-builder))
        (funcall workspace-home-builder name))
      (gethash name workspace--registry))))

(defun workspace-switch (name)
  "Select the tab for workspace NAME.

Interactively completes over workspaces currently backed by a tab.

Signals `user-error' if NAME is in a broken state (its `:home' no
longer exists on disk).  Use `workspace-re-anchor' to point it at a
new path or `workspace-purge' to remove the registry entry."
  (interactive
   (list (completing-read
          "Switch to workspace: "
          (seq-filter #'workspace--tab-for (workspace--registered-names))
          nil t)))
  (let ((ws (gethash name workspace--registry)))
    (when (and ws (workspace--broken-p ws))
      (user-error
       "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
       name (workspace--home ws))))
  (let ((idx (workspace--tab-index-for name)))
    (unless idx
      (user-error "No tab found for workspace %s" name))
    (tab-bar-select-tab idx)))

(provide 'workspace-tabs)
;;; tabs.el ends here
