;;; workspaces.el --- Named tab-based workspaces -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup workspaces nil
  "Named tab-based workspaces with per-workspace layouts and buffer scoping."
  :group 'convenience
  :prefix "workspace-")

(declare-function workspace-default-home-builder "workspace-tabs" (workspace-name))

(defcustom workspace-home-builder #'workspace-default-home-builder
  "Function called to build the `home' layout for a newly-created workspace.
Called with one argument WORKSPACE-NAME, in the context of the freshly
activated workspace.  The default implementation opens
=<:home>/home.org= for the workspace via a registry lookup; any buffers
it displays become members of the new workspace."
  :type 'function
  :group 'workspaces)

(defcustom workspaces-default-parent-directory
  (expand-file-name "emacs-workspaces" (or (getenv "HOME") "~"))
  "Default parent directory under which `workspace-new' creates new workspaces.
When `workspace-new NAME' is invoked without a prefix arg, the new
workspace's home directory is `(expand-file-name NAME this-dir)'."
  :type 'directory
  :group 'workspaces)

(defun workspace--backtrace-visible-p ()
  "Return non-nil if a *Backtrace* window is visible on the current frame."
  (cl-some (lambda (w)
             (string= (buffer-name (window-buffer w)) "*Backtrace*"))
           (window-list nil 'no-mini)))

(defcustom workspace-anti-save-predicates
  '(active-minibuffer-window
    workspace--backtrace-visible-p)
  "List of nullary predicates consulted before any workspace autosave.
If any predicate returns non-nil, the autosave is skipped silently.
The explicit `workspace-save' command never consults this list."
  :type '(repeat function)
  :group 'workspaces)

(jf/load-module (expand-file-name "config/workspaces/data-model.el"        jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/home-org.el"          jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/scaffold.el"          jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/tabs.el"              jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/buffer-membership.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/layouts.el"           jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/persistence.el"       jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/workspaces-mode.el"   jf/emacs-dir))
(require 'workspaces-mode)

(defun workspace-re-anchor (name new-home)
  "Point broken workspace NAME at NEW-HOME on the filesystem.

Clears the broken-state tag and updates `:home'.  If
=(file-name-nondirectory (directory-file-name NEW-HOME))= differs
from NAME, the registry entry is renamed to match the new basename
(per the registry-name-equals-basename invariant).

The new path must exist and must be a directory."
  (interactive
   (list (completing-read "Re-anchor workspace: "
                          (workspace--registered-names) nil t)
         (file-name-as-directory
          (read-directory-name "New home directory: " nil nil t))))
  (let ((ws (gethash name workspace--registry)))
    (unless ws
      (user-error "No workspace named %s" name))
    (unless (file-directory-p new-home)
      (user-error "Not a directory: %s" new-home))
    (let* ((new-name (file-name-nondirectory
                      (directory-file-name new-home)))
           ;; Update :home AND :name in lockstep (per
           ;; register/invariant/registry-name-equals-basename, which
           ;; pins identity on the plist :name accessor, not on the
           ;; registry key — both must move together when basename
           ;; differs, otherwise the rename is lost across save/
           ;; restore because the serializer writes (workspace--name
           ;; ws) and the deserializer puthashes under the same).
           (next-ws (workspace--clear-broken
                     (workspace--set-name
                      (workspace--set-home ws new-home)
                      new-name))))
      ;; If basename changed, rename the registry key too.
      (cond
       ((string= name new-name)
        (puthash name next-ws workspace--registry))
       (t
        (when (gethash new-name workspace--registry)
          (user-error
           "Cannot rename to %s — a workspace with that name already exists"
           new-name))
        ;; Capture the tab index BEFORE we mutate the registry —
        ;; `workspace--tab-index-for' consults `workspace--registry'
        ;; for ownership, so after `remhash' the lookup would fail.
        (let ((tab-idx (workspace--tab-index-for name)))
          (remhash name workspace--registry)
          (puthash new-name next-ws workspace--registry)
          ;; Update tab label if a live tab exists.
          (when tab-idx
            (tab-bar-rename-tab new-name tab-idx)))))
      (workspace--flush-state)
      (message "Workspace re-anchored: %s → %s" name new-home))))

(defun workspace-delete (name)
  "Remove workspace NAME from the registry and close its tab.
The home directory and all its contents (including .git/, home.org,
and sessions/) remain on disk untouched.  To delete the directory
too, use `workspace-purge'.

Interactively, completes over registered workspace names."
  (interactive
   (list (completing-read "Delete workspace (unregister only): "
                          (workspace--registered-names) nil t)))
  (unless (gethash name workspace--registry)
    (user-error "No workspace named %s" name))
  (let ((tab-idx (workspace--tab-index-for name)))
    (when tab-idx
      (tab-bar-close-tab tab-idx)))
  (remhash name workspace--registry)
  (workspace--flush-state)
  (message "Workspace %s unregistered (home directory left on disk)" name))

(defun workspace-purge (name)
  "Unregister workspace NAME and recursively delete its home directory.
Prompts for `yes-or-no-p' confirmation showing the absolute path.

Refuses to operate when `:home' is not a descendant of
`workspaces-default-parent-directory' unless a prefix argument is
supplied.  This guards against accidentally purging an anchored
external project (e.g., =~/code/myproj/=).

For broken-state workspaces (=:home= missing on disk), no
filesystem deletion is attempted — the registry entry is still
removed (register/vocabulary/workspace-broken-disposition pins
purge as =:permitted= on broken)."
  (interactive
   (list (completing-read "Purge workspace (DELETES HOME DIR): "
                          (workspace--registered-names) nil t)))
  (let* ((ws (gethash name workspace--registry))
         (home (and ws (workspace--home ws))))
    (unless ws
      (user-error "No workspace named %s" name))
    (unless home
      (user-error "Workspace %s has no :home (data corruption?)" name))
    ;; Scope safeguard: refuse to nuke a dir outside the default parent
    ;; unless the user explicitly opts in via prefix arg.  Checked BEFORE
    ;; the yes-or-no-p so an anchored external project never even shows
    ;; the destructive prompt without a deliberate two-step opt-in
    ;; (design.md §D8).
    (unless (or current-prefix-arg
                (file-in-directory-p home workspaces-default-parent-directory))
      (user-error
       "Refusing to purge %s; outside %s. Use `C-u M-x workspace-purge' to override."
       home workspaces-default-parent-directory))
    ;; Final confirmation.
    (unless (yes-or-no-p (format "Recursively delete %s? " home))
      (user-error "Cancelled"))
    ;; Unregister first; even if delete-directory errors below the
    ;; registry is already clean.
    (let ((tab-idx (workspace--tab-index-for name)))
      (when tab-idx
        (tab-bar-close-tab tab-idx)))
    (remhash name workspace--registry)
    (workspace--flush-state)
    ;; Filesystem delete (best-effort; broken-home case has nothing
    ;; to do — :home is already absent).
    (when (file-directory-p home)
      (delete-directory home t))
    (message "Workspace %s purged" name)))

(global-set-key (kbd "C-x w n") #'workspace-new)
(global-set-key (kbd "C-x w s") #'workspace-switch)
(global-set-key (kbd "C-x w o") #'workspace-restore)
(global-set-key (kbd "C-x w S") #'workspace-save)
(global-set-key (kbd "C-x w l") #'workspace-switch-layout)
(global-set-key (kbd "C-x w L") #'workspace-save-layout)
(global-set-key (kbd "C-x w D") #'workspace-delete)
(global-set-key (kbd "C-x w P") #'workspace-purge)
(global-set-key (kbd "C-x w R") #'workspace-re-anchor)
(global-set-key (kbd "C-x w T") #'workspace-switch-to-recent-layout)
(global-set-key (kbd "C-x w r") #'workspace-revert)
(global-set-key (kbd "C-x w b") #'workspace-remove-buffer)

(defun workspace-sessions-dir ()
  "Return the sessions/ directory of the current workspace, or nil.

Returns nil when no workspace owns the current tab, when the workspace
is broken, or when its :home/sessions/ directory is missing on disk.

This function is the soft-dependency entry point used by other
subsystems (notably gptel/sessions) to route new artifacts under the
active workspace's home.  It MUST NOT signal and has no side effects."
  (when-let* ((name (workspace--current-name))
              (ws   (gethash name workspace--registry))
              ((not (workspace--broken-p ws)))
              (home (workspace--home ws)))
    (let ((dir (workspace--sessions-dir home)))
      (when (file-directory-p dir)
        dir))))

(when (fboundp 'workspace--restore)
  (workspace--restore))

(provide 'workspaces)
;;; workspaces.el ends here
