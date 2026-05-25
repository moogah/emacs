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
activated workspace.  Any buffers it displays become members of the
new workspace."
  :type 'function
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

(global-set-key (kbd "C-x w n") #'workspace-new)
(global-set-key (kbd "C-x w s") #'workspace-switch)
(global-set-key (kbd "C-x w o") #'workspace-restore)
(global-set-key (kbd "C-x w S") #'workspace-save)
(global-set-key (kbd "C-x w l") #'workspace-switch-layout)
(global-set-key (kbd "C-x w L") #'workspace-save-layout)
(global-set-key (kbd "C-x w D") #'workspace-delete-layout)
(global-set-key (kbd "C-x w R") #'workspace-switch-to-recent-layout)
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
    (let ((dir (expand-file-name "sessions" home)))
      (when (file-directory-p dir)
        dir))))

(when (fboundp 'workspace--restore)
  (workspace--restore))

(provide 'workspaces)
;;; workspaces.el ends here
