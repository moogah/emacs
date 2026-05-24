;;; workspaces.el --- Named tab-based workspaces -*- lexical-binding: t; -*-

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

(jf/load-module (expand-file-name "config/workspaces/data-model.el"        jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/tabs.el"              jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/buffer-membership.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/layouts.el"           jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/persistence.el"       jf/emacs-dir))

(global-set-key (kbd "C-x w n") #'workspace-new)
(global-set-key (kbd "C-x w s") #'workspace-switch)
(global-set-key (kbd "C-x w o") #'workspace-restore)
(global-set-key (kbd "C-x w S") #'workspace-save)
(global-set-key (kbd "C-x w l") #'workspace-switch-layout)
(global-set-key (kbd "C-x w L") #'workspace-save-layout)
(global-set-key (kbd "C-x w D") #'workspace-delete-layout)
(global-set-key (kbd "C-x w R") #'workspace-switch-to-recent-layout)
(global-set-key (kbd "C-x w r") #'workspace-remove-buffer)

(when (fboundp 'workspace--restore)
  (workspace--restore))

(provide 'workspaces)
;;; workspaces.el ends here
