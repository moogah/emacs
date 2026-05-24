;;; layouts.el --- Workspace layout commands -*- lexical-binding: t; -*-

(require 'workspace-data-model)
(require 'workspace-tabs)

(defun workspace--capture-frameset ()
  "Return a window-state capturing the selected frame's root window."
  (window-state-get (frame-root-window) 'writable))

(defun workspace--restore-frameset (state)
  "Apply window-state STATE to the selected frame's root window."
  (window-state-put state (frame-root-window) 'safe))

(defun workspace--update-recent-group (ws-name group-name)
  "Set WS-NAME's recent-layout-group to GROUP-NAME in the registry."
  (let ((ws (gethash ws-name workspace--registry)))
    (when ws
      (puthash ws-name
               (workspace--set-recent-group ws group-name)
               workspace--registry))))

(defun workspace--autosave-current-layout ()
  "Snapshot the current frame into the current workspace's recent layout.
Captures only the window-state; does NOT update `:buffer-files'.
That sync is intentionally limited to explicit user save paths
(`workspace-save'); doing it here would wipe the file list whenever
a user switched away from a tab whose buffers had been killed since
the last explicit save.  No-op when not on a workspace tab or when
the workspace has no recent layout-group yet."
  (let ((ws-name (workspace--current-name)))
    (when ws-name
      (let* ((ws (gethash ws-name workspace--registry))
             (group-name (and ws (workspace--recent-group ws))))
        (when (and ws group-name)
          (let* ((layout (workspace--layout-make (workspace--capture-frameset)))
                 (updated (workspace--upsert-group ws group-name layout)))
            (puthash ws-name updated workspace--registry)))))))

(defvar workspace--save-layout-history nil
  "Minibuffer history for `workspace-save-layout'.")

(defun workspace--current-group-names ()
  "Return the layout-group names of the current workspace."
  (let* ((name (workspace--current-name))
         (ws (and name (gethash name workspace--registry))))
    (mapcar #'workspace--group-name (workspace--layout-groups ws))))

(defun workspace-save-layout (name)
  "Save the current window configuration as layout NAME in the current workspace.

Re-saving an existing layout (including the reserved `home' layout)
overwrites it without invoking any builder."
  (interactive
   (list
    (completing-read "Save layout as: "
                     (workspace--current-group-names)
                     nil nil nil 'workspace--save-layout-history)))
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    (let* ((ws (gethash ws-name workspace--registry))
           (layout (workspace--layout-make (workspace--capture-frameset)))
           (updated (workspace--upsert-group ws name layout))
           (with-recent (workspace--set-recent-group updated name)))
      (puthash ws-name with-recent workspace--registry)
      name)))

(defun workspace-switch-layout (name)
  "Switch to layout NAME within the current workspace."
  (interactive
   (list
    (completing-read "Switch to layout: "
                     (workspace--current-group-names)
                     nil t)))
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    (let* ((ws (gethash ws-name workspace--registry))
           (group (workspace--find-group ws name)))
      (unless group
        (user-error "No layout named %s in workspace %s" name ws-name))
      ;; Snapshot the outgoing layout into its slot before restoring.
      (workspace--autosave-current-layout)
      (let ((layout (workspace--group-recent-layout group)))
        (when layout
          (workspace--restore-frameset (workspace--layout-frameset layout))))
      (workspace--update-recent-group ws-name name)
      name)))

(defun workspace-switch-to-recent-layout ()
  "Switch to the current workspace's most recently activated layout."
  (interactive)
  (let* ((ws-name (workspace--current-name))
         (ws (and ws-name (gethash ws-name workspace--registry)))
         (recent (and ws (workspace--recent-group ws))))
    (unless recent
      (user-error "No recent layout for this workspace"))
    (workspace-switch-layout recent)))

(defun workspace-delete-layout (name)
  "Delete layout NAME from the current workspace.

The reserved layout `home' cannot be deleted.  If the deleted layout
was the workspace's recent pointer, it is reassigned to the first
remaining group (or `home' if none)."
  (interactive
   (list
    (completing-read "Delete layout: "
                     (workspace--current-group-names)
                     nil t)))
  (when (workspace--group-name-reserved-p name)
    (user-error "Cannot delete reserved layout: %s" name))
  (let ((ws-name (workspace--current-name)))
    (unless ws-name
      (user-error "Not on a workspaces-managed tab"))
    (let* ((ws (gethash ws-name workspace--registry))
           (existed (workspace--find-group ws name)))
      (unless existed
        (user-error "No layout named %s" name))
      (let* ((updated (workspace--remove-group ws name))
             (remaining (workspace--layout-groups updated))
             (new-recent
              (cond
               ;; Pointer was something else; keep it.
               ((and (workspace--recent-group ws)
                     (not (equal (workspace--recent-group ws) name)))
                (workspace--recent-group ws))
               ;; Pointer was the deleted layout; pick a survivor.
               (remaining
                (workspace--group-name (car remaining)))
               (t "home"))))
        (puthash ws-name
                 (workspace--set-recent-group updated new-recent)
                 workspace--registry)
        name))))

(defun workspace--capture-home-layout ()
  "Stamp the current frame's window config as the current workspace's `home'."
  (let ((ws-name (workspace--current-name)))
    (when ws-name
      (let* ((ws (gethash ws-name workspace--registry))
             (layout (workspace--layout-make (workspace--capture-frameset)))
             (updated (workspace--upsert-group ws "home" layout))
             (with-recent (workspace--set-recent-group updated "home")))
        (puthash ws-name with-recent workspace--registry)))))

(advice-add 'workspace-new :after
            (lambda (&rest _)
              (workspace--capture-home-layout)))

(provide 'workspace-layouts)
;;; layouts.el ends here
