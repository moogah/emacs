;;; layouts.el --- Workspace layout commands -*- lexical-binding: t; -*-

(require 'workspace-data-model)
(require 'workspace-tabs)
(require 'frameset)

(defun workspace--readable-p (form)
  "Return non-nil when FORM round-trips through `prin1' + `read'.
Used to drop frame parameters whose values reference live objects
(buffers, windows, hash tables of buffers) that have no `read' syntax."
  (let ((str (let ((print-length nil) (print-level nil))
               (condition-case nil (prin1-to-string form) (error nil)))))
    (and str
         (not (string-match-p "#<" str))
         (condition-case nil
             (progn (read str) t)
           (error nil)))))

(defun workspace--sanitize-frame-params (params)
  "Drop frame parameters from PARAMS whose values are not `read'-able.

This is the safety net for foreign-package frame parameters (e.g.
=perspective.el='s =persp--hash=, which embeds raw `#<buffer>'
references).  Survives during the activities/perspective side-by-side
period (design.md §D8) and becomes a no-op once those packages are
unloaded by the cutover task."
  (cl-remove-if-not
   (lambda (pair) (workspace--readable-p (cdr pair)))
   params))

(defun workspace--sanitize-frameset (frameset)
  "Return FRAMESET with non-printable frame parameters removed.

`frameset' is a `cl-defstruct' with `:type vector :named'.  The slots
are (0 tag, 1 version, 2 timestamp, 3 app, 4 name, 5 description,
6 properties, 7 states).  `states' is a list of
\(FRAME-PARAMS WINDOW-CONFIG ...\) entries; we walk it and strip
non-readable parameters."
  (when (and (vectorp frameset) (eq (aref frameset 0) 'frameset))
    (let* ((states (frameset-states frameset))
           (clean
            (mapcar
             (lambda (state)
               (if (and (consp state) (consp (car state)))
                   (cons (workspace--sanitize-frame-params (car state))
                         (cdr state))
                 state))
             states)))
      (setf (frameset-states frameset) clean)))
  frameset)

(defun workspace--capture-frameset ()
  "Return a frameset capturing the selected frame's window configuration.

The result is sanitized so it round-trips through `prin1' / `read':
frame parameters added by foreign packages (e.g. `perspective.el')
that reference unprintable objects are dropped."
  (let* ((frameset-filter-alist (or (bound-and-true-p frameset-persistent-filter-alist)
                                    frameset-filter-alist))
         (fs (frameset-save (list (selected-frame)))))
    (workspace--sanitize-frameset fs)))

(defun workspace--restore-frameset (frameset)
  "Restore FRAMESET into the selected frame."
  ;; `:reuse-frames t' attempts to reuse the selected frame.
  (let ((frameset-filter-alist (or (bound-and-true-p frameset-persistent-filter-alist)
                                   frameset-filter-alist)))
    (frameset-restore frameset :reuse-frames t :cleanup-frames t)))

(defun workspace--update-recent-group (ws-name group-name)
  "Set WS-NAME's recent-layout-group to GROUP-NAME in the registry."
  (let ((ws (gethash ws-name workspace--registry)))
    (when ws
      (puthash ws-name
               (workspace--set-recent-group ws group-name)
               workspace--registry))))

(defun workspace--autosave-current-layout ()
  "Snapshot the current frame into the current workspace's recent layout.
No-op when not on a workspace tab or when the workspace has no recent
layout-group yet."
  (let ((ws-name (workspace--current-name)))
    (when ws-name
      (let* ((ws (gethash ws-name workspace--registry))
             (group-name (and ws (workspace--recent-group ws))))
        (when (and ws group-name)
          (let* ((layout (workspace--layout-make (workspace--capture-frameset)))
                 (updated (workspace--upsert-group ws group-name layout)))
            (puthash ws-name updated workspace--registry)))))))

(defun workspace--autosave-before-select (&rest _args)
  "Capture the outgoing tab's layout before `tab-bar-select-tab' switches.
Wraps `workspace--autosave-current-layout' so the snapshot is
attributed to the tab being left."
  (workspace--autosave-current-layout))

(advice-add 'tab-bar-select-tab :before #'workspace--autosave-before-select)

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
