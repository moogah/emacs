;;; buffer-membership.el --- Per-workspace buffer scoping -*- lexical-binding: t; -*-

(require 'workspace-data-model)
(require 'workspace-tabs)

(use-package bufferlo
  :straight t
  :init
  (bufferlo-mode 1))

(defun workspace--current-file-buffers ()
  "Return the file-backed buffers in the current tab's bufferlo list.
Buffers without an associated file (e.g. *Messages*, magit-status)
are omitted: they cannot be persisted across restarts (design.md §D4).
They remain live members for the duration of the session — bufferlo
tracks them in its in-memory list; only on-disk persistence is lossy
for non-file buffers."
  (let (files)
    (dolist (buf (and (fboundp 'bufferlo-buffer-list) (bufferlo-buffer-list)))
      (let ((path (buffer-file-name buf)))
        (when path (push path files))))
    (nreverse files)))

(defun workspace--sync-registry-from-bufferlo (&optional name)
  "Update workspace NAME's :buffer-files from bufferlo's current view.
NAME defaults to the current workspace.  No-op when not on a workspace
tab."
  (let ((name (or name (workspace--current-name))))
    (when (and name (gethash name workspace--registry))
      (let* ((ws (gethash name workspace--registry))
             (files (workspace--current-file-buffers))
             (updated (let ((copy (copy-sequence ws)))
                        (plist-put copy :buffer-files files))))
        (puthash name updated workspace--registry)))))

(defun workspace--on-buffer-killed ()
  "Sync the registry to reflect that the buffer being killed is gone.

Currently unused.  See `:before' the source block for the rationale —
this is dead code preserved against a future opt-in design."
  (let ((path (buffer-file-name)))
    (when path
      (maphash
       (lambda (name ws)
         (when (member path (workspace--buffer-files ws))
           (puthash name (workspace--remove-buffer-file ws path)
                    workspace--registry)))
       workspace--registry))))

(defun workspace-remove-buffer (buffer)
  "Remove BUFFER from the current workspace's membership without killing it.

Interactively, prompts for a buffer (default: current buffer).  The
prompt is scoped to the current workspace's membership.  The
underlying buffer remains live; if it is a member of other
workspaces it remains a member there."
  (interactive
   (list
    (let* ((default (buffer-name))
           (cands (mapcar #'buffer-name
                          (and (fboundp 'bufferlo-buffer-list)
                               (bufferlo-buffer-list)))))
      (get-buffer
       (completing-read
        (format "Remove buffer from workspace (default %s): " default)
        cands nil nil nil nil default)))))
  (let ((name (workspace--current-name)))
    (unless name
      (user-error "Not on a workspaces-managed tab"))
    (when (and (fboundp 'bufferlo-remove) (buffer-live-p buffer))
      (bufferlo-remove buffer))
    (let* ((ws (gethash name workspace--registry))
           (path (and (buffer-live-p buffer) (buffer-file-name buffer))))
      (when (and ws path)
        (puthash name (workspace--remove-buffer-file ws path)
                 workspace--registry)))
    buffer))

(provide 'workspace-buffer-membership)
;;; buffer-membership.el ends here
