;;; coordinator.el --- org-graph per-file write coordinator -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom org-graph-coordinator-timeout 5.0
  "Seconds to wait for a busy file lock before signalling a timeout.
Bounds the cooperative busy-wait in `org-graph-coordinator/with-file-lock'."
  :type 'number
  :group 'org-graph)

(define-error 'org-graph-coordinator-lock-timeout
  "org-graph coordinator: timed out acquiring file lock")

(defvar org-graph-coordinator--locks (make-hash-table :test 'equal)
  "Maps canonicalised absolute file paths to non-nil while locked.
Module-private; inspected by tests but not part of the public API.")

(defun org-graph-coordinator--canonical (path)
  "Return the canonical lock key for PATH.
Resolves symlinks and `~' so two spellings of the same file share a lock."
  (file-truename (expand-file-name path)))

(defun org-graph-coordinator--acquire (key)
  "Acquire the lock for KEY, busy-waiting up to `org-graph-coordinator-timeout'.
Signals `org-graph-coordinator-lock-timeout' (with KEY as data) if the
lock is still held when the deadline passes.  Returns KEY on success."
  (let ((deadline (+ (float-time) org-graph-coordinator-timeout)))
    (while (gethash key org-graph-coordinator--locks)
      (when (> (float-time) deadline)
        (signal 'org-graph-coordinator-lock-timeout (list key)))
      (accept-process-output nil 0.05))
    (puthash key t org-graph-coordinator--locks)
    key))

(defun org-graph-coordinator--release (key)
  "Release the lock for KEY."
  (remhash key org-graph-coordinator--locks))

(defmacro org-graph-coordinator/with-file-lock (path &rest body)
  "Run BODY with an exclusive cooperative lock on PATH.

PATH is evaluated once and canonicalised.  If the path is free the lock
is taken, BODY runs, and the lock is released in `unwind-protect' — so an
error or non-local exit in BODY still frees it.  If the path is busy this
busy-waits up to `org-graph-coordinator-timeout' seconds, then signals
`org-graph-coordinator-lock-timeout'.  Locks on distinct paths are
independent."
  (declare (indent 1) (debug (form body)))
  (let ((keyv (make-symbol "key")))
    `(let ((,keyv (org-graph-coordinator--canonical ,path)))
       (org-graph-coordinator--acquire ,keyv)
       (unwind-protect
           (progn ,@body)
         (org-graph-coordinator--release ,keyv)))))

(provide 'org-graph-coordinator)
;;; coordinator.el ends here
