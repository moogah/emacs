;;; registry.el --- GPTEL Session Registry -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Global registry for gptel sessions.
;; Tracks active sessions and provides lookup functions.

;;; Code:

(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)

(defvar jf/gptel--session-registry (make-hash-table :test 'equal)
  "Global registry of active gptel sessions.
Keys are session-id strings, values are session plists.")

(defun jf/gptel--init-registry ()
  "Initialize the session registry.
Discovers all session directories and loads their metadata."
  (clrhash jf/gptel--session-registry)
  (let ((session-dirs (jf/gptel--list-session-directories)))
    (dolist (dir session-dirs)
      (when (jf/gptel--valid-session-directory-p dir)
        (let* ((session-id (jf/gptel--session-id-from-directory dir))
               (metadata-file (jf/gptel--metadata-file-path dir))
               (metadata (when (file-exists-p metadata-file)
                          (jf/gptel--read-metadata-file metadata-file))))
          (when metadata
            (puthash session-id
                    (list :session-id session-id
                          :directory dir
                          :buffer nil
                          :metadata metadata
                          :created (plist-get metadata :created)
                          :last-accessed (current-time))
                    jf/gptel--session-registry)
            (jf/gptel--log 'debug "Registered session: %s" session-id)))))
    (jf/gptel--log 'info "Initialized registry with %d sessions"
                  (hash-table-count jf/gptel--session-registry))))

(defun jf/gptel--register-session (session-dir metadata &optional buffer session-id)
  "Register a session in the global registry.
SESSION-DIR is the absolute path to the session directory.
METADATA is the metadata plist.
BUFFER is the optional buffer visiting this session.
SESSION-ID is optional; if not provided, extracted from session-dir."
  (let ((session-id (or session-id
                       (jf/gptel--session-id-from-directory session-dir))))
    (puthash session-id
            (list :session-id session-id
                  :directory session-dir
                  :buffer buffer
                  :metadata metadata
                  :created (plist-get metadata :created)
                  :last-accessed (current-time))
            jf/gptel--session-registry)
    (jf/gptel--log 'info "Registered session: %s" session-id)
    session-id))

(defun jf/gptel--unregister-session (session-id)
  "Remove SESSION-ID from the registry."
  (remhash session-id jf/gptel--session-registry)
  (jf/gptel--log 'info "Unregistered session: %s" session-id))

(defun jf/gptel-session-find (session-id)
  "Find session by SESSION-ID in registry.
Returns session plist or nil if not found."
  (gethash session-id jf/gptel--session-registry))

(defun jf/gptel--session-exists-p (session-id)
  "Return t if SESSION-ID exists in registry."
  (not (null (jf/gptel-session-find session-id))))

(defun jf/gptel--all-sessions ()
  "Return list of all session plists in registry."
  (let (sessions)
    (maphash (lambda (_id session)
               (push session sessions))
            jf/gptel--session-registry)
    sessions))

(defun jf/gptel--session-count ()
  "Return number of sessions in registry."
  (hash-table-count jf/gptel--session-registry))

(defun jf/gptel--update-session-buffer (session-id buffer)
  "Update the buffer associated with SESSION-ID."
  (when-let ((session (jf/gptel-session-find session-id)))
    (plist-put session :buffer buffer)
    (plist-put session :last-accessed (current-time))
    (puthash session-id session jf/gptel--session-registry)))

(defun jf/gptel--update-session-metadata (session-id metadata)
  "Update the metadata for SESSION-ID."
  (when-let ((session (jf/gptel-session-find session-id)))
    (plist-put session :metadata metadata)
    (plist-put session :last-accessed (current-time))
    (puthash session-id session jf/gptel--session-registry)))

(defun jf/gptel--current-session ()
  "Get session plist for current buffer.
Returns nil if current buffer is not a gptel session."
  (when jf/gptel--session-id
    (jf/gptel-session-find jf/gptel--session-id)))

(provide 'gptel-session-registry)
;;; registry.el ends here
