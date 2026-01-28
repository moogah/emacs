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
  "Global registry of active gptel branch sessions.
Keys are \"session-id/branch-name\" strings, values are session plists.")

(defun jf/gptel--registry-key (session-id branch-name)
  "Create registry key from SESSION-ID and BRANCH-NAME."
  (concat session-id "/" branch-name))

(defun jf/gptel--init-registry ()
  "Initialize the session registry.
Discovers all sessions and branches, populates registry with essential runtime state.
Metadata is read from disk on-demand when needed."
  (clrhash jf/gptel--session-registry)
  (let ((session-dirs (jf/gptel--list-session-directories)))
    (dolist (session-dir session-dirs)
      (when (jf/gptel--valid-session-directory-p session-dir)
        (let* ((session-id (jf/gptel--session-id-from-directory session-dir))
               (branches (jf/gptel--list-branches session-dir)))
          (dolist (branch-name branches)
            (let* ((branch-dir (jf/gptel--branch-dir-path session-dir branch-name))
                   (key (jf/gptel--registry-key session-id branch-name)))
              (puthash key
                      (list :session-id session-id
                            :session-dir session-dir
                            :branch-name branch-name
                            :branch-dir branch-dir
                            :buffer nil)
                      jf/gptel--session-registry)
              (jf/gptel--log 'debug "Registered branch: %s/%s" session-id branch-name))))))
    (jf/gptel--log 'info "Initialized registry with %d branches"
                  (hash-table-count jf/gptel--session-registry))))

(defun jf/gptel--register-session (session-dir buffer session-id branch-name branch-dir)
  "Register a branch session in the global registry.
SESSION-DIR is the absolute path to the session directory.
BUFFER is the buffer visiting this branch session.
SESSION-ID is the session identifier.
BRANCH-NAME is the branch name (e.g., \"main\").
BRANCH-DIR is the absolute path to the branch directory.
Metadata is read from disk on-demand when needed."
  (let ((key (jf/gptel--registry-key session-id branch-name)))
    (puthash key
            (list :session-id session-id
                  :session-dir session-dir
                  :branch-name branch-name
                  :branch-dir branch-dir
                  :buffer buffer)
            jf/gptel--session-registry)
    (jf/gptel--log 'info "Registered branch: %s/%s" session-id branch-name)
    key))

(defun jf/gptel--unregister-session (session-id branch-name)
  "Remove SESSION-ID/BRANCH-NAME from the registry."
  (let ((key (jf/gptel--registry-key session-id branch-name)))
    (remhash key jf/gptel--session-registry)
    (jf/gptel--log 'info "Unregistered branch: %s/%s" session-id branch-name)))

(defun jf/gptel-session-find (session-id branch-name)
  "Find branch session by SESSION-ID and BRANCH-NAME in registry.
Returns session plist or nil if not found."
  (gethash (jf/gptel--registry-key session-id branch-name) jf/gptel--session-registry))

(defun jf/gptel--all-sessions ()
  "Return list of all branch session plists in registry."
  (let (sessions)
    (maphash (lambda (_key session)
               (push session sessions))
            jf/gptel--session-registry)
    sessions))

(defun jf/gptel--session-count ()
  "Return number of branch sessions in registry."
  (hash-table-count jf/gptel--session-registry))

(defun jf/gptel--update-session-buffer (session-id branch-name buffer)
  "Update the buffer associated with SESSION-ID/BRANCH-NAME."
  (when-let ((session (jf/gptel-session-find session-id branch-name)))
    (plist-put session :buffer buffer)
    (puthash (jf/gptel--registry-key session-id branch-name) session jf/gptel--session-registry)))

(defun jf/gptel--current-session ()
  "Get session plist for current buffer.
Returns nil if current buffer is not a gptel session."
  (when (and jf/gptel--session-id jf/gptel--branch-name)
    (jf/gptel-session-find jf/gptel--session-id jf/gptel--branch-name)))

(provide 'gptel-session-registry)
;;; registry.el ends here
