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
Metadata is read from disk on-demand when needed.

Directory traversal (`list-session-directories' / `list-branches')
LOCATES the branch `session.org' files; identity is then read from each
file's point-min `:GPTEL_*:' drawer via `jf/gptel--read-session-drawer-head'
and resolved with `jf/gptel--resolve-session-id' /
`jf/gptel--resolve-branch-name' (drawer-first, basename/segment fallback).
Directory NAMES carry no identity meaning — a session/branch whose
basename differs from its drawer id keys on the drawer id.  A located
`session.org' that carries no `:GPTEL_' drawer at all (corrupt / partial)
is skipped and logged at debug, consistent with `valid-*-directory-p'
gating.  See register/boundary/drawer-first-identity-resolution."
  (clrhash jf/gptel--session-registry)
  (let ((session-dirs (jf/gptel--list-session-directories)))
    (dolist (session-dir session-dirs)
      (when (jf/gptel--valid-session-directory-p session-dir)
        (dolist (branch-name (jf/gptel--list-branches session-dir))
          (let* ((branch-dir (jf/gptel--branch-dir-path session-dir branch-name)))
            (when (jf/gptel--valid-branch-directory-p branch-dir)
              (let* ((session-file (jf/gptel--context-file-path branch-dir))
                     (drawer (jf/gptel--read-session-drawer-head session-file)))
                (if (null drawer)
                    (jf/gptel--log
                     'debug "Skipping branch with no GPTEL drawer: %s" branch-dir)
                  (let* ((resolved-id (jf/gptel--resolve-session-id
                                       drawer session-dir))
                         (resolved-branch (jf/gptel--resolve-branch-name
                                           drawer session-file))
                         (key (jf/gptel--registry-key resolved-id resolved-branch)))
                    (puthash key
                            (list :session-id resolved-id
                                  :session-dir session-dir
                                  :branch-name resolved-branch
                                  :branch-dir branch-dir
                                  :buffer nil)
                            jf/gptel--session-registry)
                    (jf/gptel--log 'debug "Registered branch: %s/%s"
                                   resolved-id resolved-branch)))))))))
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
