;;; filesystem.el --- GPTEL Session Filesystem Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Filesystem utilities for gptel session management.
;; Handles directory creation, file path resolution, and session discovery.

;;; Code:

(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-logging)

(defun jf/gptel--ensure-sessions-root ()
  "Ensure the global root sessions directory exists.
Creates jf/gptel-sessions-directory if it doesn't exist.
Returns the absolute path to the global sessions directory.

This is the inventory-side root used by registry init and session
listing.  For new-session creation, callers should use
`jf/gptel--target-sessions-root' instead so the active workspace's
sessions/ directory is preferred when available."
  (let ((dir (expand-file-name jf/gptel-sessions-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (jf/gptel--log 'info "Created sessions directory: %s" dir))
    dir))

(defun jf/gptel--target-sessions-root (&optional force-global)
  "Return the directory under which a new session subdirectory should be created.

When `workspaces' is loaded, `workspace-sessions-dir' is bound, and it
returns a non-nil directory, return that directory (the active
workspace's sessions/).  Otherwise return the global default
established by `jf/gptel--ensure-sessions-root'.

When FORCE-GLOBAL is non-nil, skip the workspaces consult entirely
and return the global default.

This is the producer-side resolver pinned by
register/boundary/gptel-sessions-workspace-consult (the canonical
name in the boundary contract is `gptel-sessions--target-dir'; the
`jf/gptel-' prefix here is the project's elisp convention)."
  (or (and (not force-global)
           (featurep 'workspaces)
           (fboundp 'workspace-sessions-dir)
           (workspace-sessions-dir))
      (jf/gptel--ensure-sessions-root)))

(defun jf/gptel--create-session-directory (session-id &optional force-global)
  "Create directory for SESSION-ID under the target sessions root.
Returns the absolute path to the created directory.

When FORCE-GLOBAL is non-nil, the global sessions root is used even
when a workspace is active (the escape-hatch surfaced by
`jf/gptel-persistent-session-global')."
  (let* ((root (jf/gptel--target-sessions-root force-global))
         (session-dir (expand-file-name session-id root)))
    (if (file-directory-p session-dir)
        (progn
          (jf/gptel--log 'warn "Session directory already exists: %s" session-dir)
          session-dir)
      (make-directory session-dir t)
      (jf/gptel--log 'info "Created session directory: %s" session-dir)
      session-dir)))

(defun jf/gptel--generate-session-id (base-name)
  "Generate unique session ID from BASE-NAME.
Format: <slugified-base-name>-<timestamp>
Example: 'react-refactoring-20260120153042'"
  (let* ((slug (replace-regexp-in-string "[^a-z0-9-]" "-"
                                        (downcase base-name)))
         (timestamp (format-time-string "%Y%m%d%H%M%S")))
    (format "%s-%s" slug timestamp)))

(defun jf/gptel--session-id-from-directory (session-dir)
  "Extract session ID from SESSION-DIR path.
Returns the directory name (last path component)."
  (file-name-nondirectory (directory-file-name session-dir)))

(defun jf/gptel--branches-dir-path (session-dir)
  "Get path to branches directory in SESSION-DIR."
  (expand-file-name "branches" session-dir))

(defun jf/gptel--branch-dir-path (session-dir branch-name)
  "Get path to specific BRANCH-NAME directory in SESSION-DIR."
  (expand-file-name branch-name (jf/gptel--branches-dir-path session-dir)))

(defun jf/gptel--current-symlink-path (session-dir)
  "Get path to current symlink in SESSION-DIR."
  (expand-file-name "current" session-dir))

(defun jf/gptel--get-current-branch-name (session-dir)
  "Get name of current branch in SESSION-DIR by following symlink.
Returns branch name (e.g., \"main\" or \"20260128153042-feature\") or nil if symlink doesn't exist."
  (let ((symlink (jf/gptel--current-symlink-path session-dir)))
    (when (file-symlink-p symlink)
      (file-name-nondirectory (file-truename symlink)))))

(defun jf/gptel--get-current-branch-dir (session-dir)
  "Get path to current branch directory in SESSION-DIR by following symlink.
Returns absolute path or nil if symlink doesn't exist."
  (let ((symlink (jf/gptel--current-symlink-path session-dir)))
    (when (file-symlink-p symlink)
      (file-truename symlink))))

(defun jf/gptel--create-branch-directory (session-dir branch-name)
  "Create branch directory for BRANCH-NAME in SESSION-DIR.
Returns absolute path to created branch directory."
  (let* ((branches-dir (jf/gptel--branches-dir-path session-dir))
         (branch-dir (expand-file-name branch-name branches-dir)))
    (unless (file-directory-p branches-dir)
      (make-directory branches-dir t))
    (unless (file-directory-p branch-dir)
      (make-directory branch-dir t)
      (jf/gptel--log 'info "Created branch directory: %s" branch-dir))
    branch-dir))

(defun jf/gptel--update-current-symlink (session-dir branch-name)
  "Update current symlink in SESSION-DIR to point to BRANCH-NAME.
Creates or updates the symlink. Returns path to symlink."
  (let* ((symlink (jf/gptel--current-symlink-path session-dir))
         (target (concat "branches/" branch-name)))
    ;; Remove existing symlink if present
    (when (file-exists-p symlink)
      (delete-file symlink))
    ;; Create new symlink (relative path)
    (make-symbolic-link target symlink)
    (jf/gptel--log 'debug "Updated current symlink to: %s" branch-name)
    symlink))

(defun jf/gptel--list-branches (session-dir)
  "List all branch names in SESSION-DIR.
Returns list of branch names (e.g., (\"main\" \"20260128153042-feature\"))."
  (let ((branches-dir (jf/gptel--branches-dir-path session-dir)))
    (when (file-directory-p branches-dir)
      (seq-filter
       (lambda (name)
         (and (not (string-prefix-p "." name))
              (file-directory-p (expand-file-name name branches-dir))))
       (directory-files branches-dir nil "^[^.]")))))

(defun jf/gptel--branch-file-path (branch-dir filename)
  "Get absolute path to FILENAME in BRANCH-DIR.
Does not check if file exists."
  (expand-file-name filename branch-dir))

(defun jf/gptel--context-file-path (branch-dir)
  "Get path to context file in BRANCH-DIR."
  (jf/gptel--branch-file-path branch-dir jf/gptel-session--context-file))

(defun jf/gptel--branch-metadata-file-path (branch-dir)
  "Get path to branch-metadata.yml file in BRANCH-DIR."
  (expand-file-name "branch-metadata.yml" branch-dir))

(defun jf/gptel--tools-log-path (branch-dir)
  "Get path to tools log file in BRANCH-DIR."
  (jf/gptel--branch-file-path branch-dir jf/gptel-session--tools-log-file))

(defun jf/gptel--system-prompts-log-path (branch-dir)
  "Get path to system prompts log file in BRANCH-DIR."
  (jf/gptel--branch-file-path branch-dir jf/gptel-session--system-prompts-file))

(defun jf/gptel--agents-dir-path (branch-dir)
  "Get path to agents directory in BRANCH-DIR."
  (jf/gptel--branch-file-path branch-dir jf/gptel-session--agents-dir))

(defun jf/gptel--list-session-directories ()
  "List all session directories in jf/gptel-sessions-directory.
Returns list of absolute paths to session directories."
  (let ((root (expand-file-name jf/gptel-sessions-directory)))
    (when (file-directory-p root)
      (seq-filter
       (lambda (path)
         (and (file-directory-p path)
              (not (string-prefix-p "." (file-name-nondirectory path)))))
       (directory-files root t "^[^.]")))))

(defun jf/gptel--find-session-directory (session-id)
  "Find session directory for SESSION-ID.
Returns absolute path if found, nil otherwise."
  (let ((root (expand-file-name jf/gptel-sessions-directory))
        (expected (expand-file-name session-id
                                   (expand-file-name jf/gptel-sessions-directory))))
    (when (file-directory-p expected)
      expected)))

(defun jf/gptel--find-all-branches-with-agents ()
  "Find all branches across all sessions, including their agents.

Returns list of plists with:
  :session-dir - Path to session directory
  :session-id - Session ID
  :branch-dir - Path to branch directory
  :branch-name - Branch name (e.g., \"main\" or \"20260128153042-feature\")
  :agent-dirs - List of agent directory paths in this branch

Only branch directories that pass `jf/gptel--valid-branch-directory-p'
are included; branches without `session.org' are filtered out here so
every consumer sees the same valid-branch view."
  (let ((sessions-root (expand-file-name jf/gptel-sessions-directory))
        (results nil))
    (when (file-directory-p sessions-root)
      ;; Iterate over all session directories
      (dolist (session-dir (directory-files sessions-root t "^[^.]"))
        (when (jf/gptel--valid-session-directory-p session-dir)
          (let ((session-id (jf/gptel--session-id-from-directory session-dir))
                (branches (jf/gptel--list-branches session-dir)))
            ;; Iterate over branches in this session
            (dolist (branch-name branches)
              (let* ((branch-dir (jf/gptel--branch-dir-path session-dir branch-name)))
                (when (jf/gptel--valid-branch-directory-p branch-dir)
                  (let* ((agents-dir (jf/gptel--agents-dir-path branch-dir))
                         (agent-dirs (when (file-directory-p agents-dir)
                                       (jf/gptel--list-agent-directories branch-dir))))
                    (push (list :session-dir session-dir
                                :session-id session-id
                                :branch-dir branch-dir
                                :branch-name branch-name
                                :agent-dirs (or agent-dirs nil))
                          results)))))))))
    (nreverse results)))

(defun jf/gptel--create-agent-directory (parent-branch-dir preset description)
  "Create agent directory under PARENT-BRANCH-DIR.
PRESET is the preset name (e.g., 'researcher', 'executor').
DESCRIPTION is a brief description for the directory name.
Returns the absolute path to the created agent directory."
  (let* ((agents-dir (jf/gptel--agents-dir-path parent-branch-dir))
         (slug (replace-regexp-in-string "[^a-z0-9-]" "-"
                                        (downcase description)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (dirname (format "%s-%s-%s" preset timestamp slug))
         (agent-dir (expand-file-name dirname agents-dir)))
    ;; Create agents directory if needed
    (unless (file-directory-p agents-dir)
      (make-directory agents-dir t))
    ;; Create agent session directory
    (make-directory agent-dir t)
    (jf/gptel--log 'info "Created agent directory: %s" agent-dir)
    agent-dir))

(defun jf/gptel--list-agent-directories (parent-branch-dir)
  "List all agent directories under PARENT-BRANCH-DIR.
Returns list of absolute paths."
  (let ((agents-dir (jf/gptel--agents-dir-path parent-branch-dir)))
    (when (file-directory-p agents-dir)
      (seq-filter
       #'file-directory-p
       (directory-files agents-dir t "^[^.]")))))

(defun jf/gptel--valid-session-directory-p (dir)
  "Return t if DIR is a valid session directory.
Checks for existence of branches directory."
  (and (file-directory-p dir)
       (file-directory-p (jf/gptel--branches-dir-path dir))))

(defun jf/gptel--valid-branch-directory-p (dir)
  "Return t if DIR is a valid branch directory.
Checks for existence and presence of session.org file."
  (and (file-directory-p dir)
       (file-exists-p (jf/gptel--context-file-path dir))))

(defun jf/gptel--scan-session-drawer-keys ()
  "Scan the head of the current buffer for a gptel session drawer.

Assumes the caller has positioned point and widened as needed.  Skips
leading blank lines, then requires a `:PROPERTIES:' line as the first
non-blank content.  If found, collects every `:GPTEL_<KEY>: VALUE' line
before the drawer's `:END:' and returns them as an alist keyed by the
bare key string (e.g. \"GPTEL_SESSION_ID\").

Returns nil when the first non-blank content is not a `:PROPERTIES:'
drawer, when the drawer has no `:END:', or when the drawer carries no
`:GPTEL_*:' key.  Never signals on a non-org / plain-text buffer.

Uses native `re-search-forward' (no dependency on `org-mode' being
loaded) so it is safe to call at `magic-mode-alist' time."
  (save-excursion
    (save-restriction
      (widen)
      ;; Case-sensitive throughout: the signature is the *uppercase*
      ;; :PROPERTIES: / :END: / :GPTEL_[A-Z0-9_]+: tokens, per
      ;; register/invariant/signature-anchored-to-point-min-drawer.
      ;; case-fold-search defaults to t, which would recognise a
      ;; lowercase :properties:/:gptel_*: drawer and widen the surface
      ;; magic-mode-alist hijacks into gptel-chat-mode; bind it off.
      (let ((case-fold-search nil))
        (goto-char (point-min))
        ;; The drawer must be the first non-blank content for us to
        ;; recognise it; skip leading blank lines only.
        (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (when (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
          (let ((drawer-end
                 (save-excursion
                   (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                     (line-beginning-position)))))
            (when drawer-end
              (let ((keys nil))
                ;; Collect every :GPTEL_<KEY>: VALUE line before :END:.
                ;; VALUE is optional (trailing whitespace allowed); the
                ;; key match alone is sufficient for the signature.
                (while (re-search-forward
                        "^[ \t]*:\\(GPTEL_[A-Z0-9_]+\\):[ \t]*\\(.*?\\)[ \t]*$"
                        drawer-end t)
                  (push (cons (match-string-no-properties 1)
                              (match-string-no-properties 2))
                        keys))
                (nreverse keys)))))))))

(defun jf/gptel--session-signature-p ()
  "Return non-nil if the current buffer is a gptel session by CONTENT.

Recognizes a buffer iff, scanning only the head: the first non-blank
content is a `:PROPERTIES:' drawer carrying at least one
`:GPTEL_[A-Z0-9_]+:' key line before `:END:'.  Returns nil — without
signaling — for a non-org / plain-text buffer, for an org buffer that
merely mentions a `:GPTEL_' key in prose or a `#+begin_src' block, and
for any buffer whose first content is not a `:PROPERTIES:' drawer.

The match is anchored to a real drawer at `point-min' — NEVER a bare
substring search.  Wired into `magic-mode-alist', so a false match would
hijack an ordinary user file into `gptel-chat-mode' at open time."
  (and (jf/gptel--scan-session-drawer-keys) t))

(defun jf/gptel--read-session-drawer-head (file)
  "Read FILE's head and return its gptel session-drawer keys, or nil.

Reads FILE into a temp buffer and runs the same point-min drawer scan as
`jf/gptel--session-signature-p', returning an alist of the `:GPTEL_*:'
keys found, keyed by the bare key string (e.g. \"GPTEL_SESSION_ID\" ->
\"<id>\").  Includes any identity keys present — at least
`GPTEL_SESSION_ID', `GPTEL_BRANCH', `GPTEL_PARENT_SESSION_ID', and
`GPTEL_PRESET' when authored.  Returns nil when FILE has no point-min
session drawer or does not exist (never signals on a missing file).

The session drawer is small and always at the file head, so reading the
whole file is correct today.  A byte cap belongs at the
`insert-file-contents' call below (its 3rd/4th args bound the read) —
once the cap is added, choose a bound large enough to always contain the
drawer."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      ;; NOTE: bounded head read — pass BEG/END to `insert-file-contents'
      ;; here to cap the read at the file head once a byte cap is chosen.
      (insert-file-contents file)
      (jf/gptel--scan-session-drawer-keys))))

(provide 'gptel-session-filesystem)
;;; filesystem.el ends here
