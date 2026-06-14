;;; branching.el --- GPTEL Session Branching -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Session branching support for gptel-chat-mode persistent sessions.
;; Enables creating divergent conversation paths from any user turn.

;;; Code:

(require 'cl-lib)
(require 'gptel-chat-parser)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)

(defun jf/gptel--branching-user-turns (&optional buffer)
  "Return the user turns from BUFFER in document order.
BUFFER defaults to the current buffer.  Each returned element is the
parser turn plist as produced by `gptel-chat-parse-buffer' — a plist
with `:role', `:content', `:start', and `:end'.

Only outer `#+begin_user' blocks are returned.  Assistant blocks,
nested tool blocks, and non-block content are filtered out."
  (let ((turns (gptel-chat-parse-buffer (or buffer (current-buffer)))))
    (seq-filter (lambda (turn) (eq (plist-get turn :role) 'user))
                turns)))

(defun jf/gptel--branching-turn-label (turn)
  "Return a short display label for the user TURN plist.
The label is the first non-empty line of the turn's content, trimmed
and truncated to 60 characters with a trailing `...' when longer.  An
empty or whitespace-only user block yields the string \"(empty)\"."
  (let* ((content (or (plist-get turn :content) ""))
         (trimmed (string-trim content)))
    (if (string-empty-p trimmed)
        "(empty)"
      (let* ((first-line (car (split-string trimmed "\n")))
             (flat (string-trim first-line)))
        (if (> (length flat) 60)
            (concat (substring flat 0 57) "...")
          flat)))))

(defun jf/gptel--branching-turn-branch-point (turn include-p)
  "Return the branch-point buffer position for the user TURN.
INCLUDE-P chooses the semantics:
- non-nil INCLUDE-P → position immediately after the `#+end_user' line
  (the new branch contains the selected turn);
- nil INCLUDE-P → position immediately before the `#+begin_user' line
  (the new branch stops before the selected turn).

Positions are returned as integers in the turn's source buffer."
  (let ((start-marker (plist-get turn :start))
        (end-marker   (plist-get turn :end)))
    (if include-p
        ;; Include: skip past the `#+end_user' line.
        (with-current-buffer (marker-buffer end-marker)
          (save-excursion
            (goto-char (marker-position end-marker))
            (forward-line 1)
            (point)))
      ;; Exclude: stop before the `#+begin_user' line.
      (marker-position start-marker))))

(defun jf/gptel--branching-select-branch-point ()
  "Interactively select a branch point from the current buffer's user turns.

Returns a cons (POSITION . INCLUDE-P) where POSITION is the buffer
position of the branch point (see
`jf/gptel--branching-turn-branch-point') and INCLUDE-P is non-nil when
the user chose INCLUDE.

Signals `user-error' with the message \"no available branch points\"
when the buffer contains no outer `#+begin_user' blocks."
  (let ((turns (jf/gptel--branching-user-turns)))
    (unless turns
      (user-error "No available branch points"))
    (let* ((numbered (cl-loop for turn in turns
                              for i from 1
                              collect
                              (cons (format "%d. %s"
                                            i
                                            (jf/gptel--branching-turn-label turn))
                                    turn)))
           (choice (completing-read "Select branch point: "
                                    (mapcar #'car numbered)
                                    nil t))
           (selected-turn (cdr (assoc choice numbered)))
           (include-p (y-or-n-p "Include this turn in the branch? "))
           (position (jf/gptel--branching-turn-branch-point
                      selected-turn include-p)))
      (cons position include-p))))

(defun jf/gptel--copy-truncated-context (source-file dest-file branch-position)
  "Copy SOURCE-FILE to DEST-FILE truncated at BRANCH-POSITION.

Writes bytes 1..(BRANCH-POSITION - 1) of SOURCE-FILE to DEST-FILE
verbatim.  BRANCH-POSITION is a 1-based buffer position as returned by
`jf/gptel--branching-turn-branch-point'.  A BRANCH-POSITION at or past
the end of SOURCE-FILE copies the full file.  A BRANCH-POSITION of 1
writes an empty file.

Returns t on success."
  (with-temp-buffer
    (insert-file-contents source-file)
    (let ((truncate-at (min (max branch-position (point-min)) (point-max))))
      (delete-region truncate-at (point-max))
      (write-region (point-min) (point-max) dest-file nil 'silent))
    (jf/gptel--log 'info
                   "Copied truncated context: %s -> %s (branch-position=%d)"
                   source-file dest-file branch-position)
    t))

(defun jf/gptel--copy-branch-agents (parent-branch-dir branch-dir)
  "Copy agent subdirectories from PARENT-BRANCH-DIR/agents to BRANCH-DIR/agents.
For MVP, copies ALL agent directories.
Future enhancement: Only copy agents invoked before branch point."
  (let ((parent-agents-dir (jf/gptel--agents-dir-path parent-branch-dir))
        (branch-agents-dir (jf/gptel--agents-dir-path branch-dir)))
    (when (file-directory-p parent-agents-dir)
      ;; Create agents directory in branch
      (make-directory branch-agents-dir t)

      ;; Copy each agent directory
      (dolist (agent-dir (directory-files parent-agents-dir t "^[^.]"))
        (when (file-directory-p agent-dir)
          (let* ((agent-name (file-name-nondirectory agent-dir))
                 (dest-dir (expand-file-name agent-name branch-agents-dir)))
            (copy-directory agent-dir dest-dir t t t)
            (jf/gptel--log 'debug "Copied agent directory: %s" agent-name))))

      (jf/gptel--log 'info "Copied %d agent directories to branch"
                    (length (directory-files parent-agents-dir nil "^[^.]"))))))

(defun jf/gptel--write-branch-metadata (branch-dir parent-branch-name &optional branch-point-position)
  "Write branch metadata to BRANCH-DIR.
PARENT-BRANCH-NAME is the name of the parent branch (e.g., \"main\").
BRANCH-POINT-POSITION is optional position in parent where branch was created."
  (let ((metadata-file (jf/gptel--branch-metadata-file-path branch-dir))
        (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (with-temp-file metadata-file
      (insert "parent_branch: " parent-branch-name "\n")
      (insert "created: " timestamp "\n")
      (when branch-point-position
        (insert "branch_point_position: " (number-to-string branch-point-position) "\n")))
    (jf/gptel--log 'info "Created branch-metadata.yml with parent: %s" parent-branch-name)))

(defun jf/gptel--set-drawer-property-in-buffer (key value)
  "Set drawer property KEY to VALUE in the current buffer's drawer.

Operates on the `:PROPERTIES:' / `:END:' block at `point-min'
\(`register/shape/drawer-text-block').  When a `:KEY: ...' line exists
between the header and `:END:', its value is REPLACED in place (no
duplicate key).  When the key is absent, a `:KEY: VALUE' line is
inserted immediately before `:END:', preserving the block's adjacency
invariant.  Assumes the drawer is present and well-formed; callers
guard that precondition."
  (save-excursion
    (goto-char (point-min))
    (let ((line-re (concat "^:" (regexp-quote key) ":[ \t].*$")))
      (if (re-search-forward line-re nil t)
          (replace-match (format ":%s: %s" key value) t t)
        (when (re-search-forward "^:END:$" nil t)
          (goto-char (match-beginning 0))
          (insert (format ":%s: %s\n" key value)))))))

(defun jf/gptel--rewrite-branch-identity-keys (session-file session-id branch-name)
  "Set SESSION-FILE's drawer `:GPTEL_SESSION_ID:'/`:GPTEL_BRANCH:' keys.

SESSION-FILE is the new branch's `session.org' path.  SESSION-ID is
the shared session id (set as `:GPTEL_SESSION_ID:').  BRANCH-NAME is
the new branch's own name (set as `:GPTEL_BRANCH:', overwriting any
value the parent drawer carried — `register/invariant/branch-drawer-
shares-id-not-branch').

Reads SESSION-FILE, REPLACES the two identity keys in its leading
`register/shape/drawer-text-block' (append-if-absent), and writes the
content back.  When the file has no `:PROPERTIES:' / `:END:' drawer at
`point-min', it is left untouched."
  (with-temp-buffer
    (insert-file-contents session-file)
    (goto-char (point-min))
    (when (and (looking-at-p ":PROPERTIES:\n")
               (save-excursion (re-search-forward "^:END:$" nil t)))
      (jf/gptel--set-drawer-property-in-buffer "GPTEL_SESSION_ID" session-id)
      (jf/gptel--set-drawer-property-in-buffer "GPTEL_BRANCH" branch-name)
      (write-region (point-min) (point-max) session-file nil 'silent)
      (jf/gptel--log 'info
                     "Rewrote branch identity keys: %s (session-id=%s branch=%s)"
                     session-file session-id branch-name))))

(defun jf/gptel--create-branch-session (session-dir parent-branch-name branch-name branch-position)
  "Create new branch inside SESSION-DIR.
SESSION-DIR - session directory containing branches
PARENT-BRANCH-NAME - name of parent branch (e.g., \"main\")
BRANCH-NAME - user-provided name for branch
BRANCH-POSITION - buffer position marking the branch point (already
adjusted for include/exclude by the caller; see
`jf/gptel--branching-turn-branch-point').

Returns new branch directory path."
  (let* ((parent-branch-dir (jf/gptel--branch-dir-path session-dir parent-branch-name))
         ;; Create timestamped branch name
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (new-branch-name (format "%s-%s" timestamp branch-name))
         ;; Create new branch directory
         (branch-dir (jf/gptel--create-branch-directory session-dir new-branch-name))
         (parent-context (jf/gptel--context-file-path parent-branch-dir))
         (branch-context (jf/gptel--context-file-path branch-dir)))

    (jf/gptel--log 'info "Creating branch: %s from parent: %s"
                   new-branch-name parent-branch-name)

    ;; Note: scope.yml and metadata.yml are no longer copied. The
    ;; parent's preset AND scope (`:GPTEL_SCOPE_*:' keys) propagate
    ;; to the new branch via the `:PROPERTIES:' drawer embedded at
    ;; the top of the parent's session.org, which the
    ;; truncated-context copy below preserves verbatim (design
    ;; Decision 7; gptel-scope-in-org-properties drawer-resident
    ;; scope).

    ;; Create branch-metadata.yml
    (jf/gptel--write-branch-metadata branch-dir parent-branch-name branch-position)

    ;; Copy agent directories from parent branch
    (jf/gptel--copy-branch-agents parent-branch-dir branch-dir)

    ;; Copy and truncate session.org
    (jf/gptel--copy-truncated-context parent-context branch-context branch-position)

    ;; Overwrite the new branch's identity keys in the verbatim-copied
    ;; drawer (register/invariant/branch-drawer-shares-id-not-branch):
    ;; the branch SHARES the session id but gets its OWN branch name.
    ;; The parent's `:GPTEL_BRANCH:' value (e.g. "main") rode along in
    ;; the verbatim copy; it MUST be REPLACED with NEW-BRANCH-NAME (not
    ;; appended) so two branches never collide on the registry key
    ;; "session-id/branch-name" and no duplicate drawer key survives.
    ;; `:GPTEL_SESSION_ID:' is re-asserted to the shared id (the copy
    ;; already carries it, but a parent drawer predating identity-key
    ;; emission would not).
    (jf/gptel--rewrite-branch-identity-keys
     branch-context
     (jf/gptel--session-id-from-directory session-dir)
     new-branch-name)

    ;; Update current symlink to point to new branch
    (jf/gptel--update-current-symlink session-dir new-branch-name)

    (jf/gptel--log 'info "Branch created successfully: %s" new-branch-name)
    branch-dir))

(defun jf/gptel-branch-session (&optional branch-name)
  "Create a branch from current branch at a selected user turn.

Interactively:
1. Prompts user to select a branch point — a numbered list of outer
   `#+begin_user' blocks in the current buffer.
2. Asks whether to include or exclude the selected turn.
3. Prompts for branch name.
4. Creates a new branch directory with copied metadata and truncated
   context (verbatim byte copy up to the branch point position).
5. Opens the new branch's `session.org' buffer.

The new branch is created under the same session with:
- Timestamped branch name: <timestamp>-<user-name>
- branch-metadata.yml tracking the parent branch name and branch-point position
- Copied agent subdirectories from parent branch
- session.org truncated at the branch-point position (the parent's
  `:PROPERTIES:' drawer at point-min is preserved verbatim, so the
  new branch inherits the parent's preset AND `:GPTEL_SCOPE_*:'
  keys automatically — gptel-scope-in-org-properties
  drawer-resident scope)
- current symlink updated to point to the new branch

After creation, the branch can evolve independently from the parent.

If the source session buffer has unsaved changes, `save-buffer' is
called before branch-point selection. This keeps the branch-point
position source (the live buffer) and the branch content source (the
on-disk `session.org', read verbatim by
`jf/gptel--copy-truncated-context') byte-identical, preventing silent
misalignment between computed positions and copied bytes. This mirrors
the sessions subsystem's persistence convention, where writing a
session uses plain `save-buffer' (no gptel--bounds drawer to manage)."
  (interactive)

  ;; Verify current buffer is a branch session in chat-mode.
  (unless (and (boundp 'jf/gptel--session-dir) jf/gptel--session-dir
              (boundp 'jf/gptel--branch-name) jf/gptel--branch-name)
    (user-error "Current buffer is not a gptel branch session"))

  (unless (derived-mode-p 'gptel-chat-mode)
    (user-error "Current buffer is not in gptel-chat-mode"))

  ;; Flush any unsaved parent edits before computing branch-point
  ;; positions against the live buffer. Positions and the bytes
  ;; copied by `jf/gptel--copy-truncated-context' must come from the
  ;; same source of truth; otherwise a dirty parent buffer causes
  ;; silent byte-offset misalignment in the new branch's session.org.
  (when (buffer-modified-p)
    (save-buffer))

  ;; Select branch point.
  (let* ((branch-point-data (jf/gptel--branching-select-branch-point))
         (branch-position (car branch-point-data))
         ;; Save parent info before switching buffers.
         (parent-branch-name jf/gptel--branch-name)
         (parent-session-id jf/gptel--session-id)
         ;; Prompt for branch name.
         (user-branch-name (or branch-name
                               (read-string "Branch name: ")))
         ;; Create branch.
         (new-branch-dir (jf/gptel--create-branch-session
                          jf/gptel--session-dir
                          parent-branch-name
                          user-branch-name
                          branch-position))
         (new-branch-file (jf/gptel--context-file-path new-branch-dir))
         (new-branch-name (file-name-nondirectory new-branch-dir)))

    ;; Open new branch session buffer (auto-initializes via find-file-hook).
    (find-file new-branch-file)

    (message "Created branch: %s\nFrom parent: %s\nSession: %s"
             new-branch-name
             parent-branch-name
             parent-session-id)))

(provide 'gptel-session-branching)
;;; branching.el ends here
