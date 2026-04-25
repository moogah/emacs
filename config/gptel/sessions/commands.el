;;; commands.el --- GPTEL Session Commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Interactive commands for gptel session management.
;;
;; BREAKING CHANGE: Manual resume commands removed as of 2026-01-25
;; Sessions now auto-initialize when opened via find-file-hook
;; Users should open session files directly (C-x C-f, dired, recentf, etc.)
;; The jf/gptel-resume-session, jf/gptel-list-sessions, and jf/gptel-resume-agent
;; commands have been removed. Use standard Emacs file navigation instead.
;;
;; ARCHITECTURE: Presets are registered in gptel--known-presets at startup.
;; Session creation writes scope.yml (from preset's scope profile) and
;; pre-populates session.org with a :PROPERTIES: drawer containing
;; GPTEL_PRESET (and GPTEL_PARENT_SESSION_ID for agents). Every time the
;; session file is opened, `gptel-chat-mode' activation reads the drawer
;; and applies the declared preset (plus overlaying non-preset deltas
;; and `GPTEL_PARENT_SESSION_ID') via `gptel-chat--apply-declared-preset';
;; the save hook writes it back on every save. The drawer is the
;; authoritative session-level configuration source — session buffers run
;; `gptel-chat-mode' exclusively (Decision 16) and do NOT round-trip
;; through gptel--save-state / gptel--restore-state. Session creation no
;; longer writes a metadata.yml sidecar; all session-level state is
;; either path-derivable or drawer-derived (design Decision 4 /
;; Decision 6).

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-scope-profiles)

;; `gptel-chat-mode' is defined in `config/gptel/chat/mode.el' and is
;; loaded earlier by `init.el' (see init.org: gptel/chat/chat loads
;; before gptel/gptel). Declare here to silence byte-compiler warnings
;; — the function is resolved at call time.
(declare-function gptel-chat-mode "gptel-chat-mode" ())

(defun jf/gptel--check-duplicate-hooks ()
  "Check if before-save-hook has duplicate gptel--save-state entries.
Returns the count of gptel--save-state hooks found."
  (interactive)
  (let* ((hooks (buffer-local-value 'before-save-hook (current-buffer)))
         (save-state-count (cl-count 'gptel--save-state hooks)))
    (message "before-save-hook has %d entries, %d are gptel--save-state"
             (length hooks) save-state-count)
    (when (> save-state-count 1)
      (warn "DUPLICATE HOOK DETECTED: gptel--save-state appears %d times!"
            save-state-count)
      (with-current-buffer (get-buffer-create "*Hook Duplicates*")
        (erase-buffer)
        (insert (format "Duplicate hooks detected in buffer: %s\n\n"
                        (buffer-name)))
        (insert (format "before-save-hook entries (%d total):\n" (length hooks)))
        (dolist (hook hooks)
          (insert (format "  - %s\n" hook)))
        (display-buffer (current-buffer))))
    save-state-count))

(defun jf/gptel--ensure-mode-once ()
  "Ensure `gptel-chat-mode' is the active major mode in the current buffer.
No-op when chat-mode is already active. Never enables `gptel-mode'
(minor mode) — session buffers run chat-mode exclusively (Decision 16)."
  (unless (derived-mode-p 'gptel-chat-mode)
    (gptel-chat-mode)))

(defun jf/gptel--clean-duplicate-local-vars ()
  "Remove all but the last Local Variables block in current buffer.
Useful for cleaning up files with duplicate blocks from previous bugs."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((blocks nil)
            (changes 0))
        ;; Find all Local Variables blocks
        (while (re-search-forward "^<!-- Local Variables: -->$" nil t)
          (let ((start (match-beginning 0)))
            (when (re-search-forward "^<!-- End: -->$" nil t)
              (push (cons start (match-end 0)) blocks))))

        ;; If more than one block, delete all but the last
        (when (> (length blocks) 1)
          (setq blocks (nreverse blocks))  ; oldest first
          (dolist (block (butlast blocks))  ; all except last
            (delete-region (car block) (cdr block))
            (delete-blank-lines)
            (cl-incf changes))
          (message "Removed %d duplicate Local Variables block(s)" changes)
          (set-buffer-modified-p t)
          changes)))))

(defun jf/gptel--batch-clean-sessions ()
  "Clean duplicate Local Variables from all session files.
Scans all sessions and removes duplicate blocks without opening buffers."
  (interactive)
  (let ((sessions (jf/gptel--find-all-sessions-recursive))
        (cleaned 0)
        (total 0))
    (dolist (session sessions)
      (let* ((session-dir (plist-get session :path))
             (session-file (jf/gptel--context-file-path session-dir)))
        (when (file-exists-p session-file)
          (cl-incf total)
          (with-temp-buffer
            (insert-file-contents session-file)
            (goto-char (point-min))
            (let ((block-count 0))
              (while (re-search-forward "^<!-- Local Variables: -->$" nil t)
                (cl-incf block-count))
              (when (> block-count 1)
                (erase-buffer)
                (insert-file-contents session-file)
                (goto-char (point-min))
                (let ((blocks nil))
                  (while (re-search-forward "^<!-- Local Variables: -->$" nil t)
                    (let ((start (match-beginning 0)))
                      (when (re-search-forward "^<!-- End: -->$" nil t)
                        (push (cons start (match-end 0)) blocks))))
                  (when (> (length blocks) 1)
                    (setq blocks (nreverse blocks))
                    (dolist (block (butlast blocks))
                      (delete-region (car block) (cdr block))
                      (delete-blank-lines))
                    (write-region (point-min) (point-max) session-file)
                    (cl-incf cleaned)
                    (jf/gptel--log 'info "Cleaned duplicates from: %s" session-file)))))))))
    (message "Cleaned %d of %d session files" cleaned total)))

(defun jf/gptel--auto-init-session-buffer ()
  "Auto-initialize gptel chat-mode session if current buffer is a session file.
Detects session files by path pattern:
  - Branch sessions: */<session-id>/branches/<branch-name>/session.org
  - Nested agents:   */<session-id>/branches/<branch-name>/agents/<agent-name>/session.org
  - Flat agents:     */<session-id>/agents/<agent-name>/session.org (legacy)

On match:
  1. Extracts session-id and branch-name from the path (via
     dedicated per-layout regexes — never hardcoded).
  2. Sets the four path-derived buffer-local session variables
     (`jf/gptel--session-id', `jf/gptel--session-dir',
     `jf/gptel--branch-name', `jf/gptel--branch-dir').
  3. Ensures `gptel-chat-mode' is the active major mode.  Activating
     chat-mode fires `gptel-chat-mode-hook', which runs
     `gptel-chat--apply-declared-preset' to apply any preset declared
     in the `:PROPERTIES:' drawer and to install
     `GPTEL_PARENT_SESSION_ID' as `jf/gptel--parent-session-id'.
  4. Registers the buffer in `jf/gptel--session-registry'.
  5. Updates the `current' symlink to point at this branch (skipped
     for the legacy flat agent layout, which has no `branches/' dir).

Runs on every file open via `find-file-hook', so fast-path guards are
critical."
  ;; Fast-path guards (runs on every file open).  The inner
  ;; `string=' check against "session.org" (in both regex branches
  ;; below) is stricter than a generic ".org" suffix test, so no
  ;; outer suffix guard is needed here.
  (when (and (buffer-file-name)                         ; Has file?
             (not (bound-and-true-p jf/gptel--session-id))) ; Not already initialized?
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-nondirectory file-path))
           (session-id nil)
           (branch-name nil)
           (branch-dir nil)
           (session-dir nil)
           (session-type nil)
           ;; Nested agent layout:
           ;;   .../<session-id>/branches/<branch>/agents/<agent>/session.org
           ;; Captures:
           ;;   1 -> session-id
           ;;   2 -> branch-name
           ;;   3 -> agent-name
           (nested-agent-re
            "/\\([^/]+\\)/branches/\\([^/]+\\)/agents/\\([^/]+\\)/session\\.org\\'")
           ;; Flat (legacy) agent layout:
           ;;   .../<session-id>/agents/<agent>/session.org
           ;; Captures:
           ;;   1 -> session-id
           ;;   2 -> agent-name
           (flat-agent-re
            "/\\([^/]+\\)/agents/\\([^/]+\\)/session\\.org\\'"))

      (cond
       ;; Branch session: */branches/<branch>/session.org
       ;; (No agent component between branches/<branch>/ and session.org.)
       ((and (string= file-name "session.org")
             (string-match "/branches/\\([^/]+\\)/session\\.org\\'" file-path))
        (setq branch-name (match-string 1 file-path)
              branch-dir (file-name-directory file-path)
              session-dir (expand-file-name "../.." branch-dir)
              session-id (jf/gptel--session-id-from-directory session-dir)
              session-type 'branch))

       ;; Nested agent layout: agent lives under a specific branch.
       ((and (string= file-name "session.org")
             (string-match nested-agent-re file-path))
        (let ((agent-dir (file-name-directory file-path)))
          (setq session-id (match-string 1 file-path)
                branch-name (match-string 2 file-path)
                branch-dir agent-dir
                ;; Walk up: agents/<agent>/ -> branches/<branch>/ -> <session-id>/
                session-dir (expand-file-name "../../.." agent-dir)
                session-type 'agent)))

       ;; Legacy flat agent layout: no branches/ component.
       ((and (string= file-name "session.org")
             (string-match flat-agent-re file-path))
        (let ((agent-dir (file-name-directory file-path)))
          (setq session-id (match-string 1 file-path)
                branch-name "main"
                branch-dir agent-dir
                ;; Walk up: agents/<agent>/ -> <session-id>/
                session-dir (expand-file-name "../.." agent-dir)
                session-type 'agent-flat))))

      (when session-type
        ;; Validate directories (branch sessions need both checks;
        ;; agent sessions just need branch-dir).
        (when (if (eq session-type 'branch)
                  (and (jf/gptel--valid-session-directory-p session-dir)
                       (jf/gptel--valid-branch-directory-p branch-dir))
                (jf/gptel--valid-branch-directory-p branch-dir))
          (condition-case err
              (progn
                (jf/gptel--log 'debug "Auto-initializing %s session: %s/%s"
                               session-type session-id branch-name)

                ;; Ensure chat-mode is the active major mode FIRST,
                ;; before setting any buffer-local session vars.
                ;; Activating a major mode calls
                ;; `kill-all-local-variables', which would wipe any
                ;; session vars set beforehand (they are not declared
                ;; `permanent-local').
                ;;
                ;; Activating chat-mode also fires
                ;; `gptel-chat-mode-hook', which runs
                ;; `gptel-chat--apply-declared-preset' to apply the
                ;; preset and `GPTEL_PARENT_SESSION_ID' declared in the
                ;; `:PROPERTIES:' drawer. The drawer is the
                ;; authoritative configuration source (design.md
                ;; §Decisions 5, 6, 9) — auto-init does not read
                ;; metadata.yml or apply presets itself.
                ;;
                ;; Never calls (gptel-mode 1) — Decision 16.
                (jf/gptel--ensure-mode-once)

                ;; Set buffer-local session variables (after mode
                ;; activation, since mode activation wipes them).
                (setq-local jf/gptel--session-id session-id)
                (setq-local jf/gptel--session-dir session-dir)
                (setq-local jf/gptel--branch-name branch-name)
                (setq-local jf/gptel--branch-dir branch-dir)

                ;; Register the buffer in the session registry.
                (jf/gptel--register-session session-dir
                                            (current-buffer)
                                            session-id
                                            branch-name
                                            branch-dir)
                (setq-local jf/gptel-autosave-enabled t)

                ;; Update current symlink to point to this branch.
                ;; Skip for legacy flat agent layout: the agent directory
                ;; has no `branches/' subdirectory, so pointing `current'
                ;; at `branches/main' there would create a dangling
                ;; symlink. Nested agents update the parent session's
                ;; `current' symlink to the real branch name.
                (unless (eq session-type 'agent-flat)
                  (jf/gptel--update-current-symlink session-dir branch-name))

                (jf/gptel--log 'info "Auto-initialized %s session: %s/%s"
                               session-type session-id branch-name)
                (message "Session initialized: %s (branch: %s)"
                         session-id branch-name))
            (error
             (jf/gptel--log 'error "Failed to auto-initialize %s session: %s"
                            session-type (error-message-string err))
             (message "Warning: Session auto-init failed. File opened in basic mode."))))))))

(defun jf/gptel--initial-session-content (preset-name &optional parent-session-id)
  "Return initial content for a freshly-created `session.org' file.

PRESET-NAME is a symbol naming a registered preset in
`gptel--known-presets'.

PARENT-SESSION-ID, when a non-empty string, adds a
`:GPTEL_PARENT_SESSION_ID:' line to the drawer so the chat-mode
restore path installs `jf/gptel--parent-session-id' buffer-locally
on first open (design Decision 3 / Decision 4).

Returns a string starting with a `:PROPERTIES:' drawer followed by
an empty `#+begin_user' / `#+end_user' block (the chat-mode
new-chat template, Decision 9 / Decision 18). The shape is
identical to what the save hook writes on first save with no
overrides, so creation → open → save is a no-op."
  (let ((parent-line
         (if (and parent-session-id
                  (stringp parent-session-id)
                  (not (string-empty-p parent-session-id)))
             (format ":GPTEL_PARENT_SESSION_ID: %s\n" parent-session-id)
           "")))
    (format ":PROPERTIES:\n:GPTEL_PRESET: %s\n%s:END:\n#+begin_user\n\n#+end_user\n"
            (symbol-name preset-name)
            parent-line)))

(defun jf/gptel--create-session-core (session-id session-dir preset-name &optional initial-content worktree-paths project-root parent-session-id)
  "Create session directory structure with branching support.

SESSION-ID - unique session identifier
SESSION-DIR - parent directory for session (will contain branches/)
PRESET-NAME - symbol, name of registered preset in gptel--known-presets
INITIAL-CONTENT - optional initial content for session.org (default:
  a pre-populated `:PROPERTIES:' drawer containing `GPTEL_PRESET'
  (and `GPTEL_PARENT_SESSION_ID' when PARENT-SESSION-ID is a
  non-empty string) followed by the chat-mode empty-user-block
  template — see `jf/gptel--initial-session-content'. The shape
  matches what the save hook writes on first save, so a fresh
  session looks identical to a freshly-saved standalone chat
  buffer with a preset applied (design Decision 4).
WORKTREE-PATHS - optional scope plist with explicit paths for activity isolation
PROJECT-ROOT - optional project root for scope profile variable expansion
PARENT-SESSION-ID - optional string, parent session id for agent
  sessions. When non-empty, `GPTEL_PARENT_SESSION_ID' is written
  into the drawer so the chat-mode restore path installs
  `jf/gptel--parent-session-id' buffer-locally on first open.
  Branch and standalone callers leave this nil.

Creates:
- SESSION-DIR/branches/main/ directory structure
- scope.yml (from preset's scope profile, or explicit worktree-paths)
- session.org pre-populated with the drawer + empty user block
- current symlink pointing to main branch

NOTE: No `metadata.yml' is written. The drawer embedded in
`session.org' is the authoritative session-level configuration
source (design Decision 6).

Returns plist with:
  :session-id - session identifier
  :session-dir - session directory path
  :branch-dir - main branch directory path
  :branch-name - \"main\"
  :session-file - path to session.org"

  (let* ((main-branch-dir (jf/gptel--create-branch-directory session-dir "main"))
         (session-file (jf/gptel--context-file-path main-branch-dir))
         (initial-content (or initial-content
                              (jf/gptel--initial-session-content
                               preset-name parent-session-id))))

    ;; Write scope.yml from preset's scope profile
    (jf/gptel-scope-profile--create-for-session
     preset-name main-branch-dir project-root worktree-paths)

    ;; Create current symlink pointing to main
    (jf/gptel--update-current-symlink session-dir "main")

    ;; Create session file with initial content (pre-populated drawer
    ;; + empty user block). The drawer is authoritative — no
    ;; metadata.yml sidecar is written (design Decision 6).
    (with-temp-file session-file
      (insert initial-content))
    (jf/gptel--log 'info "Created session file: %s" session-file)

    ;; Return paths as plist
    (list :session-id session-id
          :session-dir session-dir
          :branch-dir main-branch-dir
          :branch-name "main"
          :session-file session-file)))

(defun jf/gptel-persistent-session (session-name &optional backend model preset-name)
  "Create a new persistent gptel session named SESSION-NAME.

Optional BACKEND and MODEL default to Claude Opus 4.5.
Optional PRESET-NAME specifies registered preset (default: 'executor).
With prefix argument (C-u), prompts to select preset interactively.

Prompts user to select projectile projects (0 or more).
If projects selected, first project is used as project-root for scope expansion.

Creates session with branches/main/ structure and current symlink.
The session auto-initializes when opened (via find-file-hook).

To open existing sessions: Just use find-file (C-x C-f) or dired on ~/.gptel/sessions/
No special resume command needed - sessions auto-initialize when opened."
  (interactive
   (let* ((name (read-string "Session name: "))
          (preset (if current-prefix-arg
                      (let* ((presets (mapcar #'car gptel--known-presets))
                             (annotator (lambda (name)
                                          (let* ((preset (gptel-get-preset (intern name)))
                                                 (desc (plist-get preset :description)))
                                            (when desc (format "  -- %s" desc)))))
                             (completion-extra-properties
                              (list :annotation-function annotator)))
                        (intern (completing-read "Select preset: "
                                                 (mapcar #'symbol-name presets)
                                                 nil t)))
                    'executor)))
     (list name nil nil preset)))
  (let* ((session-id (jf/gptel--generate-session-id session-name))
         (session-dir (jf/gptel--create-session-directory session-id))
         (preset-name (or preset-name 'executor))
         ;; Project selection
         (selected-projects (when (y-or-n-p "Select projectile projects for this session? ")
                             (jf/gptel--select-projects)))
         (project-root (when selected-projects (car selected-projects)))
         (project-names (when selected-projects
                         (mapcar #'jf/gptel--project-display-name selected-projects))))

    ;; Create session structure using core helper.
    ;; `initial-content' is left nil so the core helper fills in the
    ;; pre-populated drawer + empty-user-block template (Decision 4 /
    ;; Decision 9). No `gptel--save-state' call, no Local Variables
    ;; block write — the drawer embedded in `session.org' is the
    ;; authoritative session-level configuration source (Decision 6).
    ;; Standalone sessions have no parent-session-id.
    (let* ((session-info (jf/gptel--create-session-core
                         session-id
                         session-dir
                         preset-name
                         nil         ; default pre-populated content
                         nil         ; no worktree-paths for standalone
                         project-root
                         nil))       ; no parent-session-id (standalone)
           (session-file (plist-get session-info :session-file)))

      ;; Open session file - auto-initialization hook will handle the rest
      (let ((buffer (find-file session-file)))
        (jf/gptel--log 'info "Created session: %s%s"
                      session-id
                      (if selected-projects
                          (format " with %d project(s)" (length selected-projects))
                        ""))
        (message "Created session: %s\nDirectory: %s\nBranch: main\nPreset: %s%s\n\nSession will auto-initialize when opened."
                 session-name
                 session-dir
                 preset-name
                 (if project-names
                     (format "\nProjects: %s" (string-join project-names ", "))
                   ""))
        buffer))))

(defun jf/gptel-refresh-sessions ()
  "Refresh the session registry by scanning session directories.
Useful if sessions were created outside Emacs or after startup."
  (interactive)
  (jf/gptel--init-registry)
  (message "Refreshed session registry: %d sessions found"
          (jf/gptel--session-count)))

(defun jf/gptel-session-diagnostics ()
  "Show diagnostic information about current session and advice installation."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPTEL Session Diagnostics*")
    (erase-buffer)
    (insert "GPTEL Session Diagnostics\n")
    (insert "=========================\n\n")

    ;; Current buffer info
    (insert "Current Buffer:\n")
    (insert (format "  Name: %s\n" (buffer-name)))
    (insert (format "  Session ID: %s\n" (or jf/gptel--session-id "NOT SET")))
    (insert (format "  Session Dir: %s\n" (or jf/gptel--session-dir "NOT SET")))
    (insert (format "  GPTel Mode: %s\n" (if (bound-and-true-p gptel-mode) "enabled" "disabled")))
    (insert (format "  Auto-save: %s\n" (if jf/gptel-autosave-enabled "enabled" "disabled")))
    (insert "\n")

    ;; Hook diagnostics
    (insert "Save Hook Status:\n")
    (let* ((hooks (buffer-local-value 'before-save-hook (current-buffer)))
           (count (cl-count 'gptel--save-state hooks)))
      (insert (format "  Total before-save-hook entries: %d\n" (length hooks)))
      (insert (format "  gptel--save-state hooks: %d\n" count))
      (when (> count 1)
        (insert (format "  WARNING: Duplicate hooks detected!\n"))))
    (insert "\n")

    ;; Advice installation
    (insert "Advice Installation:\n")
    (insert (format "  gptel--fsm-transition (tool logging): %s\n"
                    (if (advice-member-p #'jf/gptel--advice-log-fsm-transition 'gptel--fsm-transition)
                        "INSTALLED"
                      "NOT INSTALLED")))
    (insert (format "  gptel--set-with-scope (system prompt): %s\n"
                    (if (and (fboundp 'gptel--set-with-scope)
                            (advice-member-p #'jf/gptel--advice-track-system-prompt 'gptel--set-with-scope))
                        "INSTALLED"
                      "NOT INSTALLED")))
    (insert "\n")

    ;; Log level
    (insert "Configuration:\n")
    (insert (format "  Log Level: %s\n" jf/gptel-log-level))
    (insert (format "  Sessions Directory: %s\n" jf/gptel-sessions-directory))
    (insert (format "  Registry Count: %d\n" (jf/gptel--session-count)))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Branch session command is provided by branching.el
;; This is just a forward declaration for documentation
(declare-function jf/gptel-branch-session "branching" (&optional branch-name))

;; Auto-initialize sessions when opened via find-file, dired, recentf, etc.
(add-hook 'find-file-hook #'jf/gptel--auto-init-session-buffer)

(provide 'gptel-session-commands)
;;; commands.el ends here
