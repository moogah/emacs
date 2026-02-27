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
;; metadata.yml (session metadata with preset name). On first open,
;; gptel--apply-preset applies the preset; after first save, upstream's
;; Local Variables + gptel--preset become the source of truth.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-metadata)
(require 'gptel-scope-profiles)

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
  "Ensure gptel-mode is enabled exactly once with correct hooks.
Prevents duplicate before-save-hook entries that cause duplicate Local Variables."
  (unless gptel-mode
    (gptel-mode 1))

  ;; Defensive: Remove any duplicate hooks that might have accumulated
  (let ((hooks (buffer-local-value 'before-save-hook (current-buffer)))
        (count (cl-count 'gptel--save-state
                        (buffer-local-value 'before-save-hook (current-buffer)))))
    (when (> count 1)
      (jf/gptel--log 'warn "Removing %d duplicate gptel--save-state hooks" (1- count))
      ;; Remove all instances and re-add once
      (remove-hook 'before-save-hook #'gptel--save-state t)
      (add-hook 'before-save-hook #'gptel--save-state nil t))))

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
  "Auto-initialize gptel session if current buffer is a session file.
Detects session files by path pattern:
  - Branch sessions: */branches/<branch-name>/session.md
  - Agent sessions:  */agents/<agent-name>/session.md

Handles three scenarios:
  1. Existing session (has gptel--preset in Local Variables) — delegate to
     upstream gptel--restore-state via gptel-mode.
  2. Legacy session (has preset.md but no gptel--preset) — open in basic mode
     with warning.
  3. New session (no Local Variables, no preset.md) — read preset name from
     metadata.yml and apply via gptel--apply-preset.

This runs on every file open via find-file-hook, so performance is critical."
  ;; Fast path guards (performance critical - runs on every file open)
  (when (and (buffer-file-name)                      ; Has file? (fast)
             (not (bound-and-true-p jf/gptel--session-id)) ; Not already initialized? (fast)
             (string-suffix-p ".md" (buffer-file-name)))   ; Is .md file? (fast)
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-nondirectory file-path))
           ;; Detect session type from path pattern
           (branch-name nil)
           (branch-dir nil)
           (session-dir nil)
           (session-type nil))

      ;; Branch session: */branches/<branch>/session.md
      (when (and (string= file-name "session.md")
                 (string-match "/branches/\\([^/]+\\)/session\\.md$" file-path))
        (setq branch-name (match-string 1 file-path)
              branch-dir (file-name-directory file-path)
              session-dir (expand-file-name "../.." branch-dir)
              session-type 'branch))

      ;; Agent session: */agents/<agent>/session.md
      (when (and (not session-type)
                 (string= file-name "session.md")
                 (string-match "/agents/\\([^/]+\\)/session\\.md$" file-path))
        (let ((agent-dir (file-name-directory file-path)))
          (setq branch-name "main"
                branch-dir agent-dir
                session-dir agent-dir
                session-type 'agent)))

      (when session-type
        (let ((session-id (jf/gptel--session-id-from-directory session-dir)))
          ;; Validate directories (branch sessions need both checks; agent sessions just branch-dir)
          (when (if (eq session-type 'branch)
                    (and (jf/gptel--valid-session-directory-p session-dir)
                         (jf/gptel--valid-branch-directory-p branch-dir))
                  (jf/gptel--valid-branch-directory-p branch-dir))
            (condition-case err
                (let (;; Detect scenario: check if gptel--preset is buffer-local
                      ;; Upstream's differential save always writes gptel--preset
                      ;; gptel-backend may NOT be in Local Variables if it matches preset default
                      (has-preset-var (local-variable-p 'gptel--preset)))

                  (cond
                   ;; SCENARIO 1: Existing session with gptel--preset in Local Variables
                   ;; Emacs already loaded Local Variables before this hook ran.
                   ;; Delegate entirely to upstream gptel--restore-state.
                   (has-preset-var
                    (jf/gptel--log 'debug "Existing %s session detected (has gptel--preset): %s/%s"
                                  session-type session-id branch-name)
                    ;; Set buffer-local session variables FIRST
                    (setq-local jf/gptel--session-id session-id)
                    (setq-local jf/gptel--session-dir session-dir)
                    (setq-local jf/gptel--branch-name branch-name)
                    (setq-local jf/gptel--branch-dir branch-dir)

                    ;; Enable gptel-mode — triggers gptel--restore-state which applies
                    ;; preset and overlays saved overrides. We do NOT call any custom
                    ;; preset loading functions.
                    (jf/gptel--ensure-mode-once)

                    ;; Load scope from scope.yml if it exists
                    (when (file-exists-p (jf/gptel--scope-file-path branch-dir))
                      (jf/gptel--log 'debug "Scope config available at %s" (jf/gptel--scope-file-path branch-dir))))

                   ;; SCENARIO 2: Legacy session — has preset.md but no gptel--preset
                   ;; Old-format session that predates upstream preset restore.
                   ((file-exists-p (jf/gptel--preset-file-path branch-dir))
                    (jf/gptel--log 'warn "Legacy session detected (has preset.md, no gptel--preset): %s/%s"
                                  session-id branch-name)
                    ;; Set buffer-local session variables
                    (setq-local jf/gptel--session-id session-id)
                    (setq-local jf/gptel--session-dir session-dir)
                    (setq-local jf/gptel--branch-name branch-name)
                    (setq-local jf/gptel--branch-dir branch-dir)

                    ;; For now, just enable gptel-mode in basic mode
                    ;; Full legacy migration will be handled by a later bead
                    (jf/gptel--log 'warn "Legacy session opened in basic mode. Re-apply a preset from transient menu.")
                    (jf/gptel--ensure-mode-once))

                   ;; SCENARIO 3: New session — no Local Variables, no preset.md
                   ;; Read preset name from metadata.yml and apply via gptel--apply-preset
                   (t
                    (jf/gptel--log 'debug "New %s session detected (no Local Variables): %s/%s"
                                  session-type session-id branch-name)
                    ;; Set buffer-local session variables
                    (setq-local jf/gptel--session-id session-id)
                    (setq-local jf/gptel--session-dir session-dir)
                    (setq-local jf/gptel--branch-name branch-name)
                    (setq-local jf/gptel--branch-dir branch-dir)

                    ;; Read preset name from metadata.yml
                    (let* ((metadata-path (jf/gptel--metadata-file-path branch-dir))
                           (preset-name nil))
                      (when (file-exists-p metadata-path)
                        (condition-case err
                            (with-temp-buffer
                              (insert-file-contents metadata-path)
                              (let* ((parsed (yaml-parse-string (buffer-string) :object-type 'plist))
                                     (preset-str (plist-get parsed :preset)))
                                (when preset-str
                                  (setq preset-name (intern preset-str)))))
                          (error
                           (jf/gptel--log 'warn "Failed to read metadata.yml: %s" (error-message-string err)))))

                      (if (and preset-name (gptel-get-preset preset-name))
                          ;; Apply preset via upstream with buffer-local setter
                          (progn
                            (gptel--apply-preset preset-name
                                                (lambda (var val) (set (make-local-variable var) val)))
                            (jf/gptel--ensure-mode-once))
                        ;; Fallback: enable gptel-mode without preset
                        (jf/gptel--log 'warn "No preset found for new session %s, enabling basic gptel-mode" session-id)
                        (jf/gptel--ensure-mode-once)))))

                  ;; Common steps for all scenarios
                  (jf/gptel--register-session session-dir (current-buffer) session-id branch-name branch-dir)
                  (setq-local jf/gptel-autosave-enabled t)

                  ;; Load activity worktree paths if present (for activity-scoped sessions)
                  ;; This makes worktree paths available to tools like list_activity_worktrees
                  (when (local-variable-p 'gptel-activity-worktrees)
                    (jf/gptel--log 'debug "Loaded %d activity worktree path(s)"
                                  (length gptel-activity-worktrees)))

                  ;; Update current symlink to point to this branch
                  (jf/gptel--update-current-symlink session-dir branch-name)

                  (jf/gptel--log 'info "Auto-initialized %s %s session: %s/%s"
                                (cond (has-preset-var "existing")
                                      ((file-exists-p (jf/gptel--preset-file-path branch-dir)) "legacy")
                                      (t "new"))
                                session-type session-id branch-name)
                  (message "Session initialized: %s (branch: %s)" session-id branch-name))
              (error
               (jf/gptel--log 'error "Failed to auto-initialize %s session: %s"
                              session-type (error-message-string err))
               (message "Warning: Session auto-init failed. File opened in basic mode.")))))))))

(defun jf/gptel--create-session-core (session-id session-dir preset-name &optional initial-content worktree-paths project-root)
  "Create session directory structure with branching support.

SESSION-ID - unique session identifier
SESSION-DIR - parent directory for session (will contain branches/)
PRESET-NAME - symbol, name of registered preset in gptel--known-presets
INITIAL-CONTENT - optional initial content for session.md (default: \"###\\n\")
WORKTREE-PATHS - optional scope plist with explicit paths for activity isolation
PROJECT-ROOT - optional project root for scope profile variable expansion

Creates:
- SESSION-DIR/branches/main/ directory structure
- scope.yml (from preset's scope profile, or explicit worktree-paths)
- metadata.yml (session metadata with preset name)
- session.md (with initial content)
- current symlink pointing to main branch

Returns plist with:
  :session-id - session identifier
  :session-dir - session directory path
  :branch-dir - main branch directory path
  :branch-name - \"main\"
  :session-file - path to session.md"

  (let* ((main-branch-dir (jf/gptel--create-branch-directory session-dir "main"))
         (session-file (jf/gptel--context-file-path main-branch-dir))
         (initial-content (or initial-content "###\n")))

    ;; Write scope.yml from preset's scope profile
    (jf/gptel-scope-profile--create-for-session
     preset-name main-branch-dir project-root worktree-paths)

    ;; Write metadata.yml
    (let ((metadata-file (jf/gptel--metadata-file-path main-branch-dir))
          (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
      (with-temp-file metadata-file
        (insert (format "session_id: \"%s\"\n" session-id))
        (insert (format "created: \"%s\"\n" timestamp))
        (insert (format "updated: \"%s\"\n" timestamp))
        (insert (format "preset: \"%s\"\n" (symbol-name preset-name))))
      (jf/gptel--log 'info "Created metadata.yml: %s" metadata-file))

    ;; Create current symlink pointing to main
    (jf/gptel--update-current-symlink session-dir "main")

    ;; Create session file with initial content
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
                         (mapcar #'jf/gptel--project-display-name selected-projects)))
         (initial-content (format "# %s\n\n" session-name)))

    ;; Create session structure using core helper
    (let* ((session-info (jf/gptel--create-session-core
                         session-id
                         session-dir
                         preset-name
                         initial-content
                         nil         ; no worktree-paths for standalone
                         project-root))
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
