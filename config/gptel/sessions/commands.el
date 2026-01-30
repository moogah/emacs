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
;; ARCHITECTURE NOTE: Preset files are templates used at session creation.
;; Once created, gptel's Local Variables are the source of truth for settings.
;; Presets are NOT reapplied on resume - they may become stale as sessions evolve.
;;
;; SYSTEM MESSAGE HANDLING: System messages are managed via preset files, not
;; Local Variables. This prevents duplicate Local Variables blocks when system
;; messages exceed ~4000 characters. Buffer-local advice on gptel--save-state
;; prevents gptel from persisting system messages to Local Variables.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-metadata)
(require 'jf-gptel-scope-commands)

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
  "Auto-initialize gptel session if current buffer is a branch session file.
Detects session files by path pattern: sessions/<session-id>/branches/<branch-name>/session.md
Handles both new sessions (no Local Variables yet) and existing sessions
(Local Variables already loaded by Emacs).
This runs on every file open via find-file-hook, so performance is critical."
  ;; Fast path guards (performance critical - runs on every file open)
  (when (and (buffer-file-name)                      ; Has file? (fast)
             (not (bound-and-true-p jf/gptel--session-id)) ; Not already initialized? (fast)
             (string-suffix-p ".md" (buffer-file-name)))   ; Is .md file? (fast)
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-nondirectory file-path)))
      ;; Branch session file detection: path must match */branches/*/session.md
      ;; Works for both ~/.gptel/sessions/<id>/branches/<branch>/session.md
      ;; and ~/emacs-activities/<name>/session/branches/<branch>/session.md
      (when (and (string= file-name "session.md")
                 (string-match "/branches/\\([^/]+\\)/session\\.md$" file-path))
        (let* ((branch-name (match-string 1 file-path))
               (branch-dir (file-name-directory file-path))
               (session-dir (expand-file-name "../.." branch-dir))
               ;; Extract session-id from session-dir
               (session-id (jf/gptel--session-id-from-directory session-dir)))
          ;; Validate session and branch directories
          (when (and (jf/gptel--valid-session-directory-p session-dir)
                    (jf/gptel--valid-branch-directory-p branch-dir))
            ;; Initialization (inline - no separate helper function)
            (condition-case err
                (let (;; Detect scenario: check if gptel-backend is buffer-local
                      ;; If Local Variables exist, Emacs has already loaded them
                      (has-local-vars (local-variable-p 'gptel-backend)))

                  (if has-local-vars
                      ;; SCENARIO 2: Existing session with Local Variables
                      ;; Local Variables were already loaded by Emacs before hook ran
                      (progn
                        (jf/gptel--log 'debug "Existing branch session detected (has Local Variables): %s/%s"
                                      session-id branch-name)
                        ;; Enable gptel-mode (gptel--restore-state will use existing vars)
                        (jf/gptel--ensure-mode-once)
                        ;; Load system message from preset (not in Local Variables)
                        (when-let ((preset-plist (jf/gptel--load-preset-from-file branch-dir))
                                   (system-message (plist-get preset-plist :system)))
                          (jf/gptel--set-session-system-message system-message)))

                    ;; SCENARIO 1: New session without Local Variables
                    ;; Apply preset settings BEFORE enabling gptel-mode
                    (progn
                      (jf/gptel--log 'debug "New branch session detected (no Local Variables): %s/%s"
                                    session-id branch-name)
                      ;; Load and apply preset settings (includes system message with save-prevention)
                      (let ((preset-plist (jf/gptel--load-preset-from-file branch-dir)))
                        (jf/gptel--apply-session-preset preset-plist))
                      ;; Now enable gptel-mode (will see the buffer-local settings we just applied)
                      (jf/gptel--ensure-mode-once)))

                  ;; Common steps for both scenarios
                  (setq-local jf/gptel--session-id session-id)
                  (setq-local jf/gptel--session-dir session-dir)
                  (setq-local jf/gptel--branch-name branch-name)
                  (setq-local jf/gptel--branch-dir branch-dir)
                  (jf/gptel--register-session session-dir (current-buffer) session-id branch-name branch-dir)
                  (setq-local jf/gptel-autosave-enabled t)

                  ;; Load activity worktree paths if present (for activity-scoped sessions)
                  ;; This makes worktree paths available to tools like list_activity_worktrees
                  (when (local-variable-p 'gptel-activity-worktrees)
                    (jf/gptel--log 'debug "Loaded %d activity worktree path(s)"
                                  (length gptel-activity-worktrees)))

                  ;; Update current symlink to point to this branch
                  (jf/gptel--update-current-symlink session-dir branch-name)

                  (jf/gptel--log 'info "Auto-initialized %s branch session: %s/%s"
                                (if has-local-vars "existing" "new")
                                session-id branch-name)
                  (message "Session initialized: %s (branch: %s)" session-id branch-name))
              (error
               (jf/gptel--log 'error "Failed to auto-initialize branch session: %s"
                              (error-message-string err))
               (message "Warning: Session auto-init failed. File opened in basic mode.")))))))))

(defun jf/gptel--skip-system-message-save-advice (orig-fun)
  "Prevent gptel from saving system message for session buffers.
Session system messages are managed via preset files, not Local Variables.
This advice temporarily removes the buffer-local binding during save,
preventing gptel from detecting a difference from default value."
  (if (and (boundp 'jf/gptel--session-dir) jf/gptel--session-dir
           (boundp 'gptel--system-message)
           (local-variable-p 'gptel--system-message))
      ;; Temporarily kill buffer-local binding so gptel won't save it
      (let ((saved-system-msg gptel--system-message))
        (kill-local-variable 'gptel--system-message)
        (unwind-protect
            (funcall orig-fun)
          ;; Restore buffer-local binding after save
          (setq-local gptel--system-message saved-system-msg)))
    ;; Not a session buffer or no buffer-local system message, use default behavior
    (funcall orig-fun)))

(defun jf/gptel--set-session-system-message (system-message)
  "Set system message for session buffer without triggering Local Variables save.
System message is managed via preset files, not gptel's Local Variables persistence.
Installs buffer-local advice to prevent gptel--save-state from saving it."
  (when system-message
    ;; Set the system message in the buffer
    (setq-local gptel--system-message system-message)

    ;; Install buffer-local advice to prevent gptel from saving it
    (advice-add 'gptel--save-state :around
                #'jf/gptel--skip-system-message-save-advice
                '((local)))

    (jf/gptel--log 'debug "Set session system message (%d chars) with save-prevention"
                  (length system-message))))

(defun jf/gptel--list-preset-templates ()
  "List available preset template files in jf/gptel-presets-directory.
Returns list of template names (without extensions)."
  (let* ((preset-dir (expand-file-name jf/gptel-presets-directory))
         (files (directory-files preset-dir nil "\\.\\(md\\|org\\)$")))
    (mapcar (lambda (f) (file-name-sans-extension f)) files)))

(defun jf/gptel--resolve-preset-template (template-name)
  "Resolve TEMPLATE-NAME to full path.
Checks for both .md and .org extensions.
Returns nil if template doesn't exist."
  (let* ((preset-dir (expand-file-name jf/gptel-presets-directory))
         (md-file (expand-file-name (concat template-name ".md") preset-dir))
         (org-file (expand-file-name (concat template-name ".org") preset-dir)))
    (cond
     ((file-exists-p md-file) md-file)
     ((file-exists-p org-file) org-file)
     (t nil))))

(defun jf/gptel--select-preset-template ()
  "Prompt user to select a preset template.
Returns template name (without extension)."
  (let ((templates (jf/gptel--list-preset-templates)))
    (if (null templates)
        (error "No preset templates found in %s" jf/gptel-presets-directory)
      (completing-read "Select preset template: " templates nil t nil nil "programming-assistant"))))

(defun jf/gptel--copy-preset-template (template-name session-dir)
  "Copy preset template TEMPLATE-NAME to SESSION-DIR.
Copies as preset.md or preset.org depending on source format.
Signals error if template doesn't exist."
  (let ((template-path (jf/gptel--resolve-preset-template template-name)))
    (unless template-path
      (error "Preset template not found: %s (checked .md and .org)" template-name))
    (let* ((template-ext (file-name-extension template-path))
           (dest-file (expand-file-name (concat "preset." template-ext) session-dir)))
      (when (file-exists-p dest-file)
        (jf/gptel--log 'warn "Overwriting existing preset file: %s" dest-file))
      (copy-file template-path dest-file t)
      (jf/gptel--log 'info "Copied preset template %s to %s" template-name dest-file)
      dest-file)))

(defun jf/gptel--normalize-preset-for-serialization (preset-plist)
  "Convert PRESET-PLIST to serializable form.
Backend objects → names, model symbols → strings, tools → names."
  (let* ((backend (plist-get preset-plist :backend))
         (backend-name (if (gptel-backend-p backend)
                          (gptel-backend-name backend)
                        backend))
         (model (plist-get preset-plist :model))
         (model-name (if (symbolp model) (symbol-name model) model))
         (tools (plist-get preset-plist :tools))
         (tool-names (when tools
                      (mapcar (lambda (tool)
                               (if (stringp tool) tool (gptel-tool-name tool)))
                             tools))))
    (list :backend backend-name
          :model model-name
          :tools tool-names
          :description (plist-get preset-plist :description)
          :system (plist-get preset-plist :system)
          :temperature (plist-get preset-plist :temperature)
          :include-tool-results (plist-get preset-plist :include-tool-results))))

(defun jf/gptel--write-preset-md (session-dir preset-plist)
  "Write PRESET-PLIST as preset.md in SESSION-DIR.
Converts backend/model objects to names, tools to name strings."
  (let* ((normalized (jf/gptel--normalize-preset-for-serialization preset-plist))
         (backend-name (plist-get normalized :backend))
         (model-name (plist-get normalized :model))
         (system (plist-get normalized :system))
         (temperature (plist-get normalized :temperature))
         (include-tool-results (plist-get normalized :include-tool-results))
         (tool-names (plist-get normalized :tools))
         (preset-file (expand-file-name "preset.md" session-dir)))
    (with-temp-file preset-file
      (insert "---\n")
      (insert (format "description: %s\n" (plist-get normalized :description)))
      (insert (format "backend: %s\n" backend-name))
      (insert (format "model: %s\n" model-name))
      (insert (format "temperature: %s\n" temperature))
      (insert (format "include-tool-results: %s\n" include-tool-results))
      (when tool-names
        (insert "tools:\n")
        (dolist (tool-name tool-names)
          (insert (format "  - %s\n" tool-name))))
      (insert "---\n\n")
      (when system
        (insert system)))
    (jf/gptel--log 'info "Created preset.md: %s" preset-file)))

(defun jf/gptel--write-preset-org (session-dir preset-plist)
  "Write PRESET-PLIST as preset.org in SESSION-DIR.
Converts backend/model objects to names, tools to space-separated string."
  (let* ((normalized (jf/gptel--normalize-preset-for-serialization preset-plist))
         (backend-name (plist-get normalized :backend))
         (model-name (plist-get normalized :model))
         (system (plist-get normalized :system))
         (temperature (plist-get normalized :temperature))
         (include-tool-results (plist-get normalized :include-tool-results))
         (tool-names (plist-get normalized :tools))
         (preset-file (expand-file-name "preset.org" session-dir)))
    (with-temp-file preset-file
      (insert ":PROPERTIES:\n")
      (insert (format ":description: %s\n" (plist-get normalized :description)))
      (insert (format ":backend: %s\n" backend-name))
      (insert (format ":model: %s\n" model-name))
      (insert (format ":temperature: %s\n" temperature))
      (insert (format ":include-tool-results: %s\n" include-tool-results))
      (when tool-names
        (let ((tools-str (mapconcat #'identity tool-names " ")))
          (insert (format ":tools: %s\n" tools-str))))
      (insert ":END:\n\n")
      (when system
        (insert system)))
    (jf/gptel--log 'info "Created preset.org: %s" preset-file)))

(defun jf/gptel--write-preset-file (session-dir preset-plist &optional format)
  "Write PRESET-PLIST to preset file in SESSION-DIR.
FORMAT can be 'md (markdown) or 'org (default: 'md).
Converts backend objects to names, tools to name strings."
  (let ((format (or format 'md)))
    (pcase format
      ('md (jf/gptel--write-preset-md session-dir preset-plist))
      ('org (jf/gptel--write-preset-org session-dir preset-plist))
      (_ (error "Unsupported preset format: %s (expected 'md or 'org)" format)))))

(defun jf/gptel--load-preset-from-file (session-dir)
  "Load preset for session in SESSION-DIR.
Supports .md and .org formats only.

Priority order:
1. session-dir/preset.md
2. session-dir/preset.org

Returns preset plist with keys :description, :backend, :model, :system,
:temperature, :include-tool-results, :tools.

NOTE: This is a template loader, not used during session resume."
  (let* ((md-file (expand-file-name "preset.md" session-dir))
         (org-file (expand-file-name "preset.org" session-dir))
         (preset-path (cond
                       ((file-exists-p md-file) md-file)
                       ((file-exists-p org-file) org-file)
                       (t (error "No preset file found in %s (expected preset.md or preset.org)" session-dir))))
         (ext (file-name-extension preset-path)))
    (unless (member ext '("md" "org"))
      (error "Unsupported preset file format: %s (expected .md or .org)" ext))
    ;; Load using gptel-agent parser
    (require 'gptel-agent)
    ;; Pass non-nil templates to trigger :system extraction (empty list '() is nil in elisp!)
    (let* ((preset-data (gptel-agent-read-file preset-path '((dummy . dummy))))
           (preset-plist (cdr preset-data))
           ;; Convert tool names to tool objects if needed
           (tools (plist-get preset-plist :tools))
           (resolved-tools
            (when tools
              (if (listp tools)
                  tools  ; Already a list
                (split-string tools)))))  ; Space-separated string from org
      ;; Update tools in plist
      (when resolved-tools
        (plist-put preset-plist :tools resolved-tools))
      ;; Convert backend name to object
      (when-let ((backend-name (plist-get preset-plist :backend)))
        (plist-put preset-plist :backend
                   (alist-get backend-name gptel--known-backends nil nil #'equal)))
      ;; Convert model name to symbol
      (when-let ((model-name (plist-get preset-plist :model)))
        (plist-put preset-plist :model
                   (if (symbolp model-name) model-name (intern model-name))))
      preset-plist)))

(defun jf/gptel--apply-session-preset (preset-plist)
  "Apply PRESET-PLIST to current buffer buffer-locally.
Includes tools from preset and gptel-include-tool-results for native tool persistence.

NOTE: This should only be called ONCE at session creation.
After the first save, gptel's Local Variables become the source of truth.

IMPORTANT: Sets system message with save-prevention advice to prevent gptel
from saving it to Local Variables (which causes duplicate blocks with long
messages >4000 chars). System message is managed via preset files."
  (let ((backend (plist-get preset-plist :backend))
        (model (plist-get preset-plist :model))
        (system (plist-get preset-plist :system))
        (temperature (plist-get preset-plist :temperature))
        (include-tool-results (plist-get preset-plist :include-tool-results))
        (tools (plist-get preset-plist :tools)))
    (when backend
      (setq-local gptel-backend backend))
    (when model
      (setq-local gptel-model model))
    ;; Set system message with save-prevention (so it's active from session start)
    (when system
      (jf/gptel--set-session-system-message system))
    (when temperature
      (setq-local gptel-temperature temperature))
    (when (not (null include-tool-results))
      (setq-local gptel-include-tool-results include-tool-results))
    ;; Apply tools from preset
    (when tools
      (let ((resolved-tools
             (cl-loop for tool-name in tools
                      for tool = (gptel-get-tool tool-name)
                      when tool collect tool
                      else do (jf/gptel--log 'warn "Tool not found: %s" tool-name))))
        (when resolved-tools
          (setq-local gptel-tools resolved-tools)
          (jf/gptel--log 'info "Applied %d tools from preset" (length resolved-tools)))))
    (jf/gptel--log 'info "Applied session preset as initial template (system message with save-prevention)")))

(defun jf/gptel--create-session-core (session-id session-dir preset-template scope-type &optional projects initial-content worktree-paths)
  "Create session directory structure with branching support.

SESSION-ID - unique session identifier
SESSION-DIR - parent directory for session (will contain branches/)
PRESET-TEMPLATE - name of preset template to copy
SCOPE-TYPE - symbol 'project-aware or 'deny-all
PROJECTS - optional list for project-aware scope plans
INITIAL-CONTENT - optional initial content for session.md (default: \"###\\n\")
WORKTREE-PATHS - optional list of worktree paths for activity isolation

Creates:
- SESSION-DIR/branches/main/ directory structure
- preset.md (copied from template)
- scope-plan.yml (project-aware or deny-all, optionally with worktree paths)
- session.md (with initial content)
- current symlink pointing to main branch

Returns plist with:
  :session-id - session identifier
  :session-dir - session directory path
  :branch-dir - main branch directory path
  :branch-name - \"main\"
  :session-file - path to session.md"

  (let* (;; Create branches/main/ directory
         (main-branch-dir (jf/gptel--create-branch-directory session-dir "main"))
         (session-file (jf/gptel--context-file-path main-branch-dir))
         (initial-content (or initial-content "###\n")))

    ;; Create scope plan in main branch
    (let ((scope-yaml (if (eq scope-type 'project-aware)
                         (jf/gptel--generate-scope-plan-yaml
                          session-id "project-aware" projects main-branch-dir worktree-paths)
                       (jf/gptel--generate-scope-plan-yaml session-id "deny-all" nil main-branch-dir worktree-paths)))
          (scope-file (jf/gptel--scope-plan-file-path main-branch-dir)))
      (with-temp-file scope-file
        (insert scope-yaml))
      (jf/gptel--log 'info "Created scope plan: %s" scope-file))

    ;; Copy preset template to main branch directory
    (jf/gptel--copy-preset-template preset-template main-branch-dir)

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

(defun jf/gptel-persistent-session (session-name &optional backend model preset-template)
  "Create a new persistent gptel session named SESSION-NAME.

Optional BACKEND and MODEL default to Claude Opus 4.5.
Optional PRESET-TEMPLATE specifies template name (default: \"programming-assistant\").
With prefix argument (C-u), prompts to select template interactively.

Prompts user to select projectile projects (0 or more).
If projects selected, creates project-aware scope plan.
Otherwise creates deny-all scope plan.

Creates session with branches/main/ structure and current symlink.
The session auto-initializes when opened (via find-file-hook).

To open existing sessions: Just use find-file (C-x C-f) or dired on ~/.gptel/sessions/
No special resume command needed - sessions auto-initialize when opened.

The session will auto-save to ~/.gptel/sessions/SESSION-NAME-TIMESTAMP/branches/main/session.md"
  (interactive
   (let ((name (read-string "Session name: "))
         (template (if current-prefix-arg
                       (jf/gptel--select-preset-template)
                     "programming-assistant")))
     (list name nil nil template)))
  (let* ((session-id (jf/gptel--generate-session-id session-name))
         (session-dir (jf/gptel--create-session-directory session-id))
         (preset-template (or preset-template "programming-assistant"))
         ;; Project selection
         (selected-projects (when (y-or-n-p "Select projectile projects for this session? ")
                             (jf/gptel--select-projects)))
         (project-names (when selected-projects
                         (mapcar #'jf/gptel--project-display-name selected-projects)))
         (scope-type (if selected-projects 'project-aware 'deny-all))
         (initial-content (format "# %s\n\n" session-name)))

    ;; Create session structure using core helper
    (let* ((session-info (jf/gptel--create-session-core
                         session-id
                         session-dir
                         preset-template
                         scope-type
                         selected-projects
                         initial-content))
           (session-file (plist-get session-info :session-file)))

      ;; Open session file - auto-initialization hook will handle the rest
      (let ((buffer (find-file session-file)))
        (jf/gptel--log 'info "Created session: %s%s"
                      session-id
                      (if selected-projects
                          (format " with %d project(s)" (length selected-projects))
                        ""))
        (message "Created session: %s\nDirectory: %s\nBranch: main\nTemplate: %s%s\n\nSession will auto-initialize when opened."
                 session-name
                 session-dir
                 preset-template
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
