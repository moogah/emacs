;;; activities-integration.el --- GPTEL Sessions Activities Integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;; This file integrates persistent gptel sessions with activities-extensions.

;;; Code:

(require 'gptel nil t)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-metadata)

;; Optional dependency - checked at runtime
(defvar activities-ext--slugify)

(defgroup jf-gptel-activities nil
  "Integration between gptel sessions and activities."
  :group 'gptel
  :prefix "jf/gptel-activities-")

(defcustom jf/gptel-activities-auto-open-on-resume t
  "Automatically open gptel session buffer when resuming activity."
  :type 'boolean
  :group 'jf-gptel-activities)

(defun jf/gptel-session-create-persistent (activity-name &optional backend model preset-name org-file)
  "Create persistent gptel session immediately for ACTIVITY-NAME.

Unlike lazy initialization, this creates all resources upfront:
- Session directory (~/gptel-sessions/ACTIVITY-NAME-TIMESTAMP/)
- scope.yml with file access permissions (from preset scope profile)
- metadata.yml with session metadata and preset reference
- session.md file for buffer to visit
- Registers in global registry

BACKEND and MODEL default to current gptel-backend and gptel-model.
PRESET-NAME is a symbol naming a registered preset in `gptel--known-presets'
  (default: `executor').
ORG-FILE is optional path to activity org file (for worktree parsing).

Returns plist: (:session-id ... :session-dir ... :buffer-name ... :session-file ...)"
  (unless (and (fboundp 'jf/gptel--read-session-metadata)
               (fboundp 'jf/gptel--register-session))
    (error "GPTEL session registry not available"))

  (let* ((slug (if (fboundp 'activities-ext--slugify)
                   (activities-ext--slugify activity-name)
                 (replace-regexp-in-string "[^a-z0-9-]" "-"
                                          (downcase activity-name))))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         ;; Session ID is activity slug + timestamp
         (session-id (format "%s-%s" slug timestamp))
         ;; Use the provided directory directly (caller has already set it up)
         (session-dir (expand-file-name jf/gptel-sessions-directory))
         (backend (or backend gptel-backend))
         (model (or model gptel-model))
         (backend-name (gptel-backend-name backend))
         (preset-name (or preset-name 'executor))
         ;; Parse worktree paths from org-roam documentation
         ;; Ensure file is saved before parsing
         (raw-paths (jf/gptel-activities--parse-worktree-paths activity-name org-file))
         ;; Convert raw worktree paths to scope plist format
         (worktree-scope
          (when raw-paths
            (list :paths (list :read raw-paths
                               :write raw-paths
                               :deny '("**/.git/**" "**/runtime/**" "**/.env" "**/node_modules/**"))))))

    ;; Create session directory structure using core helper
    ;; This creates branches/main/, scope.yml, metadata.yml, session.md, and current symlink
    (let* ((session-info (jf/gptel--create-session-core
                          session-id
                          session-dir
                          preset-name
                          "###\n"          ; Minimal initial content
                          worktree-scope   ; Scope plist (or nil)
                          nil))            ; No project-root
           (branch-dir (plist-get session-info :branch-dir))
           (session-file (plist-get session-info :session-file)))

      ;; Store worktree paths in session file as file-local variable
      (when raw-paths
        (with-temp-buffer
          (insert-file-contents session-file)
          (goto-char (point-max))
          ;; Add file-local variable block if not already present
          (unless (search-backward "<!-- gptel-activity-worktrees:" nil t)
            (insert (format "\n<!-- gptel-activity-worktrees: %S -->\n"
                           raw-paths))
            (write-region (point-min) (point-max) session-file nil 'silent))
          (jf/gptel--log 'info "Stored %d worktree path(s) in session metadata"
                        (length raw-paths))))

      ;; Register in global registry
      (jf/gptel--log 'info "Registering session: %s" session-id)
      (jf/gptel--register-session session-dir (current-buffer) session-id "main" branch-dir)

      ;; Return session info for buffer creation
      (list :session-id session-id
            :session-dir session-dir   ; Session directory (parent of branches/)
            :branch-dir branch-dir     ; Branch directory (branches/main/)
            :branch-name "main"        ; Activities integration uses main branch
            :buffer-name (format "*gptel-%s*" slug)
            :session-file session-file
            :backend backend-name
            :model (if (symbolp model) (symbol-name model) model)))))

(defun jf/gptel-activities--parse-worktree-paths (activity-name &optional org-file)
  "Parse worktree paths from org document for ACTIVITY-NAME.

If ORG-FILE is provided, uses that path directly (preferred approach).
Otherwise searches for saved org file with title \"Activity: ACTIVITY-NAME\" in org-roam-directory.

Convention: The org file MUST exist on disk before calling this function.

Returns list of absolute, expanded worktree paths.
Returns nil if org document not found or no worktrees defined.

This uses the org documentation as source of truth for activity configuration,
following documentation-first architecture. Uses only standard org-mode functions."
  (condition-case err
      (let* ((activity-file
              (if org-file
                  ;; Explicit file path provided - use it directly
                  (progn
                    (jf/gptel--log 'debug "Using provided org file: %s" org-file)
                    (expand-file-name org-file))
                ;; No file provided - search for it on disk
                (let* ((org-dir (if (boundp 'org-roam-directory)
                                   org-roam-directory
                                 "~/org/roam"))
                       (org-dir-expanded (expand-file-name org-dir))
                       (expected-title (format "Activity: %s" activity-name))
                       (org-files (directory-files org-dir-expanded t "\\.org$"))
                       (found-file nil))
                  (jf/gptel--log 'debug "Searching for activity '%s' in %s (%d .org files)"
                                activity-name org-dir-expanded (length org-files))
                  (dolist (file org-files)
                    (when (and (file-regular-p file) (not found-file))
                      (with-temp-buffer
                        (insert-file-contents file nil 0 500)
                        (goto-char (point-min))
                        (when (re-search-forward (concat "^#\\+title: " (regexp-quote expected-title)) nil t)
                          (jf/gptel--log 'debug "Found activity file: %s" file)
                          (setq found-file file)))))
                  found-file))))

        (if (not (and activity-file (file-exists-p activity-file)))
            (progn
              (jf/gptel--log 'warn "No org document found for activity: %s (file must exist on disk)" activity-name)
              nil)
          ;; Parse the org file for PROJECT_WORKTREE properties
          (jf/gptel--log 'debug "Parsing PROJECT_WORKTREE properties from: %s" activity-file)
          ;; Use existing buffer if file is already open, otherwise open temporarily
          (let* ((existing-buf (get-file-buffer activity-file))
                 (buf (or existing-buf (find-file-noselect activity-file t))))
            (unwind-protect
                (with-current-buffer buf
                  (let ((worktree-paths '())
                        (entry-count 0))
                    (org-map-entries
                     (lambda ()
                       (cl-incf entry-count)
                       (when-let ((worktree (org-entry-get nil "PROJECT_WORKTREE")))
                         (let ((expanded (expand-file-name worktree)))
                           (jf/gptel--log 'debug "Found PROJECT_WORKTREE: %s" expanded)
                           ;; Always append /** suffix for full glob pattern
                           (push (concat expanded "/**") worktree-paths))))
                     nil 'file)
                    (jf/gptel--log 'debug "Scanned %d org entries, found %d worktree(s)"
                                  entry-count (length worktree-paths))
                    (when worktree-paths
                      (jf/gptel--log 'info "Found %d worktree path(s) for activity %s"
                                    (length worktree-paths) activity-name))
                    (nreverse worktree-paths)))
              ;; Only kill buffer if we opened it ourselves
              (unless existing-buf
                (kill-buffer buf))))))
    (error
     (jf/gptel--log 'error "Error parsing worktree paths for %s: %s"
                   activity-name (error-message-string err))
     nil)))

(defun jf/gptel-session--create-buffer (session-info)
  "Create gptel buffer associated with session file.
SESSION-INFO is plist from jf/gptel-session-create-persistent.

The find-file-hook (`jf/gptel--auto-init-session-buffer') handles
preset application and gptel-mode initialization automatically
when the session file is opened."
  (let* ((buffer-name (plist-get session-info :buffer-name))
         (session-file (plist-get session-info :session-file)))
    ;; Open session file — find-file-hook handles auto-initialization
    (let ((buffer (find-file-noselect session-file)))
      (with-current-buffer buffer
        ;; Rename buffer to expected name
        (rename-buffer buffer-name t)
        (jf/gptel--log 'info "Created buffer: %s (session: %s)"
                      buffer-name (plist-get session-info :session-id))
        buffer))))

(defun jf/gptel-session--open-existing (session-file)
  "Open existing gptel session from SESSION-FILE.
The find-file-hook handles auto-initialization and state restoration.
Returns the buffer visiting the session file."
  (unless (file-exists-p session-file)
    (error "Session file does not exist: %s" session-file))
  (let ((buffer (find-file-noselect session-file)))
    (jf/gptel--log 'info "Opened session: %s" session-file)
    buffer))

(provide 'jf-gptel-activities-integration)
;;; activities-integration.el ends here
