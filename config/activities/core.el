;; -*- lexical-binding: t; -*-

(defun activities-ext--get-metadata (activity)
  "Get extension metadata from ACTIVITY.
Returns the plist stored under :activity-extensions key,
or nil if no metadata exists."
  (when activity
    (plist-get (activities-activity-etc activity) :activity-extensions)))

(defun activities-ext--set-metadata (activity metadata)
  "Set extension METADATA for ACTIVITY.
METADATA should be a plist with :version, :projects, :org-roam, :settings keys."
  (when activity
    (setf (activities-activity-etc activity)
          (plist-put (activities-activity-etc activity)
                     :activity-extensions
                     metadata))))

(defconst activities-ext--metadata-version 1
  "Current version of the metadata schema.")

(defun activities-ext--create-metadata (&optional projects org-roam settings)
  "Create new metadata plist with optional PROJECTS, ORG-ROAM, and SETTINGS.
Returns a plist with :version, :projects, :org-roam, :settings keys."
  (list :version activities-ext--metadata-version
        :projects (or projects nil)
        :org-roam (or org-roam nil)
        :settings (or settings (list :auto-open-recent activities-ext-auto-open-recent))))

(defun activities-ext--slugify (string)
  "Convert STRING to URL-friendly slug.
Converts to lowercase, replaces spaces and special characters with hyphens."
  (when string
    (let ((slug (downcase string)))
      ;; Replace spaces and underscores with hyphens
      (setq slug (replace-regexp-in-string "[ _]+" "-" slug))
      ;; Remove non-alphanumeric except hyphens
      (setq slug (replace-regexp-in-string "[^a-z0-9-]" "" slug))
      ;; Remove leading/trailing hyphens
      (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
      ;; Collapse multiple hyphens
      (setq slug (replace-regexp-in-string "-+" "-" slug))
      slug)))

(defun activities-ext--activity-directory (activity-name)
  "Get the dedicated directory for ACTIVITY-NAME.
Returns path: ~/emacs-activities/ACTIVITY-SLUG-YYYY-MM-DD/
Creates the directory if it doesn't exist.
Appends current date to ensure uniqueness."
  (let* ((slug (activities-ext--slugify activity-name))
         (date-stamp (format-time-string "%Y-%m-%d"))
         (dir-name (format "%s-%s" slug date-stamp))
         (activity-dir (expand-file-name dir-name
                                        (expand-file-name activities-ext-base-directory))))
    (unless (file-directory-p activity-dir)
      (make-directory activity-dir t))
    activity-dir))

(defun activities-ext--worktrees-directory (activity-name)
  "Get the worktrees directory for ACTIVITY-NAME.
Returns path: ~/emacs-activities/ACTIVITY-SLUG-YYYY-MM-DD/worktrees/
Creates the directory if it doesn't exist."
  (let ((worktrees-dir (expand-file-name "worktrees"
                                         (activities-ext--activity-directory activity-name))))
    (unless (file-directory-p worktrees-dir)
      (make-directory worktrees-dir t))
    worktrees-dir))

(defun activities-ext--session-directory (activity-name)
  "Get the session directory for ACTIVITY-NAME.
Returns path: ~/emacs-activities/ACTIVITY-SLUG-YYYY-MM-DD/session/
Creates the directory if it doesn't exist."
  (let ((session-dir (expand-file-name "session"
                                       (activities-ext--activity-directory activity-name))))
    (unless (file-directory-p session-dir)
      (make-directory session-dir t))
    session-dir))

(defun activities-ext--validate-project (project-plist)
  "Validate PROJECT-PLIST data.
Returns plist with :valid t/nil, :exists t/nil, :error string.
PROJECT-PLIST should have :path key at minimum."
  (let* ((path (plist-get project-plist :path))
         (worktree (plist-get project-plist :worktree))
         (effective-path (or worktree path)))
    (cond
     ((not path)
      (list :valid nil :exists nil :error "No path specified"))

     ((not (file-exists-p effective-path))
      (list :valid nil :exists nil
            :error (format "Directory does not exist: %s" effective-path)))

     ((not (file-directory-p effective-path))
      (list :valid nil :exists t
            :error (format "Path is not a directory: %s" effective-path)))

     ((not (file-readable-p effective-path))
      (list :valid nil :exists t
            :error (format "Directory not readable: %s" effective-path)))

     (t
      (list :valid t :exists t :error nil)))))

(defun activities-ext--before-resume (activity)
  "Hook called before resuming ACTIVITY.
Opens org-roam document and recent project files based on settings."
  (when-let ((ext-data (activities-ext--get-metadata activity)))
    (let ((projects (plist-get ext-data :projects))
          (org-roam-data (plist-get ext-data :org-roam))
          (gptel-session (plist-get ext-data :gptel-session))
          (settings (plist-get ext-data :settings)))

      ;; Open gptel session buffer if exists
      (when (and gptel-session
                 (fboundp 'jf/gptel-session--open-existing)
                 (plist-get gptel-session :session-file))
        (let ((session-file (plist-get gptel-session :session-file)))
          (when (file-exists-p session-file)
            (jf/gptel-session--open-existing session-file))))

      ;; Open org-roam document in current window
      (when org-roam-data
        (activities-ext--open-org-roam-doc org-roam-data))

      ;; Open recent files if enabled
      (when (plist-get settings :auto-open-recent)
        (dolist (project projects)
          (when-let ((files (plist-get project :recent-files)))
            (activities-ext--open-recent-files files)))))))

(defun activities-ext--before-suspend (activity)
  "Hook called before suspending ACTIVITY.
Updates recent files list for all projects."
  (when-let ((ext-data (activities-ext--get-metadata activity)))
    (let* ((projects (plist-get ext-data :projects))
           (updated-projects
            (mapcar
             (lambda (proj)
               (let* ((path (plist-get proj :path))
                      (recent (when path
                               (activities-ext--get-recent-project-files
                                path
                                activities-ext-recent-files-limit))))
                 ;; Update or add recent-files key
                 (if recent
                     (plist-put (copy-sequence proj) :recent-files recent)
                   proj)))
             projects)))
      ;; Update metadata with new recent files
      (plist-put ext-data :projects updated-projects)
      (activities-ext--set-metadata activity ext-data))))

;; Register resume hook
(add-hook 'activities-before-resume-functions #'activities-ext--before-resume)

;; Note: activities package doesn't have a before-suspend-functions hook,
;; so we use advice to add our suspend handler
(advice-add 'activities-suspend :before
            (lambda (activity)
              (activities-ext--before-suspend activity)))

(defun activities-ext--current-activity ()
  "Get the currently active activity.
Returns nil if no activity is active."
  (when (bound-and-true-p activities--current)
    activities--current))

(defun activities-ext--buffer-in-current-activity-p (buffer-or-name)
  "Return non-nil if BUFFER is in current activity.
Checks frame/tab parameters depending on mode."
  (when-let ((activity (activities-ext--current-activity)))
    (cond
     ((bound-and-true-p activities-tabs-mode)
      (when-let ((tab (tab-bar--current-tab-find)))
        (member (get-buffer buffer-or-name)
                (alist-get 'activities-buffer-list (cdr tab)))))
     (t
      ;; Frame mode: check window list
      (cl-some (lambda (window)
                 (eq (get-buffer buffer-or-name)
                     (window-buffer window)))
               (window-list))))))

(defun activities-ext-buffer-activity ()
  "Show activity for current buffer.
Returns activity if buffer is in current activity, otherwise nil."
  (interactive)
  (if (activities-ext--buffer-in-current-activity-p (current-buffer))
      (when-let ((activity (activities-ext--current-activity)))
        (message "Buffer belongs to activity: %s"
                 (activities-activity-name activity))
        activity)
    (message "Buffer not in current activity")))

(defun activities-ext-buffer-all-activities ()
  "Show ALL activities that reference current buffer (deep search).
Scans all activity project paths and org-roam docs."
  (interactive)
  (let* ((buffer (current-buffer))
         (file (buffer-file-name buffer))
         (matches nil))

    ;; Check current activity first
    (when (activities-ext--buffer-in-current-activity-p buffer)
      (push (cons (activities-activity-name (activities-ext--current-activity))
                  'current)
            matches))

    ;; Scan all activities for project path matches
    (when file
      (dolist (activity-entry activities-activities)
        (let* ((activity (cdr activity-entry))
               (ext-data (activities-ext--get-metadata activity))
               (projects (plist-get ext-data :projects)))
          (dolist (project projects)
            (when (activities-ext--file-in-project-p file project)
              (cl-pushnew (cons (activities-activity-name activity)
                               'project-path)
                         matches
                         :test (lambda (a b) (string= (car a) (car b)))))))))

    ;; Display results
    (if matches
        (let ((buf (get-buffer-create "*Buffer Activities*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert (format "Activities for buffer: %s\n\n" (buffer-name)))
            (dolist (match matches)
              (insert (format "- %s (%s)\n" (car match) (cdr match))))
            (goto-char (point-min))
            (view-mode 1))
          (pop-to-buffer buf))
      (message "No activities reference this buffer"))))

(defun activities-ext-show-projects ()
  "Show projects for current activity in *Activities Projects* buffer."
  (interactive)
  (if-let* ((activity (activities-ext--current-activity))
            (ext-data (activities-ext--get-metadata activity))
            (projects (plist-get ext-data :projects)))
      (let ((buf (get-buffer-create "*Activities Projects*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "Projects for Activity: %s\n\n"
                         (activities-activity-name activity)))
          (dolist (proj projects)
            (let ((name (plist-get proj :name))
                  (path (plist-get proj :path))
                  (branch (plist-get proj :branch))
                  (worktree (plist-get proj :worktree))
                  (validation (activities-ext--validate-project proj)))
              (insert (format "* %s\n" name))
              (insert (format "  Path: %s\n"
                             (or worktree path)))
              (when branch
                (insert (format "  Branch: %s\n" branch)))
              (insert (format "  Status: %s\n"
                             (if (plist-get validation :valid)
                                 "✓ Valid"
                               (format "✗ %s" (plist-get validation :error)))))
              (insert "\n")))
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer buf))
    (message "No projects in current activity")))

(defun activities-ext-open-document ()
  "Open org-roam document for current activity."
  (interactive)
  (if-let* ((activity (activities-ext--current-activity))
            (ext-data (activities-ext--get-metadata activity))
            (org-roam-data (plist-get ext-data :org-roam)))
      (activities-ext--open-org-roam-doc org-roam-data)
    (message "No org-roam document for current activity")))

(defun activities-ext-validate ()
  "Validate current activity's projects, files, and org-roam reference.
Reports status in minibuffer and creates detailed report buffer."
  (interactive)
  (if-let ((activity (activities-ext--current-activity)))
      (let* ((ext-data (activities-ext--get-metadata activity))
             (projects (plist-get ext-data :projects))
             (org-roam-data (plist-get ext-data :org-roam))
             ;; Validate projects
             (results (when projects
                       (mapcar #'activities-ext--validate-project projects)))
             (valid-count 0)
             (total-count (length projects))
             ;; Sync org-roam metadata
             (org-roam-status (when org-roam-data
                               (activities-ext--sync-org-roam-metadata activity))))

        ;; Count valid projects
        (dolist (result results)
          (when (plist-get result :valid)
            (cl-incf valid-count)))

        ;; Build status message
        (let ((project-status (if (= valid-count total-count)
                                 (format "All %d projects valid ✓" total-count)
                               (format "Warning: %d/%d projects valid" valid-count total-count)))
              (org-roam-status-msg (pcase org-roam-status
                                    ('synced "Org-roam doc: ✓ synced")
                                    ('missing "Org-roam doc: ✗ missing")
                                    ('unavailable "Org-roam doc: ? unavailable")
                                    ('no-metadata "Org-roam doc: none")
                                    (_ "Org-roam doc: unknown"))))

          ;; Report combined status
          (message "%s | %s" project-status org-roam-status-msg)

          ;; Show detailed report if there are issues
          (when (or (< valid-count total-count)
                   (eq org-roam-status 'missing))
            (activities-ext-show-projects))))
    (message "No active activity")))

(defun activities-ext-add-project ()
  "Add a project to the current activity.
Uses the sanitized activity name for branch/worktree creation."
  (interactive)
  (if-let ((activity (activities-ext--current-activity)))
      (let* ((activity-name (activities-activity-name activity))
             (sanitized-name (activities-ext--slugify activity-name))
             (project-path (completing-read "Select project to add: "
                                           (projectile-known-projects)
                                           nil t))
             (project-name (file-name-nondirectory
                           (directory-file-name project-path)))
             ;; Pass sanitized activity name and full activity name for branch/worktree
             (git-result (activities-ext--prompt-git-action project-path sanitized-name activity-name))
             (proj-plist (list :path project-path
                             :name project-name
                             :branch (plist-get git-result :branch)
                             :worktree (plist-get git-result :worktree)
                             :recent-files nil))
             (ext-data (or (activities-ext--get-metadata activity)
                          (activities-ext--create-metadata)))
             (projects (plist-get ext-data :projects)))

        ;; Add new project to list
        (setq projects (append projects (list proj-plist)))
        (plist-put ext-data :projects projects)
        (activities-ext--set-metadata activity ext-data)

        (message "Added project %s to activity with branch %s" project-name sanitized-name))
    (message "No active activity")))

(provide 'activities-extensions-core)
;;; core.el ends here
