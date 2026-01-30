;; -*- lexical-binding: t; -*-

(defun activities-ext--add-gptel-session-to-org-doc (org-roam-data gptel-session-data)
  "Add GPTEL-SESSION-DATA to existing org-roam document specified by ORG-ROAM-DATA.
Updates the Gptel Sessions section with session information."
  (when-let ((file (plist-get org-roam-data :file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the Gptel Sessions section
        (if (re-search-forward "^\\* Gptel Sessions$" nil t)
            (progn
              ;; Found it - go to end of heading and add session
              (forward-line 1)
              (when (looking-at "^No gptel sessions associated")
                ;; Remove the "No sessions" message
                (delete-region (point) (progn (forward-line 1) (point))))
              (insert
               (format "** Session: %s\n"
                      (plist-get gptel-session-data :session-id))
               ":PROPERTIES:\n"
               (format ":GPTEL_SESSION_ID: %s\n"
                      (plist-get gptel-session-data :session-id))
               (format ":GPTEL_SESSION_FILE: %s\n"
                      (plist-get gptel-session-data :session-file))
               (format ":GPTEL_SESSION_DIR: %s\n"
                      (plist-get gptel-session-data :session-dir))
               ":END:\n\n"
               (format "[[file:%s][Open %s]]\n\n"
                      (plist-get gptel-session-data :session-file)
                      (plist-get gptel-session-data :session-id))))
          ;; Gptel Sessions section not found - this shouldn't happen
          (message "Warning: Could not find Gptel Sessions section in %s" file)))
      (save-buffer))))

(defun activities-ext--create-org-roam-doc (activity-name &optional projects gptel-session activity-directory)
  "Create org-roam document for ACTIVITY-NAME.
Optional PROJECTS is a list of project plists to include in document.
Optional GPTEL-SESSION is plist with session info to include in document.
Optional ACTIVITY-DIRECTORY is the path to the activity's base directory.
Returns plist with :node-id, :file, :title, :created keys."
  (let* ((slug (format "%s-%s"
                      (format-time-string "%Y%m%d%H%M%S")
                      (activities-ext--slugify activity-name)))
         (filename (concat slug ".org"))
         (subdirectory (if (string-empty-p activities-ext-org-roam-subdirectory)
                          ""
                        (file-name-as-directory activities-ext-org-roam-subdirectory)))
         (full-subdir (expand-file-name subdirectory org-roam-directory))
         (file-path (expand-file-name filename full-subdir))
         (title (format "Activity: %s" activity-name)))

    ;; Ensure subdirectory exists
    (unless (file-directory-p full-subdir)
      (make-directory full-subdir t))

    ;; Create the file with content
    (with-temp-file file-path
      (insert (format "#+title: %s\n" title))
      (insert (format "#+filetags: :activity:%s:\n" (activities-ext--slugify activity-name)))
      ;; Add activity directory property
      (when activity-directory
        (insert (format "#+property: activity_directory %s\n" activity-directory)))
      ;; Add gptel session properties if provided
      (when gptel-session
        (insert (format "#+property: gptel_session_id %s\n"
                       (plist-get gptel-session :session-id)))
        (insert (format "#+property: gptel_session_dir %s\n"
                       (plist-get gptel-session :session-dir))))
      (insert "\n")
      (insert (activities-ext--default-template activity-name projects gptel-session)))

    ;; Update org-roam database
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file file-path))

    ;; Get node ID and add bidirectional link
    (let ((node-id nil))
      (when (fboundp 'org-roam-node-from-file)
        (let ((node (org-roam-node-from-file file-path)))
          (when node
            (setq node-id (org-roam-node-id node)))))

      ;; Add activity reference to document (bidirectional linking)
      (activities-ext--add-activity-link-to-document file-path activity-name)

      (list :node-id node-id
            :file file-path
            :title title
            :created t
            :last-synced (current-time)))))

(defun activities-ext--default-template (activity-name projects &optional gptel-session)
  "Generate default template content for ACTIVITY-NAME with PROJECTS.
PROJECTS is a list of project plists with :path, :name, :branch, :worktree keys.
Optional GPTEL-SESSION is plist with session info to link in document.
Each project is stored as an org heading with properties for programmatic access."
  (concat
   "* Overview\n\n"
   (format "Activity: %s\n\n" activity-name)
   "* Gptel Sessions\n\n"
   (if gptel-session
       (let ((session-id (plist-get gptel-session :session-id))
             (session-file (plist-get gptel-session :session-file))
             (session-dir (plist-get gptel-session :session-dir)))
         (concat
          (format "** Session: %s\n" session-id)
          ":PROPERTIES:\n"
          (format ":GPTEL_SESSION_ID: %s\n" session-id)
          (format ":GPTEL_SESSION_FILE: %s\n" session-file)
          (format ":GPTEL_SESSION_DIR: %s\n" session-dir)
          ":END:\n\n"
          (format "[[file:%s][Open %s]]\n\n" session-file session-id)))
     "No gptel sessions associated.\n\n")
   "* Projects\n\n"
   (if projects
       (mapconcat
        (lambda (proj)
          (let* ((name (or (plist-get proj :name) "Unknown"))
                 (path (plist-get proj :path))
                 (branch (plist-get proj :branch))
                 (worktree (plist-get proj :worktree))
                 (effective-path (or worktree path)))
            (concat
             (format "** Project: %s\n" name)
             ":PROPERTIES:\n"
             (format ":PROJECT_PATH: %s\n" path)
             (format ":PROJECT_NAME: %s\n" name)
             (if branch
                 (format ":PROJECT_BRANCH: %s\n" branch)
               "")
             (if worktree
                 (format ":PROJECT_WORKTREE: %s\n" worktree)
               "")
             ":END:\n\n"
             (format "[[file:%s][Open %s]]%s\n\n"
                    effective-path
                    name
                    (if branch (format " (branch: %s)" branch) "")))))
        projects
        "")
     "No projects associated.\n\n")
   "* Tasks\n\n"
   "** TODO Task 1\n"
   "** TODO Task 2\n\n"
   "* Notes\n\n"
   "* Session Log\n\n"
   (format "** Session: %s\n\n"
          (format-time-string "%Y-%m-%d %H:%M"))))

(defun activities-ext--resolve-org-roam-file (org-roam-plist)
  "Resolve org-roam file from PLIST, preferring node-id lookup.
Returns plist with :resolved-file, :needs-sync, :original-file keys.

Resolution strategy:
1. Try node-id lookup via org-roam-node-from-id (most stable)
2. Fallback to cached file path if org-roam unavailable
3. Last resort: search by title in org-roam database

ORG-ROAM-PLIST should have :node-id and/or :file keys."
  (when org-roam-plist
    (let* ((node-id (plist-get org-roam-plist :node-id))
           (cached-file (plist-get org-roam-plist :file))
           (resolved-file nil)
           (needs-sync nil))

      ;; Strategy 1: Try node-id lookup (most stable)
      (when (and node-id (fboundp 'org-roam-node-from-id))
        (when-let ((node (org-roam-node-from-id node-id)))
          (setq resolved-file (org-roam-node-file node))
          ;; Check if file path changed
          (when (and cached-file
                     resolved-file
                     (not (string= (expand-file-name cached-file)
                                  (expand-file-name resolved-file))))
            (setq needs-sync t))))

      ;; Strategy 2: Fallback to cached file if org-roam unavailable
      (unless resolved-file
        (when (and cached-file (file-exists-p cached-file))
          (setq resolved-file cached-file)))

      ;; Strategy 3: Last resort - search by title (not implemented yet)
      ;; Could search org-roam-db for nodes with matching title

      (list :resolved-file resolved-file
            :needs-sync needs-sync
            :original-file cached-file
            :node-id node-id))))

(defun activities-ext--sync-org-roam-metadata (activity)
  "Synchronize org-roam metadata for ACTIVITY with database.
Updates file path if changed, validates node-id exists.
Returns sync status: 'synced, 'missing, 'unavailable, or 'no-metadata."
  (if-let* ((ext-data (activities-ext--get-metadata activity))
            (org-roam-data (plist-get ext-data :org-roam)))
      (let* ((resolution (activities-ext--resolve-org-roam-file org-roam-data))
             (resolved-file (plist-get resolution :resolved-file))
             (needs-sync (plist-get resolution :needs-sync))
             (node-id (plist-get org-roam-data :node-id)))

        (cond
         ;; File resolved successfully
         ((and resolved-file (file-exists-p resolved-file))
          (when needs-sync
            ;; Update cached file path
            (plist-put org-roam-data :file resolved-file)
            (plist-put org-roam-data :last-synced (current-time))
            (plist-put ext-data :org-roam org-roam-data)
            (activities-ext--set-metadata activity ext-data)
            (message "Synced org-roam path: %s" resolved-file))
          'synced)

         ;; Node-id lookup failed but org-roam is available
         ((and node-id (fboundp 'org-roam-node-from-id))
          (message "Warning: Org-roam node missing for activity: %s"
                   (activities-activity-name activity))
          'missing)

         ;; Org-roam not available, cannot sync
         (t
          'unavailable)))
    'no-metadata))

(defun activities-ext--add-activity-link-to-document (file-path activity-name)
  "Add activity reference to org-roam document.
Creates property: #+property: activity_name VALUE
Inserts after existing property lines or at beginning if none exist."
  (when (and file-path (file-exists-p file-path))
    (with-current-buffer (find-file-noselect file-path)
      (save-excursion
        (goto-char (point-min))
        ;; Find last property line
        (let ((last-prop-pos (point-min)))
          (while (re-search-forward "^#\\+property:" nil t)
            (setq last-prop-pos (line-end-position)))
          ;; Insert after last property or at beginning
          (goto-char last-prop-pos)
          (if (> last-prop-pos (point-min))
              (progn
                (end-of-line)
                (insert "\n"))
            ;; If no properties found, insert after title/filetags
            (goto-char (point-min))
            (when (re-search-forward "^#\\+\\(title\\|filetags\\):" nil t)
              (forward-line 1)))
          (insert (format "#+property: activity_name %s\n" activity-name))))
      (save-buffer))))

(defun activities-ext--open-org-roam-doc (org-roam-plist &optional activity)
  "Open org-roam document with robust resolution.
If ACTIVITY provided, optionally sync metadata if stale.
Uses node-id based resolution for stability."
  (when org-roam-plist
    (let* ((resolution (activities-ext--resolve-org-roam-file org-roam-plist))
           (resolved-file (plist-get resolution :resolved-file))
           (needs-sync (plist-get resolution :needs-sync)))

      (cond
       ;; Successfully resolved - open file
       ((and resolved-file (file-exists-p resolved-file))
        (when (and needs-sync activity)
          ;; Opportunistically sync if activity provided
          (activities-ext--sync-org-roam-metadata activity))
        (find-file resolved-file))

       ;; Could not resolve
       (t
        (let ((cached-file (plist-get org-roam-plist :file)))
          (warn "Activity document not found%s"
                (if cached-file
                    (format ": %s" cached-file)
                  " (no file path available)"))))))))

(provide 'activities-extensions-org-roam)
;;; org-roam.el ends here
