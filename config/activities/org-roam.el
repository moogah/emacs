;; -*- lexical-binding: t; -*-

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

    ;; Get node ID
    (let ((node-id nil))
      (when (fboundp 'org-roam-node-from-file)
        (let ((node (org-roam-node-from-file file-path)))
          (when node
            (setq node-id (org-roam-node-id node)))))

      (list :node-id node-id
            :file file-path
            :title title
            :created t))))

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

(defun activities-ext--open-org-roam-doc (org-roam-plist)
  "Open org-roam document from ORG-ROAM-PLIST metadata.
 ORG-ROAM-PLIST should have :file key.
Warns if file doesn't exist but doesn't error."
  (when org-roam-plist
    (let ((file (plist-get org-roam-plist :file)))
      (if (and file (file-exists-p file))
          (find-file file)
        (when file
          (warn "Activity document not found: %s" file))))))

(provide 'activities-extensions-org-roam)
;;; org-roam.el ends here
