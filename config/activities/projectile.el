;; -*- lexical-binding: t; -*-

(defun activities-ext--create-branch (project-path branch-name)
  "Create git branch BRANCH-NAME in PROJECT-PATH.
Returns t if created, nil if already exists or on error."
  (let ((default-directory project-path))
    (condition-case err
        (progn
          (if (zerop (call-process "git" nil nil nil "checkout" "-b" branch-name))
              (progn
                (message "Created branch %s in %s" branch-name project-path)
                t)
            (message "Branch creation failed in %s" project-path)
            nil))
      (error
       (message "Error creating branch: %s" (error-message-string err))
       nil))))

(defun activities-ext--create-worktree (project-path branch-name activity-name)
  "Create git worktree for BRANCH-NAME in PROJECT-PATH for ACTIVITY-NAME.
Returns worktree path or nil on failure.
Worktree is created in ~/emacs-activities/ACTIVITY-SLUG/worktrees/PROJECT-NAME/"
  (let* ((default-directory project-path)
         (project-name (file-name-nondirectory
                       (directory-file-name project-path)))
         (worktrees-base (activities-ext--worktrees-directory activity-name))
         (worktree-path (expand-file-name project-name worktrees-base)))
    (condition-case err
        (progn
          (if (zerop (call-process "git" nil nil nil "worktree" "add"
                                  worktree-path "-b" branch-name))
              (progn
                (message "Created worktree at %s" worktree-path)
                worktree-path)
            (message "Worktree creation failed for %s" project-path)
            nil))
      (error
       (message "Error creating worktree: %s" (error-message-string err))
       nil))))

(defun activities-ext--apply-git-action (project-path branch-name activity-name git-action)
  "Apply GIT-ACTION to PROJECT-PATH using BRANCH-NAME and ACTIVITY-NAME.
Returns plist with :action, :branch, :worktree, :created keys.
GIT-ACTION is one of: worktree, branch, or none."
  (let ((result (list :action git-action :branch nil :worktree nil :created nil)))
    (pcase git-action
      ('branch
       (plist-put result :branch branch-name)
       (let ((created (activities-ext--create-branch project-path branch-name)))
         (plist-put result :created created)))

      ('worktree
       (plist-put result :branch branch-name)
       (let ((worktree-path
              (activities-ext--create-worktree project-path branch-name activity-name)))
         (when worktree-path
           (plist-put result :worktree worktree-path)
           (plist-put result :created t))))

      ('none
       ;; No git operations
       nil))

    result))

(defun activities-ext--get-recent-project-files (project-path limit)
  "Get recent files from recentf for PROJECT-PATH.
Returns list of up to LIMIT file paths that belong to the project."
  (when (bound-and-true-p recentf-list)
    (let ((project-root (file-truename (expand-file-name project-path)))
          (recent-files nil)
          (count 0))
      (cl-dolist (file recentf-list)
        (when (and (< count limit)
                  (file-exists-p file)
                  (string-prefix-p project-root (file-truename file)))
          (push file recent-files)
          (cl-incf count)))
      (nreverse recent-files))))

(defun activities-ext--open-recent-files (file-list)
  "Open files in FILE-LIST, gracefully handling missing files.
Reports missing files but continues opening available ones."
  (let ((opened 0)
        (missing 0))
    (dolist (file file-list)
      (if (file-exists-p file)
          (progn
            (find-file file)
            (cl-incf opened))
        (cl-incf missing)))
    (when (> missing 0)
      (message "Opened %d files (%d missing)" opened missing))))

(provide 'activities-extensions-projectile)
;;; projectile.el ends here
