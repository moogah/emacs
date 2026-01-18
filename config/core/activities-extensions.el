;; -*- lexical-binding: t; -*-

(require 'activities)
(require 'transient)
(require 'cl-lib)

;; Optional dependencies - loaded later in init sequence
(require 'projectile nil t)  ; noerror - checked at runtime
(require 'org-roam nil t)    ; noerror - checked at runtime

(defgroup activities-extensions nil
  "Extensions for the activities package with project and documentation support."
  :group 'activities
  :prefix "activities-ext-")

(defcustom activities-ext-auto-open-recent t
  "Automatically open recent project files when resuming activities."
  :type 'boolean
  :group 'activities-extensions)

(defcustom activities-ext-recent-files-limit 5
  "Maximum number of recent files to track per project."
  :type 'integer
  :group 'activities-extensions)

(defcustom activities-ext-worktree-directory "~/"
  "Directory where git worktrees are created.
Worktrees are named as: PROJECT-NAME-BRANCH-NAME"
  :type 'directory
  :group 'activities-extensions)

(defvar activities-ext--current-activity-name nil
  "Temporary storage for activity name during creation flow.
Set by `activities-ext-create' and used by transient actions.")

(defcustom activities-ext-prompt-for-org-roam t
  "Prompt to create org-roam document when creating activities."
  :type 'boolean
  :group 'activities-extensions)

(defcustom activities-ext-org-roam-subdirectory ""
  "Subdirectory within org-roam-directory for activity documents.
Empty string means root of org-roam-directory."
  :type 'string
  :group 'activities-extensions)

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

(defun activities-ext--select-projects ()
  "Select multiple projects using completing-read-multiple.
Returns list of project paths (absolute directory paths)."
  (let ((projects (projectile-known-projects)))
    (when projects
      (completing-read-multiple
       "Select projects (comma-separated): "
       projects
       nil  ; predicate
       t    ; require-match
       ))))

(defun activities-ext--prompt-git-action (project-path branch-name)
  "Prompt user for git action on PROJECT-PATH using BRANCH-NAME.
Returns plist with :action, :branch, :worktree, :created keys.
:action can be 'branch, 'worktree, or nil.
BRANCH-NAME is the sanitized activity name to use for branch/worktree."
  (let* ((project-name (file-name-nondirectory
                       (directory-file-name project-path)))
         (action (intern
                  (completing-read
                   (format "Git action for %s (will use: %s): " project-name branch-name)
                   '("nothing" "branch" "worktree")
                   nil t nil nil "nothing")))
         (action (if (eq action 'nothing) nil action))
         (result (list :action action :branch nil :worktree nil :created nil)))

    (message "DEBUG: action=%S for project=%s with branch=%s" action project-name branch-name)

    (when action
      ;; Use the provided branch-name instead of prompting
      (plist-put result :branch branch-name)

      (cond
       ((eq action 'branch)
        (message "DEBUG: Creating branch %s in %s" branch-name project-path)
        (let ((created (activities-ext--create-branch project-path branch-name)))
          (plist-put result :created created)))

       ((eq action 'worktree)
        (message "DEBUG: Creating worktree for branch %s in %s" branch-name project-path)
        (let ((worktree-path (activities-ext--create-worktree project-path branch-name)))
          (when worktree-path
            (plist-put result :worktree worktree-path)
            (plist-put result :created t))))))

    (message "DEBUG: git result=%S" result)
    result))

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

(defun activities-ext--create-worktree (project-path branch-name)
  "Create git worktree for BRANCH-NAME in PROJECT-PATH.
Returns worktree path or nil on failure.
Worktree is created in `activities-ext-worktree-directory' with format:
PROJECT-NAME-BRANCH-NAME"
  (let* ((default-directory project-path)
         (project-name (file-name-nondirectory
                       (directory-file-name project-path)))
         (worktree-name (format "%s-%s" project-name branch-name))
         (worktree-path (expand-file-name worktree-name
                                         activities-ext-worktree-directory)))
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

(defun activities-ext--create-org-roam-doc (activity-name &optional projects)
  "Create org-roam document for ACTIVITY-NAME.
Optional PROJECTS is a list of project plists to include in document.
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
      (insert (format "#+filetags: :activity:%s:\n\n" (activities-ext--slugify activity-name)))
      (insert (activities-ext--default-template activity-name projects)))

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

(defun activities-ext--default-template (activity-name projects)
  "Generate default template content for ACTIVITY-NAME with PROJECTS.
PROJECTS is a list of project plists with :path, :name, :branch, :worktree keys.
Each project is stored as an org heading with properties for programmatic access."
  (concat
   "* Overview\n\n"
   (format "Activity: %s\n\n" activity-name)
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

(defun activities-ext--before-resume (activity)
  "Hook called before resuming ACTIVITY.
Opens org-roam document and recent project files based on settings."
  (when-let ((ext-data (activities-ext--get-metadata activity)))
    (let ((projects (plist-get ext-data :projects))
          (org-roam-data (plist-get ext-data :org-roam))
          (settings (plist-get ext-data :settings)))

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

(defvar activities-ext--transient-state nil
  "State for the activities extensions transient menu.
Plist with keys: :name, :projects, :create-org-roam.")

(defclass activities-ext--infix-projects (transient-infix)
  ((projects :initarg :projects :initform nil))
  "Transient infix for multi-selecting projects.")

(cl-defmethod transient-infix-read ((obj activities-ext--infix-projects))
  "Read projects using multi-select completion."
  (let ((selected (activities-ext--select-projects)))
    (oset obj projects selected)
    ;; Return the list - it will be added to transient args
    selected))

(cl-defmethod transient-format-value ((obj activities-ext--infix-projects))
  "Format projects for display in transient menu."
  (let ((projects (oref obj projects)))
    (if projects
        (propertize (format "%d project%s selected"
                           (length projects)
                           (if (> (length projects) 1) "s" ""))
                   'face 'transient-value)
      (propertize "none" 'face 'transient-inactive-value))))

(defun activities-ext-create ()
  "Create an extended activity with projects and documentation.
Prompts for activity name first, then opens transient for project selection."
  (interactive)
  (let ((name (read-string "Activity name: ")))
    (when (string-empty-p (string-trim name))
      (user-error "Activity name cannot be empty"))
    (setq activities-ext--current-activity-name name)
    (activities-ext-create-transient)))

(transient-define-prefix activities-ext-create-transient ()
  "Select projects and options for new activity.
Activity name has already been captured."
  ["Configuration"
   ("-p" "Select projects" activities-ext--select-projects-infix)]
  ["Documentation"
   ("-d" "Create org-roam document" "--org-roam")]
  ["Actions"
   ("RET" "Create Activity" activities-ext--create-action)
   ("q" "Cancel" transient-quit-one)])

(transient-define-infix activities-ext--select-projects-infix ()
  "Infix for selecting multiple projects."
  :class 'activities-ext--infix-projects
  :key "-p"
  :description "Select projects"
  :argument "--projects=")

(defun activities-ext--create-action (&optional args)
  "Create activity with transient ARGS.
Main action function called from transient menu.
Uses the activity name stored in `activities-ext--current-activity-name'."
  (interactive (list (transient-args 'activities-ext-create-transient)))

  ;; Get activity name from stored variable
  (let* ((activity-name activities-ext--current-activity-name)
         (sanitized-name (activities-ext--slugify activity-name))
         ;; Find the list element in args (that's the projects)
         (project-paths (cl-find-if #'listp args))
         (create-org-roam (member "--org-roam" args))
         (project-metadata nil)
         (org-roam-data nil))

    ;; Debug output
    (message "DEBUG: args=%S" args)
    (message "DEBUG: activity-name=%s sanitized=%s" activity-name sanitized-name)
    (message "DEBUG: project-paths=%S" project-paths)

    (unless activity-name
      (user-error "No activity name provided"))

    ;; Process projects if selected
    (when project-paths
      (dolist (project-path project-paths)
        (let* ((project-path (string-trim project-path))
               (project-name (file-name-nondirectory
                            (directory-file-name project-path)))
               ;; Pass sanitized name to git action prompt
               (git-result (activities-ext--prompt-git-action project-path sanitized-name))
               (proj-plist (list :path project-path
                                :name project-name
                                :branch (plist-get git-result :branch)
                                :worktree (plist-get git-result :worktree)
                                :recent-files nil)))
          (push proj-plist project-metadata))))

    (setq project-metadata (nreverse project-metadata))

    ;; Create org-roam document if requested
    (when create-org-roam
      (setq org-roam-data
            (activities-ext--create-org-roam-doc activity-name project-metadata)))

    ;; Create the activity using activities-new
    (activities-new activity-name)

    ;; Get the newly created activity and store metadata
    (when-let ((activity (map-elt activities-activities activity-name)))
      (let ((metadata (activities-ext--create-metadata
                      project-metadata
                      org-roam-data
                      (list :auto-open-recent activities-ext-auto-open-recent))))
        (activities-ext--set-metadata activity metadata)))

    ;; Open org-roam document if created
    (when org-roam-data
      (activities-ext--open-org-roam-doc org-roam-data))

    (message "Created extended activity: %s" activity-name)))

(defun activities-ext--current-activity ()
  "Get the currently active activity.
Returns nil if no activity is active."
  (when (bound-and-true-p activities--current)
    activities--current))

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
  "Validate current activity's projects and files.
Reports status in minibuffer and creates detailed report buffer."
  (interactive)
  (if-let* ((activity (activities-ext--current-activity))
            (ext-data (activities-ext--get-metadata activity))
            (projects (plist-get ext-data :projects)))
      (let ((results (mapcar #'activities-ext--validate-project projects))
            (valid-count 0)
            (total-count (length projects)))
        (dolist (result results)
          (when (plist-get result :valid)
            (cl-incf valid-count)))
        (if (= valid-count total-count)
            (message "All %d projects valid ✓" total-count)
          (message "Warning: %d/%d projects valid" valid-count total-count)
          (activities-ext-show-projects)))
    (message "No projects in current activity")))

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
             ;; Pass sanitized activity name for branch/worktree
             (git-result (activities-ext--prompt-git-action project-path sanitized-name))
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

;; Use global keybindings since activities uses :bind in use-package
(global-set-key (kbd "C-x C-a C-e") 'activities-ext-create)
(global-set-key (kbd "C-x C-a p") 'activities-ext-show-projects)
(global-set-key (kbd "C-x C-a d") 'activities-ext-open-document)
(global-set-key (kbd "C-x C-a v") 'activities-ext-validate)
(global-set-key (kbd "C-x C-a +") 'activities-ext-add-project)

(provide 'activities-extensions)
;;; activities-extensions.el ends here
