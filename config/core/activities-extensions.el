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

(defcustom activities-ext-base-directory "~/emacs-activities"
  "Base directory for all activity-related files and directories.
Each activity gets a subdirectory: BASE/ACTIVITY-SLUG-YYYY-MM-DD/
Within each activity directory:
  - worktrees/PROJECT-NAME/  (git worktrees)
  - session/                  (gptel session)
Date stamp ensures uniqueness for activities created on different days."
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

(defvar activities-ext--default-git-action 'worktree
  "Default git action for new activities: worktree, branch, or none.")

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
   ;; Add gptel session link if provided
   (if gptel-session
       (format "GPTEL Session: [[file:%s][%s]]\n\n"
               (plist-get gptel-session :session-file)
               (plist-get gptel-session :session-id))
     "")
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

(defun activities-ext--init-scope (activity-name)
  "Initialize scope for transient menu with ACTIVITY-NAME."
  (list :projects nil
        :git-action activities-ext--default-git-action
        :activity-name activity-name
        :sanitized-name (activities-ext--slugify activity-name)))

(defun activities-ext--scope-projects ()
  "Get selected projects from current transient scope."
  (plist-get (transient-scope) :projects))

(defun activities-ext--scope-git-action ()
  "Get git action from current transient scope."
  (plist-get (transient-scope) :git-action))

(defun activities-ext--scope-toggle-project (project-path)
  "Toggle PROJECT-PATH selection in scope."
  (let* ((scope (transient-scope))
         (projects (plist-get scope :projects)))
    (if (assoc project-path projects)
        (plist-put scope :projects (assoc-delete-all project-path projects))
      (plist-put scope :projects (cons (cons project-path t) projects)))))

(defun activities-ext--scope-set-git-action (action)
  "Set git ACTION in scope."
  (let ((scope (transient-scope)))
    (plist-put scope :git-action action)))

(defclass activities-ext--switch-project (transient-infix)
  ((project-path :initarg :project-path)
   (project-name :initarg :project-name))
  "Infix for toggling individual project selection.")

(cl-defmethod transient-infix-read ((obj activities-ext--switch-project))
  "Toggle project selection."
  (let* ((path (oref obj project-path))
         (projects (activities-ext--scope-projects))
         (currently-selected (assoc path projects)))
    (activities-ext--scope-toggle-project path)
    (not currently-selected)))

(cl-defmethod transient-format-value ((obj activities-ext--switch-project))
  "Format project toggle value display."
  (let* ((path (oref obj project-path))
         (projects (activities-ext--scope-projects))
         (selected (assoc path projects)))
    (if selected
        (propertize "(✓)" 'face 'transient-value)
      (propertize "( )" 'face 'transient-inactive-value))))

(cl-defmethod transient-init-value ((obj activities-ext--switch-project))
  "Initialize value from scope."
  (let* ((path (oref obj project-path))
         (projects (activities-ext--scope-projects)))
    (oset obj value (not (null (assoc path projects))))))

(defclass activities-ext--switch-git-action (transient-infix)
  ((action :initarg :action))
  "Radio button for git action selection.")

(cl-defmethod transient-infix-read ((obj activities-ext--switch-git-action))
  "Select this git action."
  (let ((action (oref obj action)))
    (activities-ext--scope-set-git-action action)
    action))

(cl-defmethod transient-format-value ((obj activities-ext--switch-git-action))
  "Format git action radio button."
  (let ((action (oref obj action))
        (current (activities-ext--scope-git-action)))
    (if (eq action current)
        (propertize "(✓)" 'face 'transient-value)
      (propertize "( )" 'face 'transient-inactive-value))))

(cl-defmethod transient-init-value ((obj activities-ext--switch-git-action))
  "Initialize value from scope."
  (let ((action (oref obj action))
        (current (activities-ext--scope-git-action)))
    (oset obj value (eq action current))))

(defun activities-ext--make-project-infix (key project-path project-name)
  "Create a project toggle infix for KEY, PROJECT-PATH, and PROJECT-NAME."
  (let ((command-name (intern (format "activities-ext--toggle-project-%s" key))))
    (defalias command-name
      (lambda ()
        (interactive)
        (let* ((projects (activities-ext--scope-projects))
               (currently-selected (assoc project-path projects)))
          (activities-ext--scope-toggle-project project-path)
          (not currently-selected))))

    (put command-name 'transient-format-value
         (lambda ()
           (let* ((projects (activities-ext--scope-projects))
                  (selected (assoc project-path projects)))
             (if selected
                 (propertize "(✓)" 'face 'transient-value)
               (propertize "( )" 'face 'transient-inactive-value)))))

    command-name))

(transient-define-infix activities-ext--infix-worktree ()
  "Select worktree git action."
  :class 'activities-ext--switch-git-action
  :action 'worktree
  :argument ""
  :key "w"
  :description "Worktree (default)"
  :transient t)

(transient-define-infix activities-ext--infix-branch ()
  "Select branch git action."
  :class 'activities-ext--switch-git-action
  :action 'branch
  :argument ""
  :key "b"
  :description "Branch"
  :transient t)

(transient-define-infix activities-ext--infix-none ()
  "Select none git action."
  :class 'activities-ext--switch-git-action
  :action 'none
  :argument ""
  :key "n"
  :description "None"
  :transient t)

(defun activities-ext--generate-project-suffixes ()
  "Generate suffix list for all known projects with smart key assignment.
Uses first letter of project name when available, otherwise assigns next free key.
Supports up to 36 projects (a-z + 0-9)."
  (when (fboundp 'projectile-relevant-known-projects)
    (let ((projects (projectile-relevant-known-projects)))
      (when projects
        (cl-loop for project in projects
                 ;; Pool of available keys (a-z, 0-9), excluding reserved transient keys
                 with unused-keys = (delete ?q  ; q is typically quit
                                           (nconc (number-sequence ?a ?z)
                                                  (number-sequence ?0 ?9)))
                 for name = (file-name-nondirectory (directory-file-name project))
                 ;; Try to find first character of name in unused keys, else take first available
                 for key-char = (seq-find (lambda (k) (member k unused-keys))
                                         name
                                         (seq-first unused-keys))
                 ;; Remove assigned key from pool to prevent collisions
                 do (setq unused-keys (delete key-char unused-keys))
                 ;; Convert character to key description string
                 for key-str = (key-description (list key-char))
                 for command = (activities-ext--make-project-infix key-str project name)
                 collect (list key-str name command :transient t))))))

(defun activities-ext--generate-git-action-suffixes ()
  "Generate suffix list for git action selection."
  (list
   (list "w" "Worktree (default)" 'activities-ext--infix-worktree)
   (list "b" "Branch" 'activities-ext--infix-branch)
   (list "n" "None" 'activities-ext--infix-none)))

(defun activities-ext--format-status ()
  "Format status line showing current selection."
  (let* ((projects (activities-ext--scope-projects))
         (count (length projects))
         (git-action (activities-ext--scope-git-action))
         (action-str (pcase git-action
                      ('worktree "worktrees")
                      ('branch "branches")
                      ('none "no git operations"))))
    (format "Selected: %d project%s, will create %s"
            count
            (if (= count 1) "" "s")
            action-str)))

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
  "Create extended activity with visual project and git action selection."
  :refresh-suffixes t
  [:description
   (lambda () (format "Activities Extensions: Create \"%s\""
                     (plist-get (transient-scope) :activity-name)))

   ;; Project Selection Section
   [:class transient-column
    :description "Project Selection"
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'activities-ext-create-transient
       (activities-ext--generate-project-suffixes)))]

   ;; Git Configuration Section
   [:class transient-column
    :description "Git Configuration"
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'activities-ext-create-transient
       (activities-ext--generate-git-action-suffixes)))]

   ;; Creation Options
   ["Creation Options"
    ("-G" "Skip gptel session" "--no-gptel")
    ("-D" "Skip org-roam document" "--no-org-roam")]

   ;; Status Display
   [""
    (:info (lambda () (activities-ext--format-status)))]

   ;; Actions
   ["Actions"
    ("RET" "Create Activity" activities-ext--create-action-v2)
    ("q" "Cancel" transient-quit-one)]]

  (interactive)
  (unless activities-ext--current-activity-name
    (user-error "No activity name set. Call activities-ext-create first."))
  (transient-setup
   'activities-ext-create-transient nil nil
   :scope (activities-ext--init-scope activities-ext--current-activity-name)))

(defun activities-ext--create-action-v2 (&optional args)
  "Create activity using state from transient scope.
ARGS are the transient arguments (skip flags only)."
  (interactive (list (transient-args 'activities-ext-create-transient)))

  (let* ((scope (transient-scope))
         (activity-name (plist-get scope :activity-name))
         (sanitized-name (plist-get scope :sanitized-name))
         (selected-projects (plist-get scope :projects))
         (git-action (plist-get scope :git-action))
         (create-org-roam (not (member "--no-org-roam" args)))
         (create-gptel (not (member "--no-gptel" args)))
         (project-metadata nil)
         (org-roam-data nil)
         (gptel-session-data nil))

    ;; Validate we have an activity name
    (unless activity-name
      (user-error "No activity name provided"))

    ;; Process selected projects with uniform git action
    (dolist (project-entry selected-projects)
      (let* ((project-path (car project-entry))
             (project-name (file-name-nondirectory
                           (directory-file-name project-path)))
             (git-result (activities-ext--apply-git-action
                         project-path sanitized-name activity-name git-action))
             (proj-plist (list :path project-path
                             :name project-name
                             :branch (plist-get git-result :branch)
                             :worktree (plist-get git-result :worktree)
                             :recent-files nil)))
        (push proj-plist project-metadata)))

    (setq project-metadata (nreverse project-metadata))

    ;; Get activity directory
    (let ((activity-dir (activities-ext--activity-directory activity-name)))

      ;; Create gptel session if requested
      (when (and create-gptel (fboundp 'jf/gptel-session-create-persistent))
        (let ((jf/gptel-sessions-directory
               (activities-ext--session-directory activity-name)))
          (setq gptel-session-data
                (jf/gptel-session-create-persistent activity-name))))

      ;; Create org-roam document if requested
      (when create-org-roam
        (setq org-roam-data
              (activities-ext--create-org-roam-doc
               activity-name project-metadata gptel-session-data activity-dir)))

      ;; Create the activity
      (activities-new activity-name)

      ;; Store metadata
      (when-let ((activity (map-elt activities-activities activity-name)))
        (let ((metadata (activities-ext--create-metadata
                        project-metadata
                        org-roam-data
                        (list :auto-open-recent activities-ext-auto-open-recent))))
          (plist-put metadata :activity-directory activity-dir)
          (when gptel-session-data
            (plist-put metadata :gptel-session gptel-session-data))
          (activities-ext--set-metadata activity metadata)))

      ;; Set up window layout
      (delete-other-windows)
      (let ((left-window (selected-window))
            (right-window nil))

        (when org-roam-data
          (activities-ext--open-org-roam-doc org-roam-data))

        (when (and gptel-session-data
                   (fboundp 'jf/gptel-session--create-buffer))
          (setq right-window (split-window-right))
          (select-window right-window)
          (let ((buffer (jf/gptel-session--create-buffer gptel-session-data)))
            (switch-to-buffer buffer))
          (select-window left-window)))

      ;; Save current state as default
      (when-let ((activity (map-elt activities-activities activity-name)))
        (activities-save activity :defaultp t :lastp t))

      (message "Created extended activity: %s" activity-name))))

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

;; Use global keybindings since activities uses :bind in use-package
(global-set-key (kbd "C-x C-a C-e") 'activities-ext-create)
(global-set-key (kbd "C-x C-a p") 'activities-ext-show-projects)
(global-set-key (kbd "C-x C-a d") 'activities-ext-open-document)
(global-set-key (kbd "C-x C-a v") 'activities-ext-validate)
(global-set-key (kbd "C-x C-a +") 'activities-ext-add-project)

(provide 'activities-extensions)
;;; activities-extensions.el ends here
