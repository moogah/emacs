;; -*- lexical-binding: t; -*-

(defun activities-ext--init-scope (activity-name)
  "Initialize scope for transient menu with ACTIVITY-NAME."
  (list :projects nil
        :git-action activities-ext--default-git-action
        :selected-project nil
        :activity-name activity-name
        :sanitized-name (activities-ext--slugify activity-name)
        :gptel-preset 'programming-assistant))

(defun activities-ext--scope-projects ()
  "Get selected projects from current transient scope.
Returns alist of (project-path . git-action)."
  (plist-get (transient-scope) :projects))

(defun activities-ext--scope-git-action ()
  "Get default git action from current transient scope.
This is used as default for newly selected projects."
  (plist-get (transient-scope) :git-action))

(defun activities-ext--scope-project-selected-p (project-path)
  "Check if PROJECT-PATH is selected in scope."
  (not (null (assoc project-path (activities-ext--scope-projects)))))

(defun activities-ext--scope-get-project-action (project-path)
  "Get git action for PROJECT-PATH from scope.
Returns nil if project not selected."
  (cdr (assoc project-path (activities-ext--scope-projects))))

(defun activities-ext--scope-get-selected-project ()
  "Get currently selected project from scope for detail view.
Returns project path or nil."
  (plist-get (transient-scope) :selected-project))

(defun activities-ext--scope-set-selected-project (project-path)
  "Set PROJECT-PATH as currently selected project for detail view."
  (let ((scope (transient-scope)))
    (plist-put scope :selected-project project-path)))

(defun activities-ext--scope-toggle-project (project-path)
  "Toggle PROJECT-PATH selection in scope.
Adds with default git action if not selected, removes if already selected."
  (let* ((scope (transient-scope))
         (projects (plist-get scope :projects))
         (entry (assoc project-path projects)))
    (if entry
        ;; Already selected - remove from list
        (plist-put scope :projects (assoc-delete-all project-path projects))
      ;; Not selected - add with default git action from scope
      (let ((default-action (plist-get scope :git-action)))
        (plist-put scope :projects (cons (cons project-path default-action) projects))))))

(defun activities-ext--scope-cycle-selected-project-action (project-path)
  "Cycle git action for already-selected PROJECT-PATH: worktree → branch → none → worktree.
Project must already be in selected projects list."
  (let* ((scope (transient-scope))
         (projects (plist-get scope :projects))
         (entry (assoc project-path projects)))
    (when entry
      (let ((current-action (cdr entry)))
        (pcase current-action
          ('worktree (setcdr entry 'branch))
          ('branch (setcdr entry 'none))
          ('none (setcdr entry 'worktree)))))))

(defun activities-ext--scope-set-git-action (action)
  "Set default git ACTION in scope.
This will be used for newly selected projects."
  (let ((scope (transient-scope)))
    (plist-put scope :git-action action)))

(defun activities-ext--scope-set-project-action (project-path action)
  "Set git ACTION for specific PROJECT-PATH in scope.
Project must already be selected."
  (let* ((scope (transient-scope))
         (projects (plist-get scope :projects))
         (entry (assoc project-path projects)))
    (when entry
      (setcdr entry action))))

(defun activities-ext--scope-gptel-preset ()
  "Get selected gptel preset from current transient scope."
  (plist-get (transient-scope) :gptel-preset))

(defun activities-ext--scope-set-gptel-preset (preset-name)
  "Set PRESET-NAME as selected gptel preset in scope."
  (let ((scope (transient-scope)))
    (plist-put scope :gptel-preset preset-name)))

(defclass activities-ext--switch-project (transient-infix)
  ((project-path :initarg :project-path)
   (project-name :initarg :project-name))
  "Infix for toggling individual project selection.")

(cl-defmethod transient-infix-read ((obj activities-ext--switch-project))
  "Toggle project selection on/off."
  (let* ((path (oref obj project-path)))
    (activities-ext--scope-toggle-project path)
    ;; Return current selection state for value storage
    (not (null (assoc path (activities-ext--scope-projects))))))

(cl-defmethod transient-format-value ((obj activities-ext--switch-project))
  "Format project toggle value display with action indicator."
  (let* ((path (oref obj project-path))
         (projects (activities-ext--scope-projects))
         (selected (assoc path projects)))
    (if selected
        (let* ((action (cdr selected))
               (indicator (pcase action
                           ('worktree "W")
                           ('branch "B")
                           ('none "·"))))
          (propertize (format "(✓ %s)" indicator) 'face 'transient-value))
      (propertize "( )" 'face 'transient-inactive-value))))

(cl-defmethod transient-init-value ((obj activities-ext--switch-project))
  "Initialize value from scope."
  (let* ((path (oref obj project-path))
         (projects (activities-ext--scope-projects)))
    (oset obj value (not (null (assoc path projects))))))

;; Git action radio class removed - no longer needed

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

;; Git action infixes removed - now handled by Selected Projects column cycling

(defun activities-ext--generate-project-suffixes ()
  "Generate suffix list for all known projects with smart key assignment.
Uses first letter of project name when available, otherwise assigns next free key.
Supports up to 26 projects (a-z only, numbers reserved for Selected Projects)."
  (when (fboundp 'projectile-relevant-known-projects)
    (let ((projects (projectile-relevant-known-projects)))
      (when projects
        (cl-loop for project in projects
                 ;; Pool of available keys (a-z only)
                 ;; Reserved: q (quit), number keys 0-9 (used by Selected Projects column)
                 with unused-keys = (cl-set-difference
                                    (number-sequence ?a ?z)
                                    '(?q))
                 for name = (file-name-nondirectory (directory-file-name project))
                 ;; Try to find first character of name in unused keys, else take first available
                 for key-char = (seq-find (lambda (k) (member k unused-keys))
                                         name
                                         (seq-first unused-keys))
                 ;; Remove assigned key from pool to prevent collisions
                 do (setq unused-keys (delete key-char unused-keys))
                 ;; Skip projects that don't get a key (>26 projects)
                 when key-char
                 collect (let ((key-str (key-description (list key-char)))
                              (command (activities-ext--make-project-infix
                                       (key-description (list key-char)) project name)))
                          (list key-str name command :transient t)))))))

(defun activities-ext--generate-selected-project-suffixes ()
  "Generate suffix list for selected projects with cycling keys.
Each selected project gets a numbered key to cycle through git actions."
  (let ((projects (activities-ext--scope-projects)))
    (when projects
      (cl-loop for (project . action) in projects
               for idx from 1
               for key = (if (<= idx 9)
                            (number-to-string idx)
                          (if (= idx 10) "0" nil))
               while key  ; Stop after 10 projects (1-9, 0)
               for name = (file-name-nondirectory (directory-file-name project))
               for indicator = (pcase action
                                ('worktree "W")
                                ('branch "B")
                                ('none "·"))
               for display-name = (format "%s [%s]" name indicator)
               collect (list key
                           display-name
                           `(lambda ()
                              (interactive)
                              (activities-ext--scope-cycle-selected-project-action ,project))
                           :transient t)))))

(defun activities-ext--get-project-keys ()
  "Get list of keys that would be used by project selection.
Returns list of character codes."
  (when (fboundp 'projectile-relevant-known-projects)
    (let ((projects (projectile-relevant-known-projects)))
      (when projects
        (cl-loop for project in projects
                 with unused-keys = (cl-set-difference
                                    (number-sequence ?a ?z)
                                    '(?q))
                 for name = (file-name-nondirectory (directory-file-name project))
                 for key-char = (seq-find (lambda (k) (member k unused-keys))
                                         name
                                         (seq-first unused-keys))
                 do (setq unused-keys (delete key-char unused-keys))
                 when key-char
                 collect key-char)))))

(defun activities-ext--generate-gptel-preset-suffixes ()
  "Generate suffix list for available gptel presets.
Each preset gets a letter key, excluding keys used by projects.
Reads preset names from `gptel--known-presets' (symbol-keyed alist)."
  (when (bound-and-true-p gptel--known-presets)
    (let ((preset-names (mapcar #'car gptel--known-presets))
          (current-preset (activities-ext--scope-gptel-preset))
          (project-keys (activities-ext--get-project-keys)))
      (when preset-names
        (cl-loop for preset in preset-names
                 ;; Start with all keys except q (quit) and project keys
                 with unused-keys = (cl-set-difference
                                    (number-sequence ?a ?z)
                                    (cons ?q project-keys))
                 for preset-str = (symbol-name preset)
                 ;; Try to use first letter of preset name, else first available
                 for key-char = (seq-find (lambda (k) (member k unused-keys))
                                         preset-str
                                         (seq-first unused-keys))
                 do (setq unused-keys (delete key-char unused-keys))
                 when key-char
                 for key-str = (key-description (list key-char))
                 for is-selected = (eq preset current-preset)
                 for display = (if is-selected
                                   (propertize (format "%s (selected)" preset-str)
                                             'face 'transient-value)
                                 preset-str)
                 collect (list key-str
                             display
                             `(lambda ()
                                (interactive)
                                (activities-ext--scope-set-gptel-preset ',preset))
                             :transient t))))))

(defun activities-ext--generate-project-git-action-suffixes ()
  "Generate git action suffixes for currently selected project.
Returns nil if no project selected for detail view."
  (when-let ((proj-path (activities-ext--scope-get-selected-project)))
    (let ((current-action (activities-ext--scope-get-project-action proj-path)))
      (list
       (list "w"
             (if (eq current-action 'worktree)
                 "Worktree (active)"
               "Worktree")
             `(lambda ()
                (interactive)
                (activities-ext--scope-set-project-action ,proj-path 'worktree))
             :transient t)
       (list "b"
             (if (eq current-action 'branch)
                 "Branch (active)"
               "Branch")
             `(lambda ()
                (interactive)
                (activities-ext--scope-set-project-action ,proj-path 'branch))
             :transient t)
       (list "n"
             (if (eq current-action 'none)
                 "None (active)"
               "None")
             `(lambda ()
                (interactive)
                (activities-ext--scope-set-project-action ,proj-path 'none))
             :transient t)))))

(defun activities-ext--select-all-projects ()
  "Select all known projects with current default git action."
  (interactive)
  (when (fboundp 'projectile-relevant-known-projects)
    (let* ((scope (transient-scope))
           (projects (plist-get scope :projects))
           (default-action (plist-get scope :git-action))
           (all-projects (projectile-relevant-known-projects)))
      (dolist (proj all-projects)
        (unless (assoc proj projects)
          (push (cons proj default-action) projects)))
      (plist-put scope :projects projects))))

(defun activities-ext--clear-selection ()
  "Clear all selected projects."
  (interactive)
  (let ((scope (transient-scope)))
    (plist-put scope :projects nil)
    (plist-put scope :selected-project nil)))

(defun activities-ext--reset-all-actions ()
  "Reset all selected projects to current default action."
  (interactive)
  (let* ((scope (transient-scope))
         (projects (plist-get scope :projects))
         (default-action (plist-get scope :git-action)))
    (plist-put scope :projects
               (mapcar (lambda (entry)
                        (cons (car entry) default-action))
                      projects))))

(defun activities-ext--format-status ()
  "Format status line with usage instructions."
  (let* ((projects (activities-ext--scope-projects))
         (count (length projects)))
    (concat
     (if (zerop count)
         (propertize "Press project key (left) to select with worktree"
                    'face 'transient-inactive-value)
       (propertize (format "%d project%s selected"
                          count
                          (if (= count 1) "" "s"))
                  'face 'transient-value))
     (when (> count 0)
       (propertize " • Press number key (right) to cycle: W → B → ·"
                  'face 'transient-inactive-value)))))

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
  "Create extended activity with two-column selection and cycling.
Left: toggle selection. Right: cycle git actions (W → B → ·)."
  :refresh-suffixes t
  [:description
   (lambda () (format "Activities Extensions: Create \"%s\""
                     (plist-get (transient-scope) :activity-name)))

   ;; Project Selection Section
   [:class transient-column
    :description "Project Selection"
    :pad-keys t
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'activities-ext-create-transient
       (activities-ext--generate-project-suffixes)))]

   ;; Selected Projects Section (with numbered keys for cycling)
   [:class transient-column
    :description (lambda ()
                   (let ((count (length (activities-ext--scope-projects))))
                     (format "Selected Projects (%d)" count)))
    :if (lambda () (activities-ext--scope-projects))
    :pad-keys t
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'activities-ext-create-transient
       (activities-ext--generate-selected-project-suffixes)))]

   ;; Gptel Preset Selection
   [:class transient-column
    :description (lambda ()
                   (let ((preset (activities-ext--scope-gptel-preset)))
                     (format "Gptel Preset: %s"
                            (propertize (symbol-name preset) 'face 'transient-value))))
    :if (lambda () (bound-and-true-p gptel--known-presets))
    :pad-keys t
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'activities-ext-create-transient
       (activities-ext--generate-gptel-preset-suffixes)))]

   ;; Creation Options
   ["Creation Options"
    ("-G" "Skip gptel session" "--no-gptel")
    ("-D" "Skip org-roam document" "--no-org-roam")]

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
         (gptel-preset (plist-get scope :gptel-preset))
         (create-org-roam (not (member "--no-org-roam" args)))
         (create-gptel (not (member "--no-gptel" args)))
         (project-metadata nil)
         (org-roam-data nil)
         (gptel-session-data nil))

    ;; Validate we have an activity name
    (unless activity-name
      (user-error "No activity name provided"))

    ;; Process selected projects with their individual git actions
    (dolist (project-entry selected-projects)
      (let* ((project-path (car project-entry))
             (git-action (cdr project-entry))  ; Read action from project entry
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

      ;; Create org-roam document FIRST (if requested) so file exists for parsing
      (when create-org-roam
        (setq org-roam-data
              (activities-ext--create-org-roam-doc
               activity-name project-metadata nil activity-dir)))

      ;; Create gptel session SECOND (if requested) so it can parse saved org file
      (when (and create-gptel (fboundp 'jf/gptel-session-create-persistent))
        (let ((jf/gptel-sessions-directory
               (activities-ext--session-directory activity-name))
              (org-file (when org-roam-data (plist-get org-roam-data :file))))
          (setq gptel-session-data
                (jf/gptel-session-create-persistent activity-name nil nil gptel-preset org-file))))

      ;; Update org-roam doc with gptel session info if both were created
      (when (and org-roam-data gptel-session-data)
        (activities-ext--add-gptel-session-to-org-doc org-roam-data gptel-session-data))

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

(provide 'activities-extensions-transient)
;;; transient.el ends here
