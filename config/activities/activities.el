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

;; Set custom persist location for activities-activities
;; This moves the data from runtime/persist/ to state/activities/ (git-controlled)
;; Default location - can be overridden per-machine in config/local/{machine-role}.el
;; Note: persist-location must be a directory; symbol name is appended automatically
(put 'activities-activities 'persist-location
     (expand-file-name "state/activities/default/" jf/emacs-dir))

;; Load core (no dependencies)
(jf/load-module (expand-file-name "config/activities/core.el" jf/emacs-dir))

;; Load integrations (depend on core)
(jf/load-module (expand-file-name "config/activities/projectile.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/activities/org-roam.el" jf/emacs-dir))

;; Load UI (depends on core and integrations)
(jf/load-module (expand-file-name "config/activities/transient.el" jf/emacs-dir))

;; Use global keybindings since activities uses :bind in use-package
(global-set-key (kbd "C-x C-a C-e") 'activities-ext-create)
(global-set-key (kbd "C-x C-a p") 'activities-ext-show-projects)
(global-set-key (kbd "C-x C-a d") 'activities-ext-open-document)
(global-set-key (kbd "C-x C-a v") 'activities-ext-validate)
(global-set-key (kbd "C-x C-a +") 'activities-ext-add-project)

(provide 'activities-extensions)
;;; activities.el ends here
