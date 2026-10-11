;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Frame Size and Position
;; ===============================================================================

;; Get screen dimensions
(defvar jf/display-width (display-pixel-width))
(defvar jf/display-height (display-pixel-height))

;; Calculate desired frame size (full height, half width)
(defvar jf/frame-width (/ jf/display-width 2))
(defvar jf/frame-height jf/display-height)

;; Set default frame size and position
;; Use text-pixels to specify exact pixel dimensions, avoiding font-dependent calculations
(setq default-frame-alist
      `((width . (text-pixels . ,jf/frame-width))
        (height . (text-pixels . ,jf/frame-height))
        (left . 0)
        (top . 0)))

;; Apply to the initial frame too
(setq initial-frame-alist default-frame-alist)

;; NOTE: frame-resize-pixelwise disabled due to bug#38497
;; Setting this to t can cause theme faces not to render correctly
;; when combined with custom fonts and certain themes
;; (setq frame-resize-pixelwise t)

;; ===============================================================================
;; Clean UI Settings
;; ===============================================================================

;; Remove toolbar, menubar and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; No startup screen
(setq inhibit-startup-screen t)

;; No startup message in echo area
(setq inhibit-startup-echo-area-message t)

;; No startup message in scratch buffer
(setq initial-scratch-message nil)

;; Show column number in modeline
(column-number-mode t)

;; ===============================================================================
;; Editor Settings
;; ===============================================================================

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Default tab width
(setq-default tab-width 4)

;; Typed text replaces selection
(delete-selection-mode t)

;; Show matching parentheses
(show-paren-mode t)

;; Autosave on focus loss
(defun save-all-buffers ()
  "Save all modified buffers without prompting."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all-buffers)

;; Always use y-or-n-p, not yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; ESC cancels prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ===============================================================================
;; Backup Settings
;; ===============================================================================

;; Store all backup files in a single directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Don't make backup files for version-controlled repos.
;; Backup renames (file.ts → file.ts~) trigger lsp-mode's rename-file advice,
;; which sends workspace/willRenameFiles to the language server, causing
;; cascading edits across the project. VC provides its own history.
(setq vc-make-backup-files nil)

;; Set auto-save location
(let ((auto-save-dir (expand-file-name "auto-save-list/" user-emacs-directory)))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; ===============================================================================
;; State Persistence Settings
;; ===============================================================================

;; Store bookmarks under state/bookmarks/ in this checkout.  Not in git:
;; .gitignore ignores state/** except READMEs (see state/bookmarks/README.md).
;; Default location - can be overridden per-machine in config/local/{machine-role}.el
(setq bookmark-default-file
      (expand-file-name "state/bookmarks/bookmarks" jf/emacs-dir))

;; Save bookmarks whenever bookmark list is modified (auto-save)
(setq bookmark-save-flag 1)

;; ===============================================================================
;; Path Configuration
;; ===============================================================================

;; Base directory for org files.  The data profile below (EMACS_AGENT_DATA)
;; moves it before any other module loads.  config/local/<role>.el loads
;; after every module, so a value set there does not reach the note paths
;; modules fix at load time (org-roam-directory, the capture file, the
;; org-graph roots, ...).
(defvar jf/org-directory (expand-file-name "~/org")
  "Base directory for org-mode files and org-roam notes.")

;; ===============================================================================
;; Data Profile (EMACS_AGENT_DATA)
;; ===============================================================================

(defvar workspace-state-directory-override)
(defvar org-roam-db-location)
(defvar org-id-locations-file)
(defvar org-graph-watch-workspace-homes)
(defvar org-graph-sync-enabled)

(defvar jf/data-profile nil
  "The data profile this Emacs started with, as `jf/data-profile-parse' reads it.
nil when EMACS_AGENT_DATA is unset or empty.")

(defvar jf/data-state-directory nil
  "Per-process directory for the note databases and workspace state.
Set by `jf/data-profile-apply' for the scratch and none profiles; nil
otherwise, and they stay under `user-emacs-directory'.")

(defun jf/data-profile-parse (value)
  "Read VALUE, the value of EMACS_AGENT_DATA, as a data profile.
\"live\" gives (:kind live), \"none\" gives (:kind none) and
\"scratch:DIR\" with DIR absolute gives (:kind scratch :dir DIR/).
nil or the empty string gives nil.  Any other value, or a scratch DIR
that is not absolute, gives (:kind none) and a warning naming VALUE."
  (let ((scratch (and (stringp value) (string-prefix-p "scratch:" value)
                      (substring value (length "scratch:")))))
    (cond
     ((or (null value) (equal value "")) nil)
     ((equal value "live") (list :kind 'live))
     ((equal value "none") (list :kind 'none))
     ((and scratch (file-name-absolute-p scratch))
      (list :kind 'scratch
            :dir (file-name-as-directory (expand-file-name scratch))))
     (t
      (display-warning
       'data-profile
       (format "EMACS_AGENT_DATA value `%s' is not live, none or scratch:<absolute dir>; using none"
               value)
       :warning)
      (list :kind 'none)))))

(defun jf/data-profile-apply (profile)
  "Turn PROFILE, as `jf/data-profile-parse' returns it, into settings.
Record PROFILE in `jf/data-profile'.  For live or nil change nothing
else.  For scratch and none make a fresh `jf/data-state-directory' under
`temporary-file-directory', put the org-roam and org-id databases and
the workspace state in it, and stop org-graph indexing workspace homes.
Scratch points `jf/org-directory' at its directory, creating it; none
points it at an empty directory in the state directory and turns
org-graph sync off."
  (setq jf/data-profile profile)
  (let ((kind (plist-get profile :kind)))
    (when (memq kind '(scratch none))
      (let ((state (file-name-as-directory
                    (make-temp-file "emacs-agent-data-" t))))
        (setq jf/data-state-directory state
              workspace-state-directory-override (expand-file-name "workspaces/" state)
              org-roam-db-location (expand-file-name "org-roam.db" state)
              org-id-locations-file (expand-file-name ".org-id-locations" state)
              org-graph-watch-workspace-homes nil)
        (if (eq kind 'scratch)
            (let ((dir (plist-get profile :dir)))
              (make-directory dir t)
              (setq jf/org-directory dir))
          (let ((empty (file-name-as-directory (expand-file-name "org" state))))
            (make-directory empty)
            (setq jf/org-directory empty
                  org-graph-sync-enabled nil)))))))

(jf/data-profile-apply (jf/data-profile-parse (getenv "EMACS_AGENT_DATA")))

;; ===============================================================================
;; OSX Specific Configs
;; ===============================================================================

;; Make sure we have access to the same PATH as in our zsh
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))
