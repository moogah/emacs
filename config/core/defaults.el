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

;; Make backup files even when under version control
(setq vc-make-backup-files t)

;; Set auto-save location
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; ===============================================================================
;; State Persistence Settings
;; ===============================================================================

;; Store bookmarks in git-controlled location
;; Default location - can be overridden per-machine in config/local/{machine-role}.el
(setq bookmark-default-file
      (expand-file-name "state/bookmarks/bookmarks" jf/emacs-dir))

;; Save bookmarks whenever bookmark list is modified (auto-save)
(setq bookmark-save-flag 1)

;; ===============================================================================
;; Path Configuration
;; ===============================================================================

;; Base directory for org files
;; Can be overridden in machine-specific config files
(defvar jf/org-directory (expand-file-name "~/org")
  "Base directory for org-mode files and org-roam notes.")

;; ===============================================================================
;; OSX Specific Configs
;; ===============================================================================

;; Make sure we have access to the same PATH as in our zsh
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))
