;; -*- lexical-binding: t; -*-

;; Startup profiling - uncomment to debug startup time
;; (defvar jf/init-start-time (current-time))

;; Start with debugging enabled during development
(setq debug-on-error t)

;; Register shortcut to quickly open this file (updated dynamically below)
;; (set-register ?i (cons 'file "~/emacs/config/init.org"))

;; Define root directory (dynamically resolves for worktree support)
(defvar jf/emacs-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of the Emacs configuration.
Resolves to the directory containing init.el, supporting git worktrees.")

;; Update register to point to this init file dynamically
(set-register ?i (cons 'file (expand-file-name "init.org" jf/emacs-dir)))

;; Debug mode for troubleshooting
(defvar jf/module-debug nil
  "When non-nil, print extra debug information during module loading.")

;; Module loading function with error handling
(defun jf/load-module (module-path)
  "Load MODULE-PATH with error handling and reporting."
  (when jf/module-debug
    (message "Loading module: %s" module-path))
  
  (let ((start-time (current-time)))
    (condition-case-unless-debug err
        (progn
          (load module-path nil nil t)
          (when jf/module-debug
            (message "Loaded %s in %.3f seconds" 
                     module-path 
                     (float-time (time-subtract (current-time) start-time)))))
      (error
       (message "ERROR in %s: %s" module-path (error-message-string err))
       nil))))

;; Function to resolve a module path to a file path
(defun jf/resolve-module-path (module-path)
  "Convert a MODULE-PATH like 'core/defaults' or 'transient' to a file path.
Handles both 'dir/name' format and 'name' format."
  (if (string-match-p "/" module-path)
      ;; Has subdirectory: "core/defaults" -> "config/core/defaults.el"
      (let* ((parts (split-string module-path "/"))
             (dir (car parts))
             (name (cadr parts)))
        (expand-file-name (concat "config/" dir "/" name ".el") jf/emacs-dir))
    ;; No subdirectory: "transient" -> "config/transient.el"
    (expand-file-name (concat "config/" module-path ".el") jf/emacs-dir)))

;; Function to reload a specific module (useful for debugging)
(defun jf/reload-module (module-path)
  "Reload a specific MODULE-PATH for debugging."
  (interactive 
   (list (completing-read "Reload module: " 
                          (mapcar #'car jf/enabled-modules))))
  
  (let ((jf/module-debug t))
    (jf/load-module (jf/resolve-module-path module-path))))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable package.el
(setq package-enable-at-startup nil)

;; Install and configure use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Ensure we're using a consistent org version
;; This is important as org-roam depends on specific org versions
;; Force straight to use org
(straight-use-package 'org)

;; Define enabled modules with descriptions
(defvar jf/enabled-modules
  '(
    ;; Core modules - add these as you create them
    ("core/defaults"      "Basic Emacs behavior")
    ("core/auth"      "GPG and authinfo setup")
    ("core/avy"      "avy navigation package")
    ("core/browser-hist"     "Search browser history")
    ("core/ripgrep"      "")
    ("core/ultra-scroll"      "")
    ("core/expand-region"      "")
    ("core/evil"          "Evil mode configuration")
    ("core/completion"    "Modern completion framework")
    ("core/window-management"    "Save and restore window configs")
    ("core/yasnippet"     "Snippet system for templates")

    ;; Feature modules
    ("look-and-feel/look-and-feel" "UI appearance and behavior")

    ;; Transient - must load before magit and docker (overrides built-in version)
    ("transient" "Transient menu system (newer than built-in)")

    ;; Activities extensions - requires new transient, so loads after it
    ("activities/activities" "Extended activity management with projects and docs")

    ;; Language mode modules (docker.el requires transient, so load transient first)
    ("language-modes/language-modes" "Programming language modes")

    ;; Major mode modules
    ("major-modes/magit"  "Git interface")
    ("major-modes/org"    "Org-mode configuration")
    ("major-modes/org-roam" "Org-roam knowledge management")
    ("major-modes/dirvish" "Enhanced directory viewer")

    ;; GPTEL - LLM/AI integration (moved from major-modes)
    ("gptel/gptel"        "GPTEL LLM/AI integration")
    )
  "List of enabled modules with their paths and descriptions.")

;; Define machine-specific configurations
;; Machine Role system: Uses ~/.machine-role file for stable machine role identification
;; Available roles:
;;   - apploi-mac: Work MacBook Air
;;   - personal-mac: Personal MacBook Pro
;;   - personal-mac-air: Personal MacBook Air
;; Setup: echo "apploi-mac" > ~/.machine-role

(defun jf/get-machine-role ()
  "Get stable machine role from ~/.machine-role file.
Returns the machine role string, or nil if the file doesn't exist.
Shows a warning message with setup instructions if the file is missing."
  (let ((machine-role-file (expand-file-name "~/.machine-role")))
    (if (file-exists-p machine-role-file)
        (with-temp-buffer
          (insert-file-contents machine-role-file)
          (string-trim (buffer-string)))
      (progn
        (warn "Machine role file not found: %s

To set up your machine role, create the file with one of:
  - apploi-mac (Work MacBook Air)
  - personal-mac (Personal MacBook Pro)
  - personal-mac-air (Personal MacBook Air)

Example: echo \"apploi-mac\" > ~/.machine-role

This ensures stable machine role identification even when hostname changes."
              machine-role-file)
        nil))))

(defvar jf/machine-role (jf/get-machine-role)
  "The machine's stable role identifier from ~/.machine-role, used to load machine-specific configurations.")

;; Load all enabled modules
(dolist (module-spec jf/enabled-modules)
  (let ((module-path (car module-spec)))
    (jf/load-module (jf/resolve-module-path module-path))))

;; Load secrets file if it exists (outside repository)
(let ((secrets-file (expand-file-name "~/.emacs-secrets.el")))
  (when (file-exists-p secrets-file)
    (load secrets-file nil 'nomessage)))

;; Load machine-specific configuration if it exists
(let ((machine-config (expand-file-name (concat "config/local/" jf/machine-role ".el") jf/emacs-dir)))
  (when (file-exists-p machine-config)
    (jf/load-module machine-config)))

;; Reset garbage collection threshold after startup
(setq gc-cons-threshold 2000000) ;; 2MB

;; Report startup time if debugging
(when (boundp 'jf/init-start-time)
  (let ((elapsed (float-time (time-subtract (current-time) jf/init-start-time))))
    (message "Loading Emacs took %.3f seconds" elapsed)))

;; Don't show this init message after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil)
            (message "Emacs ready!")))

;; Store customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
