;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Evil mode - Vim emulation for Emacs
;; ===============================================================================

(use-package evil
  :straight t
  :init
  ;; Must be set before evil loads, required by evil-collection
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state) ;; use insert by default for org capture
  (add-hook 'git-commit-mode-hook 'evil-insert-state)) ;; use insert mode by default for magit commits


;; Set up proper dired mode integration with dirvish, but only after dirvish is loaded
;; this get h and l navigation to work properly where h goes up one directory and j opens a file or directory.
(with-eval-after-load 'dirvish
  (evil-set-initial-state 'dired-mode 'emacs))

;; Add visual indicators for common vim commands
(use-package evil-goggles
  :straight t
  :after evil
  :config
  (evil-goggles-mode))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-exchange
  :straight t
  :after evil
  :config
  (evil-exchange-install))

;; ===============================================================================
;; Evil Collection provides vim-like bindings for many Emacs modes
;; ===============================================================================

(use-package evil-collection
  :straight t
  :after evil)

(defun my-split-or-switch-window-left ()
  "Create a new window on the left and open dired, if a window already exists move there"
  (interactive)
  (if (= 1 (count-windows))
      (progn
        (split-window-horizontally)
        (dired-jump nil))
    (progn
      (evil-window-left 1))))

(defun my-split-or-switch-window-right ()
  "Create a new window on the right and open dired, if a window already exists move there"
  (interactive)
  (if (= 1 (count-windows))
      (progn
        (split-window-horizontally)
        (other-window 1)
        (dired-jump nil))
    (progn
      (evil-window-right 1))))

(defun my-find-implementation-or-test-other-window ()
  "Finds the corresponding test or implementation window and opens it in a new or existing horizontal split"
  (interactive)
  (let ((file (projectile-find-implementation-or-test (buffer-file-name))))
    (if file
        (progn (my-split-or-switch-window-right) (find-file file)))))

(defun my/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

;; Basic global commands
(evil-define-key 'normal 'global (kbd "<SPC> x") 'my/kill-current-buffer)
(evil-define-key 'normal 'global (kbd "<SPC> d") 'dired-jump)

;; Define prefix key for window commands
(define-prefix-command 'my-window-command-map)
(evil-define-key 'normal 'global (kbd "<SPC> w") 'my-window-command-map)
(define-key my-window-command-map (kbd "c") 'delete-window)
(define-key my-window-command-map (kbd "v") 'split-window-vertically)
(define-key my-window-command-map (kbd "j") 'evil-window-down)
(define-key my-window-command-map (kbd "k") 'evil-window-up)
(define-key my-window-command-map (kbd "h") 'my-split-or-switch-window-left)
(define-key my-window-command-map (kbd "l") 'my-split-or-switch-window-right)

;; Define prefix key for space prefix commands
(define-prefix-command 'my-space-command-map)
(evil-define-key 'normal 'global (kbd "<SPC> <SPC>") 'my-space-command-map)
(define-key my-space-command-map (kbd "j") 'previous-buffer)
(define-key my-space-command-map (kbd "k") 'next-buffer)

;; Avy Bindings
(with-eval-after-load 'avy
  (evil-define-key 'normal 'global (kbd "<SPC> j") 'avy-goto-line))

;; Org mode bindings
(with-eval-after-load 'org
  ;; Global org bindings
  (evil-define-key 'normal 'global (kbd "<SPC> e") 'org-babel-execute-src-block)
  (evil-define-key 'normal 'global (kbd "<SPC> s l") 'org-store-link)
  (evil-define-key 'normal 'global (kbd "<SPC> i l") 'jf/org-insert-link-dwim)

  ;; Org mode map specific bindings
  (evil-define-key 'normal org-mode-map (kbd "<SPC> h") 'org-insert-heading)
  (evil-define-key 'normal org-mode-map (kbd "<SPC> H") 'org-insert-subheading))

;; evil-org-mode provides comprehensive org and org-agenda bindings
(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  ;; Enable standard evil-org keybindings for org-mode
  (evil-org-set-key-theme))

;; Enable evil keybindings for org-agenda (must be after org-agenda loads)
(with-eval-after-load 'org-agenda
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; Explicitly ensure j/k work everywhere in agenda, including on headers
  ;; (evil-define-key 'motion org-agenda-mode-map
  ;;   "j" 'org-agenda-next-line
  ;;   "k" 'org-agenda-previous-line
  ;;   (kbd "C-j") 'org-agenda-goto-date  ; Keep date jump accessible with C-j
  ;;   "gj" 'org-agenda-next-item
  ;;   "gk" 'org-agenda-previous-item
  ;;   "gg" 'evil-goto-first-line
  ;;   "G" 'evil-goto-line)

  ;; Fix org-super-agenda headers that have local keymaps overriding evil bindings
  (defun my/fix-evil-in-agenda-headers ()
    "Remove conflicting keybindings from org-super-agenda header keymaps."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((keymap (get-text-property (point) 'keymap)))
          ;; Remove j and k bindings from local keymap so evil bindings work
          (define-key keymap "j" nil)
          (define-key keymap "k" nil))
        (goto-char (or (next-single-property-change (point) 'keymap) (point-max))))))

  ;; Run after agenda finalization (when org-super-agenda has added headers)
  (add-hook 'org-agenda-finalize-hook #'my/fix-evil-in-agenda-headers))

;; Org-roam bindings
(with-eval-after-load 'org-roam
  (evil-define-key 'normal 'global (kbd "<SPC> n") 'org-roam-node-find)
  (evil-define-key 'normal 'global (kbd "<SPC> u") 'org-roam-dailies-goto-today))

;; Projectile bindings
(with-eval-after-load 'projectile
  ;; Global projectile bindings
  (evil-define-key 'normal 'global (kbd "<SPC> r") 'consult-projectile-ripgrep)
  (evil-define-key 'normal 'global (kbd "<SPC> f") 'consult-projectile-find-file)

  ;; Define prefix key for projectile commands
  (define-prefix-command 'my-projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<SPC> p") 'my-projectile-command-map)
  (define-key my-projectile-command-map (kbd "p") 'consult-projectile-switch-project)

  ;; Python mode specific bindings
  (evil-define-key 'normal python-mode-map (kbd "<SPC> T") 'my-find-implementation-or-test-other-window))

;; Magit bindings
(with-eval-after-load 'magit
  ;; Define prefix key for magit commands
  (define-prefix-command 'my-magit-command-map)
  (evil-define-key 'normal 'global (kbd "<SPC> g") 'magit)
  (evil-collection-init 'magit))

;; Consult bindings
(with-eval-after-load 'consult
  (evil-define-key 'normal 'global (kbd "<SPC> b") 'consult-bookmark)
  (evil-define-key 'normal 'global (kbd "<SPC> o") 'consult-buffer)

  ;; Define prefix key for menu commands
  (define-prefix-command 'my-menu-command-map)
  (evil-define-key 'normal 'global (kbd "<SPC> m") 'my-menu-command-map)
  (define-key my-menu-command-map (kbd "m") 'consult-imenu-multi)
  (define-key my-menu-command-map (kbd "i") 'consult-imenu))

;; Perspective bindings
(with-eval-after-load 'perspective
  ;; Add to projectile prefix map, creating it if needed
  (unless (fboundp 'my-projectile-command-map)
    (define-prefix-command 'my-projectile-command-map)
    (evil-define-key 'normal 'global (kbd "<SPC> p") 'my-projectile-command-map))

  (define-key my-projectile-command-map (kbd "s") 'persp-switch)
  (define-key my-projectile-command-map (kbd "S") 'persp-state-save)
  (define-key my-projectile-command-map (kbd "L") 'persp-state-load))

;; Tab and activity bindings
(with-eval-after-load 'tab-bar
  (defun my-switch-tab-or-activity ()
    "Switch to an open tab, resume an activity, or create a new activity.
If input matches an existing tab name, switch to it.
If input matches an existing activity name, resume it.
If input doesn't match either, create a new activity with that name."
    (interactive)
    (let* ((all-tabs (funcall tab-bar-tabs-function))
           ;; Get raw tab names
           (tabs (mapcar (lambda (tab) (alist-get 'name tab)) all-tabs))
           ;; Get activity names
           (activities (activities-names))
           ;; Get current tab and activity
           (current-tab-name (alist-get 'name (tab-bar--current-tab)))
           (current-activity (when (activities-current)
                               (activities-activity-name (activities-current))))

           ;; Create list of items to display
           ;; - Deduplicate activities that are also tabs
           ;; - Strip prefix from activity tabs
           (display-items 
            (delete-dups
             (append
              activities  ;; All activities
              ;; Tabs that aren't prefixed with the activity prefix
              (cl-remove-if 
               (lambda (tab-name)
                 (cl-some (lambda (activity-name)
                            (string= tab-name (concat activities-name-prefix activity-name)))
                          activities))
               tabs))))

           ;; Get user selection - using simple list with no annotations
           (selected (completing-read
                      "Switch to tab/activity: "
                      display-items
                      nil nil nil nil
                      (cond
                       ;; Default to current activity if there is one
                       (current-activity)
                       ;; If current tab is a prefixed activity tab, strip the prefix
                       ((and current-tab-name 
                             (string-prefix-p activities-name-prefix current-tab-name))
                        (substring current-tab-name (length activities-name-prefix)))
                       ;; Otherwise use current tab name
                       (t current-tab-name)))))
      (cond
       ;; If an activity with that name exists, check if it has a tab first
       ((member selected activities)
        (let ((activity-tab-name (concat activities-name-prefix selected)))
          (if (member activity-tab-name tabs)
              ;; If activity already has a tab, switch to that tab
              (tab-bar-switch-to-tab activity-tab-name)
            ;; Otherwise resume the activity
            (activities-resume (activities-named selected)))))

       ;; If it's just a plain tab (not an activity tab), switch to it
       ((member selected tabs)
        (tab-bar-switch-to-tab selected))

       ;; Otherwise create a new activity
       (t
        (activities-new selected)))))

  (evil-define-key 'normal 'global (kbd "<SPC> t") 'my-switch-tab-or-activity))
