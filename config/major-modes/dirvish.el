;; -*- lexical-binding: t; -*-

;; Core Dirvish setup with specific version
(use-package dirvish
  :straight (dirvish :host github :repo "alexluigit/dirvish" :tag "2.2.3")
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-mode-line-format
   '(:left (path) :right (sort file-time " " file-size symlink omit yank index)))
  (dirvish-attributes '(vscode-icon file-size collapse subtree-state vc-state))
  :config
  ;; Platform-specific settings
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  
  ;; Listing options
  (setq dired-dwim-target t)  ;; enable side-by-side copy/move
  (setq delete-by-moving-to-trash t)
  
  ;; Reserved for Emacs 29
  ;;(setq dired-mouse-drag-files t)
  ;;(setq mouse-drag-and-drop-region-cross-program t)
  
  ;; Configure ls display options
  (setq dired-listed-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  
  ;; Prevent buffer proliferation when navigating
  (setf dired-kill-when-opening-new-dired-buffer t))

;; Define quick access directories
(setq dirvish-quick-access-entries
 `(("h" "~/"              "Home")
   ("s" "~/src/"          "Source Code")
   ("d" "~/src/dotfiles"  "Dotfiles")
   ("e" ,user-emacs-directory "Emacs Config")
   ("o" "~/org/"          "Org")
   ("l" "~/src/homelab/"  "Homelab")))

;; Use VS Code icons for a nicer visual experience
(use-package vscode-icon
  :straight t
  :config
  (setq dirvish-vscode-icon-size 18))

;; Enable editing of dired buffers with 'C-x C-q'
(put 'dired-find-alternate-file 'disabled nil)

;; Automatically update dired buffers when state-on-disk changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Custom function to copy file paths relative to git repo root
(defun dirvish-copy-file-path-relative (&optional multi-line)
  "Copy filepath of marked files relative to project root.
If MULTI-LINE, make every path occupy a new line.
Uses dirvish's built-in project detection via project.el."
  (interactive "P")
  (if-let ((project-root (dirvish--vc-root-dir)))
      (let* ((files (mapcar #'file-local-name (dired-get-marked-files)))
             (relative-files (mapcar (lambda (file)
                                       (file-relative-name file project-root))
                                     files))
             (names (mapconcat #'concat relative-files (if multi-line "\n" " ")))
             (result (if multi-line (concat "\n" names) names)))
        (kill-new result)
        (message "Copied: %s" result))
    (message "Not in a project")))

;; Global and mode-specific keybindings
(with-eval-after-load 'dirvish
  ;; Global keys
  (global-set-key (kbd "C-x d") 'dired-jump)
  (global-set-key (kbd "C-c f") 'dirvish-fd)
  
  ;; Dirvish mode keys - Vim-style navigation
  (define-key dirvish-mode-map (kbd "h") 'dired-up-directory)
  (define-key dirvish-mode-map (kbd "j") 'dired-next-line)
  (define-key dirvish-mode-map (kbd "k") 'dired-previous-line)
  (define-key dirvish-mode-map (kbd "l") 'dired-find-file)
  
  ;; Editing and view controls
  (define-key dirvish-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
  (define-key dirvish-mode-map (kbd ".") 'dired-omit-mode)
  
  ;; Navigation and utility
  (define-key dirvish-mode-map (kbd "b") 'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f") 'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") 'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "Y") 'dirvish-copy-file-path-relative)
  (define-key dirvish-mode-map (kbd "N") 'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "/") 'dired-isearch-filenames)
  (define-key dirvish-mode-map (kbd "^") 'dirvish-history-last)
  (define-key dirvish-mode-map (kbd "H") 'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "s") 'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "TAB") 'dirvish-subtree-toggle)
  
  ;; Advanced operations with Meta key
  (define-key dirvish-mode-map (kbd "M-n") 'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-p") 'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") 'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m") 'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-f") 'dirvish-toggle-fullscreen)
  (define-key dirvish-mode-map (kbd "M-s") 'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") 'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") 'dirvish-fd-jump))
