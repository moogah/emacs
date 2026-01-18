;; -*- lexical-binding: t; -*-

(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; use short answers ie y, n instead of yes, no
(if (boundp 'use-short-answers)
    (setq use-short-answers t))

;; automatically update buffers when their state-on-disk changes
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Do not saves duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; enable mouse horizontal scrolling
;; use toggle-truncate-lines or visual-line-mode as alternatives
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)
(set-default 'truncate-lines t) ;; don't wrap lines

;; install adaptive-wrap so that visual-line-mode will respect local indentation levels
(use-package adaptive-wrap
  :straight t
  :config
  (setq adaptive-wrap-extra-indent 2)) ;; set evil respect visual line mode
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode 1)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)
