;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Winner Mode - Window configuration history
;; ===============================================================================

;; Enable winner-mode for window configuration undo/redo
(winner-mode 1)

;; ===============================================================================
;; Perspective - Workspace management for Emacs
;; ===============================================================================

  (use-package perspective
    :straight t
    :custom
    (persp-mode-prefix-key (kbd "C-c M-p"))
    (persp-modestring-short t)
    :config
    (persp-mode))

;; ===============================================================================
;; Ace Window - Quick window selection
;; ===============================================================================

(use-package ace-window
  :straight t)

;; ===============================================================================
;; Activities - Advanced workspace and workflow management
;; ===============================================================================

(use-package activities
  :straight (:host github :repo "alphapapa/activities.el" :branch "master" :files ("*.el"))
  :init
  ;; Set persist location BEFORE package loads (persist-defvar runs at load time)
  (let ((subdir (if (and (boundp 'jf/machine-role) jf/machine-role)
                    (concat "state/activities/" jf/machine-role "/")
                  "state/activities/default/")))
    (put 'activities-activities 'persist-location
         (expand-file-name subdir jf/emacs-dir)))
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :config
  (activities-mode)
  (activities-tabs-mode)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))
