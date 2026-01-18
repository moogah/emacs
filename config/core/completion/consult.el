;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Consult - Enhanced Search and Navigation
;; ===============================================================================

(use-package consult
  :straight (consult :host github :repo "minad/consult") ; later versions require emacs 29
  :bind (
   ("C-x b" . consult-buffer)
   ("C-s" . consult-line))
  :config
  ;; Only add perspective integration if it's loaded
  (with-eval-after-load 'perspective
    (when (boundp 'persp-consult-source)
      (add-to-list 'consult-buffer-sources persp-consult-source))))

;; Project integration for Consult
(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))
