;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Mini-Frame for Minibuffer Display
;; ===============================================================================
(use-package mini-frame
  :straight t
  :config
  (setq mini-frame-mode t)
  (setq resize-mini-frames t))

(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 80))))
