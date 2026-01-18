;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Vertico - Vertical Completion UI
;; ===============================================================================

(use-package vertico
  :straight (vertico :host github :repo "minad/vertico") ; later versions require emacs 29
  :init
  (vertico-mode)
  (setq vertico-cycle t))

;; Save minibuffer history
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; Track recently opened files
(recentf-mode 1)

;; Configure flexible completion style
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        comletion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
