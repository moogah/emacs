;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Marginalia - Rich Annotations for Minibuffer Completions
;; ===============================================================================

(use-package marginalia
  :straight (marginalia :host github :repo "minad/marginalia")
  :bind (
   :map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
