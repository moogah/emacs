;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Embark - Context-Aware Actions
;; ===============================================================================

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)) ;; @TODO this is overwritten by evil mode
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nill
                 (window-parameters (mode-line-format . none)))))

;; Integrate Embark with Consult
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
