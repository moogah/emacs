;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Corfu Auto-Completion
;; ===============================================================================
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :init)
;;  (global-corfu-mode))

;; ===============================================================================
;; Configure Company Auto-Completion
;; ===============================================================================
;; also try out https://github.com/minad/corfu
(use-package company
  :straight t
  ;;:bind(:map company-active-map
  ;;           ([return] . nil)
  ;;           ("RET" . nil))
  :config
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode))

;; use C-RET to cancel completion
