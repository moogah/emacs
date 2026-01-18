;; -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package blacken
  :straight t
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package python-isort
  :straight (:host github :repo "wyuenho/emacs-python-isort")
  :config
  (add-hook 'python-mode-hook 'python-isort-on-save-mode))

(add-hook 'python-mode-hook 'tree-sitter-hl-mode)

;; Set the path to pylint executable if needed
;; (setq flycheck-python-pylint-executable "pylint")
