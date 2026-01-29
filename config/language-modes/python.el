;; -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(add-hook 'python-mode-hook 'tree-sitter-hl-mode)
