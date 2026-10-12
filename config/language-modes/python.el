;; -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(add-hook 'python-mode-hook 'tree-sitter-hl-mode)

(with-eval-after-load 'info
  (add-to-list 'Info-additional-directory-list
               (expand-file-name "~/.local/share/info/python3.14")))
