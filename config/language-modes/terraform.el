;; -*- lexical-binding: t; -*-

(use-package terraform-mode
  :straight t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
