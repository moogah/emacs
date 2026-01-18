;; -*- lexical-binding: t; -*-

(use-package yaml-mode
  :straight t)

(use-package yaml
  :straight t)

(use-package yaml-pro
  :straight t
  :hook (yaml-mode . yaml-pro-mode))
