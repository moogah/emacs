;; -*- lexical-binding: t; -*-

;; Add sidebar via imenu
(use-package imenu-list
  :straight t
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))
