;; -*- lexical-binding: t; -*-

(use-package vim-tab-bar
  :straight (:host github :repo "jamescherti/vim-tab-bar.el")
  :commands vim-tab-bar-mode
  :hook
  (after-init . vim-tab-bar-mode))
