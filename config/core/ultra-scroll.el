;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure smooth scrolling
;; ===============================================================================

(use-package ultra-scroll
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))
