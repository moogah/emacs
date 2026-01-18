;; -*- lexical-binding: t; -*-

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)

;; this setting will break capture templates 
;;(setq org-blank-before-new-entry nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.75))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
