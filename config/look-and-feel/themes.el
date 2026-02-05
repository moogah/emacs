;; -*- lexical-binding: t; -*-

(use-package monokai-theme
  :straight t)
(use-package material-theme
  :straight t)
(use-package vs-light-theme
  :straight t)
(use-package color-theme-sanityinc-tomorrow
  :straight t)
(use-package cyberpunk-theme
  :straight t)
(use-package afternoon-theme
  :straight t)
(use-package darkburn-theme
  :straight t)
(use-package distinguished-theme
  :straight t)
(use-package doom-themes
  :straight t
  :init
  ;; Enable bold and italic (set before theme loads)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))
(use-package timu-macos-theme
  :straight t)

;; set default theme
;; doom-henna
;; doom-peacock
;; doom-vibrant
;; doom-ir
;; doom-old-hope
;; doom-ephemeral
;; doom-laserwave
;; doom-moonlight
;; doom-palenight

;; Load theme after Emacs fully starts to ensure all UI elements are initialized
;; This fixes issues with doom themes not applying colors correctly on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (load-theme 'doom-palenight t)
            ;; Apply doom-themes configuration after theme loads
            (doom-themes-org-config)))

;; (load-theme 'distinguished)
;; (enable-theme 'distinguished)
;;(set-background-color "black")

;; Apply custom face overrides after theme loads
(defun jf/apply-theme-customizations ()
  "Apply custom face overrides to doom-palenight theme."
  (custom-set-faces
   ;; Base faces
   '(default ((t (:background "black" :foreground "#F1F1F1"))))
   '(font-lock-comment-face ((t (:foreground "dark gray"))))
   '(font-lock-string-face ((t (:foreground "wheat1"))))
   '(highlight ((t (:foreground "burlywood1" :background "inherit"))))

   ;; Magit diff faces
   '(magit-diff-hunk-heading ((t (:background "black" :foreground "dark gray"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "black" :foreground "wheat1"))))
   '(magit-diff-context-highlight ((t (:background "black"))))
   '(magit-diff-removed-highlight ((t (:background "black" :foreground "red"))))
   '(magit-diff-removed ((t (:background "#292D3E" :foreground "#cc4259"))))
   '(magit-diff-added-highlight ((t (:background "black" :foreground "green"))))
   '(magit-diff-added ((t (:background "#292D3E" :foreground "#9cb970"))))

   ;; Org-mode faces
   '(org-table ((t (:background "#111111" :foreground "wheat1"))))
   '(org-block ((t (:background "#111111"))))
   '(org-block-begin-line ((t (:background "#111111"))))
   '(org-block-end-line ((t (:background "#111111"))))
   '(org-default ((t (:foreground "wheat1"))))
   '(org-verbatim ((t (:foreground "light sky blue"))))
   '(org-list-dt ((t (:foreground "SlateGray1"))))
   '(org-link ((t (:foreground "DodgerBlue1"))))
   '(org-level-1 ((t (:inherit outline-1 :height 2.0 :foreground "#F1F1FF"))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.5 :foreground "#F1F1FF"))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.5 :foreground "#F1F1FF"))))
   '(org-level-4 ((t (:inherit outline-1 :height 1.0 :foreground "#F1F1FF"))))
   '(org-level-5 ((t (:inherit outline-1 :height 1.0 :foreground "#F1F1FF"))))

   ;; Markdown-mode faces
   '(markdown-header-delimiter-face ((t (:foreground "SlateGray4"))))
   '(markdown-header-face-1 ((t (:foreground "SlateGrey1"))))
   '(markdown-header-face-2 ((t (:foreground "SlateGrey1"))))
   '(markdown-header-face-3 ((t (:foreground "SlateGrey1"))))
   '(markdown-header-face-4 ((t (:foreground "SlateGrey1"))))
   '(markdown-header-face-5 ((t (:foreground "SlateGrey1"))))
   '(markdown-list-face ((t (:foreground "SlateGray4"))))
   '(markdown-code-face ((t (:background "#111111"))))
   '(markdown-language-keyword-face ((t (:foreground "DodgerBlue3"))))
   '(markdown-inline-code-face ((t (:foreground "light sky blue"))))

   ;; Uncomment if using external tree-sitter package (not needed for built-in treesit)
   ;; '(tree-sitter-hl-face:method.call ((t (:foreground "light blue"))))
   ;; '(tree-sitter-hl-face:function.call ((t (:foreground "light blue"))))
   ;; '(tree-sitter-hl-face:type ((t (:foreground "DodgerBlue1"))))
   ))

;; Apply customizations after theme loads
(add-hook 'emacs-startup-hook #'jf/apply-theme-customizations)

;; set font size to 14pt for my aging eyes
(setq default-frame-alist '((font . "Menlo-14")))
