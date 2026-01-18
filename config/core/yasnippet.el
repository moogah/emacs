;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :straight t
  :config
  (use-package yasnippet-snippets
    :straight t)
    (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)                     ;; personal snippets
              (expand-file-name "straight/repos/yasnippet-snippets/snippets" user-emacs-directory)))   ;; yasnippet snippets
  (yas-global-mode 1))

(use-package auto-yasnippet
  :straight t)
;; @TODO
;; bind aya-create and aya-expand

(use-package autoinsert
  :straight t
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'file-file-hook 'auto-insert)
  (auto-insert-mode 1))

(defun jf/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (interactive)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(define-auto-insert "\\.pp$" ["default-puppet.pp" jf/autoinsert-yas-expand])
(define-auto-insert "dotfiles\/emacs.*\.org" "emacs-config-org-tangle.org")
