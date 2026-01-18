(use-package elfeed
  :straight t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed))

(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))
