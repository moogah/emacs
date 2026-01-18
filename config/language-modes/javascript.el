(use-package js2-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode)))

(use-package rjsx-mode
  :straight t
  :mode "\\.jsx\\'"
  :init
  (add-hook 'rjsx-mode-hook 'lsp))

(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((project-root (or
                        (projectile-project-root)
                        (locate-dominating-file default-directory "node_modules")))
         (eslint (and project-root
                      (expand-file-name "node_modules/.bin/eslint"
                                        project-root))))
    (if (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)
      (setq-local flycheck-javascript-eslint-executable "eslint"))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/use-prettier-from-node-modules ()
  "Use local prettier from node_modules before global."
  (let* ((project-root (or
                        (projectile-project-root)
                        (locate-dominating-file default-directory "node_modules")))
         (prettier (and project-root
                        (expand-file-name "node_modules/.bin/prettier"
                                          project-root))))
    (if (and prettier (file-executable-p prettier))
        (setq-local prettier-js-command prettier)
      (setq-local prettier-js-command "prettier"))))
(add-hook 'prettier-js-mode-hook #'my/use-prettier-from-node-modules)

(use-package turbo-log
  :straight (:host github :repo "artawower/turbo-log.el")
  ;; :bind (("C-s-l" . turbo-log-print)
  ;;        ("C-s-i" . turbo-log-print-immediately)
  ;;        ("C-s-h" . turbo-log-comment-all-logs)
  ;;        ("C-s-s" . turbo-log-uncomment-all-logs)
  ;;        ("C-s-[" . turbo-log-paste-as-logger)
  ;;        ("C-s-]" . turbo-log-paste-as-logger-immediately)
  ;;        ("C-s-d" . turbo-log-delete-all-logs))
  :config
  (setq turbo-log-msg-format-template "\"🚀: %s\"")
  (setq turbo-log-allow-insert-without-treesit-p t))

(turbo-log-configure
 :modes (typescript-mode typescriptreact-mode js2-mode web-mode)
 :strategy merge

 :loggers ("console.log(%s)" "console.debug(%s)" "console.warn(%s)")
 :jump-list ((class_declaration (method_definition "constructor")))
 :identifier-node-types (identifier member_expression)
 :post-insert-hook (prettier-prettify)
 :msg-format-template "'🦄: %s'")

;; -*- lexical-binding: t; -*-

(use-package js2-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode)))

(use-package rjsx-mode
  :straight t
  :mode "\\.jsx\\'"
  :init
  (add-hook 'rjsx-mode-hook 'lsp))

(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(defun my/use-prettier-from-node-modules ()
  "Use local prettier from node_modules before global."
  (let* ((project-root (or
                        (projectile-project-root)
                        (locate-dominating-file default-directory "node_modules")))
         (prettier (and project-root
                        (expand-file-name "node_modules/.bin/prettier"
                                          project-root))))
    (if (and prettier (file-executable-p prettier))
        (setq-local prettier-js-command prettier)
      (setq-local prettier-js-command "prettier"))))
(add-hook 'prettier-js-mode-hook #'my/use-prettier-from-node-modules)

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package turbo-log
  :straight (:host github :repo "artawower/turbo-log.el")
  ;; :bind (("C-s-l" . turbo-log-print)
  ;;        ("C-s-i" . turbo-log-print-immediately)
  ;;        ("C-s-h" . turbo-log-comment-all-logs)
  ;;        ("C-s-s" . turbo-log-uncomment-all-logs)
  ;;        ("C-s-[" . turbo-log-paste-as-logger)
  ;;        ("C-s-]" . turbo-log-paste-as-logger-immediately)
  ;;        ("C-s-d" . turbo-log-delete-all-logs))
  :config
  (setq turbo-log-msg-format-template "\"🚀: %s\"")
  (setq turbo-log-allow-insert-without-treesit-p t))

(turbo-log-configure
 :modes (typescript-mode typescriptreact-mode js2-mode web-mode)
 :strategy merge

 :loggers ("console.log(%s)" "console.debug(%s)" "console.warn(%s)")
 :jump-list ((class_declaration (method_definition "constructor")))
 :identifier-node-types (identifier member_expression)
 :post-insert-hook (prettier-prettify)
 :msg-format-template "'🦄: %s'")
