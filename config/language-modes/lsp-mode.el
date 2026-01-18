(use-package lsp-mode
  :straight t
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp)
  (typescript-mode . lsp)
  (js2-mode . lsp)
  (js-mode . lsp)
  ;; If you want lsp on org babel src blocks
  ;; (org-mode . lsp-deferred)
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  ;; Enable breadcrumbs in header line
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;; Configure lsp-ui-peek
  (setq lsp-ui-peek-enable t))

;; Ensure LSP uses flycheck
(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil))

;; Python - pyright (Microsoft's Python Language Server)
(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; TypeScript and JavaScript setup via typescript-language-server
(use-package lsp-mode
  :config
  ;; TypeScript specific settings
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-typescript-format-enable t)
  ;; JavaScript specific settings
  (setq lsp-javascript-suggest-complete-function-calls t)
  (setq lsp-javascript-format-enable t))

;; Performance adjustments for LSP
(setq gc-cons-threshold 100000000)          ;; 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)                 ;; 500ms
(setq lsp-log-io nil)                       ;; Disable logging for better performance
(setq lsp-completion-provider :capf)        ;; Use capf for completion

;; Optional performance settings
(setq lsp-enable-file-watchers nil)         ;; Disable file watchers for large projects
(setq lsp-enable-folding nil)               ;; Disable folding
(setq lsp-enable-on-type-formatting nil)    ;; Disable on-type formatting
(setq lsp-enable-snippet nil)               ;; Disable snippets (if you don't use them)

;; DAP Mode for debugging capabilities
;; (use-package dap-mode
;;   :straight t
;;   :after lsp-mode
;;   :config
;;   (dap-auto-configure-mode))

;; LSP Treemacs integration for project symbols view
;; (use-package lsp-treemacs
;;   :straight t
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (lsp-treemacs-sync-mode 1))
