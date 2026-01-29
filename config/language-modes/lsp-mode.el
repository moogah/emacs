;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :straight t
  :init
  ;; Set prefix for lsp-command-keymap (default: C-c l)
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  ;; Documentation popup
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)

  ;; Sideline diagnostics and code actions
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)

  ;; Breadcrumb navigation in header line
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))

  ;; Peek definitions and references
  (setq lsp-ui-peek-enable t))

(use-package lsp-mode
  :config
  ;; Use flycheck instead of flymake for diagnostics
  (setq lsp-prefer-flymake nil))

;; Increase garbage collection threshold during LSP operations
(setq gc-cons-threshold 100000000)  ; 100MB

;; Increase data read from processes (LSP servers send lots of data)
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Delay before LSP actions trigger (prevents constant rechecks while typing)
(setq lsp-idle-delay 0.500)  ; 500ms

;; Disable I/O logging (significant performance impact when enabled)
(setq lsp-log-io nil)

;; Use completion-at-point-functions (CAPF) for completion
;; This integrates with Company via company-capf backend
(setq lsp-completion-provider :capf)

;; Disable file watchers for better performance in large projects
;; Re-enable per-project if needed with .dir-locals.el
(setq lsp-enable-file-watchers nil)

;; Disable features that may impact performance
(setq lsp-enable-folding nil)              ; Folding regions
(setq lsp-enable-on-type-formatting nil)   ; Format-on-type
(setq lsp-enable-snippet nil)              ; Snippet support

(use-package lsp-mode
  :config
  ;; Disable LSP formatting - delegate to Prettier instead
  ;; LSP formatting uses TypeScript's built-in formatter which is less configurable
  (setq lsp-typescript-format-enable nil)
  (setq lsp-javascript-format-enable nil)

  ;; Enable function signature help during completion
  ;; Shows parameter types and documentation while typing function calls
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-javascript-suggest-complete-function-calls t))

;; Enable LSP for TypeScript and JavaScript modes
;; Use lsp-deferred instead of lsp for better startup performance
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'js2-mode-hook #'lsp-deferred)

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :config
  ;; Use basedpyright instead of standard pyright
  (setq lsp-pyright-langserver-command "basedpyright")

  ;; Type checking mode: "off", "basic", "standard", "strict"
  ;; "basic" - Good balance of strictness vs. usability
  ;; "strict" - Maximum type safety (may require many annotations)
  (setq lsp-pyright-typechecking-mode "basic")

  ;; Enable auto-import completions
  (setq lsp-pyright-auto-import-completions t)

  ;; Auto-search for virtual environments and library paths
  (setq lsp-pyright-auto-search-paths t)

  ;; Use project's Python interpreter from virtual environment
  (setq lsp-pyright-venv-path nil))  ; Auto-detect, or set explicitly

(use-package lsp-mode
  :config
  ;; Register ruff as an LSP server
  ;; Ruff v0.4.5+ includes built-in LSP support via 'ruff server' command
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff" "server"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'ruff-lsp
    :priority -1))  ; Lower priority than basedpyright

  ;; Enable ruff for Python files
  ;; Both basedpyright and ruff will run simultaneously
  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-deferred))))

;; DAP Mode for debugging capabilities
;; Uncomment to enable debugging support through Debug Adapter Protocol
;; (use-package dap-mode
;;   :straight t
;;   :after lsp-mode
;;   :config
;;   (dap-auto-configure-mode))

;; LSP Treemacs integration for project symbols view
;; Uncomment to enable treemacs integration for browsing symbols
;; (use-package lsp-treemacs
;;   :straight t
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (lsp-treemacs-sync-mode 1))
