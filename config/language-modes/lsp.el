(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((js2-mode . lsp)
         (typescript-mode . lsp)
         (rjsx-mode . lsp))
  :config
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
