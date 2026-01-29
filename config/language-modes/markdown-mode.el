;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc -f markdown -t html"
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-additional-languages '("sh" "bash" "python" "elisp" "javascript")))

(use-package grip-mode
  :straight t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode))
  :config
  (setq grip-preview-use-webkit t))

(use-package markdown-toc
  :straight t
  :bind (:map markdown-mode-command-map
         ("t" . markdown-toc-generate-or-refresh-toc)))

(use-package edit-indirect
  :straight t
  :bind (:map markdown-mode-map
         ("C-c '" . edit-indirect-region)))
