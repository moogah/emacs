;; -*- lexical-binding: t; -*-

(use-package tree-sitter
  :straight t
  :hook ((python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-mode)
         (js-mode . tree-sitter-hl-mode)
         (javascript-mode . tree-sitter-mode)
         (javascript-mode . tree-sitter-hl-mode)
         ;; NOTE: typescript-mode uses built-in tree-sitter, not elisp-tree-sitter
         ;; so we don't add hooks here
         (sh-mode . tree-sitter-mode)
         (sh-mode . tree-sitter-hl-mode)
         (php-mode . tree-sitter-mode)
         (php-mode . tree-sitter-hl-mode)
         (hcl-mode . tree-sitter-mode)
         (hcl-mode . tree-sitter-hl-mode)
         (terraform-mode . tree-sitter-mode)
         (terraform-mode . tree-sitter-hl-mode)
         (sql-mode . tree-sitter-mode)
         (sql-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.21.0")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css" "v0.21.0")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp" "1.3.0")
     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.21.1")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.4" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.2.3" "tree-sitter-markdown-inline/src")
     (python "https://github.com/tree-sitter/tree-sitter-python" "v0.21.0")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package evil-textobj-tree-sitter
  :straight t)

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook ((python-mode . combobulate-mode)
         (js-mode . combobulate-mode)
         (typescript-mode . combobulate-mode)))
