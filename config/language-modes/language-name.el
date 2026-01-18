;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Language Modes Configuration with Modular Loading
;; ===============================================================================

;; Code navigation and indexing
(jf/load-module (expand-file-name "language-modes/gtags.el" jf/emacs-dir))

;; Syntax highlighting and parsing
(jf/load-module (expand-file-name "language-modes/tree-sitter.el" jf/emacs-dir))

;; Project management
(jf/load-module (expand-file-name "language-modes/projectile.el" jf/emacs-dir))


;; AI-assisted coding
(jf/load-module (expand-file-name "language-modes/copilot.el" jf/emacs-dir))

;; Data and configuration formats
(jf/load-module (expand-file-name "language-modes/yaml.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/json.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/markdown-mode.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/terraform.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/puppet-mode.el" jf/emacs-dir))

;; Programming languages
(jf/load-module (expand-file-name "language-modes/python.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/typescript.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/javascript.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/golang.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/postgres.el" jf/emacs-dir))
(jf/load-module (expand-file-name "language-modes/docker.el" jf/emacs-dir))

(use-package wgrep
  :straight t)

;; -*- lexical-binding: t; -*-

(use-package language-mode
  :straight t)
