;;; bash-parser.el --- Parse bash commands with tree-sitter -*- lexical-binding: t; -*-

(require 'treesit)
(require 'cl-lib)

;; Add current directory to load-path for sub-modules
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Core modules (no dependencies on other parser modules)
(require 'bash-parser-glob)
(require 'bash-parser-semantics)
(require 'bash-parser-variables)

;; Security module (depends on glob)
(require 'bash-parser-security)

;; Core parsing (depends on security - for dangerous pattern detection)
(require 'bash-parser-core)

;; File operations (depends on core, semantics, variables)
(require 'bash-parser-file-ops)

;; Recursive analysis (depends on file-ops)
(require 'bash-parser-recursive)

;; Extensions (depends on file-ops)
(require 'bash-parser-extensions)

(provide 'bash-parser)
;;; bash-parser.el ends here
