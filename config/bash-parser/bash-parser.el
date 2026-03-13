;; Author: Jeff Farr
;; Keywords: bash, parser, loader
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Bash command parser using tree-sitter for semantic extraction.
;; Main loader module that coordinates all bash parser subsystems.

;;; Code:

;;; bash-parser.el --- Parse bash commands with tree-sitter -*- lexical-binding: t; -*-

(require 'treesit)
(require 'cl-lib)

;; Add current directory and subdirectories to load-path for sub-modules
(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path base-dir)
  (add-to-list 'load-path (expand-file-name "core" base-dir))
  (add-to-list 'load-path (expand-file-name "analysis" base-dir))
  (add-to-list 'load-path (expand-file-name "plugins" base-dir))
  (add-to-list 'load-path (expand-file-name "semantics" base-dir))
  (add-to-list 'load-path (expand-file-name "commands" base-dir))
  (add-to-list 'load-path (expand-file-name "utils" base-dir)))

;; Protocol module (forward declarations - no dependencies)
(require 'bash-parser-protocol)

;; Core modules (no dependencies on other parser modules)
(require 'bash-parser-glob)
(require 'bash-parser-semantics)
(require 'bash-parser-variables)

;; Security module (depends on glob)
(require 'bash-parser-security)

;; Core parsing (depends on security - for dangerous pattern detection)
(require 'bash-parser-core)

;; Analysis layer (orchestrator and coverage)
(require 'bash-parser-coverage)
(require 'bash-parser-orchestrator)

;; File operations (depends on core, semantics, variables)
(require 'bash-parser-file-ops)

;; Recursive analysis (depends on file-ops)
(require 'bash-parser-recursive)

;; Extensions (depends on file-ops)
(require 'bash-parser-extensions)

(provide 'bash-parser)
;;; bash-parser.el ends here
