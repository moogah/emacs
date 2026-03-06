;;; bash-parser-protocol.el --- Bash parser shared protocol -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, protocol
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Shared protocol module for bash parser subsystems.
;; Contains forward declarations for cross-module interfaces.

;;; Code:

;; Glob pattern detection
(declare-function jf/bash--has-glob-pattern-p "bash-parser-file-ops"
                  (file-path))

;; Path variable resolution
(declare-function jf/bash--resolve-path-variables "bash-parser-file-ops"
                  (file-path var-context &optional debug))

;; Exec block extraction
(declare-function jf/bash-extract-from-exec-blocks "bash-parser-file-ops"
                  (parsed-command var-context))

;; Flag matching
(declare-function jf/bash--flag-present-p "bash-parser-file-ops"
                  (flag flags-list))

;; Command semantics lookup
(declare-function jf/bash-lookup-command-semantics "bash-parser-semantics"
                  (command-name))

(provide 'bash-parser-protocol)
;;; bash-parser-protocol.el ends here
