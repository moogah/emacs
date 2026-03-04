;;; bash-parser-recursive.el --- Recursive semantic analysis -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, recursion, semantics
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Recursive tree traversal for semantic analysis.
;; Extracts file operations from all nesting levels including:
;; - Command substitutions
;; - Pipeline commands
;; - Chain commands (with variable tracking)
;; - Future: Loop bodies, conditional branches

;;; Code:

(require 'cl-lib)

(defvar jf/bash-recursive-max-depth 10
  "Maximum recursion depth for semantic analysis.
Prevents infinite recursion in pathological cases.")

(defun jf/bash-analyze-file-operations-recursive (parsed-command var-context &optional depth)
  "Recursively extract file operations from PARSED-COMMAND at all nesting levels.

PARSED-COMMAND is the output from `jf/bash-parse'.
VAR-CONTEXT is an alist mapping variable names (symbols) to values (strings).
DEPTH prevents infinite recursion (default 0, max 10).

Returns flat list of all operations including nested operations from:
- Command substitutions (:command-substitutions with :parsed)
- Loop bodies (:loop-body-parsed if implemented)
- Conditional branches (:then-branch-parsed, :else-branch-parsed if implemented)
- Pipeline commands (:all-commands)
- Chain commands (:all-commands with variable tracking)

Operations from command substitutions are marked with :from-substitution t.

This is the core recursive engine that powers the semantic analysis system.
It coordinates with jf/bash--extract-from-single-command to extract operations
at each level, then recursively descends into nested structures.

Example:
  ;; cat $(find . -name '*.log')
  (let* ((parsed (jf/bash-parse \"cat $(find . -name '*.log')\"))
         (ops (jf/bash-analyze-file-operations-recursive parsed nil 0)))
    ops)
  => (;; Operations from find (marked :from-substitution t)
      (:file \".\" :operation :read-directory :from-substitution t ...)
      (:file \"*.log\" :operation :match-pattern :from-substitution t ...)
      ;; Operation from cat
      (:file \"$(find . -name '*.log')\" :operation :read ...))"
  (let ((depth (or depth 0))
        (operations nil))

    ;; Depth check - prevent infinite recursion
    (when (>= depth jf/bash-recursive-max-depth)
      (error "Max recursion depth exceeded in semantic analysis"))

    ;; 1. Extract operations from this command level
    ;; This requires bash-parser-file-ops to be loaded
    (when (fboundp 'jf/bash--extract-from-single-command)
      (let ((this-level-ops (jf/bash--extract-from-single-command
                             parsed-command var-context)))
        (setq operations (append operations this-level-ops))))

    ;; 2. Recursively process command substitutions
    (when-let ((substs (plist-get parsed-command :command-substitutions)))
      (dolist (subst substs)
        (when-let ((parsed-subst (plist-get subst :parsed)))
          (when (plist-get parsed-subst :success)  ; Only if parse succeeded
            (let ((subst-ops (jf/bash-analyze-file-operations-recursive
                             parsed-subst var-context (1+ depth))))
              ;; Mark all nested operations as from-substitution
              ;; Use copy-tree for deep copy of plist structure
              (let ((marked-ops nil))
                (dolist (op subst-ops)
                  (let ((marked-op (copy-tree op)))
                    (plist-put marked-op :from-substitution t)
                    (push marked-op marked-ops)))
                (setq operations (append operations (nreverse marked-ops)))))))))

    ;; 3. Recursively process pipeline commands
    (when (eq (plist-get parsed-command :type) :pipeline)
      (dolist (cmd (plist-get parsed-command :all-commands))
        (let ((cmd-ops (jf/bash-analyze-file-operations-recursive
                       cmd var-context (1+ depth))))
          (setq operations (append operations cmd-ops)))))

    ;; 4. Recursively process chain commands (with variable tracking)
    (when (eq (plist-get parsed-command :type) :chain)
      (let ((chain-context var-context))
        (dolist (cmd (plist-get parsed-command :all-commands))
          ;; Extract variable assignments from this command (if function exists)
          (when (fboundp 'jf/bash--extract-assignments-from-command)
            (let ((assignments (jf/bash--extract-assignments-from-command cmd)))
              (when assignments
                (setq chain-context (append assignments chain-context)))))
          ;; Extract operations with updated context
          (let ((cmd-ops (jf/bash-analyze-file-operations-recursive
                         cmd chain-context (1+ depth))))
            (setq operations (append operations cmd-ops))))))

    ;; Future: Add loop body and conditional branch traversal here
    ;; (when-let ((loop-body (plist-get parsed-command :loop-body-parsed)))
    ;;   ...)
    ;; (when-let ((then-branch (plist-get parsed-command :then-branch-parsed)))
    ;;   ...)

    operations))

(provide 'bash-parser-recursive)
;;; bash-parser-recursive.el ends here
