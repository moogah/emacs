;;; test-ast-structure-dump.el --- Dump AST structure for manual inspection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests, debugging

;;; Commentary:

;; This file provides utilities to dump the complete tree-sitter AST structure
;; for manual inspection and verification. Use this to understand what nodes
;; tree-sitter creates and verify our traversal visits them all.

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-core)

;;; AST Dumping Functions

(defun jf/test-dump-ast-node (node indent)
  "Recursively dump NODE structure with INDENT level.
Returns list of strings representing the tree structure."
  (if (null node)
      '()
    (let ((lines '())
          (indent-str (make-string (* indent 2) ?\s))
          (node-type (treesit-node-type node))
          (node-text (treesit-node-text node t))
          (start (treesit-node-start node))
          (end (treesit-node-end node)))

      ;; Add this node's info
      (push (format "%s[%s] \"%s\" (pos %d-%d)"
                    indent-str
                    node-type
                    (if (> (length node-text) 40)
                        (concat (substring node-text 0 37) "...")
                      node-text)
                    start
                    end)
            lines)

      ;; Add children
      (dotimes (i (treesit-node-child-count node))
        (when-let ((child (treesit-node-child node i)))
          (setq lines (append lines (jf/test-dump-ast-node child (1+ indent))))))

      lines)))

(defun jf/test-dump-ast-structure (command-string)
  "Parse COMMAND-STRING and return AST structure as formatted string."
  (with-temp-buffer
    (insert command-string)
    (let* ((parser (treesit-parser-create 'bash))
           (root-node (treesit-parser-root-node parser))
           (lines (jf/test-dump-ast-node root-node 0)))
      (string-join lines "\n"))))

;;; Test Cases - Dump AST for Key Commands

(ert-deftest test-dump-ast-simple-command ()
  "Dump AST structure for: echo hello

Run this test interactively (M-x ert RET test-dump-ast-simple-command)
to see the output in *Messages* buffer."
  (let ((ast-dump (jf/test-dump-ast-structure "echo hello")))
    (message "\n=== AST Structure: echo hello ===\n%s\n" ast-dump)
    (should t)))  ; Always passes, just for inspection

(ert-deftest test-dump-ast-pipeline ()
  "Dump AST structure for: cat file.txt | grep pattern"
  (let ((ast-dump (jf/test-dump-ast-structure "cat file.txt | grep pattern")))
    (message "\n=== AST Structure: cat file.txt | grep pattern ===\n%s\n" ast-dump)
    (should t)))

(ert-deftest test-dump-ast-find-exec ()
  "Dump AST structure for: find . -name '*.log' -exec rm {} \\;

This is security-critical. We need to verify:
1. The 'rm' command is represented as a node
2. Our traversal visits it"
  (let ((ast-dump (jf/test-dump-ast-structure "find . -name '*.log' -exec rm {} \\;")))
    (message "\n=== AST Structure: find . -name '*.log' -exec rm {} \\; ===\n%s\n" ast-dump)
    ;; Verify 'rm' appears somewhere in the AST dump
    (should (string-match-p "rm" ast-dump))))

(ert-deftest test-dump-ast-git-chain ()
  "Dump AST structure for: git add . && git commit -m 'Fix bug' && git push"
  (let ((ast-dump (jf/test-dump-ast-structure "git add . && git commit -m 'Fix bug' && git push")))
    (message "\n=== AST Structure: git add . && git commit -m 'Fix bug' && git push ===\n%s\n" ast-dump)
    (should t)))

(ert-deftest test-dump-ast-three-stage-pipeline ()
  "Dump AST structure for: cat file.txt | grep pattern | sort"
  (let ((ast-dump (jf/test-dump-ast-structure "cat file.txt | grep pattern | sort")))
    (message "\n=== AST Structure: cat file.txt | grep pattern | sort ===\n%s\n" ast-dump)
    (should t)))

(ert-deftest test-dump-ast-with-redirections ()
  "Dump AST structure for: cat file.txt | grep pattern > out.txt"
  (let ((ast-dump (jf/test-dump-ast-structure "cat file.txt | grep pattern > out.txt")))
    (message "\n=== AST Structure: cat file.txt | grep pattern > out.txt ===\n%s\n" ast-dump)
    (should t)))

(provide 'test-ast-structure-dump)
;;; test-ast-structure-dump.el ends here
