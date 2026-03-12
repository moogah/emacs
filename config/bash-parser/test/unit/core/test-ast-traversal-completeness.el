;;; test-ast-traversal-completeness.el --- Validate complete AST traversal -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests that validate our tree traversal visits EVERY node in the tree-sitter AST.
;;
;; These tests address a critical security concern: if our visitor doesn't traverse
;; every node, we might miss parts of the command that contain security-critical
;; operations.
;;
;; Test approach:
;; 1. Count total nodes in tree-sitter AST (ground truth)
;; 2. Count nodes visited by our traversal function
;; 3. Verify counts match (proving complete traversal)
;; 4. Test with complex statements (pipelines, chains, nested structures)

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-core)

;;; Helper Functions

(defun jf/test-count-all-ast-nodes (node)
  "Recursively count ALL nodes in tree-sitter AST starting from NODE.
This is the ground truth - the total number of nodes tree-sitter created."
  (if (null node)
      0
    (let ((count 1))  ; Count this node
      ;; Add count of all children
      (dotimes (i (treesit-node-child-count node))
        (when-let ((child (treesit-node-child node i)))
          (setq count (+ count (jf/test-count-all-ast-nodes child)))))
      count)))

(defun jf/test-count-visited-nodes (node)
  "Count how many nodes our visitor function actually visits.
This is what our code sees - the nodes we traverse."
  (let ((visited-count 0))
    (jf/bash-parse--visit-node
     node
     (lambda (_n)
       (setq visited-count (1+ visited-count))
       nil))  ; Return nil to continue normal recursion
    visited-count))

(defun jf/test-parse-and-count-nodes (command-string)
  "Parse COMMAND-STRING and return (total-nodes . visited-nodes).
Uses temporary buffer to create AST, counts both total and visited nodes."
  (with-temp-buffer
    (insert command-string)
    (let* ((parser (treesit-parser-create 'bash))
           (root-node (treesit-parser-root-node parser))
           (total-nodes (jf/test-count-all-ast-nodes root-node))
           (visited-nodes (jf/test-count-visited-nodes root-node)))
      (cons total-nodes visited-nodes))))

;;; Simple Command Tests

(ert-deftest test-ast-traversal-simple-command ()
  "Verify complete traversal of simple command AST.

Command: echo hello
This is the baseline - if we can't traverse a simple command completely,
we have a fundamental problem."
  (let* ((counts (jf/test-parse-and-count-nodes "echo hello"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Simple command: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 0))
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-command-with-args ()
  "Verify complete traversal of command with multiple arguments.

Command: ls -la /tmp/foo /tmp/bar
Tests that all argument nodes are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "ls -la /tmp/foo /tmp/bar"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Command with args: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 5))  ; At least: command + several args
    (should (= visited-nodes total-nodes))))

;;; Pipeline Tests

(ert-deftest test-ast-traversal-simple-pipeline ()
  "Verify complete traversal of pipeline AST.

Command: cat file.txt | grep pattern
Pipelines have more complex AST structure - multiple command nodes plus pipe operator."
  (let* ((counts (jf/test-parse-and-count-nodes "cat file.txt | grep pattern"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Simple pipeline: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 10))  ; Pipelines have significant AST structure
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-three-stage-pipeline ()
  "Verify complete traversal of three-stage pipeline.

Command: cat file.txt | grep pattern | sort
Tests that all pipeline segments and operators are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "cat file.txt | grep pattern | sort"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Three-stage pipeline: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (= total-nodes 15))  ; Documented in AST-TRAVERSAL-COMPLETENESS.org
    (should (= visited-nodes total-nodes))))

;;; Chain Tests

(ert-deftest test-ast-traversal-and-chain ()
  "Verify complete traversal of && command chain.

Command: cd /tmp && ls -la
Tests that both commands and the && operator are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "cd /tmp && ls -la"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "AND chain: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 10))
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-or-chain ()
  "Verify complete traversal of || command chain.

Command: test -f file.txt || echo 'not found'
Tests error-handling chain pattern."
  (let* ((counts (jf/test-parse-and-count-nodes "test -f file.txt || echo 'not found'"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "OR chain: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 10))
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-complex-chain ()
  "Verify complete traversal of complex chain with multiple operators.

Command: git add -A && git commit -m 'message' && git push
Tests that all three commands and both && operators are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "git add -A && git commit -m 'message' && git push"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Complex chain: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (= total-nodes 20))  ; Documented in AST-TRAVERSAL-COMPLETENESS.org
    (should (= visited-nodes total-nodes))))

;;; Redirection Tests

(ert-deftest test-ast-traversal-output-redirection ()
  "Verify complete traversal with output redirection.

Command: echo test > output.txt
Tests that redirection operators and targets are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "echo test > output.txt"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Output redirection: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 5))
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-multiple-redirections ()
  "Verify complete traversal with multiple redirections.

Command: cmd > out.txt 2> err.txt < in.txt
Tests complex redirection patterns."
  (let* ((counts (jf/test-parse-and-count-nodes "cmd > out.txt 2> err.txt < in.txt"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Multiple redirections: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 10))
    (should (= visited-nodes total-nodes))))

;;; Command Substitution Tests

(ert-deftest test-ast-traversal-command-substitution ()
  "Verify complete traversal with command substitution.

Command: echo $(pwd)
Tests that command substitution nodes are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "echo $(pwd)"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Command substitution: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 5))
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-nested-substitution ()
  "Verify complete traversal with nested command substitution.

Command: echo $(dirname $(pwd))
Tests that nested substitution nodes are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "echo $(dirname $(pwd))"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Nested substitution: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 10))
    (should (= visited-nodes total-nodes))))

;;; Subshell Tests

(ert-deftest test-ast-traversal-subshell ()
  "Verify complete traversal of subshell.

Command: (cd /tmp && ls)
Tests that subshell and its contents are visited."
  (let* ((counts (jf/test-parse-and-count-nodes "(cd /tmp && ls)"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Subshell: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 10))
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-nested-subshells ()
  "Verify complete traversal of nested subshells.

Command: ( (echo foo) )
Tests that deeply nested structures are fully visited."
  (let* ((counts (jf/test-parse-and-count-nodes "( (echo foo) )"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Nested subshells: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 5))
    (should (= visited-nodes total-nodes))))

;;; Real-World Complex Command Tests

(ert-deftest test-ast-traversal-real-world-git-commit ()
  "Verify complete traversal of real git commit command.

Command: git add . && git commit -m 'Fix bug' && git push origin main
This is a common real-world pattern that must be fully analyzed."
  (let* ((counts (jf/test-parse-and-count-nodes "git add . && git commit -m 'Fix bug' && git push origin main"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Real git commit: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (= total-nodes 22))  ; Similar to 20-node chain + 2 extra args
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-real-world-find-exec ()
  "Verify complete traversal of find with -exec.

Command: find . -name '*.log' -exec rm {} \\;
This is security-critical - we must see the rm command."
  (let* ((counts (jf/test-parse-and-count-nodes "find . -name '*.log' -exec rm {} \\;"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Find with exec: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (= total-nodes 13))  ; Documented in AST-TRAVERSAL-COMPLETENESS.org
    (should (= visited-nodes total-nodes))))

(ert-deftest test-ast-traversal-real-world-pipeline-chain-combo ()
  "Verify complete traversal of pipeline + chain combination.

Command: cat file.txt | grep pattern > results.txt && echo 'Done'
Tests the most complex real-world pattern: pipeline + redirection + chain."
  (let* ((counts (jf/test-parse-and-count-nodes "cat file.txt | grep pattern > results.txt && echo 'Done'"))
         (total-nodes (car counts))
         (visited-nodes (cdr counts)))
    (message "Pipeline+chain combo: %d total nodes, %d visited" total-nodes visited-nodes)
    (should (> total-nodes 20))
    (should (= visited-nodes total-nodes))))

;;; Node Type Distribution Test

(ert-deftest test-ast-traversal-reports-all-node-types ()
  "Verify visitor sees all node types that tree-sitter created.

This test collects the node types from both full traversal and our visitor,
then verifies we didn't miss any node types."
  (with-temp-buffer
    (insert "cat file.txt | grep pattern > out.txt")
    (let* ((parser (treesit-parser-create 'bash))
           (root-node (treesit-parser-root-node parser))
           (all-node-types (make-hash-table :test 'equal))
           (visited-node-types (make-hash-table :test 'equal)))

      ;; Collect all node types in AST
      (cl-labels ((collect-all (node)
                    (when node
                      (puthash (treesit-node-type node) t all-node-types)
                      (dotimes (i (treesit-node-child-count node))
                        (collect-all (treesit-node-child node i))))))
        (collect-all root-node))

      ;; Collect node types our visitor sees
      (jf/bash-parse--visit-node
       root-node
       (lambda (node)
         (puthash (treesit-node-type node) t visited-node-types)
         nil))

      ;; Compare
      (let ((all-types (hash-table-keys all-node-types))
            (visited-types (hash-table-keys visited-node-types)))
        (message "All node types: %S" all-types)
        (message "Visited node types: %S" visited-types)

        ;; Every type in the AST should have been visited
        (dolist (node-type all-types)
          (should (gethash node-type visited-node-types)))

        (should (= (length all-types) (length visited-types)))))))

(provide 'test-ast-traversal-completeness)
;;; test-ast-traversal-completeness.el ends here
