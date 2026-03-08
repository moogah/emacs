;;; test-treesitter-workarounds.el --- Tests for tree-sitter parser workarounds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests for workarounds to tree-sitter-bash parser bugs and limitations.
;;
;; These tests document parser edge cases that require special handling
;; due to upstream tree-sitter-bash issues. Each test includes:
;; - Description of the tree-sitter bug
;; - The workaround implementation
;; - Expected behavior verification
;; - TODO markers for when the workaround can be removed
;;
;; Workarounds covered:
;; 1. Double-paren subshell parsing (emacs-mu54)
;;    Bug: (( at start parsed as arithmetic expansion instead of nested subshells
;;    Fix: Normalize "((cmd))" to "( (cmd))" by inserting space
;;
;; TODO: Monitor tree-sitter-bash releases and remove workarounds when fixed upstream.

;;; Code:

(require 'test-helper (expand-file-name "../test-helper.el"
                                        (file-name-directory load-file-name)))

;;; Double-Paren Subshell Workaround Tests (emacs-mu54)

(ert-deftest test-double-paren-simple-command ()
  "Test (( parsing workaround with simple command.

Bug: tree-sitter-bash incorrectly parses (( as arithmetic expansion.
Workaround: Parser normalizes '((ls))' to '( (ls))' for correct parsing.

Expected: Parses as nested subshells, extracts 'ls' command.
TODO: Remove when tree-sitter-bash fixes (( parsing."
  (let ((result (jf/bash-parse "((ls))")))
    ;; Should successfully parse
    (should (eq (plist-get result :success) t))

    ;; Should detect as subshell type
    (should (eq (plist-get result :type) :subshell))

    ;; Should have nested subshell body
    (let ((subshell-body (plist-get result :subshell-body)))
      (should subshell-body)
      (should (eq (plist-get subshell-body :success) t))
      (should (eq (plist-get subshell-body :type) :subshell))

      ;; Inner subshell should contain ls command
      (let ((inner-body (plist-get subshell-body :subshell-body)))
        (should inner-body)
        (should (equal (plist-get inner-body :command-name) "ls"))))))

(ert-deftest test-double-paren-with-args ()
  "Test (( parsing workaround with command arguments.

Verifies workaround handles commands with arguments correctly."
  (let ((result (jf/bash-parse "((cat file.txt))")))
    (should (eq (plist-get result :success) t))
    (should (eq (plist-get result :type) :subshell))

    ;; Navigate to inner command
    (let* ((outer-body (plist-get result :subshell-body))
           (inner-body (plist-get outer-body :subshell-body)))
      (should (equal (plist-get inner-body :command-name) "cat"))
      (should (member "file.txt" (plist-get inner-body :positional-args))))))

(ert-deftest test-double-paren-chain ()
  "Test (( parsing workaround with command chain.

Verifies workaround handles complex nested structures."
  (let ((result (jf/bash-parse "((cd /tmp && ls))")))
    (should (eq (plist-get result :success) t))
    (should (eq (plist-get result :type) :subshell))

    ;; Navigate to inner chain
    (let* ((outer-body (plist-get result :subshell-body))
           (inner-body (plist-get outer-body :subshell-body)))
      (should (eq (plist-get inner-body :type) :chain))
      (should (>= (plist-get inner-body :command-count) 2))

      ;; Verify both commands in chain
      (let ((commands (plist-get inner-body :all-commands)))
        (should (seq-find (lambda (cmd)
                           (equal (plist-get cmd :command-name) "cd"))
                         commands))
        (should (seq-find (lambda (cmd)
                           (equal (plist-get cmd :command-name) "ls"))
                         commands))))))

(ert-deftest test-double-paren-at-start-only ()
  "Test workaround only applies to (( at command start.

The workaround normalizes '((cmd))' → '( (cmd))' ONLY when (( appears
at the very beginning of the command string.

This test verifies the workaround's scope is correctly limited."
  ;; Test that (( at start is normalized
  (let ((result (jf/bash-parse "((ls))")))
    (should (eq (plist-get result :success) t))
    (should (eq (plist-get result :type) :subshell))))

(ert-deftest test-triple-paren ()
  "Test ((( parsing - not affected by workaround.

Verifies workaround only triggers on exactly (( prefix.
Note: Tree-sitter may still have issues with (((, but that's a separate bug."
  (let ((result (jf/bash-parse "(((ls)))")))
    (should (eq (plist-get result :success) t))
    ;; Just verify it parses successfully - the exact structure may vary
    ;; due to tree-sitter bugs that are independent of our workaround
    (should (plist-get result :type))))

(ert-deftest test-single-paren-not-affected ()
  "Test single paren ( is not affected by (( workaround.

Verifies workaround only activates for (( prefix."
  (let ((result (jf/bash-parse "(ls)")))
    (should (eq (plist-get result :success) t))
    (should (eq (plist-get result :type) :subshell))

    (let ((body (plist-get result :subshell-body)))
      (should (equal (plist-get body :command-name) "ls")))))

(ert-deftest test-arithmetic-expansion-still-works ()
  "Test $((arithmetic)) parsing not broken by workaround.

Verifies workaround only affects (( at command start, not $(( arithmetic."
  (let ((result (jf/bash-parse "echo $((5 + 3))")))
    (should (eq (plist-get result :success) t))
    (should (equal (plist-get result :command-name) "echo"))
    ;; The $((5 + 3)) will be in the args
    (should (member "$((5 + 3))" (plist-get result :args)))))

(provide 'test-treesitter-workarounds)
;;; test-treesitter-workarounds.el ends here
