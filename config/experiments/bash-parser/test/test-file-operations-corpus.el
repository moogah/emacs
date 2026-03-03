;;; test-file-operations-corpus.el --- Corpus-based tests for file operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Corpus-based ERT tests for file operations extraction.
;; Uses file-operations-corpus.el to provide clear, example-driven test cases.
;;
;; Each test case clearly shows:
;; - The bash command
;; - Which files are accessed
;; - What operation is performed (read, write, delete, modify, create)
;; - The confidence level
;; - The source of the operation (positional-arg, redirection, exec-block)
;;
;; This provides a comprehensive reference for what file operations are
;; extracted from different bash command patterns.

;;; Code:

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))
(require 'file-operations-corpus (expand-file-name "file-operations-corpus.el"
                                                    (file-name-directory load-file-name)))

;;; Helper Functions

(defun jf/test-file-op-matches-p (actual expected)
  "Return t if ACTUAL operation matches EXPECTED operation plist.

Compares all specified keys in EXPECTED against ACTUAL.
EXPECTED may omit keys for partial matching."
  (cl-every (lambda (key)
              (equal (plist-get actual key)
                     (plist-get expected key)))
            ;; Get all keys from expected plist
            (cl-loop for (key _val) on expected by #'cddr
                     collect key)))

(defun jf/test-find-matching-op (expected-op actual-ops)
  "Find operation in ACTUAL-OPS that matches EXPECTED-OP.

Returns the matching operation or nil if not found."
  (cl-find-if (lambda (actual-op)
                (jf/test-file-op-matches-p actual-op expected-op))
              actual-ops))

(defun jf/test-run-corpus-case (test-case)
  "Run a single file operations test case from corpus.

TEST-CASE is a plist with :id, :command, :expect-ops, and optional :var-context."
  (let* ((test-id (plist-get test-case :id))
         (command (plist-get test-case :command))
         (expected-ops (plist-get test-case :expect-ops))
         (var-context (plist-get test-case :var-context))
         (note (plist-get test-case :note))
         (parsed (jf/bash-parse command))
         (actual-ops (if var-context
                        (jf/bash-extract-file-operations parsed var-context)
                      (jf/bash-extract-file-operations parsed))))

    ;; Verify parsing succeeded
    (unless (plist-get parsed :success)
      (error "Test %s: parsing failed for command: %s" test-id command))

    ;; Check operation count matches
    (unless (= (length expected-ops) (length actual-ops))
      (error "Test %s (%s): expected %d operations, got %d\nExpected: %S\nActual: %S"
             test-id note
             (length expected-ops) (length actual-ops)
             expected-ops actual-ops))

    ;; Check each expected operation is present
    (dolist (expected-op expected-ops)
      (let ((matching-op (jf/test-find-matching-op expected-op actual-ops)))
        (unless matching-op
          (error "Test %s (%s): expected operation not found\nExpected: %S\nActual ops: %S"
                 test-id note expected-op actual-ops))))))

;;; Corpus-Based Tests - READ Operations

(ert-deftest test-corpus-read-001 ()
  "File operations: read-001 - Simple file read"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "read-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-read-002 ()
  "File operations: read-002 - Grep reads file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "read-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-read-003 ()
  "File operations: read-003 - Head reads file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "read-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-read-004 ()
  "File operations: read-004 - Tail reads file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "read-004"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-read-005 ()
  "File operations: read-005 - Word count reads file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "read-005"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-read-006 ()
  "File operations: read-006 - Less reads file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "read-006"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - WRITE Operations

(ert-deftest test-corpus-write-001 ()
  "File operations: write-001 - Touch creates file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "write-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-write-002 ()
  "File operations: write-002 - Redirection writes file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "write-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-write-003 ()
  "File operations: write-003 - Append redirection"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "write-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-write-004 ()
  "File operations: write-004 - Tee writes file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "write-004"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - DELETE Operations

(ert-deftest test-corpus-delete-001 ()
  "File operations: delete-001 - Simple file deletion"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "delete-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-delete-002 ()
  "File operations: delete-002 - Recursive deletion"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "delete-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-delete-003 ()
  "File operations: delete-003 - Remove directory"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "delete-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-delete-004 ()
  "File operations: delete-004 - Delete multiple files"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "delete-004"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - COPY Operations

(ert-deftest test-corpus-copy-001 ()
  "File operations: copy-001 - Copy file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "copy-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-copy-002 ()
  "File operations: copy-002 - Recursive copy"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "copy-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-copy-003 ()
  "File operations: copy-003 - Copy multiple files"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "copy-003"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - MOVE Operations

(ert-deftest test-corpus-move-001 ()
  "File operations: move-001 - Move/rename file"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "move-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-move-002 ()
  "File operations: move-002 - Move to directory"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "move-002"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - MODIFY Operations

(ert-deftest test-corpus-modify-001 ()
  "File operations: modify-001 - Chmod modifies permissions"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "modify-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-modify-002 ()
  "File operations: modify-002 - Chown modifies ownership"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "modify-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-modify-003 ()
  "File operations: modify-003 - Sed in-place edit"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "modify-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-modify-004 ()
  "File operations: modify-004 - Sed read-only"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "modify-004"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - REDIRECTION

(ert-deftest test-corpus-redirect-001 ()
  "File operations: redirect-001 - Input redirection"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-redirect-002 ()
  "File operations: redirect-002 - Stderr redirection"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-redirect-003 ()
  "File operations: redirect-003 - Multiple redirections"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-redirect-004 ()
  "File operations: redirect-004 - Read, write, error"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-004"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - GLOB PATTERNS

(ert-deftest test-corpus-glob-001 ()
  "File operations: glob-001 - Simple glob pattern"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-glob-002 ()
  "File operations: glob-002 - Recursive glob"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-glob-003 ()
  "File operations: glob-003 - Copy with glob"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-glob-004 ()
  "File operations: glob-004 - Brace expansion (no ops)"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-004"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - PIPELINES

(ert-deftest test-corpus-pipeline-001 ()
  "File operations: pipeline-001 - Pipeline with read and write"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "pipeline-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-pipeline-002 ()
  "File operations: pipeline-002 - Three-stage pipeline"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "pipeline-002"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - COMMAND CHAINS

(ert-deftest test-corpus-chain-001 ()
  "File operations: chain-001 - Delete then create"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-chain-002 ()
  "File operations: chain-002 - Overwrite then append"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-chain-003 ()
  "File operations: chain-003 - Backup then delete"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-003"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - FIND -EXEC

(ert-deftest test-corpus-find-001 ()
  "File operations: find-001 - Find and delete"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-find-002 ()
  "File operations: find-002 - Find, read, redirect"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-find-003 ()
  "File operations: find-003 - Multiple exec blocks"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-003"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - VARIABLES

(ert-deftest test-corpus-variable-001 ()
  "File operations: variable-001 - Unresolved variable"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-variable-002 ()
  "File operations: variable-002 - Resolved variable"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-variable-003 ()
  "File operations: variable-003 - Braces unresolved"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-variable-004 ()
  "File operations: variable-004 - Braces resolved"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-004"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-variable-005 ()
  "File operations: variable-005 - Multiple variables partial"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-005"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - VARIABLE CHAINS

(ert-deftest test-corpus-variable-chain-001 ()
  "File operations: variable-chain-001 - Assignment and usage"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-chain-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-variable-chain-002 ()
  "File operations: variable-chain-002 - Sequential resolution"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-chain-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-variable-chain-003 ()
  "File operations: variable-chain-003 - Multiple uses"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-chain-003"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - GIT COMMANDS

(ert-deftest test-corpus-git-001 ()
  "File operations: git-001 - Git add"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-git-002 ()
  "File operations: git-002 - Git add with glob"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-git-003 ()
  "File operations: git-003 - Git checkout"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-git-004 ()
  "File operations: git-004 - Git log with redirection"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-004"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - DIRECTORIES

(ert-deftest test-corpus-directory-001 ()
  "File operations: directory-001 - Create directory"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "directory-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-directory-002 ()
  "File operations: directory-002 - Create nested dirs"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "directory-002"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - ARCHIVES

(ert-deftest test-corpus-archive-001 ()
  "File operations: archive-001 - Create tar archive"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "archive-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-archive-002 ()
  "File operations: archive-002 - Extract tar archive"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "archive-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-archive-003 ()
  "File operations: archive-003 - Create zip archive"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "archive-003"))
             jf/bash-file-operations-test-corpus)))

;;; Corpus-Based Tests - NO OPERATIONS

(ert-deftest test-corpus-no-ops-001 ()
  "File operations: no-ops-001 - Echo (no files)"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "no-ops-001"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-no-ops-002 ()
  "File operations: no-ops-002 - Ls (read-only)"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "no-ops-002"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-no-ops-003 ()
  "File operations: no-ops-003 - Pwd"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "no-ops-003"))
             jf/bash-file-operations-test-corpus)))

(ert-deftest test-corpus-no-ops-004 ()
  "File operations: no-ops-004 - Git status"
  (jf/test-run-corpus-case
   (seq-find (lambda (tc) (equal (plist-get tc :id) "no-ops-004"))
             jf/bash-file-operations-test-corpus)))

;;; Interactive Test Runner

(defun jf/file-operations-corpus-run-all ()
  "Run all file operations corpus tests and display results."
  (interactive)
  (ert-run-tests-interactively "test-corpus-"))

(defun jf/file-operations-corpus-test-command (command &optional var-context)
  "Interactively test file operations extraction for COMMAND.

Optional VAR-CONTEXT is an alist of variable bindings."
  (interactive "sCommand to parse: ")
  (let* ((parsed (jf/bash-parse command))
         (ops (if var-context
                 (jf/bash-extract-file-operations parsed var-context)
               (jf/bash-extract-file-operations parsed))))
    (with-current-buffer (get-buffer-create "*file-operations-test*")
      (erase-buffer)
      (insert (format "Command: %s\n\n" command))
      (when var-context
        (insert (format "Variable Context: %S\n\n" var-context)))
      (insert "Parse Result:\n")
      (insert (pp-to-string parsed))
      (insert "\n\nFile Operations:\n")
      (if ops
          (dolist (op ops)
            (insert (format "\n  File: %s\n" (plist-get op :file)))
            (insert (format "  Operation: %s\n" (plist-get op :operation)))
            (insert (format "  Confidence: %s\n" (plist-get op :confidence)))
            (insert (format "  Source: %s\n" (plist-get op :source)))
            (when (plist-get op :pattern)
              (insert "  Pattern: t\n"))
            (when (plist-get op :unresolved)
              (insert (format "  Unresolved: %S\n" (plist-get op :unresolved-vars)))))
        (insert "  (no file operations)\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'test-file-operations-corpus)
;;; test-file-operations-corpus.el ends here
