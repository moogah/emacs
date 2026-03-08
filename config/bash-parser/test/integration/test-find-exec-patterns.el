;;; test-find-exec-patterns.el --- Integration tests for find -exec patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Integration tests for find command with -exec patterns.
;; Tests verify that find operations and exec block operations are both extracted.
;;
;; Originally created as verification tests for bead emacs-3kgg, these tests
;; validate the complete integration of find command parsing with exec blocks.

;;; Code:

(require 'test-helper (expand-file-name "../test-helper.el"
                                        (file-name-directory load-file-name)))

(ert-deftest test-find-exec-rm-pattern ()
  "Verify find with exec rm extracts both find and rm operations."
  (let* ((cmd "find . -name '*.log' -exec rm {} \\;")
         (parsed (jf/bash-parse cmd))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have 3 operations total
    (should (= (length ops) 3))

    ;; Should have :read-directory operation from find
    (let ((read-dir-op (cl-find-if (lambda (op)
                                      (and (eq (plist-get op :operation) :read-directory)
                                           (equal (plist-get op :file) ".")
                                           (eq (plist-get op :source) :positional-arg)))
                                    ops)))
      (should read-dir-op)
      (should (eq (plist-get read-dir-op :confidence) :high))
      (should (equal (plist-get read-dir-op :command) "find")))

    ;; Should have :match-pattern operation from find -name
    (let ((match-op (cl-find-if (lambda (op)
                                   (and (eq (plist-get op :operation) :match-pattern)
                                        (equal (plist-get op :file) "*.log")
                                        (eq (plist-get op :source) :flag-arg)))
                                 ops)))
      (should match-op)
      (should (eq (plist-get match-op :confidence) :high))
      (should (plist-get match-op :pattern))
      (should (equal (plist-get match-op :search-scope) "."))
      (should (equal (plist-get match-op :command) "find")))

    ;; Should have :delete operation from rm in exec block
    (let ((delete-op (cl-find-if (lambda (op)
                                    (and (eq (plist-get op :operation) :delete)
                                         (equal (plist-get op :file) "{}")
                                         (eq (plist-get op :source) :exec-block)))
                                  ops)))
      (should delete-op)
      (should (eq (plist-get delete-op :confidence) :high))
      (should (plist-get delete-op :indirect))
      (should (equal (plist-get delete-op :exec-type) "-exec"))
      (should (equal (plist-get delete-op :command) "rm")))))

(ert-deftest test-find-multiple-exec-blocks ()
  "Verify find with multiple exec blocks extracts operations from each."
  (let* ((cmd "find . -name '*.txt' -exec grep pattern {} \\; -exec cat {} \\;")
         (parsed (jf/bash-parse cmd))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have operations from find AND exec blocks
    ;; Note: Only commands that perform file operations produce operations.
    ;; grep produces :read, cat produces :read, so we expect 3 total:
    ;; 1. :read-directory from find
    ;; 2. :match-pattern from find -name
    ;; 3. :read from grep (cat's :read might be dedup'd or both present)
    (should (>= (length ops) 3))

    ;; Find operations
    (should (cl-find-if (lambda (op)
                          (and (eq (plist-get op :operation) :read-directory)
                               (equal (plist-get op :file) ".")))
                         ops))
    (should (cl-find-if (lambda (op)
                          (and (eq (plist-get op :operation) :match-pattern)
                               (equal (plist-get op :file) "*.txt")))
                         ops))

    ;; First exec block (grep - should be :read)
    (let ((grep-op (cl-find-if (lambda (op)
                                  (and (equal (plist-get op :command) "grep")
                                       (eq (plist-get op :source) :exec-block)))
                                ops)))
      (should grep-op)
      (should (eq (plist-get grep-op :operation) :read))
      (should (plist-get grep-op :indirect)))

    ;; Second exec block (cat - should be :read)
    (let ((cat-op (cl-find-if (lambda (op)
                                 (and (equal (plist-get op :command) "cat")
                                      (eq (plist-get op :source) :exec-block)))
                               ops)))
      (should cat-op)
      (should (eq (plist-get cat-op :operation) :read))
      (should (plist-get cat-op :indirect)))))

(ert-deftest test-find-exec-workspace-validation ()
  "Verify workspace validation via find exec pattern.

Tests that find /workspace extracts :read-directory operation for
security validation of workspace access."
  (let* ((cmd "find /workspace -name \"*.txt\" -exec cat {} \\;")
         (parsed (jf/bash-parse cmd))
         (ops (jf/bash-extract-file-operations parsed)))

    ;; Should have :read-directory on /workspace for security validation
    (let ((workspace-op (cl-find-if (lambda (op)
                                       (and (eq (plist-get op :operation) :read-directory)
                                            (equal (plist-get op :file) "/workspace")))
                                     ops)))
      (should workspace-op)
      (should (eq (plist-get workspace-op :source) :positional-arg))
      (should (equal (plist-get workspace-op :command) "find")))

    ;; Should also have match pattern and cat operation
    (should (cl-find-if (lambda (op)
                          (eq (plist-get op :operation) :match-pattern))
                         ops))
    (should (cl-find-if (lambda (op)
                          (and (eq (plist-get op :operation) :read)
                               (eq (plist-get op :source) :exec-block)))
                         ops))))

(provide 'test-find-exec-patterns)
;;; test-find-exec-patterns.el ends here
