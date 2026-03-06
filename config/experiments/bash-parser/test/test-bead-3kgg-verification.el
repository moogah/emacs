;;; test-bead-3kgg-verification.el --- Verify bead emacs-3kgg requirements -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verification tests for bead emacs-3kgg requirements.
;;
;; Spec requirement (openspec/specs/bash-file-operations/spec.md lines 38-47):
;; - Scenario: Find with exec rm
;;   WHEN extracting from "find . -name '*.log' -exec rm {} \;"
;;   THEN system returns operations from BOTH find (:read-directory on ".")
;;        AND rm (:delete on matched files)
;;
;; - Scenario: Find with multiple exec blocks
;;   WHEN extracting from command with multiple -exec blocks
;;   THEN system returns operations from each exec block separately

;;; Code:

(require 'ert)
(require 'bash-parser)

(ert-deftest test-bead-3kgg-find-with-exec-rm ()
  "Verify find with exec rm extracts both find and rm operations.

Spec: openspec/specs/bash-file-operations/spec.md lines 40-42"
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

(ert-deftest test-bead-3kgg-find-with-multiple-exec-blocks ()
  "Verify find with multiple exec blocks extracts operations from each.

Spec: openspec/specs/bash-file-operations/spec.md lines 44-46"
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

(ert-deftest test-bead-3kgg-workspace-validation-scenario ()
  "Verify workspace validation scenario from bead description.

Test: find /workspace -name \"*.txt\" -exec cat {} \\;
Should validate workspace access via :read-directory operation."
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

(provide 'test-bead-3kgg-verification)
;;; test-bead-3kgg-verification.el ends here
