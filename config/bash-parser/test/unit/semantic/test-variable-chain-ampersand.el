;;; test-variable-chain-ampersand.el --- Variable chain tests with && syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verification tests for variable chain tracking implementation (bead emacs-1op2).
;;
;; These tests use && syntax instead of semicolon syntax to avoid parser bug
;; with semicolon-separated assignment chains. The implementation correctly
;; resolves assignment values before adding them to context.

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))

(ert-deftest test-variable-chain-pwd-assignment-ampersand ()
  "Test BASE=$PWD assignment with && chain.

SECURITY: BASE=$PWD && cat $BASE/file.txt must resolve to /base/dir/file.txt
IMPLEMENTATION: Variable chain tracking (bead emacs-1op2) - working with &&"
  (let* ((parsed (jf/bash-parse "BASE=$PWD && cat $BASE/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find the cat operation with resolved path
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/file.txt")))))

(ert-deftest test-variable-chain-relative-path-assignment-ampersand ()
  "Test DIR=./sub assignment with && chain.

SECURITY: DIR=./sub && cat $DIR/file.txt → /base/dir/sub/file.txt
IMPLEMENTATION: Variable chain tracking resolves relative paths in assignments."
  (let* ((parsed (jf/bash-parse "DIR=./sub && cat $DIR/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/sub/file.txt")))))

(ert-deftest test-variable-chain-parent-path-assignment-ampersand ()
  "Test PATH=../other assignment with && chain.

SECURITY: PATH=../other && cat $PATH/file.txt with PWD=/dir/sub → /dir/other/file.txt"
  (let* ((parsed (jf/bash-parse "PATH=../other && cat $PATH/file.txt"))
         (var-context '((PWD . "/dir/sub")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/dir/other/file.txt")))))

(ert-deftest test-variable-chain-multi-level ()
  "Test multi-level variable chain: BASE=$PWD && DIR=$BASE/sub && cat $DIR/file.txt

SECURITY: Multi-level variable chains must resolve transitively.
IMPLEMENTATION: Context is built incrementally as commands are processed."
  (let* ((parsed (jf/bash-parse "BASE=$PWD && DIR=$BASE/sub && cat $DIR/file.txt"))
         (var-context '((PWD . "/workspace")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/workspace/sub/file.txt")))))

(provide 'test-variable-chain-ampersand)
;;; test-variable-chain-ampersand.el ends here
