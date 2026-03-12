;;; test-assertions.el --- Common test assertion helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Common assertion helpers to reduce duplication across bash-parser tests.
;;
;; These helpers provide readable, reusable patterns for common file operation
;; assertions. Each function includes clear error messages for test failures.
;;
;; Usage:
;;   (require 'test-assertions)
;;   (should-have-file-operation ops "/workspace/foo.txt" :read)
;;   (should-have-operations-count ops 2)
;;   (should-operation-have-metadata op :source :positional-arg)
;;   (should-have-unresolved-variables op '("UNKNOWN"))

;;; Code:

(require 'cl-lib)

;;; File Operation Assertions

(defun should-have-file-operation (operations file-path operation-type)
  "Assert OPERATIONS contains an operation for FILE-PATH with OPERATION-TYPE.

OPERATIONS is a list of file operation plists.
FILE-PATH is the expected :file value.
OPERATION-TYPE is the expected :operation value (keyword).

Returns the matching operation if found.

Example:
  (should-have-file-operation ops \"/workspace/foo.txt\" :read)"
  (let ((matching-op (cl-find-if
                      (lambda (op)
                        (and (equal (plist-get op :file) file-path)
                             (eq (plist-get op :operation) operation-type)))
                      operations)))
    (unless matching-op
      (ert-fail (format "Expected operation not found:\n  File: %s\n  Operation: %s\n  Available ops: %S"
                        file-path operation-type operations)))
    matching-op))

(defun should-have-operations-count (operations expected-count)
  "Assert OPERATIONS list has exactly EXPECTED-COUNT elements.

OPERATIONS is a list of file operation plists.
EXPECTED-COUNT is the expected number of operations.

Example:
  (should-have-operations-count ops 2)"
  (let ((actual-count (length operations)))
    (unless (= actual-count expected-count)
      (ert-fail (format "Expected %d operations, got %d:\n  %S"
                        expected-count actual-count operations)))))

(defun should-operation-have-metadata (operation metadata-key expected-value)
  "Assert OPERATION has METADATA-KEY with EXPECTED-VALUE.

OPERATION is a file operation plist.
METADATA-KEY is the plist key to check (keyword).
EXPECTED-VALUE is the expected value for that key.

Example:
  (should-operation-have-metadata op :source :positional-arg)
  (should-operation-have-metadata op :confidence :high)"
  (let ((actual-value (plist-get operation metadata-key)))
    (unless (equal actual-value expected-value)
      (ert-fail (format "Operation metadata mismatch:\n  Key: %s\n  Expected: %S\n  Actual: %S\n  Operation: %S"
                        metadata-key expected-value actual-value operation)))))

(defun should-have-unresolved-variables (operation expected-vars)
  "Assert OPERATION has :unresolved t and :unresolved-vars matching EXPECTED-VARS.

OPERATION is a file operation plist.
EXPECTED-VARS is a list of expected unresolved variable names (strings or symbols).

Example:
  (should-have-unresolved-variables op '(\"UNKNOWN\"))
  (should-have-unresolved-variables op '(\"FILE\" \"DIR\"))"
  (let ((unresolved (plist-get operation :unresolved))
        (unresolved-vars (plist-get operation :unresolved-vars)))
    (unless (eq unresolved t)
      (ert-fail (format "Operation not marked as unresolved:\n  Operation: %S"
                        operation)))
    ;; Normalize both to strings for comparison
    (let ((normalized-expected (mapcar (lambda (v) (if (symbolp v) (symbol-name v) v))
                                       expected-vars))
          (normalized-actual (mapcar (lambda (v) (if (symbolp v) (symbol-name v) v))
                                     unresolved-vars)))
      (unless (equal (sort normalized-expected #'string<)
                     (sort normalized-actual #'string<))
        (ert-fail (format "Unresolved variables mismatch:\n  Expected: %S\n  Actual: %S\n  Operation: %S"
                          expected-vars unresolved-vars operation))))))

(defun should-have-file-operation-with-metadata (operations file-path operation-type metadata-plist)
  "Assert OPERATIONS contains operation matching FILE-PATH, OPERATION-TYPE, and METADATA-PLIST.

OPERATIONS is a list of file operation plists.
FILE-PATH is the expected :file value.
OPERATION-TYPE is the expected :operation value (keyword).
METADATA-PLIST is a plist of additional key-value pairs to match.

Returns the matching operation if found.

Example:
  (should-have-file-operation-with-metadata
    ops
    \"output.txt\"
    :write
    '(:source :redirection :confidence :high))"
  (let ((matching-op (cl-find-if
                      (lambda (op)
                        (and (equal (plist-get op :file) file-path)
                             (eq (plist-get op :operation) operation-type)
                             (cl-every (lambda (key)
                                        (equal (plist-get op key)
                                               (plist-get metadata-plist key)))
                                      ;; Get all keys from metadata-plist
                                      (cl-loop for (key _val) on metadata-plist by #'cddr
                                               collect key))))
                      operations)))
    (unless matching-op
      (ert-fail (format "Expected operation not found:\n  File: %s\n  Operation: %s\n  Metadata: %S\n  Available ops: %S"
                        file-path operation-type metadata-plist operations)))
    matching-op))

(defun should-not-have-file-operation (operations file-path operation-type)
  "Assert OPERATIONS does NOT contain an operation for FILE-PATH with OPERATION-TYPE.

OPERATIONS is a list of file operation plists.
FILE-PATH is the file path that should not be present.
OPERATION-TYPE is the operation type that should not be present (keyword).

Example:
  (should-not-have-file-operation ops \"/etc/passwd\" :write)"
  (let ((matching-op (cl-find-if
                      (lambda (op)
                        (and (equal (plist-get op :file) file-path)
                             (eq (plist-get op :operation) operation-type)))
                      operations)))
    (when matching-op
      (ert-fail (format "Unexpected operation found:\n  File: %s\n  Operation: %s\n  Operation data: %S"
                        file-path operation-type matching-op)))))

(provide 'test-assertions)
;;; test-assertions.el ends here
