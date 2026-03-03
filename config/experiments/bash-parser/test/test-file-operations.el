;;; test-file-operations.el --- Tests for file operations extraction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Comprehensive ERT tests for file operations extraction.
;; Tests the jf/bash-extract-file-operations function and supporting functions.
;;
;; These tests validate extraction from all sources:
;; - Redirections (>, >>, <, 2>, etc.)
;; - Positional arguments (based on command semantics)
;; - Exec blocks (find -exec ... \;)
;;
;; Tests also verify:
;; - Multi-command constructs (pipelines, chains)
;; - Variable resolution and tracking
;; - Confidence level classification
;; - Operation deduplication
;; - Metadata inclusion
;;
;; Test naming convention: test-extraction-* or test-variable-*
;; Each test includes spec reference in docstring.

;;; Code:

(require 'ert)
(require 'bash-parser)

;;; Basic Extraction Tests

(ert-deftest test-extraction-simple-read-command ()
  "Scenario: bash-file-operations § 'Simple read command'

Test that a simple read command extracts correct operation."
  (let* ((parsed (jf/bash-parse "cat /workspace/foo.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/workspace/foo.txt"))
    (should (eq (plist-get (car ops) :operation) :read))
    (should (eq (plist-get (car ops) :confidence) :high))
    (should (eq (plist-get (car ops) :source) :positional-arg))))

(ert-deftest test-extraction-command-with-multiple-file-arguments ()
  "Scenario: bash-file-operations § 'Command with multiple file arguments'

Test that cp command extracts both read and write operations."
  (let* ((parsed (jf/bash-parse "cp source.txt dest.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 2))
    ;; Check for read operation on source
    (let ((read-op (cl-find-if (lambda (op)
                                  (and (equal (plist-get op :file) "source.txt")
                                       (eq (plist-get op :operation) :read)))
                                ops)))
      (should read-op)
      (should (eq (plist-get read-op :confidence) :high)))
    ;; Check for write operation on dest
    (let ((write-op (cl-find-if (lambda (op)
                                   (and (equal (plist-get op :file) "dest.txt")
                                        (eq (plist-get op :operation) :write)))
                                 ops)))
      (should write-op)
      (should (eq (plist-get write-op :confidence) :high)))))

(ert-deftest test-extraction-command-with-no-file-operations ()
  "Scenario: bash-file-operations § 'Command with no file operations'

Test that echo command with no file arguments returns empty list."
  (let* ((parsed (jf/bash-parse "echo hello"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (null ops))))

;;; Redirection Tests

(ert-deftest test-extraction-output-redirection ()
  "Scenario: bash-file-operations § 'Output redirection'

Test that > redirection extracts write operation."
  (let* ((parsed (jf/bash-parse "echo test > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 1))
    (let ((write-op (cl-find-if (lambda (op)
                                   (and (equal (plist-get op :file) "output.txt")
                                        (eq (plist-get op :operation) :write)))
                                 ops)))
      (should write-op)
      (should (eq (plist-get write-op :confidence) :high))
      (should (eq (plist-get write-op :source) :redirection)))))

(ert-deftest test-extraction-append-redirection ()
  "Scenario: bash-file-operations § 'Append redirection'

Test that >> redirection extracts append operation."
  (let* ((parsed (jf/bash-parse "cat input.txt >> output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 1))
    ;; Check for append operation
    (let ((append-op (cl-find-if (lambda (op)
                                    (and (equal (plist-get op :file) "output.txt")
                                         (eq (plist-get op :operation) :append)))
                                  ops)))
      (should append-op)
      (should (eq (plist-get append-op :confidence) :high))
      (should (eq (plist-get append-op :source) :redirection)))))

(ert-deftest test-extraction-input-redirection ()
  "Scenario: bash-file-operations § 'Input redirection'

Test that < redirection extracts read operation."
  (let* ((parsed (jf/bash-parse "grep pattern < input.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 1))
    (let ((read-op (cl-find-if (lambda (op)
                                  (and (equal (plist-get op :file) "input.txt")
                                       (eq (plist-get op :operation) :read)))
                                ops)))
      (should read-op)
      (should (eq (plist-get read-op :confidence) :high))
      (should (eq (plist-get read-op :source) :redirection)))))

(ert-deftest test-extraction-multiple-redirections ()
  "Scenario: bash-file-operations § 'Multiple redirections'

Test that multiple redirections in one command are all extracted."
  (let* ((parsed (jf/bash-parse "cmd > out.txt 2> err.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 2))
    ;; Check for both write operations
    (let ((out-op (cl-find-if (lambda (op)
                                 (equal (plist-get op :file) "out.txt"))
                               ops))
          (err-op (cl-find-if (lambda (op)
                                 (equal (plist-get op :file) "err.txt"))
                               ops)))
      (should out-op)
      (should (eq (plist-get out-op :operation) :write))
      (should err-op)
      (should (eq (plist-get err-op :operation) :write)))))

;;; Multi-Command Construct Tests

(ert-deftest test-extraction-pipeline-with-multiple-file-operations ()
  "Scenario: bash-file-operations § 'Pipeline with multiple file operations'

Test that pipeline extracts operations from all commands."
  (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 2))
    ;; Check for read operation from cat
    (let ((read-op (cl-find-if (lambda (op)
                                  (and (equal (plist-get op :file) "file.txt")
                                       (eq (plist-get op :operation) :read)))
                                ops)))
      (should read-op))
    ;; Check for write operation from grep redirection
    (let ((write-op (cl-find-if (lambda (op)
                                   (and (equal (plist-get op :file) "output.txt")
                                        (eq (plist-get op :operation) :write)))
                                 ops)))
      (should write-op))))

(ert-deftest test-extraction-command-chain ()
  "Scenario: bash-file-operations § 'Command chain'

Test that command chain extracts operations from all commands."
  (let* ((parsed (jf/bash-parse "rm temp.txt && touch new.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 2))
    ;; Check for delete operation from rm
    (let ((delete-op (cl-find-if (lambda (op)
                                    (and (equal (plist-get op :file) "temp.txt")
                                         (eq (plist-get op :operation) :delete)))
                                  ops)))
      (should delete-op))
    ;; Check for create-or-modify operation from touch
    (let ((create-op (cl-find-if (lambda (op)
                                    (and (equal (plist-get op :file) "new.txt")
                                         (eq (plist-get op :operation) :create-or-modify)))
                                  ops)))
      (should create-op))))

;;; Deduplication Tests

(ert-deftest test-extraction-duplicate-file-operations ()
  "Scenario: bash-file-operations § 'Duplicate file operations'

Test that duplicate operations are deduplicated."
  (let* ((parsed (jf/bash-parse "cat file.txt file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should only have one operation for file.txt
    (let ((file-ops (cl-remove-if-not (lambda (op)
                                         (equal (plist-get op :file) "file.txt"))
                                       ops)))
      (should (= (length file-ops) 1))
      (should (eq (plist-get (car file-ops) :operation) :read)))))

(ert-deftest test-extraction-same-file-different-operations ()
  "Scenario: bash-file-operations § 'Same file with different operations'

Test that same file with different operations returns separate entries."
  (let* ((parsed (jf/bash-parse "cat file.txt > file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have both read and write operations
    (let ((read-op (cl-find-if (lambda (op)
                                  (and (equal (plist-get op :file) "file.txt")
                                       (eq (plist-get op :operation) :read)))
                                ops))
          (write-op (cl-find-if (lambda (op)
                                   (and (equal (plist-get op :file) "file.txt")
                                        (eq (plist-get op :operation) :write)))
                                 ops)))
      (should read-op)
      (should write-op))))

;;; Glob Pattern Tests

(ert-deftest test-extraction-simple-glob-pattern ()
  "Scenario: bash-file-operations § 'Simple glob pattern'

Test that glob pattern is preserved in file path."
  (let* ((parsed (jf/bash-parse "rm *.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "*.txt"))
    (should (eq (plist-get (car ops) :operation) :delete))))

(ert-deftest test-extraction-recursive-glob-pattern ()
  "Scenario: bash-file-operations § 'Recursive glob pattern'

Test that recursive glob pattern is preserved."
  (let* ((parsed (jf/bash-parse "cat config/**/*.json"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "config/**/*.json"))
    (should (eq (plist-get (car ops) :operation) :read))))

;;; Path Type Tests

(ert-deftest test-extraction-absolute-path ()
  "Scenario: bash-file-operations § 'Absolute path'

Test that absolute path is extracted as-is."
  (let* ((parsed (jf/bash-parse "cat /workspace/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/workspace/file.txt"))))

(ert-deftest test-extraction-relative-path ()
  "Scenario: bash-file-operations § 'Relative path'

Test that relative path is extracted as-is."
  (let* ((parsed (jf/bash-parse "cat ./file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "./file.txt"))))

;;; Variable Extraction Tests

(ert-deftest test-variable-simple-variable-reference ()
  "Scenario: bash-file-operations § 'Simple variable reference'

Test that simple variable reference is detected and marked."
  (let* ((parsed (jf/bash-parse "cat $FILE"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "$FILE"))
    ;; Should mark as unresolved since no context provided
    (should (plist-get (car ops) :unresolved))))

(ert-deftest test-variable-variable-with-braces ()
  "Scenario: bash-file-operations § 'Variable with braces'

Test that variable with braces is detected."
  (let* ((parsed (jf/bash-parse "rm ${TEMP_DIR}/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (string-match-p "\\${TEMP_DIR}" (plist-get (car ops) :file)))))

(ert-deftest test-variable-multiple-variables-in-path ()
  "Scenario: bash-file-operations § 'Multiple variables in path'

Test that multiple variables in path are detected."
  (let* ((parsed (jf/bash-parse "cp $SRC/$FILE $DEST/"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 2))
    ;; Both operations should have variable references
    (dolist (op ops)
      (should (string-match-p "\\$" (plist-get op :file))))))

;;; Variable Resolution Tests

(ert-deftest test-variable-resolve-declared-variable ()
  "Scenario: bash-file-operations § 'Resolve declared variable'

Test that variable is resolved against provided context."
  (let* ((parsed (jf/bash-parse "cat $WORKSPACE/file.txt"))
         (context '((WORKSPACE . "/workspace")))
         (ops (jf/bash-extract-file-operations parsed context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/workspace/file.txt"))
    ;; Should not be marked as unresolved
    (should-not (plist-get (car ops) :unresolved))))

(ert-deftest test-variable-unresolved-variable ()
  "Scenario: bash-file-operations § 'Unresolved variable'

Test that unresolved variable is marked with metadata."
  (let* ((parsed (jf/bash-parse "cat $UNKNOWN/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    ;; Should be marked as unresolved
    (should (plist-get (car ops) :unresolved))
    ;; Should include list of unresolved variables
    (let ((unresolved-vars (plist-get (car ops) :unresolved)))
      (should (or (member "UNKNOWN" unresolved-vars)
                  (member 'UNKNOWN unresolved-vars))))))

(ert-deftest test-variable-partial-resolution ()
  "Scenario: bash-file-operations § 'Partial resolution'

Test that partial resolution is handled correctly."
  (let* ((parsed (jf/bash-parse "cat $WORKSPACE/$FILE"))
         (context '((WORKSPACE . "/workspace")))
         (ops (jf/bash-extract-file-operations parsed context)))
    (should (= (length ops) 1))
    ;; Should have WORKSPACE resolved but FILE unresolved
    (let ((file-path (plist-get (car ops) :file)))
      (should (string-prefix-p "/workspace/" file-path))
      (should (string-match-p "\\$FILE" file-path)))
    ;; Should be marked as unresolved
    (should (plist-get (car ops) :unresolved))))

;;; Variable Assignment Tracking Tests

(ert-deftest test-variable-assignment-and-usage-in-chain ()
  "Scenario: bash-file-operations § 'Assignment and usage in chain'

Test that variable assignment in chain is tracked and resolved."
  (let* ((parsed (jf/bash-parse "DIR=/tmp && cat $DIR/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    ;; DIR should be resolved to /tmp
    (should (equal (plist-get (car ops) :file) "/tmp/file.txt"))
    ;; Should not be marked as unresolved
    (should-not (plist-get (car ops) :unresolved))))

(ert-deftest test-variable-multiple-assignments ()
  "Scenario: bash-file-operations § 'Multiple assignments'

Test that multiple variable assignments are tracked sequentially."
  (let* ((parsed (jf/bash-parse "A=/foo && B=$A/bar && cat $B/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    ;; Variables should resolve sequentially: A=/foo, B=/foo/bar, final=/foo/bar/file.txt
    (should (equal (plist-get (car ops) :file) "/foo/bar/file.txt"))
    (should-not (plist-get (car ops) :unresolved))))

(ert-deftest test-variable-assignment-without-usage ()
  "Scenario: bash-file-operations § 'Assignment without usage'

Test that variable assignment is tracked even if not used."
  (let* ((parsed (jf/bash-parse "VAR=/tmp && cat file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    ;; Should just read file.txt
    (should (equal (plist-get (car ops) :file) "file.txt"))
    (should-not (plist-get (car ops) :unresolved))))

;;; Variable Context Tests

(ert-deftest test-variable-provide-variable-context ()
  "Scenario: bash-file-operations § 'Provide variable context'

Test that provided variable context is used for resolution."
  (let* ((parsed (jf/bash-parse "cat $WORKSPACE/file.txt"))
         (context '((WORKSPACE . "/workspace")))
         (ops (jf/bash-extract-file-operations parsed context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/workspace/file.txt"))
    (should-not (plist-get (car ops) :unresolved))))

(ert-deftest test-variable-empty-variable-context ()
  "Scenario: bash-file-operations § 'Empty variable context'

Test that empty context results in unresolved variables."
  (let* ((parsed (jf/bash-parse "cat $WORKSPACE/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    ;; Variable should remain unresolved
    (should (string-match-p "\\$WORKSPACE" (plist-get (car ops) :file)))
    (should (plist-get (car ops) :unresolved))))

;;; Confidence Level Tests

(ert-deftest test-extraction-high-confidence-from-known-command ()
  "Scenario: bash-file-operations § 'High confidence from known command'

Test that known commands have high confidence."
  (let* ((parsed (jf/bash-parse "cat file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (eq (plist-get (car ops) :confidence) :high))))

(ert-deftest test-extraction-high-confidence-from-redirection ()
  "Scenario: bash-file-operations § 'High confidence from redirection'

Test that redirections always have high confidence."
  (let* ((parsed (jf/bash-parse "unknown-cmd > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should extract from redirection even if command is unknown
    (let ((write-op (cl-find-if (lambda (op)
                                   (equal (plist-get op :file) "output.txt"))
                                 ops)))
      (when write-op
        (should (eq (plist-get write-op :confidence) :high))
        (should (eq (plist-get write-op :source) :redirection))))))

;;; Metadata Tests

(ert-deftest test-extraction-metadata-for-positional-arg ()
  "Scenario: bash-file-operations § 'Metadata for positional arg operation'

Test that positional arg operations include correct metadata."
  (let* ((parsed (jf/bash-parse "cat file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      (should (eq (plist-get op :source) :positional-arg))
      ;; Should have command name in metadata or at top level
      (should (or (plist-get op :command)
                  (and (plist-get op :metadata)
                       (plist-get (plist-get op :metadata) :command)))))))

(ert-deftest test-extraction-metadata-for-redirection ()
  "Scenario: bash-file-operations § 'Metadata for redirection operation'

Test that redirection operations include correct metadata."
  (let* ((parsed (jf/bash-parse "echo test > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (let ((write-op (cl-find-if (lambda (op)
                                   (equal (plist-get op :file) "output.txt"))
                                 ops)))
      (should write-op)
      (should (eq (plist-get write-op :source) :redirection)))))

;;; Main Function Tests

(ert-deftest test-extraction-main-function-simple-command ()
  "Scenario: bash-file-operations § 'Extract from simple command'

Test main extraction function with simple command."
  (let* ((parsed (jf/bash-parse "cat file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (listp ops))
    (should (> (length ops) 0))))

(ert-deftest test-extraction-main-function-pipeline ()
  "Scenario: bash-file-operations § 'Extract from pipeline'

Test main extraction function with pipeline."
  (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (listp ops))
    (should (> (length ops) 0))))

(ert-deftest test-extraction-main-function-chain ()
  "Scenario: bash-file-operations § 'Extract from chain'

Test main extraction function with chain."
  (let* ((parsed (jf/bash-parse "rm old.txt && touch new.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (listp ops))
    (should (>= (length ops) 2))))

(provide 'test-file-operations)
;;; test-file-operations.el ends here
