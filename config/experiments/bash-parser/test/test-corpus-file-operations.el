;;; test-corpus-file-operations.el --- Corpus-based tests for file operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Corpus-based ERT tests for file operations extraction.
;; Uses corpus-semantic-file-operations.el to provide clear, example-driven test cases.
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
(require 'corpus-semantic-file-operations (expand-file-name "corpus-semantic-file-operations.el"
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

;;; Test Generation Macro

(defmacro jf/bash-test-define-corpus-tests (corpus-var test-prefix runner-fn)
  "Define individual ERT tests for each case in CORPUS-VAR.

TEST-PREFIX is a string used for test naming (e.g., \"test-corpus-\").
RUNNER-FN is the function symbol that executes the test case.

Each test case in the corpus must have :id and :note properties.
Generates one `ert-deftest' per corpus case at macro expansion time."
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (test-case)
          (let* ((test-id (plist-get test-case :id))
                 (note (or (plist-get test-case :note) ""))
                 (test-name (intern (concat test-prefix test-id))))
            `(ert-deftest ,test-name ()
               ,(format "%s: %s" test-id note)
               (,runner-fn
                (seq-find (lambda (tc) (equal (plist-get tc :id) ,test-id))
                          ,corpus-var)))))
        (symbol-value corpus-var))))

;;; Generated Corpus Tests

;; This single macro call replaces 60 manual test definitions.
;; Each corpus case in jf/bash-file-operations-test-corpus becomes
;; an individual ERT test with name test-corpus-{id}.
(jf/bash-test-define-corpus-tests
  jf/bash-file-operations-test-corpus
  "test-corpus-"
  jf/test-run-corpus-case)
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

(provide 'test-corpus-file-operations)
;;; test-corpus-file-operations.el ends here
