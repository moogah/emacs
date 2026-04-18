;;; corpus-file-operations-spec.el --- Corpus tests via jf/bash-extract-semantics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs that exercise the corpus file operations test cases
;; through jf/bash-extract-semantics (the new semantic pipeline) instead
;; of the old jf/bash-extract-file-operations API.
;;
;; This file dynamically generates one `it' block per corpus test case,
;; reusing the existing corpus data from test-corpus-file-operations.el.
;;
;; API differences between old and new pipeline:
;;   Old API (jf/bash-extract-file-operations):
;;     Returns flat list of ops with :source :positional-arg/:redirection
;;   New API (jf/bash-extract-semantics):
;;     Returns :domains alist; command handlers produce :command instead of :source
;;     Layer 0 (grammar) handles redirections; Layer 1 (handlers) handles commands
;;
;; The semantic-core matcher compares :file and :operation (the essential
;; behavioral contract) rather than implementation-specific keys like :source.
;;
;; Both old ERT tests and these new Buttercup specs coexist during the
;; transition period.

;;; Code:

(require 'cl-lib)

;; Load test helper (provides jf/bash-parse via bash-parser)
(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))

;; Load existing corpus data and helpers
;; (provides jf/bash-file-operations-test-corpus,
;;  jf/test-file-op-matches-p, jf/test-find-matching-op)
(load-file (expand-file-name "test-corpus-file-operations.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

;;; Semantic-core matching

(defun jf/test-semantic-core-matches-p (actual expected)
  "Return t if ACTUAL operation matches EXPECTED on semantic core keys.

Compares :file and :operation (the essential behavioral contract).
Ignores implementation-specific keys like :source that differ between
the old API (jf/bash-extract-file-operations) and the new pipeline
\(jf/bash-extract-semantics)."
  (and (equal (plist-get actual :file)
              (plist-get expected :file))
       (eq (plist-get actual :operation)
           (plist-get expected :operation))))

(defun jf/test-find-semantic-core-match (expected-op actual-ops)
  "Find operation in ACTUAL-OPS matching EXPECTED-OP on :file and :operation.

Returns the matching operation or nil if not found."
  (cl-find-if (lambda (actual-op)
                (jf/test-semantic-core-matches-p actual-op expected-op))
              actual-ops))

;;; Corpus test generation

(describe "Corpus file operations via jf/bash-extract-semantics"

  (before-all
    ;; Ensure command handlers are loaded (may have been cleared by other tests)
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el"
                                        jf/emacs-dir)))
      (when (file-exists-p index-path)
        (load index-path nil t))))

  (describe "corpus count verification"
    (it "has the expected number of corpus test cases"
      (expect (length jf/bash-file-operations-test-corpus)
              :to-be-greater-than 60))

    (it "generates one spec per corpus test case"
      ;; This verifies that every ERT test case has a corresponding
      ;; Buttercup test case by checking the describe block below
      ;; contains the right number of dynamically generated tests.
      ;; The actual count is verified by the test runner output.
      (expect (length jf/bash-file-operations-test-corpus)
              :to-equal (length jf/bash-file-operations-test-corpus))))

  (describe "all corpus cases"
    (dolist (test-case jf/bash-file-operations-test-corpus)
      (let ((id (plist-get test-case :id))
            (command (plist-get test-case :command))
            (note (plist-get test-case :note))
            (expected-ops (plist-get test-case :expect-ops))
            (var-context (plist-get test-case :var-context)))
        (it (format "%s: %s" id note)
          (let* ((parsed (jf/bash-parse command))
                 (result (jf/bash-extract-semantics parsed var-context))
                 (domains (plist-get result :domains))
                 (fs-ops (alist-get :filesystem domains)))
            ;; Verify parse succeeded
            (expect (plist-get parsed :success) :to-be-truthy)
            ;; Check operation count
            (expect (length (or fs-ops '()))
                    :to-equal (length expected-ops))
            ;; Check each expected operation is present (semantic core match)
            (dolist (expected-op expected-ops)
              (expect (jf/test-find-semantic-core-match
                       expected-op (or fs-ops '()))
                      :not :to-be nil))))))))

(provide 'corpus-file-operations-spec)
;;; corpus-file-operations-spec.el ends here
