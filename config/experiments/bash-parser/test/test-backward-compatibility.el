;;; test-backward-compatibility.el --- Backward compatibility tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Backward compatibility tests for recursive analyzer integration.
;; Ensures that the new recursive analyzer maintains compatibility with
;; existing code that uses jf/bash-extract-file-operations.
;;
;; Key invariants:
;; - Simple commands still return simple results (core fields preserved)
;; - Existing operation types remain unchanged
;; - Existing confidence levels remain unchanged
;; - New context flags are additive (don't break existing code)

;;; Code:

(require 'ert)
(require 'bash-parser)

;;; Simple Command Backward Compatibility

(ert-deftest test-backward-compat-simple-read ()
  "Verify recursive analyzer maintains backward compatibility for simple read commands.

Simple read commands should return the same core fields as before:
:file, :operation, :confidence, :source, :command

New context flags like :pattern, :loop-context, etc. are optional additions."
  (let* ((parsed (jf/bash-parse "cat file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      ;; Core fields must be present and correct
      (should (equal (plist-get op :file) "file.txt"))
      (should (eq (plist-get op :operation) :read))
      (should (eq (plist-get op :confidence) :high))
      (should (eq (plist-get op :source) :positional-arg))
      (should (equal (plist-get op :command) "cat")))))

(ert-deftest test-backward-compat-simple-write ()
  "Verify simple write commands maintain backward compatibility."
  (let* ((parsed (jf/bash-parse "echo 'test' > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have exactly one write operation
    (let ((write-op (cl-find-if (lambda (op)
                                   (eq (plist-get op :operation) :write))
                                 ops)))
      (should write-op)
      (should (equal (plist-get write-op :file) "output.txt"))
      (should (eq (plist-get write-op :confidence) :high))
      (should (eq (plist-get write-op :source) :redirection)))))

(ert-deftest test-backward-compat-simple-delete ()
  "Verify simple delete commands maintain backward compatibility."
  (let* ((parsed (jf/bash-parse "rm temp.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      (should (equal (plist-get op :file) "temp.txt"))
      (should (eq (plist-get op :operation) :delete))
      (should (eq (plist-get op :confidence) :high))
      (should (eq (plist-get op :source) :positional-arg))
      (should (equal (plist-get op :command) "rm")))))

(ert-deftest test-backward-compat-copy-command ()
  "Verify cp command extracts both read and write operations."
  (let* ((parsed (jf/bash-parse "cp src.txt dst.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 2))
    ;; Check read operation
    (let ((read-op (cl-find-if (lambda (op)
                                  (and (equal (plist-get op :file) "src.txt")
                                       (eq (plist-get op :operation) :read)))
                                ops)))
      (should read-op)
      (should (eq (plist-get read-op :confidence) :high)))
    ;; Check write operation
    (let ((write-op (cl-find-if (lambda (op)
                                   (and (equal (plist-get op :file) "dst.txt")
                                        (eq (plist-get op :operation) :write)))
                                 ops)))
      (should write-op)
      (should (eq (plist-get write-op :confidence) :high)))))

(ert-deftest test-backward-compat-pipeline ()
  "Verify pipeline commands extract operations from all commands."
  (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern > output.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have read from file.txt and write to output.txt
    (should (>= (length ops) 2))
    ;; Check read operation
    (should (cl-find-if (lambda (op)
                          (and (equal (plist-get op :file) "file.txt")
                               (eq (plist-get op :operation) :read)))
                        ops))
    ;; Check write operation
    (should (cl-find-if (lambda (op)
                          (and (equal (plist-get op :file) "output.txt")
                               (eq (plist-get op :operation) :write)))
                        ops))))

;;; Variable Resolution Backward Compatibility

(ert-deftest test-backward-compat-variable-resolution ()
  "Verify variable resolution works with recursive analyzer."
  (let* ((parsed (jf/bash-parse "cat $WORKSPACE/file.txt"))
         (var-context '((WORKSPACE . "/workspace")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      ;; Variable should be resolved
      (should (equal (plist-get op :file) "/workspace/file.txt"))
      (should (eq (plist-get op :operation) :read))
      (should (eq (plist-get op :confidence) :high)))))

(ert-deftest test-backward-compat-unresolved-variables ()
  "Verify unresolved variables are tracked correctly."
  (let* ((parsed (jf/bash-parse "cat $UNKNOWN/file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      ;; File path should contain unresolved variable
      (should (string-match-p "\\$UNKNOWN" (plist-get op :file)))
      (should (eq (plist-get op :operation) :read)))))

;;; Test Corpus Compatibility

(ert-deftest test-backward-compat-corpus-read-operations ()
  "Verify corpus test cases for read operations still pass."
  (let ((test-cases '(("cat /workspace/foo.txt"
                       ((:file "/workspace/foo.txt" :operation :read)))
                      ("grep pattern file.txt"
                       ((:file "file.txt" :operation :read)))
                      ("head -n 10 /tmp/log.txt"
                       ((:file "/tmp/log.txt" :operation :read))))))
    (dolist (case test-cases)
      (let* ((command (car case))
             (expected-ops (cadr case))
             (parsed (jf/bash-parse command))
             (actual-ops (jf/bash-extract-file-operations parsed)))
        ;; Verify expected operations are present
        (dolist (expected-op expected-ops)
          (should (cl-find-if (lambda (actual-op)
                                (and (equal (plist-get actual-op :file)
                                           (plist-get expected-op :file))
                                     (eq (plist-get actual-op :operation)
                                        (plist-get expected-op :operation))))
                              actual-ops)))))))

(ert-deftest test-backward-compat-corpus-write-operations ()
  "Verify corpus test cases for write operations still pass."
  (let ((test-cases '(("echo 'content' > output.txt"
                       ((:file "output.txt" :operation :write)))
                      ("cat input.txt >> output.txt"
                       ((:file "input.txt" :operation :read)
                        (:file "output.txt" :operation :append))))))
    (dolist (case test-cases)
      (let* ((command (car case))
             (expected-ops (cadr case))
             (parsed (jf/bash-parse command))
             (actual-ops (jf/bash-extract-file-operations parsed)))
        ;; Verify expected operations are present
        (dolist (expected-op expected-ops)
          (should (cl-find-if (lambda (actual-op)
                                (and (equal (plist-get actual-op :file)
                                           (plist-get expected-op :file))
                                     (eq (plist-get actual-op :operation)
                                        (plist-get expected-op :operation))))
                              actual-ops)))))))

(ert-deftest test-backward-compat-corpus-delete-operations ()
  "Verify corpus test cases for delete operations still pass."
  (let ((test-cases '(("rm temp.txt"
                       ((:file "temp.txt" :operation :delete)))
                      ("rm file1.txt file2.txt"
                       ((:file "file1.txt" :operation :delete)
                        (:file "file2.txt" :operation :delete))))))
    (dolist (case test-cases)
      (let* ((command (car case))
             (expected-ops (cadr case))
             (parsed (jf/bash-parse command))
             (actual-ops (jf/bash-extract-file-operations parsed)))
        ;; Verify expected operations are present
        (dolist (expected-op expected-ops)
          (should (cl-find-if (lambda (actual-op)
                                (and (equal (plist-get actual-op :file)
                                           (plist-get expected-op :file))
                                     (eq (plist-get actual-op :operation)
                                        (plist-get expected-op :operation))))
                              actual-ops)))))))

;;; Feature Detection Tests

(ert-deftest test-feature-detection-recursive-analysis ()
  "Verify feature detection for recursive analysis."
  (should (jf/bash-parser-has-feature-p :recursive-analysis)))

(ert-deftest test-feature-detection-pattern-flow ()
  "Verify feature detection for pattern flow tracking."
  ;; Pattern flow is fully implemented and all tests pass
  (should (jf/bash-parser-has-feature-p :pattern-flow)))

(ert-deftest test-feature-detection-unknown-feature ()
  "Verify unknown features return nil."
  (should-not (jf/bash-parser-has-feature-p :nonexistent-feature)))

(provide 'test-backward-compatibility)
;;; test-backward-compatibility.el ends here
