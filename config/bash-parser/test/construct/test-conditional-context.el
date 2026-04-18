;;; test-conditional-context.el --- Tests for conditional context tracking -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, testing, conditionals

;;; Commentary:

;; Tests for conditional context tracking in the orchestrator
;; Verifies that file operations in conditionals are properly marked:
;; - Test operations ([ -f file ]) marked with :test-condition t
;; - Then branch operations marked with :conditional t :branch :then
;; - Else branch operations marked with :conditional t :branch :else
;; - Command-based tests (grep -q) marked with :test-condition t

;;; Code:

(require 'test-helper (expand-file-name "../test-helper.el"
                                        (file-name-directory load-file-name)))

(ert-deftest test-conditional-then-branch ()
  "Test operations in then branch marked as conditional."
  (let* ((parsed (jf/bash-parse "if [ -f config.yml ]; then cat config.yml; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Test reads metadata (file existence check)
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "config.yml")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))
    ;; Then branch reads content
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "config.yml")
                            (eq (plist-get op :operation) :read)
                            (plist-get op :conditional)
                            (eq (plist-get op :branch) :then)))
                     ops))))

(ert-deftest test-conditional-else-branch ()
  "Test operations in else branch marked as conditional."
  (let* ((parsed (jf/bash-parse "if [ -f old.txt ]; then cat old.txt; else echo 'missing' > log.txt; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Then branch operation
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "old.txt")
                            (eq (plist-get op :branch) :then)))
                     ops))
    ;; Else branch operation
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "log.txt")
                            (eq (plist-get op :operation) :write)
                            (eq (plist-get op :branch) :else)))
                     ops))))

(ert-deftest test-conditional-test-vs-branch ()
  "Test that test operations are distinct from branch operations."
  (let* ((parsed (jf/bash-parse "if [ -f test.txt ]; then rm test.txt; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have two operations for test.txt
    (let ((test-txt-ops (seq-filter (lambda (op)
                                      (string= (plist-get op :file) "test.txt"))
                                   ops)))
      ;; One test operation
      (should (seq-find (lambda (op)
                         (and (eq (plist-get op :operation) :read-metadata)
                              (plist-get op :test-condition)
                              (not (plist-get op :conditional))))
                       test-txt-ops))
      ;; One branch operation
      (should (seq-find (lambda (op)
                         (and (eq (plist-get op :operation) :delete)
                              (plist-get op :conditional)
                              (not (plist-get op :test-condition))))
                       test-txt-ops)))))

(ert-deftest test-conditional-with-simple-path ()
  "Test conditional with simple file path in test."
  (let* ((parsed (jf/bash-parse "if [ -f /usr/bin/emacs ]; then echo 'found'; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; File test operation should be in test context
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "/usr/bin/emacs")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))))

(ert-deftest test-conditional-command-based ()
  "Test conditional using command exit status."
  (let* ((parsed (jf/bash-parse "if grep -q pattern file.txt; then echo 'found'; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; grep reads file in test context
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "file.txt")
                            (eq (plist-get op :operation) :read)
                            (plist-get op :test-condition)))
                     ops))))

(ert-deftest test-conditional-directory-test ()
  "Test directory test operator."
  (let* ((parsed (jf/bash-parse "if [ -d mydir ]; then cat mydir/file.txt; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Directory test
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "mydir")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))
    ;; cat in then branch
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "mydir/file.txt")
                            (eq (plist-get op :operation) :read)
                            (plist-get op :conditional)
                            (eq (plist-get op :branch) :then)))
                     ops))))

(ert-deftest test-conditional-exists-test ()
  "Test file exists operator."
  (let* ((parsed (jf/bash-parse "if [ -e somepath ]; then cat somepath; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Exists test
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "somepath")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))))

(ert-deftest test-conditional-readable-test ()
  "Test readable file operator."
  (let* ((parsed (jf/bash-parse "if [ -r data.txt ]; then head data.txt; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Readable test
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "data.txt")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))))

(ert-deftest test-conditional-writable-test ()
  "Test writable file operator."
  (let* ((parsed (jf/bash-parse "if [ -w log.txt ]; then echo 'data' >> log.txt; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Writable test
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "log.txt")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))
    ;; Append in then branch
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "log.txt")
                            (eq (plist-get op :operation) :append)
                            (plist-get op :conditional)))
                     ops))))

(ert-deftest test-conditional-executable-test ()
  "Test executable file operator."
  (let* ((parsed (jf/bash-parse "if [ -x /usr/bin/emacs ]; then /usr/bin/emacs; fi"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Executable test
    (should (seq-find (lambda (op)
                       (and (string= (plist-get op :file) "/usr/bin/emacs")
                            (eq (plist-get op :operation) :read-metadata)
                            (plist-get op :test-condition)))
                     ops))))

(provide 'test-conditional-context)
;;; test-conditional-context.el ends here
