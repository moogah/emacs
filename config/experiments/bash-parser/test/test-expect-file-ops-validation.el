;;; test-expect-file-ops-validation.el --- Validate :expect-file-ops in corpus -*- lexical-binding: t; -*-

;; Test that corpus files with :expect-file-ops have valid expectations

(require 'ert)
(require 'cl-lib)

;; Load corpus files
(require 'corpus-parse-command-substitution
         (expand-file-name "corpus-parse-command-substitution.el"
                          (file-name-directory load-file-name)))
(require 'corpus-parse-combined-patterns
         (expand-file-name "corpus-parse-combined-patterns.el"
                          (file-name-directory load-file-name)))
(require 'corpus-parse-conditional
         (expand-file-name "corpus-parse-conditional.el"
                          (file-name-directory load-file-name)))
(require 'corpus-parse-for-loop
         (expand-file-name "corpus-parse-for-loop.el"
                          (file-name-directory load-file-name)))
(require 'corpus-parse-heredoc
         (expand-file-name "corpus-parse-heredoc.el"
                          (file-name-directory load-file-name)))

(ert-deftest test-corpus-has-expect-file-ops ()
  "Verify corpus files contain :expect-file-ops expectations."
  (let ((corpus-vars '(jf/bash-command-substitution-corpus
                      jf/bash-combined-patterns-corpus
                      jf/bash-conditional-corpus
                      jf/bash-for-loop-corpus
                      jf/bash-heredoc-corpus))
        (total-with-expectations 0))
    (dolist (corpus-var corpus-vars)
      (let* ((corpus-cases (symbol-value corpus-var))
             (cases-with-expectations
              (seq-count (lambda (tc) (plist-get tc :expect-file-ops))
                        corpus-cases)))
        (setq total-with-expectations (+ total-with-expectations cases-with-expectations))
        (message "Corpus %s: %d/%d tests have :expect-file-ops"
                (symbol-name corpus-var)
                cases-with-expectations
                (length corpus-cases))))
    (message "Total tests with :expect-file-ops: %d" total-with-expectations)
    (should (> total-with-expectations 0))))

(ert-deftest test-expect-file-ops-structure ()
  "Verify :expect-file-ops have correct structure."
  (let ((test-case (car (seq-filter
                         (lambda (tc) (plist-get tc :expect-file-ops))
                         jf/bash-command-substitution-corpus))))
    (should test-case)
    (let ((file-ops (plist-get test-case :expect-file-ops)))
      (should (listp file-ops))
      (when file-ops
        (let ((first-op (car file-ops)))
          (should (plist-get first-op :file))
          (should (plist-get first-op :operation)))))))

(ert-deftest test-integration-tests-added ()
  "Verify integration tests were added to test-corpus-file-operations."
  (require 'test-corpus-file-operations
           (expand-file-name "test-corpus-file-operations.el"
                            (file-name-directory load-file-name)))
  (let ((integration-tests
         (seq-filter (lambda (tc)
                      (string-prefix-p "integration-"
                                      (plist-get tc :id)))
                    jf/bash-file-operations-test-corpus)))
    (message "Found %d integration tests" (length integration-tests))
    (should (>= (length integration-tests) 5))))

(provide 'test-expect-file-ops-validation)
;;; test-expect-file-ops-validation.el ends here
