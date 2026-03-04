;;; test-bash-parser-semantics.el --- ERT tests for pattern-producing commands -*- lexical-binding: t; -*-

;; Tests validate pattern producer detection in the semantic database.

;;; Code:

(require 'ert)
(load-file (expand-file-name "../bash-parser.el"
                              (file-name-directory (or load-file-name buffer-file-name))))

;;; Pattern Producer Detection Tests

(ert-deftest test-pattern-producer-find ()
  "Test find is detected as pattern producer."
  (should (jf/bash--command-produces-file-list-p "find"))
  (let ((semantics (jf/bash-lookup-command-semantics "find")))
    (should (plist-get semantics :produces-file-list))
    (should (eq (plist-get semantics :pattern-source) :flag-arg))
    (should (eq (plist-get semantics :search-scope-arg) :first-positional))))

(ert-deftest test-pattern-producer-ls ()
  "Test ls is detected as pattern producer."
  (should (jf/bash--command-produces-file-list-p "ls"))
  (let ((semantics (jf/bash-lookup-command-semantics "ls")))
    (should (plist-get semantics :produces-file-list))
    (should (eq (plist-get semantics :pattern-source) :positional-args))
    (should (eq (plist-get semantics :search-scope-arg) :implicit))))

(ert-deftest test-pattern-producer-grep-with-flag ()
  "Test grep -l is pattern producer but grep without -l is not."
  (should (jf/bash--command-produces-file-list-p "grep" nil '("-l")))
  (should-not (jf/bash--command-produces-file-list-p "grep" nil '("-n")))
  (should-not (jf/bash--command-produces-file-list-p "grep" nil nil)))

(ert-deftest test-pattern-producer-grep-long-flag ()
  "Test grep --files-with-matches is pattern producer."
  (should (jf/bash--command-produces-file-list-p "grep" nil '("--files-with-matches"))))

(ert-deftest test-pattern-producer-egrep ()
  "Test egrep -l is pattern producer but egrep without -l is not."
  (should (jf/bash--command-produces-file-list-p "egrep" nil '("-l")))
  (should-not (jf/bash--command-produces-file-list-p "egrep" nil '("-n"))))

(ert-deftest test-pattern-producer-fgrep ()
  "Test fgrep -l is pattern producer but fgrep without -l is not."
  (should (jf/bash--command-produces-file-list-p "fgrep" nil '("-l")))
  (should-not (jf/bash--command-produces-file-list-p "fgrep" nil '("-n"))))

(ert-deftest test-pattern-producer-git-subcommand ()
  "Test git ls-files is pattern producer but git status is not."
  (should (jf/bash--command-produces-file-list-p "git" "ls-files"))
  (should-not (jf/bash--command-produces-file-list-p "git" "status"))
  (should-not (jf/bash--command-produces-file-list-p "git" "add"))
  (should-not (jf/bash--command-produces-file-list-p "git" "commit")))

(ert-deftest test-pattern-producer-git-ls-files-metadata ()
  "Test git ls-files has correct pattern metadata."
  (let* ((semantics (jf/bash-lookup-command-semantics "git"))
         (subcommand-handlers (plist-get semantics :subcommand-handlers))
         (ls-files-spec (cdr (assoc 'ls-files subcommand-handlers))))
    (should (plist-get ls-files-spec :produces-file-list))
    (should (eq (plist-get ls-files-spec :pattern-source) :positional-args))
    (should (eq (plist-get ls-files-spec :search-scope-arg) :implicit))))

;;; Non-Pattern Producer Tests

(ert-deftest test-not-pattern-producer-cat ()
  "Test cat is not a pattern producer."
  (should-not (jf/bash--command-produces-file-list-p "cat")))

(ert-deftest test-not-pattern-producer-rm ()
  "Test rm is not a pattern producer."
  (should-not (jf/bash--command-produces-file-list-p "rm")))

(ert-deftest test-not-pattern-producer-cp ()
  "Test cp is not a pattern producer."
  (should-not (jf/bash--command-produces-file-list-p "cp")))

(ert-deftest test-not-pattern-producer-mv ()
  "Test mv is not a pattern producer."
  (should-not (jf/bash--command-produces-file-list-p "mv")))

(ert-deftest test-not-pattern-producer-unknown ()
  "Test unknown command returns nil."
  (should-not (jf/bash--command-produces-file-list-p "unknown-command-xyz")))

;;; Metadata Validation Tests

(ert-deftest test-grep-pattern-metadata ()
  "Test grep has correct pattern metadata."
  (let ((semantics (jf/bash-lookup-command-semantics "grep")))
    (should (plist-get semantics :produces-file-list))
    (should (eq (plist-get semantics :pattern-source) :positional-args))
    (should (equal (plist-get semantics :pattern-requires-flag)
                   '("-l" "--files-with-matches")))))

(ert-deftest test-find-pattern-metadata ()
  "Test find has correct pattern metadata."
  (let ((semantics (jf/bash-lookup-command-semantics "find")))
    (should (plist-get semantics :produces-file-list))
    (should (eq (plist-get semantics :pattern-source) :flag-arg))
    (should (eq (plist-get semantics :search-scope-arg) :first-positional))))

(ert-deftest test-ls-pattern-metadata ()
  "Test ls has correct pattern metadata."
  (let ((semantics (jf/bash-lookup-command-semantics "ls")))
    (should (plist-get semantics :produces-file-list))
    (should (eq (plist-get semantics :pattern-source) :positional-args))
    (should (eq (plist-get semantics :search-scope-arg) :implicit))))

(provide 'test-bash-parser-semantics)
;;; test-bash-parser-semantics.el ends here
