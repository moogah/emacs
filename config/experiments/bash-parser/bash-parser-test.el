;;; bash-parser-test.el --- Tests for bash-parser -*- lexical-binding: t; -*-

;; ERT tests for bash command parsing

(require 'ert)
(require 'bash-parser (expand-file-name "bash-parser.el"
                                        (file-name-directory load-file-name)))
(require 'test-corpus (expand-file-name "test-corpus.el"
                                        (file-name-directory load-file-name)))

;;; Helper Functions

(defun jf/bash-parser-test--run-corpus-test (test-case)
  "Run a single test case from corpus.
TEST-CASE is a plist with :id, :command, and :expect."
  (let* ((test-id (plist-get test-case :id))
         (command (plist-get test-case :command))
         (expected (plist-get test-case :expect))
         (result (jf/bash-parse command)))

    ;; Check that parsing succeeded
    (should (plist-get result :success))

    ;; Check each expected field
    (dolist (key '(:command-name :subcommand :flags :positional-args :dangerous-p :type :command-count))
      (when (plist-member expected key)
        (let ((expected-val (plist-get expected key))
              (actual-val (plist-get result key)))
          (unless (equal expected-val actual-val)
            (error "Test %s failed for %s: expected %S, got %S"
                   test-id key expected-val actual-val)))))

    ;; Check :all-commands if present (for pipelines and chains)
    (when (plist-member expected :all-commands)
      (let ((expected-commands (plist-get expected :all-commands))
            (actual-commands (plist-get result :all-commands)))
        (unless (equal expected-commands actual-commands)
          (error "Test %s failed for :all-commands: expected %S, got %S"
                 test-id expected-commands actual-commands))))))

;;; Corpus-Based Tests

(ert-deftest jf/bash-parser-test-simple-001 ()
  "Test: simple-001 - ls"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "simple-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-simple-002 ()
  "Test: simple-002 - ls -la"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "simple-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-simple-003 ()
  "Test: simple-003 - ls -la /tmp"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "simple-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-simple-004 ()
  "Test: simple-004 - grep -r 'pattern' ./config"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "simple-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-git-001 ()
  "Test: git-001 - git log"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-git-002 ()
  "Test: git-002 - git log --oneline -10"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-git-003 ()
  "Test: git-003 - git add config/gptel/scope.el"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-git-004 ()
  "Test: git-004 - git commit -m 'test message'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-git-005 ()
  "Test: git-005 - git push --force"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "git-005"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-dangerous-001 ()
  "Test: dangerous-001 - rm -rf /tmp/test"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "dangerous-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-dangerous-002 ()
  "Test: dangerous-002 - git reset --hard HEAD~1"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "dangerous-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-pipeline-001 ()
  "Test: pipeline-001 - ls -la | grep test"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "pipeline-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-pipeline-002 ()
  "Test: pipeline-002 - cat file.txt | sort | uniq -c"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "pipeline-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-pipeline-003 ()
  "Test: pipeline-003 - git log --oneline | head -10"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "pipeline-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-chain-001 ()
  "Test: chain-001 - git add . && git commit -m 'test'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-chain-002 ()
  "Test: chain-002 - rm file.txt || echo 'failed to delete'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-chain-003 ()
  "Test: chain-003 - cd /tmp; ls -la; pwd"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-chain-004 ()
  "Test: chain-004 - git add . && git commit -m 'fix' && git push"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "chain-004"))
             jf/bash-parser-test-corpus)))

;;; Manual Tests (for debugging specific cases)

(ert-deftest jf/bash-parser-test-manual-basic ()
  "Manual test: basic command parsing."
  (let ((result (jf/bash-parse "ls -la /tmp")))
    (should (plist-get result :success))
    (should (equal (plist-get result :command-name) "ls"))
    (should (member "-la" (plist-get result :flags)))
    (should (member "/tmp" (plist-get result :positional-args)))))

(ert-deftest jf/bash-parser-test-manual-git-subcommand ()
  "Manual test: git subcommand detection."
  (let ((result (jf/bash-parse "git log --oneline")))
    (should (plist-get result :success))
    (should (equal (plist-get result :command-name) "git"))
    (should (equal (plist-get result :subcommand) "log"))
    (should (member "--oneline" (plist-get result :flags)))))

(ert-deftest jf/bash-parser-test-manual-dangerous-rm ()
  "Manual test: dangerous rm -rf detection."
  (let ((result (jf/bash-parse "rm -rf /tmp/test")))
    (should (plist-get result :success))
    (should (equal (plist-get result :command-name) "rm"))
    (should (plist-get result :dangerous-p))))

(ert-deftest jf/bash-parser-test-manual-dangerous-git-push ()
  "Manual test: dangerous git push --force detection."
  (let ((result (jf/bash-parse "git push --force")))
    (should (plist-get result :success))
    (should (equal (plist-get result :command-name) "git"))
    (should (equal (plist-get result :subcommand) "push"))
    (should (plist-get result :dangerous-p))))

;;; Interactive Test Runner

(defun jf/bash-parser-run-all-tests ()
  "Run all bash-parser tests and display results."
  (interactive)
  (ert-run-tests-interactively "jf/bash-parser-test-"))

(defun jf/bash-parser-test-command (command)
  "Interactively test parsing of COMMAND string."
  (interactive "sCommand to parse: ")
  (let ((result (jf/bash-parse command)))
    (with-current-buffer (get-buffer-create "*bash-parser-test*")
      (erase-buffer)
      (insert (format "Command: %s\n\n" command))
      (insert "Parse Result:\n")
      (insert (pp-to-string result))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'bash-parser-test)
;;; bash-parser-test.el ends here
