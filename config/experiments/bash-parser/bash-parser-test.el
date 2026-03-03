;;; bash-parser-test.el --- Tests for bash-parser -*- lexical-binding: t; -*-

;; ERT tests for bash command parsing

(require 'ert)
(require 'bash-parser (expand-file-name "bash-parser.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-corpus (expand-file-name "bash-parser-corpus.el"
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
    (dolist (key '(:command-name :subcommand :flags :positional-args :dangerous-p :type :command-count :redirections :exec-blocks))
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

(ert-deftest jf/bash-parser-test-redirect-001 ()
  "Test: redirect-001 - echo 'hello' > output.txt"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-redirect-002 ()
  "Test: redirect-002 - cat input.txt >> output.txt"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-redirect-003 ()
  "Test: redirect-003 - grep error < logfile.txt"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-redirect-004 ()
  "Test: redirect-004 - command 2>&1"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-redirect-005 ()
  "Test: redirect-005 - git log > /dev/null 2>&1"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-005"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-redirect-006 ()
  "Test: redirect-006 - echo 'test' 2> error.log"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "redirect-006"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-001 ()
  "Test: variable-001 - echo $PATH"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-002 ()
  "Test: variable-002 - rm -rf $HOME/tmp"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-003 ()
  "Test: variable-003 - git commit -m \"$commit_message\""
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-004 ()
  "Test: variable-004 - echo ${HOME}"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-005 ()
  "Test: variable-005 - echo $VAR1 $VAR2"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-005"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-006 ()
  "Test: variable-006 - ls $HOME/*.txt"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-006"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-007 ()
  "Test: variable-007 - echo prefix$VAR"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-007"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-variable-008 ()
  "Test: variable-008 - echo $VAR/suffix"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "variable-008"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-quote-001 ()
  "Test: quote-001 - echo \"hello 'world'\""
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "quote-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-quote-003 ()
  "Test: quote-003 - git commit -m $'test\\nwith\\nnewlines'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "quote-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-quote-004 ()
  "Test: quote-004 - echo $'it\\'s working'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "quote-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-quote-005 ()
  "Test: quote-005 - echo $'line1\\nline2\\ttab'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "quote-005"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-glob-001 ()
  "Test: glob-001 - ls *.txt"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-glob-002 ()
  "Test: glob-002 - rm config/**/*.el"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-glob-003 ()
  "Test: glob-003 - git add *.{el,org}"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-glob-004 ()
  "Test: glob-004 - echo {a,b,c}"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-glob-005 ()
  "Test: glob-005 - ls file.{txt,md,json}"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-005"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-glob-006 ()
  "Test: glob-006 - rm test.{1,2,3}.bak"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "glob-006"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-background-001 ()
  "Test: background-001 - npm run build &"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "background-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-background-002 ()
  "Test: background-002 - python server.py & echo 'Server started'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "background-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-heredoc-001 ()
  "Test: heredoc-001 - cat << EOF..."
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "heredoc-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-complex-001 ()
  "Test: complex-001 - find . -name '*.log' -exec rm {} \\;"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "complex-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-find-001 ()
  "Test: find-001 - find . -type f -exec chmod 644 {} +"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-find-002 ()
  "Test: find-002 - find /tmp -name '*.tmp' -execdir rm -rf {} \\;"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-find-003 ()
  "Test: find-003 - find . -name '*.txt' -exec grep pattern {} \\; -exec echo {} \\;"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-find-004 ()
  "Test: find-004 - find . -maxdepth 2 -name '*.log'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "find-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-complex-004 ()
  "Test: complex-004 - python -c 'import sys; print(sys.version)'"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "complex-004"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-complex-005 ()
  "Test: complex-005 - sed -i.bak 's/foo/bar/g' *.txt"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "complex-005"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-wrapper-001 ()
  "Test: wrapper-001 - sudo rm -rf /tmp/test"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "wrapper-001"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-wrapper-002 ()
  "Test: wrapper-002 - sudo -u www-data php script.php"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "wrapper-002"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-wrapper-003 ()
  "Test: wrapper-003 - sudo -E env 'PATH=/custom/path' command"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "wrapper-003"))
             jf/bash-parser-test-corpus)))

(ert-deftest jf/bash-parser-test-wrapper-004 ()
  "Test: wrapper-004 - env -i HOME=$HOME command"
  (jf/bash-parser-test--run-corpus-test
   (seq-find (lambda (tc) (equal (plist-get tc :id) "wrapper-004"))
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
