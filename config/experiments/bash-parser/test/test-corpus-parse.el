;;; test-corpus-parse.el --- Tests for bash-parser corpus -*- lexical-binding: t; -*-

;; ERT tests for bash command parsing using parse-only corpus

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-corpus (expand-file-name "../bash-parser-corpus.el"
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
               ,(format "Test: %s - %s" test-id note)
               (,runner-fn
                (seq-find (lambda (tc) (equal (plist-get tc :id) ,test-id))
                          ,corpus-var)))))
        (symbol-value corpus-var))))

;;; Generated Corpus Tests

;; This single macro call replaces 60 manual test definitions.
;; Each corpus case in jf/bash-parser-test-corpus becomes
;; an individual ERT test with name jf/bash-parser-test-{id}.
(jf/bash-test-define-corpus-tests
  jf/bash-parser-test-corpus
  "jf/bash-parser-test-"
  jf/bash-parser-test--run-corpus-test)

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
