;;; test-command-substitution.el --- Tests for command substitution parsing -*- lexical-binding: t; -*-

;; ERT tests for bash command substitution detection and extraction
;; Tests cover: $(...) syntax, backtick syntax, nesting, quoting, pipes

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))
(require 'corpus-parse-command-substitution (expand-file-name "corpus-parse-command-substitution.el"
                                                              (file-name-directory load-file-name)))

;;; Helper Functions

(defun jf/bash-parser-test-cmdsub--run-corpus-test (test-case)
  "Run a single command substitution test case from corpus.
TEST-CASE is a plist with :id, :command, :category, :expect, and :notes."
  (let* ((test-id (plist-get test-case :id))
         (command (plist-get test-case :command))
         (category (plist-get test-case :category))
         (expected (plist-get test-case :expect))
         (notes (plist-get test-case :notes))
         (result (jf/bash-parse command)))

    ;; Check that parsing succeeded
    (should (plist-get result :success))

    ;; Check basic command structure fields
    (dolist (key '(:command-name :subcommand :flags :positional-args))
      (when (plist-member expected key)
        (let ((expected-val (plist-get expected key))
              (actual-val (plist-get result key)))
          (unless (equal expected-val actual-val)
            (ert-fail (format "Test %s (%s) failed for %s: expected %S, got %S\nNotes: %s"
                              test-id category key expected-val actual-val notes))))))

    ;; Check command substitution extraction (if implemented)
    (when (plist-member expected :command-substitutions)
      (let ((expected-subs (plist-get expected :command-substitutions))
            (actual-subs (plist-get result :command-substitutions)))
        (unless (equal expected-subs actual-subs)
          (ert-fail (format "Test %s (%s) failed for :command-substitutions: expected %S, got %S\nNotes: %s"
                            test-id category expected-subs actual-subs notes)))))))

(defun jf/bash-parser-test-cmdsub--find-test (test-id)
  "Find test case by TEST-ID in corpus."
  (seq-find (lambda (tc) (equal (plist-get tc :id) test-id))
            jf/bash-command-substitution-corpus))

;;; Test Suite

;; ============================================================
;; TIER 1: SIMPLE COMMAND SUBSTITUTION
;; ============================================================

(ert-deftest test-cmdsub-simple-pwd ()
  "Test simple command substitution: $(pwd)
Scenario: bash-parser § 'Simple command substitution with $()'
Verifies basic $() syntax is detected and extracted."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-simple-001")))

(ert-deftest test-cmdsub-simple-date ()
  "Test simple command substitution: $(date)
Scenario: bash-parser § 'Simple command substitution with $()'
Common pattern for capturing current date/time."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-simple-002")))

(ert-deftest test-cmdsub-simple-whoami ()
  "Test simple command substitution: $(whoami)
Scenario: bash-parser § 'Simple command substitution with $()'
User identity capture pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-simple-003")))

(ert-deftest test-cmdsub-variable-assignment ()
  "Test command substitution in variable assignment: dir=$(pwd)
Scenario: bash-parser § 'Command substitution in assignment'
Common pattern for capturing command output to variable."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-simple-004")))

(ert-deftest test-cmdsub-with-pipe-inside ()
  "Test command substitution containing pipe: $(ls -1 | wc -l)
Scenario: bash-parser § 'Pipe inside command substitution'
Verifies parser handles pipe inside $() correctly."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-simple-005")))

(ert-deftest test-cmdsub-backtick-syntax ()
  "Test legacy backtick command substitution: `pwd`
Scenario: bash-parser § 'Backtick command substitution'
Legacy syntax still common in scripts."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-backtick-001")))

;; ============================================================
;; TIER 2: NESTED SUBSTITUTION
;; ============================================================

(ert-deftest test-cmdsub-nested-dirname-which ()
  "Test nested command substitution: $(dirname $(which openspec))
Scenario: bash-parser § 'Nested command substitution'
REAL EXAMPLE from research - 2-level nesting for directory inspection."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-nested-001")))

(ert-deftest test-cmdsub-nested-basename ()
  "Test nested command substitution: $(basename $(pwd))
Scenario: bash-parser § 'Nested command substitution'
Common pattern to get current directory name."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-nested-002")))

(ert-deftest test-cmdsub-nested-find ()
  "Test substitution with find command: cat $(find . -name config.yml)
Scenario: bash-parser § 'Command substitution with find'
Find file then read it - common pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-nested-003")))

(ert-deftest test-cmdsub-triple-nesting ()
  "Test 3-level nested substitution: $(dirname $(dirname $(pwd)))
Scenario: bash-parser § 'Deep nesting (3 levels)'
Tests parser's ability to handle multiple nesting levels."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-nested-004")))

;; ============================================================
;; TIER 2: SUBSTITUTION IN QUOTES
;; ============================================================

(ert-deftest test-cmdsub-in-double-quotes ()
  "Test substitution inside double quotes: \"Current time: $(date)\"
Scenario: bash-parser § 'Command substitution in double quotes'
String interpolation pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-quoted-001")))

(ert-deftest test-cmdsub-nested-quotes ()
  "Test substitution with nested quotes: \"=== $(basename \"$dir\") ===\"
Scenario: bash-parser § 'Nested quotes with substitution'
REAL EXAMPLE from research - mixed quote levels."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-quoted-002")))

(ert-deftest test-cmdsub-git-commit-message ()
  "Test substitution in git commit: git commit -m \"Update $(date +%Y-%m-%d)\"
Scenario: bash-parser § 'Command substitution in commit message'
Common pattern for timestamped commits."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-quoted-003")))

(ert-deftest test-cmdsub-single-quotes-literal ()
  "Test single quotes prevent substitution: echo 'Literal $(date)'
Scenario: bash-parser § 'Single quotes make substitution literal'
Verifies parser knows single quotes disable substitution."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-quoted-004")))

;; ============================================================
;; TIER 2: PIPES AND REDIRECTS IN SUBSTITUTION
;; ============================================================

(ert-deftest test-cmdsub-find-pipe-wc ()
  "Test substitution with pipe: \"$(find \"$dir\" -name \"*.org\" | wc -l) files\"
Scenario: bash-parser § 'Pipe inside command substitution'
REAL EXAMPLE from research - count files matching pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-pipe-001")))

(ert-deftest test-cmdsub-cat-wc ()
  "Test substitution with cat and wc: count=$(cat file.txt | wc -l)
Scenario: bash-parser § 'Pipe inside command substitution'
Line count capture pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-pipe-002")))

(ert-deftest test-cmdsub-multi-stage-pipe ()
  "Test multi-stage pipeline in substitution: $(ls | grep test | head -1)
Scenario: bash-parser § 'Multiple pipes in substitution'
Three-stage pipeline inside $()."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-pipe-003")))

(ert-deftest test-cmdsub-find-sort ()
  "Test find with sort: files=$(find . -type f | sort)
Scenario: bash-parser § 'Pipe inside command substitution'
Common file discovery pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-pipe-004")))

;; ============================================================
;; TIER 2: MULTIPLE SUBSTITUTIONS
;; ============================================================

(ert-deftest test-cmdsub-two-independent ()
  "Test two independent substitutions: echo $(pwd) $(date)
Scenario: bash-parser § 'Multiple independent substitutions'
Both should be detected and extracted separately."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-multiple-001")))

(ert-deftest test-cmdsub-cp-two-which ()
  "Test two substitutions as arguments: cp $(which oldcmd) $(which newcmd)
Scenario: bash-parser § 'Multiple substitutions as arguments'
Source and destination both from command substitution."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-multiple-002")))

(ert-deftest test-cmdsub-three-in-string ()
  "Test three substitutions in string: \"User: $(whoami) at $(hostname) on $(date)\"
Scenario: bash-parser § 'Multiple substitutions in formatted string'
String interpolation with three substitutions."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-multiple-003")))

(ert-deftest test-cmdsub-two-with-globs ()
  "Test substitutions with glob patterns: diff $(ls *.old) $(ls *.new)
Scenario: bash-parser § 'Multiple substitutions with globs'
Glob patterns inside substitutions."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-multiple-004")))

;; ============================================================
;; TIER 3: COMPLEX REAL-WORLD PATTERNS
;; ============================================================

(ert-deftest test-cmdsub-for-loop-find ()
  "Test substitution feeding for loop: for file in $(find . -name \"*.el\" | sort)
Scenario: bash-parser § 'Command substitution in for loop'
REAL EXAMPLE from research - common iteration pattern."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-complex-001")))

(ert-deftest test-cmdsub-for-loop-basename ()
  "Test substitution in loop body: for dir in */; do echo \"=== $(basename \"$dir\") ===\"
Scenario: bash-parser § 'Command substitution in for loop body'
REAL EXAMPLE from research - loop with basename."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-complex-002")))

(ert-deftest test-cmdsub-in-test-conditional ()
  "Test substitution in test command: test -f $(which emacs) && echo \"Found\"
Scenario: bash-parser § 'Command substitution in test command'
Conditional execution based on command output."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-complex-003")))

(ert-deftest test-cmdsub-conditional-inside ()
  "Test conditional inside substitution: result=$(if [ -f test ]; then cat test; fi)
Scenario: bash-parser § 'Conditional inside command substitution'
Complex control flow inside $()."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-complex-004")))

;; ============================================================
;; TIER 3: EDGE CASES
;; ============================================================

(ert-deftest test-cmdsub-in-arithmetic ()
  "Test substitution in arithmetic: echo $(($(date +%s) + 3600))
Scenario: bash-parser § 'Command substitution in arithmetic expansion'
Nested syntax: $(()) contains $()."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-edge-001")))

(ert-deftest test-cmdsub-in-heredoc ()
  "Test substitution in heredoc: cat <<EOF\\nLine with $(date)\\nEOF
Scenario: bash-parser § 'Command substitution in heredoc'
Substitution within here-document."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-edge-002")))

(ert-deftest test-cmdsub-nested-backticks ()
  "Test nested backticks: echo `echo \\`date\\``
Scenario: bash-parser § 'Nested backticks with escaping'
Legacy syntax with escaping for nesting."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-edge-003")))

(ert-deftest test-cmdsub-empty ()
  "Test empty substitution: echo $()
Scenario: bash-parser § 'Empty command substitution'
Edge case: no command inside $()."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-edge-004")))

(ert-deftest test-cmdsub-escaped ()
  "Test escaped substitution: echo \\$(not-a-substitution)
Scenario: bash-parser § 'Escaped substitution is literal'
Backslash should prevent substitution."
  (jf/bash-parser-test-cmdsub--run-corpus-test
   (jf/bash-parser-test-cmdsub--find-test "cmdsub-edge-005")))

;;; Summary Function

(defun jf/bash-parser-test-cmdsub-summary ()
  "Display summary of command substitution test coverage."
  (interactive)
  (let* ((total (length jf/bash-command-substitution-corpus))
         (categories '(("simple" . 0) ("backtick" . 0) ("nested" . 0)
                      ("quoted" . 0) ("pipe" . 0) ("multiple" . 0)
                      ("complex" . 0) ("edge" . 0))))

    (dolist (test-case jf/bash-command-substitution-corpus)
      (let* ((cat (plist-get test-case :category))
             (entry (assoc cat categories)))
        (when entry
          (setcdr entry (1+ (cdr entry))))))

    (message "Command Substitution Test Coverage:\n%s\nTotal: %d tests"
             (mapconcat (lambda (cat)
                         (format "  %s: %d" (car cat) (cdr cat)))
                       categories "\n")
             total)))

(provide 'test-command-substitution)
;;; test-command-substitution.el ends here
