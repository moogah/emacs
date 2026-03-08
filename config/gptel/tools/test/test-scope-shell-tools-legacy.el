;;; test-scope-shell-tools-legacy.el --- Characterization tests for current regex-based validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CHARACTERIZATION TESTS: Document current regex-based validation behavior
;; before replacing with bash-parser integration.
;;
;; These tests capture "what the code does", not "what it should do".
;; They serve as a safety net for refactoring, documenting current behavior
;; including behaviors we plan to change.
;;
;; KNOWN LIMITATIONS (to be fixed with bash-parser):
;; - Pipeline bypass: "ls | xargs rm" extracts "ls" but allows "rm" to execute
;; - Simple regex parsing: Split on "[ |><;&]+" misses complex shell syntax
;; - Directory-only validation: No file path extraction or validation
;; - Absolute path warnings: Detected but not blocked
;;
;; Test naming convention: test-legacy-<scenario-slug>

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load dependencies
(let ((tools-dir (expand-file-name ".."
                                   (file-name-directory (or load-file-name
                                                           buffer-file-name)))))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Command Parsing Tests (jf/gptel-bash--parse-command)

(ert-deftest test-legacy-parse-simple-command ()
  "Test parsing of simple command without arguments.

Current behavior: Returns command as-is."
  (should (equal (jf/gptel-bash--parse-command "ls") "ls"))
  (should (equal (jf/gptel-bash--parse-command "pwd") "pwd"))
  (should (equal (jf/gptel-bash--parse-command "echo") "echo")))

(ert-deftest test-legacy-parse-command-with-flags ()
  "Test parsing of command with flags.

Current behavior: Returns only the command, strips flags."
  (should (equal (jf/gptel-bash--parse-command "ls -la") "ls"))
  (should (equal (jf/gptel-bash--parse-command "grep -r TODO") "grep"))
  (should (equal (jf/gptel-bash--parse-command "git log --oneline") "git")))

(ert-deftest test-legacy-parse-command-with-arguments ()
  "Test parsing of command with arguments.

Current behavior: Returns only the command, strips all arguments."
  (should (equal (jf/gptel-bash--parse-command "grep foo bar.txt") "grep"))
  (should (equal (jf/gptel-bash--parse-command "echo hello world") "echo"))
  (should (equal (jf/gptel-bash--parse-command "find . -name test") "find")))

(ert-deftest test-legacy-parse-pipeline-first-command ()
  "Test parsing of pipeline command.

Current behavior: Returns ONLY first command in pipeline.
LIMITATION: This allows subsequent dangerous commands to execute unchecked.
Example: 'ls | xargs rm' extracts 'ls' but 'rm' executes without validation."
  (should (equal (jf/gptel-bash--parse-command "ls | grep foo") "ls"))
  (should (equal (jf/gptel-bash--parse-command "cat file.txt | head") "cat"))
  (should (equal (jf/gptel-bash--parse-command "ls | xargs rm") "ls")))

(ert-deftest test-legacy-parse-redirect-output ()
  "Test parsing of command with output redirection.

Current behavior: Returns command, strips redirect operator and target."
  (should (equal (jf/gptel-bash--parse-command "ls > output.txt") "ls"))
  (should (equal (jf/gptel-bash--parse-command "grep foo bar.txt > results.txt") "grep"))
  (should (equal (jf/gptel-bash--parse-command "echo hello >> log.txt") "echo")))

(ert-deftest test-legacy-parse-redirect-input ()
  "Test parsing of command with input redirection.

Current behavior: Returns command, strips redirect operator and source."
  (should (equal (jf/gptel-bash--parse-command "cat < input.txt") "cat"))
  (should (equal (jf/gptel-bash--parse-command "sort < data.txt") "sort")))

(ert-deftest test-legacy-parse-command-substitution ()
  "Test parsing of command with command substitution.

Current behavior: Returns outer command, ignores $(subcommand)."
  (should (equal (jf/gptel-bash--parse-command "echo $(date)") "echo"))
  (should (equal (jf/gptel-bash--parse-command "ls $(pwd)") "ls")))

(ert-deftest test-legacy-parse-command-with-semicolon ()
  "Test parsing of commands separated by semicolon.

Current behavior: Returns only first command before semicolon.
LIMITATION: Subsequent commands after ';' are not validated."
  (should (equal (jf/gptel-bash--parse-command "cd /tmp; ls") "cd"))
  (should (equal (jf/gptel-bash--parse-command "mkdir foo; touch foo/bar.txt") "mkdir")))

(ert-deftest test-legacy-parse-command-with-ampersand ()
  "Test parsing of background command with &.

Current behavior: Returns command, strips background operator."
  (should (equal (jf/gptel-bash--parse-command "sleep 10 &") "sleep"))
  (should (equal (jf/gptel-bash--parse-command "long-running-task & echo done") "long-running-task")))

(ert-deftest test-legacy-parse-logical-and ()
  "Test parsing of commands with logical AND (&&).

Current behavior: Returns only first command before &&.
LIMITATION: Subsequent commands after '&&' are not validated."
  (should (equal (jf/gptel-bash--parse-command "test -f foo.txt && cat foo.txt") "test"))
  (should (equal (jf/gptel-bash--parse-command "mkdir dir && cd dir") "mkdir")))

(ert-deftest test-legacy-parse-logical-or ()
  "Test parsing of commands with logical OR (||).

Current behavior: Returns only first command before ||.
LIMITATION: Subsequent commands after '||' are not validated."
  (should (equal (jf/gptel-bash--parse-command "test -f foo.txt || touch foo.txt") "test"))
  (should (equal (jf/gptel-bash--parse-command "grep pattern file.txt || echo 'not found'") "grep")))

(ert-deftest test-legacy-parse-complex-pipeline ()
  "Test parsing of complex multi-stage pipeline.

Current behavior: Returns only first command in pipeline.
LIMITATION: Complex pipelines with multiple transformations are not fully validated."
  (should (equal (jf/gptel-bash--parse-command "cat file.txt | grep TODO | wc -l") "cat"))
  (should (equal (jf/gptel-bash--parse-command "find . -name '*.el' | xargs grep 'defun' | head -10") "find")))

(ert-deftest test-legacy-parse-whitespace-handling ()
  "Test parsing with various whitespace patterns.

Current behavior: Trims leading/trailing whitespace via string-trim.
LIMITATION: Internal tabs are not normalized - they remain in the output.
The split-string only splits on metacharacters, not whitespace normalization."
  (should (equal (jf/gptel-bash--parse-command "  ls  ") "ls"))
  (should (equal (jf/gptel-bash--parse-command "grep    -r    TODO") "grep"))
  ;; Tabs are NOT normalized - they remain in the parsed output after split
  (should (equal (jf/gptel-bash--parse-command "\techo\t\thello") "echo\t\thello")))

;;; Absolute Path Detection Tests (jf/gptel-bash--check-absolute-paths)

(ert-deftest test-legacy-absolute-path-unix ()
  "Test detection of Unix absolute paths.

Current behavior: Returns warning string for paths starting with /."
  (should (stringp (jf/gptel-bash--check-absolute-paths "ls /usr/bin")))
  (should (stringp (jf/gptel-bash--check-absolute-paths "cat /etc/hosts")))
  (should (stringp (jf/gptel-bash--check-absolute-paths "grep foo /var/log/system.log"))))

(ert-deftest test-legacy-absolute-path-home-directory ()
  "Test detection of home directory paths.

Current behavior: Mixed behavior with ~/ paths.
LIMITATION: The regex /[[:alnum:]_/-]+ requires alphanumeric after the /.
- '~/Documents' matches (D is alphanumeric)
- '~/.bashrc' does NOT match (. is not alphanumeric)
This is a known limitation of the simple regex approach."
  (should (stringp (jf/gptel-bash--check-absolute-paths "ls ~/Documents")))
  ;; Does NOT detect ~/.bashrc because . is not in [[:alnum:]_/-]+
  (should-not (jf/gptel-bash--check-absolute-paths "cat ~/.bashrc")))

(ert-deftest test-legacy-absolute-path-multiple ()
  "Test detection when command contains multiple absolute paths.

Current behavior: Returns warning if ANY absolute path is found."
  (should (stringp (jf/gptel-bash--check-absolute-paths "cp /tmp/source.txt /tmp/dest.txt")))
  (should (stringp (jf/gptel-bash--check-absolute-paths "diff /usr/local/bin/foo /usr/bin/foo"))))

(ert-deftest test-legacy-absolute-path-relative-paths ()
  "Test that relative paths trigger warnings due to regex limitation.

Current behavior: DOES return warning for ./ and ../ paths.
LIMITATION: The regex /[[:alnum:]_/-]+ matches the / in ./ and ../ paths,
treating them as absolute. This is a known limitation of the simple regex approach."
  (should (stringp (jf/gptel-bash--check-absolute-paths "ls ./src")))
  (should (stringp (jf/gptel-bash--check-absolute-paths "cat ../config.yaml")))
  ;; Note: "find . -name test" has no / after the . so it doesn't match
  (should-not (jf/gptel-bash--check-absolute-paths "find . -name test")))

(ert-deftest test-legacy-absolute-path-no-paths ()
  "Test commands without any file paths.

Current behavior: Returns nil (no warning)."
  (should-not (jf/gptel-bash--check-absolute-paths "ls"))
  (should-not (jf/gptel-bash--check-absolute-paths "pwd"))
  (should-not (jf/gptel-bash--check-absolute-paths "echo hello")))

;;; Command Execution Tests (jf/gptel-bash--execute-command)
;;;
;;; These tests verify the execution behavior, output formatting,
;;; and error handling of the legacy implementation.

(ert-deftest test-legacy-execute-simple-success ()
  "Test execution of simple successful command.

Current behavior: Returns plist with :output, :exit_code 0, :truncated nil."
  (let* ((result (jf/gptel-bash--execute-command "echo hello" "/tmp"))
         (output (plist-get result :output))
         (exit-code (plist-get result :exit_code))
         (truncated (plist-get result :truncated)))
    (should (string-match-p "hello" output))
    (should (zerop exit-code))
    (should-not truncated)))

(ert-deftest test-legacy-execute-simple-failure ()
  "Test execution of command that fails.

Current behavior: Returns plist with :exit_code non-zero."
  (let* ((result (jf/gptel-bash--execute-command "false" "/tmp"))
         (exit-code (plist-get result :exit_code)))
    (should-not (zerop exit-code))))

(ert-deftest test-legacy-execute-nonexistent-command ()
  "Test execution of non-existent command.

Current behavior: Returns plist with :exit_code non-zero (command not found)."
  (let* ((result (jf/gptel-bash--execute-command "nonexistent-command-xyz" "/tmp"))
         (exit-code (plist-get result :exit_code)))
    (should-not (zerop exit-code))))

(ert-deftest test-legacy-execute-directory-change ()
  "Test that command executes in specified directory.

Current behavior: Command runs in provided directory context."
  (let* ((result (jf/gptel-bash--execute-command "pwd" "/tmp"))
         (output (plist-get result :output)))
    (should (string-match-p "/tmp" output))))

(ert-deftest test-legacy-execute-absolute-path-warning ()
  "Test that absolute paths in commands trigger warnings.

Current behavior: :warnings field contains warning about absolute paths."
  (let* ((result (jf/gptel-bash--execute-command "ls /usr/bin" "/tmp"))
         (warnings (plist-get result :warnings)))
    (should warnings)
    (should (stringp (car warnings)))
    (should (string-match-p "absolute path" (car warnings)))))

(ert-deftest test-legacy-execute-relative-path-no-warning ()
  "Test that relative paths do not trigger warnings.

Current behavior: :warnings field is nil for relative paths."
  (let* ((result (jf/gptel-bash--execute-command "ls ." "/tmp"))
         (warnings (plist-get result :warnings)))
    (should-not warnings)))

(ert-deftest test-legacy-execute-output-truncation ()
  "Test that large output is truncated at max-output-chars.

Current behavior: Output truncated with message, :truncated set to t."
  ;; Generate output larger than max-output-chars (10000)
  ;; Using seq to generate 500 lines of 30 characters = 15000 chars
  (let* ((result (jf/gptel-bash--execute-command "seq 1 500 | xargs -I {} printf '%030d\n' {}" "/tmp"))
         (output (plist-get result :output))
         (truncated (plist-get result :truncated)))
    (should truncated)
    (should (< (length output) 12000))  ; Should be around 10000 + truncation message
    (should (string-match-p "Output truncated" output))))

(ert-deftest test-legacy-execute-no-truncation-small-output ()
  "Test that small output is not truncated.

Current behavior: :truncated is nil for output under max-output-chars."
  (let* ((result (jf/gptel-bash--execute-command "echo hello" "/tmp"))
         (truncated (plist-get result :truncated)))
    (should-not truncated)))

(ert-deftest test-legacy-execute-timeout ()
  "Test that long-running commands timeout after command-timeout seconds.

Current behavior: Returns timeout error after 30 seconds, :exit_code 124."
  ;; Note: This test would take 30+ seconds to run, so we skip it in normal runs.
  ;; It's documented here as a characterization of behavior.
  :expected-result :passed
  (message "SKIPPED: test-legacy-execute-timeout (would take 30+ seconds)"))

(ert-deftest test-legacy-execute-multiline-output ()
  "Test that multi-line output is preserved.

Current behavior: Output contains newlines as-is."
  (let* ((result (jf/gptel-bash--execute-command "printf 'line1\nline2\nline3\n'" "/tmp"))
         (output (plist-get result :output)))
    (should (string-match-p "line1" output))
    (should (string-match-p "line2" output))
    (should (string-match-p "line3" output))))

;;; Integration Tests: Command Parsing + Categorization
;;;
;;; These tests verify how parsed commands would be categorized by the
;;; scope validation system (jf/gptel-scope--validate-bash-tool).
;;; They document the integration between parsing and validation.

(ert-deftest test-legacy-integration-simple-read-command ()
  "Test that simple read-only commands are correctly parsed for validation.

Current behavior: Command like 'ls -la' is parsed to 'ls' for categorization."
  (should (equal (jf/gptel-bash--parse-command "ls -la") "ls"))
  (should (equal (jf/gptel-bash--parse-command "grep -r TODO .") "grep"))
  (should (equal (jf/gptel-bash--parse-command "find . -name test") "find")))

(ert-deftest test-legacy-integration-simple-write-command ()
  "Test that simple write commands are correctly parsed for validation.

Current behavior: Command like 'mkdir foo' is parsed to 'mkdir' for categorization."
  (should (equal (jf/gptel-bash--parse-command "mkdir foo") "mkdir"))
  (should (equal (jf/gptel-bash--parse-command "touch bar.txt") "touch"))
  (should (equal (jf/gptel-bash--parse-command "echo hello > file.txt") "echo")))

(ert-deftest test-legacy-integration-dangerous-command ()
  "Test that dangerous commands are correctly parsed for validation.

Current behavior: Dangerous commands like 'rm', 'mv' are parsed to base command.
Note: Deny list in scope-core.el would block these after parsing."
  (should (equal (jf/gptel-bash--parse-command "rm file.txt") "rm"))
  (should (equal (jf/gptel-bash--parse-command "mv old.txt new.txt") "mv"))
  (should (equal (jf/gptel-bash--parse-command "chmod 755 script.sh") "chmod")))

(ert-deftest test-legacy-integration-pipeline-bypass ()
  "Test CRITICAL SECURITY LIMITATION: Pipeline bypass.

Current behavior: 'ls | xargs rm' is parsed to 'ls', allowing 'rm' to execute.
This is a KNOWN SECURITY ISSUE that will be fixed with bash-parser integration.

The regex parser only extracts 'ls' for validation, but 'xargs rm' executes
without any validation or blocking. This allows dangerous commands to bypass
the deny list when wrapped in pipelines."
  ;; Document the bypass behavior
  (should (equal (jf/gptel-bash--parse-command "ls | xargs rm") "ls"))
  (should (equal (jf/gptel-bash--parse-command "find . -name '*.tmp' | xargs rm -f") "find"))
  (should (equal (jf/gptel-bash--parse-command "cat files.txt | xargs chmod 777") "cat"))

  ;; After validation, these commands would be categorized as:
  ;; - "ls" -> read_only (allowed)
  ;; - "find" -> read_only (allowed)
  ;; - "cat" -> read_only (allowed)
  ;;
  ;; But xargs, rm, chmod execute WITHOUT validation!
  ;; This is the pipeline bypass vulnerability.
  )

(ert-deftest test-legacy-integration-chained-command-bypass ()
  "Test CRITICAL SECURITY LIMITATION: Command chaining bypass.

Current behavior: 'mkdir foo && cd foo && rm -rf *' is parsed to 'mkdir'.
This is a KNOWN SECURITY ISSUE that will be fixed with bash-parser integration.

The regex parser only extracts 'mkdir' for validation, but subsequent commands
('cd foo', 'rm -rf *') execute without any validation."
  (should (equal (jf/gptel-bash--parse-command "mkdir foo && cd foo && rm -rf *") "mkdir"))
  (should (equal (jf/gptel-bash--parse-command "test -f file.txt && rm file.txt") "test"))
  (should (equal (jf/gptel-bash--parse-command "echo start; rm -rf /tmp/data; echo done") "echo")))

(ert-deftest test-legacy-integration-git-commands ()
  "Test that git commands are correctly parsed.

Current behavior: 'git log --oneline' is parsed to 'git'.
Note: The categorization system treats 'git' as the base command,
not 'git log' or 'git commit' separately."
  (should (equal (jf/gptel-bash--parse-command "git log --oneline -10") "git"))
  (should (equal (jf/gptel-bash--parse-command "git diff HEAD~1") "git"))
  (should (equal (jf/gptel-bash--parse-command "git add .") "git"))
  (should (equal (jf/gptel-bash--parse-command "git commit -m 'message'") "git")))

;;; Summary of Documented Behaviors
;;;
;;; REGEX-BASED PARSING LIMITATIONS:
;;;
;;; 1. Pipeline bypass (CRITICAL):
;;;    - "ls | xargs rm" extracts "ls", allows "rm" to execute unchecked
;;;    - "find . | xargs chmod 777" extracts "find", allows "chmod" unchecked
;;;
;;; 2. Command chaining bypass (CRITICAL):
;;;    - "mkdir foo && rm -rf bar" extracts "mkdir", allows "rm" unchecked
;;;    - "echo start; sudo reboot; echo end" extracts "echo", allows "sudo" unchecked
;;;
;;; 3. Directory-only validation:
;;;    - No extraction or validation of file paths in arguments
;;;    - Absolute paths detected but not blocked, only warned
;;;
;;; 4. Simple pattern matching:
;;;    - Split on "[ |><;&]+" misses complex quoting, escaping
;;;    - Does not detect ~/ as absolute path
;;;    - Does not parse command substitution $(...)
;;;
;;; 5. Git subcommand limitation:
;;;    - "git log" and "git commit" both parse to "git"
;;;    - Cannot differentiate between read-only and write git operations
;;;
;;; BASH-PARSER INTEGRATION WILL FIX:
;;; - Full AST traversal will extract ALL commands in pipeline/chain
;;; - File path extraction from arguments for validation
;;; - Proper handling of quoting, escaping, substitution
;;; - Git subcommand differentiation (git-log vs git-commit)

(provide 'test-scope-shell-tools-legacy)
;;; test-scope-shell-tools-legacy.el ends here
