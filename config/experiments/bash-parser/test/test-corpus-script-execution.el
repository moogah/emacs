;;; test-corpus-script-execution.el --- Tests for script execution detection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Comprehensive ERT tests for script execution detection with embedded test corpus.

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                       (file-name-directory load-file-name)))

;;; Test Corpus Data

(defvar jf/bash-script-execution-test-corpus
  '(
    ;; ============================================================
    ;; INTERPRETER COMMANDS - Python
    ;; ============================================================
    (:id "exec-python-001"
     :command "python script.py"
     :note "Python interpreter execution"
     :expect-ops ((:file "script.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-python-002"
     :command "python3 deploy.py --env staging"
     :note "Python3 with script arguments"
     :expect-ops ((:file "deploy.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ("--env" "staging"))))

    (:id "exec-python-003"
     :command "python process.py input.csv output.json"
     :note "Python script with file arguments"
     :expect-ops ((:file "process.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ("input.csv" "output.json"))))

    ;; ============================================================
    ;; INTERPRETER COMMANDS - Node
    ;; ============================================================
    (:id "exec-node-001"
     :command "node server.js"
     :note "Node interpreter execution"
     :expect-ops ((:file "server.js"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-node-002"
     :command "node server.js --port 3000 --verbose"
     :note "Node with flag arguments"
     :expect-ops ((:file "server.js"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ("--port" "3000" "--verbose"))))

    ;; ============================================================
    ;; INTERPRETER COMMANDS - Bash/Shell
    ;; ============================================================
    (:id "exec-bash-001"
     :command "bash deploy.sh"
     :note "Bash interpreter execution"
     :expect-ops ((:file "deploy.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-sh-001"
     :command "sh script.sh arg1 arg2"
     :note "Sh interpreter with arguments"
     :expect-ops ((:file "script.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ("arg1" "arg2"))))

    (:id "exec-zsh-001"
     :command "zsh setup.sh"
     :note "Zsh interpreter execution"
     :expect-ops ((:file "setup.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    ;; ============================================================
    ;; SHELL BUILT-INS - source and .
    ;; ============================================================
    (:id "exec-source-001"
     :command "source config.sh"
     :note "Source built-in execution"
     :expect-ops ((:file "config.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-dot-001"
     :command ". config.sh"
     :note "Dot source built-in execution"
     :expect-ops ((:file "config.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-source-002"
     :command "source /etc/profile.d/custom.sh"
     :note "Source with absolute path"
     :expect-ops ((:file "/etc/profile.d/custom.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    ;; ============================================================
    ;; GO SUBCOMMANDS
    ;; ============================================================
    (:id "exec-go-run-001"
     :command "go run main.go"
     :note "Go run subcommand executes"
     :expect-ops ((:file "main.go"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-go-test-001"
     :command "go test ./..."
     :note "Go test subcommand executes"
     :expect-ops ((:file "./..."
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-go-build-001"
     :command "go build main.go"
     :note "Go build does NOT execute (reads only)"
     :expect-ops ((:file "main.go"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "exec-go-run-002"
     :command "go run cmd/server/main.go --config prod.yaml"
     :note "Go run with script arguments"
     :expect-ops ((:file "cmd/server/main.go"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ("--config" "prod.yaml"))))

    ;; ============================================================
    ;; SELF-EXECUTING SCRIPTS - Relative paths
    ;; ============================================================
    (:id "exec-self-relative-001"
     :command "./script.sh"
     :note "Self-executing relative path"
     :expect-ops ((:file "./script.sh"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ())))

    (:id "exec-self-relative-002"
     :command "./script.sh arg1 arg2"
     :note "Self-executing with arguments"
     :expect-ops ((:file "./script.sh"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ("arg1" "arg2"))))

    (:id "exec-self-relative-003"
     :command "./deploy.sh staging us-west-2"
     :note "Self-executing deploy script"
     :expect-ops ((:file "./deploy.sh"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ("staging" "us-west-2"))))

    ;; ============================================================
    ;; SELF-EXECUTING SCRIPTS - Absolute paths
    ;; ============================================================
    (:id "exec-self-absolute-001"
     :command "/usr/local/bin/tool"
     :note "Self-executing absolute path"
     :expect-ops ((:file "/usr/local/bin/tool"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ())))

    (:id "exec-self-absolute-002"
     :command "/opt/scripts/backup.sh /data /backup"
     :note "Absolute path with arguments"
     :expect-ops ((:file "/opt/scripts/backup.sh"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ("/data" "/backup"))))

    ;; ============================================================
    ;; SELF-EXECUTING SCRIPTS - Parent directory
    ;; ============================================================
    (:id "exec-self-parent-001"
     :command "../bin/runner"
     :note "Parent directory execution"
     :expect-ops ((:file "../bin/runner"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ())))

    (:id "exec-self-parent-002"
     :command "../bin/runner data.txt"
     :note "Parent directory with argument"
     :expect-ops ((:file "../bin/runner"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ("data.txt"))))

    ;; ============================================================
    ;; NON-SELF-EXECUTING COMMANDS
    ;; ============================================================
    (:id "exec-not-self-001"
     :command "cat file.txt"
     :note "Regular command is NOT self-executing"
     :expect-ops ((:file "file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "exec-not-self-002"
     :command "script.sh"
     :note "Command without path is NOT self-executing"
     :expect-ops ())  ;; No operation - not a known command

    ;; ============================================================
    ;; VARIABLE RESOLUTION IN EXECUTION
    ;; ============================================================
    (:id "exec-variable-001"
     :command "python $SCRIPT"
     :note "Execute with resolved variable"
     :var-context (("SCRIPT" . "deploy.py"))
     :expect-ops ((:file "deploy.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-variable-002"
     :command "python $UNKNOWN_SCRIPT"
     :note "Execute with unresolved variable"
     :var-context nil
     :expect-ops ((:file "$UNKNOWN_SCRIPT"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :unresolved t
                   :unresolved-vars ("UNKNOWN_SCRIPT")
                   :script-args ())))

    (:id "exec-variable-003"
     :command "./$SCRIPT_NAME input.txt"
     :note "Self-executing with variable (unresolved)"
     :var-context nil
     :expect-ops ((:file "./$SCRIPT_NAME"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :unresolved t
                   :unresolved-vars ("SCRIPT_NAME")
                   :script-args ("input.txt"))))

    ;; ============================================================
    ;; PIPELINES WITH EXECUTION
    ;; ============================================================
    (:id "exec-pipeline-001"
     :command "python generate.py | node process.js"
     :note "Pipeline with two execute operations"
     :expect-ops ((:file "generate.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())
                  (:file "process.js"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-pipeline-002"
     :command "./script.sh | python filter.py > output.txt"
     :note "Pipeline with self-execution and write"
     :expect-ops ((:file "./script.sh"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ())
                  (:file "filter.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())
                  (:file "output.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    ;; ============================================================
    ;; CHAINS WITH EXECUTION
    ;; ============================================================
    (:id "exec-chain-001"
     :command "bash setup.sh && python main.py"
     :note "Chain with two execute operations"
     :expect-ops ((:file "setup.sh"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())
                  (:file "main.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :script-args ())))

    (:id "exec-chain-002"
     :command "./configure && make && ./bin/app"
     :note "Chain with self-executing scripts"
     :expect-ops ((:file "./configure"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ())
                  (:file "./bin/app"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :script-args ())))

    ;; ============================================================
    ;; NESTED COMMANDS (bash -c, sh -c)
    ;; ============================================================
    (:id "exec-nested-001"
     :command "bash -c 'python script.py'"
     :note "Nested python execution"
     :expect-ops ((:file "script.py"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :indirect t
                   :script-args ())))

    (:id "exec-nested-002"
     :command "sh -c './deploy.sh'"
     :note "Nested self-execution"
     :expect-ops ((:file "./deploy.sh"
                   :operation :execute
                   :confidence :low
                   :source :command-name
                   :self-executing t
                   :indirect t
                   :script-args ())))

    (:id "exec-nested-003"
     :command "bash -c 'node server.js --port 3000'"
     :note "Nested execution with arguments"
     :expect-ops ((:file "server.js"
                   :operation :execute
                   :confidence :high
                   :source :positional-arg
                   :indirect t
                   :script-args ("--port" "3000"))))

    ;; ============================================================
    ;; EDGE CASES
    ;; ============================================================
    (:id "exec-edge-001"
     :command "python"
     :note "Python without script (no file operation)"
     :expect-ops ())

    (:id "exec-edge-002"
     :command "python -c 'print(1)'"
     :note "Python -c with inline code (no file operation)"
     :expect-ops ())

    (:id "exec-edge-003"
     :command "bash"
     :note "Bash without script (no file operation)"
     :expect-ops ())

    ))


;;; Python Interpreter Tests

(ert-deftest test-script-execution-python-interpreter ()
  "Scenario: script-execution § 'Python interpreter execution'

Test that python interpreter extracts execute operation."
  (let* ((parsed (jf/bash-parse "python script.py"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "script.py"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high))
      (should (eq (plist-get exec-op :source) :positional-arg))
      (should (equal (plist-get exec-op :script-args) ())))))

(ert-deftest test-script-execution-python3-with-args ()
  "Scenario: script-execution § 'Script with file arguments'

Test that python3 captures script arguments."
  (let* ((parsed (jf/bash-parse "python3 process.py input.csv output.json"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "process.py"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (equal (plist-get exec-op :script-args) '("input.csv" "output.json"))))))

;;; Node Interpreter Tests

(ert-deftest test-script-execution-node-interpreter ()
  "Scenario: script-execution § 'Node interpreter execution'

Test that node interpreter extracts execute operation."
  (let* ((parsed (jf/bash-parse "node server.js"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "server.js"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high))
      (should (eq (plist-get exec-op :source) :positional-arg))
      (should (equal (plist-get exec-op :script-args) ())))))

(ert-deftest test-script-execution-node-with-flags ()
  "Scenario: script-execution § 'Script with flag arguments'

Test that node captures flag arguments."
  (let* ((parsed (jf/bash-parse "node server.js --port 3000 --verbose"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "server.js"))
      (should (equal (plist-get exec-op :script-args) '("--port" "3000" "--verbose"))))))

;;; Bash/Shell Interpreter Tests

(ert-deftest test-script-execution-bash-interpreter ()
  "Scenario: script-execution § 'Bash interpreter execution'

Test that bash interpreter extracts execute operation."
  (let* ((parsed (jf/bash-parse "bash deploy.sh"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "deploy.sh"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high))
      (should (eq (plist-get exec-op :source) :positional-arg))
      (should (equal (plist-get exec-op :script-args) ())))))

(ert-deftest test-script-execution-sh-interpreter ()
  "Test sh interpreter with arguments."
  (let* ((parsed (jf/bash-parse "sh script.sh arg1 arg2"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "script.sh"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (equal (plist-get exec-op :script-args) '("arg1" "arg2"))))))

(ert-deftest test-script-execution-zsh-interpreter ()
  "Test zsh interpreter execution."
  (let* ((parsed (jf/bash-parse "zsh setup.sh"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "setup.sh"))
      (should (eq (plist-get exec-op :operation) :execute)))))

;;; Shell Built-in Tests

(ert-deftest test-script-execution-shell-builtin-source ()
  "Scenario: script-execution § 'Shell built-in source execution'

Test that source built-in extracts execute operation."
  (let* ((parsed (jf/bash-parse "source config.sh"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "config.sh"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high))
      (should (eq (plist-get exec-op :source) :positional-arg))
      (should (equal (plist-get exec-op :script-args) ())))))

(ert-deftest test-script-execution-shell-builtin-dot ()
  "Scenario: script-execution § 'Dot source execution'

Test that dot (.) built-in extracts execute operation."
  (let* ((parsed (jf/bash-parse ". config.sh"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "config.sh"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high)))))

;;; Go Subcommand Tests

(ert-deftest test-script-execution-go-run-subcommand ()
  "Scenario: script-execution § 'Go run subcommand'

Test that go run extracts execute operation."
  (let* ((parsed (jf/bash-parse "go run main.go"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "main.go"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high))
      (should (eq (plist-get exec-op :source) :positional-arg))
      (should (equal (plist-get exec-op :script-args) ())))))

(ert-deftest test-script-execution-go-test-subcommand ()
  "Scenario: script-execution § 'Go test subcommand'

Test that go test extracts execute operation."
  (let* ((parsed (jf/bash-parse "go test ./..."))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "./..."))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :confidence) :high)))))

(ert-deftest test-script-execution-go-build-not-execute ()
  "Scenario: script-execution § 'Go build subcommand does not execute'

Test that go build reads but does NOT execute."
  (let* ((parsed (jf/bash-parse "go build main.go"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      (should (equal (plist-get op :file) "main.go"))
      (should (eq (plist-get op :operation) :read))  ;; NOT :execute
      (should (eq (plist-get op :confidence) :high)))))

;;; Self-Executing Script Tests - Relative Paths

(ert-deftest test-script-execution-self-executing-relative ()
  "Scenario: script-execution § 'Relative path execution'

Test that ./script.sh is detected as self-execution."
  (let* ((parsed (jf/bash-parse "./script.sh arg1 arg2"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "./script.sh"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :source) :command-name))
      (should (eq (plist-get exec-op :confidence) :low))
      (should (eq (plist-get exec-op :self-executing) t))
      (should (equal (plist-get exec-op :script-args) '("arg1" "arg2"))))))

(ert-deftest test-script-execution-self-executing-parent ()
  "Scenario: script-execution § 'Parent directory path execution'

Test that ../bin/runner is detected as self-execution."
  (let* ((parsed (jf/bash-parse "../bin/runner"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "../bin/runner"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :source) :command-name))
      (should (eq (plist-get exec-op :confidence) :low))
      (should (eq (plist-get exec-op :self-executing) t)))))

;;; Self-Executing Script Tests - Absolute Paths

(ert-deftest test-script-execution-self-executing-absolute ()
  "Scenario: script-execution § 'Absolute path execution'

Test that /usr/local/bin/tool is detected as self-execution."
  (let* ((parsed (jf/bash-parse "/usr/local/bin/tool"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "/usr/local/bin/tool"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :source) :command-name))
      (should (eq (plist-get exec-op :confidence) :low))
      (should (eq (plist-get exec-op :self-executing) t))
      (should (null (plist-get exec-op :script-args))))))

;;; Non-Self-Executing Tests

(ert-deftest test-script-execution-not-self-executing ()
  "Scenario: script-execution § 'Non-path command not self-executing'

Test that regular commands like 'cat' are NOT detected as self-execution."
  (let* ((parsed (jf/bash-parse "cat file.txt"))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have read operation from cat, but no execute operation
    (let ((exec-op (cl-find-if (lambda (op)
                                  (eq (plist-get op :operation) :execute))
                                ops)))
      (should (null exec-op)))))

;;; Script Arguments Tests

(ert-deftest test-script-execution-script-args-none ()
  "Scenario: script-execution § 'Script with no arguments'

Test that script with no args has empty script-args list."
  (let* ((parsed (jf/bash-parse "python script.py"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :script-args) ()))))

(ert-deftest test-script-execution-script-args-files ()
  "Scenario: script-execution § 'Script with file arguments'

Test that script file arguments are captured."
  (let* ((parsed (jf/bash-parse "python process.py input.csv output.json"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :script-args) '("input.csv" "output.json")))))

(ert-deftest test-script-execution-script-args-flags ()
  "Scenario: script-execution § 'Script with flag arguments'

Test that flag arguments are captured."
  (let* ((parsed (jf/bash-parse "node server.js --port 3000 --verbose"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :script-args) '("--port" "3000" "--verbose")))))

(ert-deftest test-script-execution-script-args-self-executing ()
  "Scenario: script-execution § 'Self-executing script with arguments'

Test that self-executing script arguments are captured."
  (let* ((parsed (jf/bash-parse "./deploy.sh staging us-west-2"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :script-args) '("staging" "us-west-2")))))

;;; Confidence Level Tests

(ert-deftest test-script-execution-confidence-high-interpreter ()
  "Scenario: script-execution § 'High confidence for known interpreter'

Test that known interpreters have high confidence."
  (let* ((parsed (jf/bash-parse "python script.py"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (eq (plist-get (car ops) :confidence) :high))))

(ert-deftest test-script-execution-confidence-low-path ()
  "Scenario: script-execution § 'Low confidence for path-based execution'

Test that path-based execution has low confidence."
  (let* ((parsed (jf/bash-parse "./unknown-binary"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (eq (plist-get (car ops) :confidence) :low))))

(ert-deftest test-script-execution-confidence-high-builtin ()
  "Scenario: script-execution § 'High confidence for known shell built-in'

Test that shell built-ins have high confidence."
  (let* ((parsed (jf/bash-parse "source config.sh"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (should (eq (plist-get (car ops) :confidence) :high))))

;;; Variable Resolution Tests

(ert-deftest test-script-execution-variable-resolved ()
  "Scenario: script-execution § 'Execute with resolved variable'

Test that variables in script paths are resolved."
  (let* ((parsed (jf/bash-parse "python $SCRIPT"))
         (context '((SCRIPT . "deploy.py")))
         (ops (jf/bash-extract-file-operations parsed context)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "deploy.py"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should-not (plist-get exec-op :unresolved)))))

(ert-deftest test-script-execution-variable-unresolved ()
  "Scenario: script-execution § 'Execute with unresolved variable'

Test that unresolved variables are marked."
  (let* ((parsed (jf/bash-parse "python $UNKNOWN_SCRIPT"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "$UNKNOWN_SCRIPT"))
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :unresolved) t))
      (let ((unresolved-vars (plist-get exec-op :unresolved-vars)))
        (should (or (member "UNKNOWN_SCRIPT" unresolved-vars)
                    (member 'UNKNOWN_SCRIPT unresolved-vars)))))))

;;; Pipeline Tests

(ert-deftest test-script-execution-pipeline ()
  "Scenario: script-execution § 'Execute in pipeline'

Test that execute operations are extracted from all commands in pipeline."
  (let* ((parsed (jf/bash-parse "python generate.py | node process.js"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 2))
    ;; Check for python execution
    (let ((python-op (cl-find-if (lambda (op)
                                    (equal (plist-get op :file) "generate.py"))
                                  ops)))
      (should python-op)
      (should (eq (plist-get python-op :operation) :execute)))
    ;; Check for node execution
    (let ((node-op (cl-find-if (lambda (op)
                                  (equal (plist-get op :file) "process.js"))
                                ops)))
      (should node-op)
      (should (eq (plist-get node-op :operation) :execute)))))

;;; Command Chain Tests

(ert-deftest test-script-execution-chain ()
  "Scenario: script-execution § 'Execute in command chain'

Test that execute operations are extracted from all commands in chain."
  (let* ((parsed (jf/bash-parse "bash setup.sh && python main.py"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (= (length ops) 2))
    ;; Check for bash execution
    (let ((bash-op (cl-find-if (lambda (op)
                                  (equal (plist-get op :file) "setup.sh"))
                                ops)))
      (should bash-op)
      (should (eq (plist-get bash-op :operation) :execute)))
    ;; Check for python execution
    (let ((python-op (cl-find-if (lambda (op)
                                    (equal (plist-get op :file) "main.py"))
                                  ops)))
      (should python-op)
      (should (eq (plist-get python-op :operation) :execute)))))

;;; Nested Command Tests

(ert-deftest test-script-execution-nested-python ()
  "Scenario: script-execution § 'Nested python execution'

Test that nested commands extract execute operations with :indirect flag."
  (let* ((parsed (jf/bash-parse "bash -c 'python script.py'"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 1))
    (let ((exec-op (cl-find-if (lambda (op)
                                  (equal (plist-get op :file) "script.py"))
                                ops)))
      (should exec-op)
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :indirect) t)))))

(ert-deftest test-script-execution-nested-self-executing ()
  "Scenario: script-execution § 'Nested self-execution'

Test that nested self-executing scripts have both :indirect and :self-executing."
  (let* ((parsed (jf/bash-parse "sh -c './deploy.sh'"))
         (ops (jf/bash-extract-file-operations parsed)))
    (should (>= (length ops) 1))
    (let ((exec-op (cl-find-if (lambda (op)
                                  (equal (plist-get op :file) "./deploy.sh"))
                                ops)))
      (should exec-op)
      (should (eq (plist-get exec-op :operation) :execute))
      (should (eq (plist-get exec-op :indirect) t))
      (should (eq (plist-get exec-op :self-executing) t)))))

;;; Corpus-Based Tests

(ert-deftest test-script-execution-corpus ()
  "Run all script execution corpus test cases.

Validates that all scenarios in the corpus produce expected operations."
  (let ((failures nil))
    (dolist (test-case jf/bash-script-execution-test-corpus)
      (let* ((test-id (plist-get test-case :id))
             (cmd (plist-get test-case :command))
             (expected (plist-get test-case :expect-ops))
             (var-context (plist-get test-case :var-context))
             (note (plist-get test-case :note))
             (parsed (jf/bash-parse cmd))
             (actual (jf/bash-extract-file-operations parsed var-context)))

        ;; Compare expected vs actual operations
        (unless (equal (length expected) (length actual))
          (push (list :id test-id
                      :note note
                      :error "Operation count mismatch"
                      :expected-count (length expected)
                      :actual-count (length actual)
                      :command cmd)
                failures))

        ;; For each expected operation, verify it exists in actual
        (dolist (expected-op expected)
          (let* ((expected-file (plist-get expected-op :file))
                 (expected-operation (plist-get expected-op :operation))
                 (actual-op (cl-find-if
                            (lambda (op)
                              (and (equal (plist-get op :file) expected-file)
                                   (eq (plist-get op :operation) expected-operation)))
                            actual)))

            (unless actual-op
              (push (list :id test-id
                          :note note
                          :error "Expected operation not found"
                          :expected-op expected-op
                          :actual-ops actual
                          :command cmd)
                    failures))

            ;; Verify specific fields if operation was found
            (when actual-op
              (dolist (key '(:confidence :source :self-executing :indirect :unresolved))
                (when (plist-member expected-op key)
                  (unless (equal (plist-get expected-op key)
                                (plist-get actual-op key))
                    (push (list :id test-id
                                :note note
                                :error (format "Field mismatch: %s" key)
                                :expected (plist-get expected-op key)
                                :actual (plist-get actual-op key)
                                :command cmd)
                          failures)))))))))

    ;; Report failures
    (when failures
      (message "\n=== CORPUS TEST FAILURES ===\n")
      (dolist (failure failures)
        (message "FAIL: %s - %s" (plist-get failure :id) (plist-get failure :note))
        (message "  Error: %s" (plist-get failure :error))
        (message "  Command: %s" (plist-get failure :command))
        (when (plist-get failure :expected)
          (message "  Expected: %S" (plist-get failure :expected)))
        (when (plist-get failure :actual)
          (message "  Actual: %S" (plist-get failure :actual)))
        (message "")))

    ;; Assert no failures
    (should (null failures))))

(provide 'test-script-execution)
;;; test-script-execution.el ends here
