;;; script-execution-corpus.el --- Test corpus for bash script execution detection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Test corpus for script execution extraction with clear inputs and expected operations

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

(provide 'script-execution-corpus)
;;; script-execution-corpus.el ends here
