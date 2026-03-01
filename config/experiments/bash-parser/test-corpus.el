;;; test-corpus.el --- Test corpus for bash command parsing -*- lexical-binding: t; -*-

;; Test corpus for bash command parsing with expected results

(defvar jf/bash-parser-test-corpus
  '(
    ;; ============================================================
    ;; SIMPLE COMMANDS (baseline)
    ;; ============================================================
    (:id "simple-001"
     :command "ls"
     :expect (:command-name "ls"
              :subcommand nil
              :flags ()
              :positional-args ()
              :dangerous-p nil))

    (:id "simple-002"
     :command "ls -la"
     :expect (:command-name "ls"
              :flags ("-la")
              :positional-args ()
              :dangerous-p nil))

    (:id "simple-003"
     :command "ls -la /tmp"
     :expect (:command-name "ls"
              :flags ("-la")
              :positional-args ("/tmp")
              :dangerous-p nil))

    (:id "simple-004"
     :command "grep -r 'pattern' ./config"
     :expect (:command-name "grep"
              :flags ("-r")
              :positional-args ("pattern" "./config")
              :dangerous-p nil))

    ;; ============================================================
    ;; GIT SUBCOMMANDS (subcommand detection)
    ;; ============================================================
    (:id "git-001"
     :command "git log"
     :expect (:command-name "git"
              :subcommand "log"
              :flags ()
              :positional-args ()
              :dangerous-p nil))

    (:id "git-002"
     :command "git log --oneline -10"
     :expect (:command-name "git"
              :subcommand "log"
              :flags ("--oneline" "-10")
              :positional-args ()
              :dangerous-p nil))

    (:id "git-003"
     :command "git add config/gptel/scope.el"
     :expect (:command-name "git"
              :subcommand "add"
              :flags ()
              :positional-args ("config/gptel/scope.el")
              :dangerous-p nil))

    (:id "git-004"
     :command "git commit -m 'test message'"
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :positional-args ("test message")
              :dangerous-p nil))

    (:id "git-005"
     :command "git push --force"
     :expect (:command-name "git"
              :subcommand "push"
              :flags ("--force")
              :positional-args ()
              :dangerous-p t))

    ;; ============================================================
    ;; DANGEROUS FLAGS (flag pattern detection)
    ;; ============================================================
    (:id "dangerous-001"
     :command "rm -rf /tmp/test"
     :expect (:command-name "rm"
              :subcommand nil
              :flags ("-rf")
              :positional-args ("/tmp/test")
              :dangerous-p t))

    (:id "dangerous-002"
     :command "git reset --hard HEAD~1"
     :expect (:command-name "git"
              :subcommand "reset"
              :flags ("--hard")
              :positional-args ("HEAD~1")
              :dangerous-p t))

    ;; ============================================================
    ;; PIPELINES (multiple commands connected)
    ;; LIMITATION: Parser only sees first command, pipeline ignored
    ;; ============================================================
    (:id "pipeline-001"
     :command "ls -la | grep test"
     :note "LIMITATION: Only sees 'ls -la', ignores '| grep test'"
     :expect nil)

    (:id "pipeline-002"
     :command "cat file.txt | sort | uniq -c"
     :note "LIMITATION: Only sees 'cat file.txt', ignores pipeline stages"
     :expect nil)

    (:id "pipeline-003"
     :command "git log --oneline | head -10"
     :note "LIMITATION: Only sees git command, ignores '| head -10'"
     :expect nil)

    ;; ============================================================
    ;; COMMAND CHAINS (sequential execution)
    ;; LIMITATION: Parser only sees first command, chains ignored
    ;; ============================================================
    (:id "chain-001"
     :command "git add . && git commit -m 'test'"
     :note "LIMITATION: Only sees 'git add .', ignores '&& git commit'"
     :expect nil)

    (:id "chain-002"
     :command "rm file.txt || echo 'failed to delete'"
     :note "LIMITATION: Only sees 'rm file.txt', ignores '|| echo'"
     :expect nil)

    (:id "chain-003"
     :command "cd /tmp; ls -la; pwd"
     :note "LIMITATION: Only sees 'cd /tmp', ignores '; ls; pwd'"
     :expect nil)

    (:id "chain-004"
     :command "git add . && git commit -m 'fix' && git push"
     :note "LIMITATION: Only sees 'git add .', ignores multi-stage chain"
     :expect nil)

    ;; ============================================================
    ;; REDIRECTIONS (file I/O)
    ;; LIMITATION: Redirection operators and targets stripped
    ;; ============================================================
    (:id "redirect-001"
     :command "echo 'hello' > output.txt"
     :note "LIMITATION: Sees 'echo hello' but loses '> output.txt'"
     :expect nil)

    (:id "redirect-002"
     :command "cat input.txt >> output.txt"
     :note "LIMITATION: Sees 'cat input.txt' but loses '>> output.txt'"
     :expect nil)

    (:id "redirect-003"
     :command "grep error < logfile.txt"
     :note "LIMITATION: Sees 'grep error' but loses '< logfile.txt'"
     :expect nil)

    (:id "redirect-004"
     :command "command 2>&1"
     :note "LIMITATION: Sees 'command' but loses '2>&1'"
     :expect nil)

    (:id "redirect-005"
     :command "git log > /dev/null 2>&1"
     :note "LIMITATION: Sees 'git log' but loses redirections"
     :expect nil)

    ;; ============================================================
    ;; COMMAND SUBSTITUTION (nested commands)
    ;; MESSY: Inner command text extracted but creates duplicate tokens
    ;; ============================================================
    (:id "substitution-001"
     :command "echo $(date)"
     :note "MESSY: Extracts 'date' as positional arg (not marked as substitution)"
     :expect nil)

    (:id "substitution-002"
     :command "echo `pwd`"
     :note "MESSY: Extracts 'pwd' as positional arg (backtick ignored)"
     :expect nil)

    (:id "substitution-003"
     :command "git commit -m \"$(cat message.txt)\""
     :note "MESSY: Creates duplicate tokens for substitution and inner command"
     :expect nil)

    ;; ============================================================
    ;; VARIABLE EXPANSION
    ;; ============================================================
    (:id "variable-001"
     :command "echo $PATH"
     :note "BROKEN: Variable completely disappears"
     :expect nil)

    (:id "variable-002"
     :command "rm -rf $HOME/tmp"
     :note "BROKEN: Extracts both '$HOME/tmp' AND '/tmp' (weird duplication)"
     :expect nil)

    (:id "variable-003"
     :command "git commit -m \"$commit_message\""
     :note "Variable in quoted string - preserved correctly"
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :positional-args ("$commit_message")
              :dangerous-p nil))

    ;; ============================================================
    ;; COMPLEX QUOTING
    ;; ============================================================
    (:id "quote-001"
     :command "echo \"hello 'world'\""
     :note "Double quotes containing single quotes - works correctly"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("hello 'world'")
              :dangerous-p nil))

    (:id "quote-002"
     :command "echo 'it\\'s working'"
     :note "BROKEN: Escaped single quote splits into multiple tokens"
     :expect nil)

    (:id "quote-003"
     :command "git commit -m $'test\\nwith\\nnewlines'"
     :note "BROKEN: ANSI-C quoting completely disappears"
     :expect nil)

    ;; ============================================================
    ;; GLOB PATTERNS
    ;; ============================================================
    (:id "glob-001"
     :command "ls *.txt"
     :note "Simple glob pattern - preserved as literal string"
     :expect (:command-name "ls"
              :subcommand nil
              :flags ()
              :positional-args ("*.txt")
              :dangerous-p nil))

    (:id "glob-002"
     :command "rm config/**/*.el"
     :note "Recursive glob pattern - preserved as literal string"
     :expect (:command-name "rm"
              :subcommand nil
              :flags ()
              :positional-args ("config/**/*.el")
              :dangerous-p nil))

    (:id "glob-003"
     :command "git add *.{el,org}"
     :note "BROKEN: Brace expansion splits into multiple tokens"
     :expect nil)

    ;; ============================================================
    ;; BACKGROUND PROCESSES
    ;; ============================================================
    (:id "background-001"
     :command "npm run build &"
     :note "Background operator ignored, command parsed correctly"
     :expect (:command-name "npm"
              :subcommand "run"
              :flags ()
              :positional-args ("build")
              :dangerous-p nil))

    (:id "background-002"
     :command "python server.py & echo 'Server started'"
     :note "Only first command visible (same as pipeline/chain limitation)"
     :expect nil)

    ;; ============================================================
    ;; HEREDOCS
    ;; ============================================================
    (:id "heredoc-001"
     :command "cat << EOF\nHello\nWorld\nEOF"
     :note "Heredoc content ignored, command parsed correctly"
     :expect (:command-name "cat"
              :subcommand nil
              :flags ()
              :positional-args ()
              :dangerous-p nil))

    ;; ============================================================
    ;; COMBINED COMPLEXITY
    ;; ============================================================
    (:id "complex-001"
     :command "find . -name '*.log' -exec rm {} \\;"
     :note "MESSY: Treats -exec as flag, misclassifies structure"
     :expect nil)

    (:id "complex-002"
     :command "docker run -it --rm -v $(pwd):/app ubuntu bash"
     :note "MESSY: Command substitution creates duplicate tokens"
     :expect nil)

    (:id "complex-003"
     :command "git log --pretty=format:'%h - %an, %ar : %s' --graph"
     :note "MESSY: Format string gets split oddly"
     :expect nil)

    (:id "complex-004"
     :command "python -c 'import sys; print(sys.version)'"
     :note "Dangerous -c flag correctly detected"
     :expect (:command-name "python"
              :subcommand nil
              :flags ("-c")
              :positional-args ("import sys; print(sys.version)")
              :dangerous-p t))

    (:id "complex-005"
     :command "sed -i.bak 's/foo/bar/g' *.txt"
     :note "In-place sed with glob - parsed correctly"
     :expect (:command-name "sed"
              :subcommand nil
              :flags ("-i.bak")
              :positional-args ("s/foo/bar/g" "*.txt")
              :dangerous-p nil))
    )
  "Test corpus for bash command parsing.
Each entry has :id, :command, and :expect plist.
Entries with :expect nil are exploratory - we don't know how the parser handles them yet.")

(provide 'test-corpus)
;;; test-corpus.el ends here
