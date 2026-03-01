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
    ;; FULLY PARSED: All commands extracted and validated
    ;; ============================================================
    (:id "pipeline-001"
     :command "ls -la | grep test"
     :note "Pipeline with 2 commands - both extracted"
     :expect (:success t
              :type :pipeline
              :command-count 2
              :dangerous-p nil
              :all-commands ((:command-name "ls"
                              :subcommand nil
                              :flags ("-la")
                              :positional-args ()
                              :dangerous-p nil)
                            (:command-name "grep"
                              :subcommand nil
                              :flags ()
                              :positional-args ("test")
                              :dangerous-p nil))))

    (:id "pipeline-002"
     :command "cat file.txt | sort | uniq -c"
     :note "Three-stage pipeline - all commands extracted"
     :expect (:success t
              :type :pipeline
              :command-count 3
              :dangerous-p nil
              :all-commands ((:command-name "cat"
                              :subcommand nil
                              :flags ()
                              :positional-args ("file.txt")
                              :dangerous-p nil)
                            (:command-name "sort"
                              :subcommand nil
                              :flags ()
                              :positional-args ()
                              :dangerous-p nil)
                            (:command-name "uniq"
                              :subcommand nil
                              :flags ("-c")
                              :positional-args ()
                              :dangerous-p nil))))

    (:id "pipeline-003"
     :command "git log --oneline | head -10"
     :note "Git command with pipeline"
     :expect (:success t
              :type :pipeline
              :command-count 2
              :dangerous-p nil
              :all-commands ((:command-name "git"
                              :subcommand "log"
                              :flags ("--oneline")
                              :positional-args ()
                              :dangerous-p nil)
                            (:command-name "head"
                              :subcommand nil
                              :flags ("-10")
                              :positional-args ()
                              :dangerous-p nil))))

    ;; ============================================================
    ;; COMMAND CHAINS (sequential execution)
    ;; FULLY PARSED: All commands extracted and validated
    ;; ============================================================
    (:id "chain-001"
     :command "git add . && git commit -m 'test'"
     :note "AND chain with 2 git commands"
     :expect (:success t
              :type :chain
              :command-count 2
              :dangerous-p nil
              :all-commands ((:command-name "git"
                              :subcommand "add"
                              :flags ()
                              :positional-args (".")
                              :dangerous-p nil)
                            (:command-name "git"
                              :subcommand "commit"
                              :flags ("-m")
                              :positional-args ("test")
                              :dangerous-p nil))))

    (:id "chain-002"
     :command "rm file.txt || echo 'failed to delete'"
     :note "OR chain fallback pattern"
     :expect (:success t
              :type :chain
              :command-count 2
              :dangerous-p nil
              :all-commands ((:command-name "rm"
                              :subcommand nil
                              :flags ()
                              :positional-args ("file.txt")
                              :dangerous-p nil)
                            (:command-name "echo"
                              :subcommand nil
                              :flags ()
                              :positional-args ("failed to delete")
                              :dangerous-p nil))))

    (:id "chain-003"
     :command "cd /tmp; ls -la; pwd"
     :note "Semicolon-separated commands"
     :expect (:success t
              :type :chain
              :command-count 3
              :dangerous-p nil
              :all-commands ((:command-name "cd"
                              :subcommand nil
                              :flags ()
                              :positional-args ("/tmp")
                              :dangerous-p nil)
                            (:command-name "ls"
                              :subcommand nil
                              :flags ("-la")
                              :positional-args ()
                              :dangerous-p nil)
                            (:command-name "pwd"
                              :subcommand nil
                              :flags ()
                              :positional-args ()
                              :dangerous-p nil))))

    (:id "chain-004"
     :command "git add . && git commit -m 'fix' && git push"
     :note "Three-stage git workflow chain"
     :expect (:success t
              :type :chain
              :command-count 3
              :dangerous-p nil
              :all-commands ((:command-name "git"
                              :subcommand "add"
                              :flags ()
                              :positional-args (".")
                              :dangerous-p nil)
                            (:command-name "git"
                              :subcommand "commit"
                              :flags ("-m")
                              :positional-args ("fix")
                              :dangerous-p nil)
                            (:command-name "git"
                              :subcommand "push"
                              :flags ()
                              :positional-args ()
                              :dangerous-p nil))))

    ;; ============================================================
    ;; REDIRECTIONS (file I/O)
    ;; FULLY PARSED: Redirection operators and targets extracted
    ;; ============================================================
    (:id "redirect-001"
     :command "echo 'hello' > output.txt"
     :note "Output redirection - extracts operator and destination"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("hello")
              :dangerous-p nil
              :redirections ((:type :file
                              :operator ">"
                              :descriptor nil
                              :destination "output.txt"))))

    (:id "redirect-002"
     :command "cat input.txt >> output.txt"
     :note "Append redirection - extracts >> operator"
     :expect (:command-name "cat"
              :subcommand nil
              :flags ()
              :positional-args ("input.txt")
              :dangerous-p nil
              :redirections ((:type :file
                              :operator ">>"
                              :descriptor nil
                              :destination "output.txt"))))

    (:id "redirect-003"
     :command "grep error < logfile.txt"
     :note "Input redirection - extracts < operator"
     :expect (:command-name "grep"
              :subcommand nil
              :flags ()
              :positional-args ("error")
              :dangerous-p nil
              :redirections ((:type :file
                              :operator "<"
                              :descriptor nil
                              :destination "logfile.txt"))))

    (:id "redirect-004"
     :command "command 2>&1"
     :note "Stderr to stdout redirection with descriptor"
     :expect (:command-name "command"
              :subcommand nil
              :flags ()
              :positional-args ()
              :dangerous-p nil
              :redirections ((:type :file
                              :operator ">&"
                              :descriptor "2"
                              :destination "1"))))

    (:id "redirect-005"
     :command "git log > /dev/null 2>&1"
     :note "Multiple redirections - both extracted"
     :expect (:command-name "git"
              :subcommand "log"
              :flags ()
              :positional-args ()
              :dangerous-p nil
              :redirections ((:type :file
                              :operator ">"
                              :descriptor nil
                              :destination "/dev/null")
                             (:type :file
                              :operator ">&"
                              :descriptor "2"
                              :destination "1"))))

    (:id "redirect-006"
     :command "echo 'test' 2> error.log"
     :note "Separate stderr redirection to file"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("test")
              :dangerous-p nil
              :redirections ((:type :file
                              :operator ">"
                              :descriptor "2"
                              :destination "error.log"))))

    ;; ============================================================
    ;; COMMAND SUBSTITUTION (nested commands)
    ;; TODO: Not yet fully implemented - extracts text but doesn't parse nested command
    ;; ============================================================
    (:id "substitution-001"
     :command "echo $(date)"
     :note "Command substitution - extracts text but nested command not separately parsed"
     :expect nil)

    (:id "substitution-002"
     :command "echo `pwd`"
     :note "Backtick substitution - extracts text but nested command not separately parsed"
     :expect nil)

    (:id "substitution-003"
     :command "git commit -m \"$(cat message.txt)\""
     :note "Command substitution in quoted string - not yet handled"
     :expect nil)

    ;; ============================================================
    ;; VARIABLE EXPANSION
    ;; FULLY SUPPORTED: Variables extracted correctly in all contexts
    ;; ============================================================
    (:id "variable-001"
     :command "echo $PATH"
     :note "Bare variable - simple_expansion node extracted correctly"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("$PATH")
              :dangerous-p nil))

    (:id "variable-002"
     :command "rm -rf $HOME/tmp"
     :note "Variable concatenation - extracted as single unit"
     :expect (:command-name "rm"
              :subcommand nil
              :flags ("-rf")
              :positional-args ("$HOME/tmp")
              :dangerous-p t))

    (:id "variable-003"
     :command "git commit -m \"$commit_message\""
     :note "Variable in quoted string - preserved correctly"
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :positional-args ("$commit_message")
              :dangerous-p nil))

    (:id "variable-004"
     :command "echo ${HOME}"
     :note "Braced expansion - expansion node extracted correctly"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("${HOME}")
              :dangerous-p nil))

    (:id "variable-005"
     :command "echo $VAR1 $VAR2"
     :note "Multiple separate variables - both extracted"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("$VAR1" "$VAR2")
              :dangerous-p nil))

    (:id "variable-006"
     :command "ls $HOME/*.txt"
     :note "Variable with glob pattern - concatenation preserved"
     :expect (:command-name "ls"
              :subcommand nil
              :flags ()
              :positional-args ("$HOME/*.txt")
              :dangerous-p nil))

    (:id "variable-007"
     :command "echo prefix$VAR"
     :note "Prefix concatenated with variable"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("prefix$VAR")
              :dangerous-p nil))

    (:id "variable-008"
     :command "echo $VAR/suffix"
     :note "Variable concatenated with suffix"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("$VAR/suffix")
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
     :note "Invalid bash syntax - cannot escape single quote within single quotes"
     :expect nil)

    (:id "quote-003"
     :command "git commit -m $'test\\nwith\\nnewlines'"
     :note "ANSI-C quoting with escape sequences - preserved with $'' wrapper"
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :positional-args ("$'test\\nwith\\nnewlines'")
              :dangerous-p nil))

    (:id "quote-004"
     :command "echo $'it\\'s working'"
     :note "ANSI-C quoting with escaped single quote - works correctly"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("$'it\\'s working'")
              :dangerous-p nil))

    (:id "quote-005"
     :command "echo $'line1\\nline2\\ttab'"
     :note "ANSI-C quoting with multiple escape sequences"
     :expect (:command-name "echo"
              :subcommand nil
              :flags ()
              :positional-args ("$'line1\\nline2\\ttab'")
              :dangerous-p nil))

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
     :note "Background with multiple commands - parsed as list/chain"
     :expect (:success t
              :type :chain
              :command-count 2
              :dangerous-p nil))

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
