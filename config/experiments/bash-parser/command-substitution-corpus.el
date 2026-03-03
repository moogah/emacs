;;; command-substitution-corpus.el --- Test corpus for command substitution parsing -*- lexical-binding: t; -*-

;; Test corpus for command substitution with $(...) and backtick syntax
;; Covers: simple, nested, quoted, piped, backtick, multiple, and complex patterns

(defvar jf/bash-command-substitution-corpus
  '(
    ;; ============================================================
    ;; TIER 1: BASIC COMMAND SUBSTITUTION (pedagogical)
    ;; ============================================================

    (:id "cmdsub-simple-001"
     :category "simple"
     :command "echo $(pwd)"
     :expect (:command-name "echo"
              :positional-args ("$(pwd)")
              :command-substitutions ((:syntax "$()" :content "pwd" :nesting-level 1)))
     :notes "Simplest case: single command substitution")

    (:id "cmdsub-simple-002"
     :category "simple"
     :command "echo $(date)"
     :expect (:command-name "echo"
              :positional-args ("$(date)")
              :command-substitutions ((:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Common pattern: capture date")

    (:id "cmdsub-simple-003"
     :category "simple"
     :command "echo $(whoami)"
     :expect (:command-name "echo"
              :positional-args ("$(whoami)")
              :command-substitutions ((:syntax "$()" :content "whoami" :nesting-level 1)))
     :notes "User identity capture")

    (:id "cmdsub-simple-004"
     :category "simple"
     :command "dir=$(pwd)"
     :expect (:command-name "dir"
              :positional-args ("$(pwd)")
              :command-substitutions ((:syntax "$()" :content "pwd" :nesting-level 1)))
     :notes "Variable assignment with substitution")

    (:id "cmdsub-simple-005"
     :category "simple"
     :command "count=$(ls -1 | wc -l)"
     :expect (:command-name "count"
              :positional-args ("$(ls -1 | wc -l)")
              :command-substitutions ((:syntax "$()" :content "ls -1 | wc -l" :nesting-level 1)))
     :notes "Pipe inside substitution")

    (:id "cmdsub-backtick-001"
     :category "backtick"
     :command "echo `pwd`"
     :expect (:command-name "echo"
              :positional-args ("`pwd`")
              :command-substitutions ((:syntax "`" :content "pwd" :nesting-level 1)))
     :notes "Legacy backtick syntax")

    ;; ============================================================
    ;; TIER 2: NESTED SUBSTITUTION
    ;; ============================================================

    (:id "cmdsub-nested-001"
     :category "nested"
     :command "ls -la $(dirname $(which openspec))"
     :expect (:command-name "ls"
              :flags ("-la")
              :positional-args ("$(dirname $(which openspec))")
              :command-substitutions ((:syntax "$()" :content "dirname $(which openspec)" :nesting-level 1)
                                     (:syntax "$()" :content "which openspec" :nesting-level 2)))
     :notes "REAL: From research - nested directory inspection")

    (:id "cmdsub-nested-002"
     :category "nested"
     :command "echo $(basename $(pwd))"
     :expect (:command-name "echo"
              :positional-args ("$(basename $(pwd))")
              :command-substitutions ((:syntax "$()" :content "basename $(pwd)" :nesting-level 1)
                                     (:syntax "$()" :content "pwd" :nesting-level 2)))
     :notes "Get current directory name")

    (:id "cmdsub-nested-003"
     :category "nested"
     :command "cat $(find . -name config.yml)"
     :expect (:command-name "cat"
              :positional-args ("$(find . -name config.yml)")
              :command-substitutions ((:syntax "$()" :content "find . -name config.yml" :nesting-level 1)))
     :notes "Find and read file")

    (:id "cmdsub-nested-004"
     :category "nested"
     :command "echo $(dirname $(dirname $(pwd)))"
     :expect (:command-name "echo"
              :positional-args ("$(dirname $(dirname $(pwd)))")
              :command-substitutions ((:syntax "$()" :content "dirname $(dirname $(pwd))" :nesting-level 1)
                                     (:syntax "$()" :content "dirname $(pwd)" :nesting-level 2)
                                     (:syntax "$()" :content "pwd" :nesting-level 3)))
     :notes "Triple nesting - grandparent directory")

    ;; ============================================================
    ;; TIER 2: SUBSTITUTION IN QUOTES
    ;; ============================================================

    (:id "cmdsub-quoted-001"
     :category "quoted"
     :command "echo \"Current time: $(date)\""
     :expect (:command-name "echo"
              :positional-args ("Current time: $(date)")
              :command-substitutions ((:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Substitution inside double quotes")

    (:id "cmdsub-quoted-002"
     :category "quoted"
     :command "echo \"=== $(basename \"$dir\") ===\""
     :expect (:command-name "echo"
              :positional-args ("=== $(basename \"$dir\") ===")
              :command-substitutions ((:syntax "$()" :content "basename \"$dir\"" :nesting-level 1)))
     :notes "REAL: From research - nested quotes with variable")

    (:id "cmdsub-quoted-003"
     :category "quoted"
     :command "git commit -m \"Update $(date +%Y-%m-%d)\""
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :positional-args ("Update $(date +%Y-%m-%d)")
              :command-substitutions ((:syntax "$()" :content "date +%Y-%m-%d" :nesting-level 1)))
     :notes "Substitution in git commit message")

    (:id "cmdsub-quoted-004"
     :category "quoted"
     :command "echo 'Literal $(date)'"
     :expect (:command-name "echo"
              :positional-args ("Literal $(date)")
              :command-substitutions ())
     :notes "Single quotes prevent substitution (literal)")

    ;; ============================================================
    ;; TIER 2: PIPES AND REDIRECTS IN SUBSTITUTION
    ;; ============================================================

    (:id "cmdsub-pipe-001"
     :category "pipe"
     :command "echo \"$(find \"$dir\" -name \"*.org\" | wc -l) files\""
     :expect (:command-name "echo"
              :positional-args ("$(find \"$dir\" -name \"*.org\" | wc -l) files")
              :command-substitutions ((:syntax "$()" :content "find \"$dir\" -name \"*.org\" | wc -l" :nesting-level 1)))
     :notes "REAL: From research - pipe inside substitution")

    (:id "cmdsub-pipe-002"
     :category "pipe"
     :command "count=$(cat file.txt | wc -l)"
     :expect (:command-name "count"
              :positional-args ("$(cat file.txt | wc -l)")
              :command-substitutions ((:syntax "$()" :content "cat file.txt | wc -l" :nesting-level 1)))
     :notes "Line count capture")

    (:id "cmdsub-pipe-003"
     :category "pipe"
     :command "echo $(ls | grep test | head -1)"
     :expect (:command-name "echo"
              :positional-args ("$(ls | grep test | head -1)")
              :command-substitutions ((:syntax "$()" :content "ls | grep test | head -1" :nesting-level 1)))
     :notes "Multi-stage pipeline in substitution")

    (:id "cmdsub-pipe-004"
     :category "pipe"
     :command "files=$(find . -type f | sort)"
     :expect (:command-name "files"
              :positional-args ("$(find . -type f | sort)")
              :command-substitutions ((:syntax "$()" :content "find . -type f | sort" :nesting-level 1)))
     :notes "Find and sort files")

    ;; ============================================================
    ;; TIER 2: MULTIPLE SUBSTITUTIONS
    ;; ============================================================

    (:id "cmdsub-multiple-001"
     :category "multiple"
     :command "echo $(pwd) $(date)"
     :expect (:command-name "echo"
              :positional-args ("$(pwd)" "$(date)")
              :command-substitutions ((:syntax "$()" :content "pwd" :nesting-level 1)
                                     (:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Two independent substitutions")

    (:id "cmdsub-multiple-002"
     :category "multiple"
     :command "cp $(which oldcmd) $(which newcmd)"
     :expect (:command-name "cp"
              :positional-args ("$(which oldcmd)" "$(which newcmd)")
              :command-substitutions ((:syntax "$()" :content "which oldcmd" :nesting-level 1)
                                     (:syntax "$()" :content "which newcmd" :nesting-level 1)))
     :notes "Two substitutions as copy source and dest")

    (:id "cmdsub-multiple-003"
     :category "multiple"
     :command "echo \"User: $(whoami) at $(hostname) on $(date)\""
     :expect (:command-name "echo"
              :positional-args ("User: $(whoami) at $(hostname) on $(date)")
              :command-substitutions ((:syntax "$()" :content "whoami" :nesting-level 1)
                                     (:syntax "$()" :content "hostname" :nesting-level 1)
                                     (:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Three substitutions in formatted string")

    (:id "cmdsub-multiple-004"
     :category "multiple"
     :command "diff $(ls *.old) $(ls *.new)"
     :expect (:command-name "diff"
              :positional-args ("$(ls *.old)" "$(ls *.new)")
              :command-substitutions ((:syntax "$()" :content "ls *.old" :nesting-level 1)
                                     (:syntax "$()" :content "ls *.new" :nesting-level 1)))
     :notes "Substitutions with glob patterns")

    ;; ============================================================
    ;; TIER 3: COMPLEX REAL-WORLD PATTERNS
    ;; ============================================================

    (:id "cmdsub-complex-001"
     :category "complex"
     :command "for file in $(find . -name \"*.el\" -not -name \"gptel.el\" | sort); do echo \"=== $file ===\"; done"
     :expect (:command-name "for"
              :command-substitutions ((:syntax "$()" :content "find . -name \"*.el\" -not -name \"gptel.el\" | sort" :nesting-level 1)))
     :notes "REAL: From research - substitution feeds for loop")

    (:id "cmdsub-complex-002"
     :category "complex"
     :command "for dir in openspec/changes/*/; do echo \"=== $(basename \"$dir\") ===\"; done"
     :expect (:command-name "for"
              :command-substitutions ((:syntax "$()" :content "basename \"$dir\"" :nesting-level 1)))
     :notes "REAL: From research - loop with basename substitution")

    (:id "cmdsub-complex-003"
     :category "complex"
     :command "test -f $(which emacs) && echo \"Found\""
     :expect (:command-name "test"
              :flags ("-f")
              :positional-args ("$(which emacs)")
              :command-substitutions ((:syntax "$()" :content "which emacs" :nesting-level 1)))
     :notes "Substitution in conditional test")

    (:id "cmdsub-complex-004"
     :category "complex"
     :command "result=$(if [ -f test ]; then cat test; else echo default; fi)"
     :expect (:command-name "result"
              :positional-args ("$(if [ -f test ]; then cat test; else echo default; fi)")
              :command-substitutions ((:syntax "$()" :content "if [ -f test ]; then cat test; else echo default; fi" :nesting-level 1)))
     :notes "Conditional inside substitution")

    ;; ============================================================
    ;; TIER 3: EDGE CASES
    ;; ============================================================

    (:id "cmdsub-edge-001"
     :category "edge"
     :command "echo $(($(date +%s) + 3600))"
     :expect (:command-name "echo"
              :positional-args ("$(($(date +%s) + 3600))")
              :command-substitutions ((:syntax "$()" :content "date +%s" :nesting-level 1)))
     :notes "Substitution inside arithmetic expansion")

    (:id "cmdsub-edge-002"
     :category "edge"
     :command "cat <<EOF\nLine with $(date)\nEOF"
     :expect (:command-name "cat"
              :command-substitutions ((:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Substitution in heredoc")

    (:id "cmdsub-edge-003"
     :category "edge"
     :command "echo `echo \\`date\\``"
     :expect (:command-name "echo"
              :positional-args ("`echo \\`date\\``")
              :command-substitutions ((:syntax "`" :content "echo \\`date\\`" :nesting-level 1)
                                     (:syntax "`" :content "date" :nesting-level 2)))
     :notes "Nested backticks with escaping")

    (:id "cmdsub-edge-004"
     :category "edge"
     :command "echo $()"
     :expect (:command-name "echo"
              :positional-args ("$()")
              :command-substitutions ((:syntax "$()" :content "" :nesting-level 1)))
     :notes "Empty substitution")

    (:id "cmdsub-edge-005"
     :category "edge"
     :command "echo \\$(not-a-substitution)"
     :expect (:command-name "echo"
              :positional-args ("\\$(not-a-substitution)")
              :command-substitutions ())
     :notes "Escaped substitution (literal)")

    )
  "Test corpus for command substitution patterns.

Total: 30 test cases
- 6 simple/basic patterns
- 4 nested substitutions
- 4 quoted contexts
- 4 pipe/redirect patterns
- 4 multiple substitutions
- 4 complex real-world patterns
- 4 edge cases

NOTE: Expected parse results assume command substitution extraction
is implemented in bash-parser. Current implementation may not
extract :command-substitutions field yet.")

(provide 'command-substitution-corpus)
;;; command-substitution-corpus.el ends here
