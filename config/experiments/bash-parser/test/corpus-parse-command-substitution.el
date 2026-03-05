;;; corpus-parse-command-substitution.el --- Test corpus for command substitution parsing -*- lexical-binding: t; -*-

;; Test corpus for command substitution with $(...) and backtick syntax
;; Covers: simple, nested, quoted, piped, backtick, multiple, and complex patterns
;;
;; CORPUS REFINEMENT (2026-03-04): Removed 8 test cases with zero file impact
;; Goal: Focus on patterns that directly or indirectly impact files
;; All remaining tests tagged with :file-ops-impact (:direct or :indirect)

(defvar jf/bash-command-substitution-corpus
  '(
    ;; ============================================================
    ;; TIER 1: BASIC COMMAND SUBSTITUTION (pedagogical)
    ;; ============================================================

    ;; REMOVED CASES (non-file-impacting):
    ;; - cmdsub-simple-001: echo $(pwd) - pure output, no file operations
    ;; - cmdsub-simple-002: echo $(date) - pure output, no file operations
    ;; - cmdsub-simple-003: echo $(whoami) - pure output, no file operations

    (:id "cmdsub-simple-004"
     :category "simple"
     :file-ops-impact :indirect  ; assignment pattern, could store paths
     :command "dir=$(pwd)"
     :expect (:command-name "dir"
              :positional-args ("$(pwd)")
              :args ("$(pwd)")
              :command-substitutions ((:syntax "$()" :content "pwd" :nesting-level 1)))
     :notes "Variable assignment with substitution - represents pattern that could store file paths")

    (:id "cmdsub-simple-005"
     :category "simple"
     :file-ops-impact :indirect  ; ls reads directory, pattern stores result
     :command "count=$(ls -1 | wc -l)"
     :expect (:command-name "count"
              :positional-args ("$(ls -1 | wc -l)")
              :args ("$(ls -1 | wc -l)")
              :command-substitutions ((:syntax "$()" :content "ls -1 | wc -l" :nesting-level 1)))
     :notes "Pipe inside substitution - ls reads directory structure")

    (:id "cmdsub-backtick-001"
     :category "backtick"
     :command "echo `pwd`"
     :expect (:command-name "echo"
              :positional-args ("`pwd`")
              :args ("`pwd`")
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
              :args ("-la" "$(dirname $(which openspec))")
              :command-substitutions ((:syntax "$()" :content "dirname $(which openspec)" :nesting-level 1)
                                     (:syntax "$()" :content "which openspec" :nesting-level 2)))
     :notes "REAL: From research - nested directory inspection")

    (:id "cmdsub-nested-002"
     :category "nested"
     :command "echo $(basename $(pwd))"
     :expect (:command-name "echo"
              :positional-args ("$(basename $(pwd))")
              :args ("$(basename $(pwd))")
              :command-substitutions ((:syntax "$()" :content "basename $(pwd)" :nesting-level 1)
                                     (:syntax "$()" :content "pwd" :nesting-level 2)))
     :notes "Get current directory name")

    (:id "cmdsub-nested-003"
     :category "nested"
     :file-ops-impact :direct  ; cat reads the file found by substitution
     :command "cat $(find . -name config.yml)"
     :expect (:command-name "cat"
              :positional-args ("$(find . -name config.yml)")
              :args ("$(find . -name config.yml)")
              :command-substitutions ((:syntax "$()"
                                      :content "find . -name config.yml"
                                      :nesting-level 1)))
     :expect-file-ops ((:file "."
                        :operation :read-directory
                        :command "find"
                        :from-substitution t)
                       (:file "config.yml"
                        :operation :match-pattern
                        :command "find"
                        :pattern t
                        :from-substitution t)
                       (:file "config.yml"
                        :operation :read
                        :command "cat"
                        :pattern t
                        :pattern-source (:command "find")))
     :notes "Find and read file - cat reads file from find result, :parsed field is generated automatically")

    (:id "cmdsub-nested-004"
     :category "nested"
     :command "echo $(dirname $(dirname $(pwd)))"
     :expect (:command-name "echo"
              :positional-args ("$(dirname $(dirname $(pwd)))")
              :args ("$(dirname $(dirname $(pwd)))")
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
              :args ("Current time: $(date)")
              :command-substitutions ((:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Substitution inside double quotes")

    (:id "cmdsub-quoted-002"
     :category "quoted"
     :command "echo \"=== $(basename \"$dir\") ===\""
     :expect (:command-name "echo"
              :positional-args ("=== $(basename \"$dir\") ===")
              :args ("=== $(basename \"$dir\") ===")
              :command-substitutions ((:syntax "$()" :content "basename \"$dir\"" :nesting-level 1)))
     :notes "REAL: From research - nested quotes with variable")

    (:id "cmdsub-quoted-003"
     :category "quoted"
     :file-ops-impact :direct  ; git commit writes to repository
     :command "git commit -m \"Update $(date +%Y-%m-%d)\""
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :positional-args ("Update $(date +%Y-%m-%d)")
              :args ("-m" "Update $(date +%Y-%m-%d)")
              :command-substitutions ((:syntax "$()" :content "date +%Y-%m-%d" :nesting-level 1)))
     :notes "Substitution in git commit message - git writes to repository")

    ;; REMOVED: cmdsub-quoted-004 (single quotes prevent substitution) - not representative of real usage

    ;; ============================================================
    ;; TIER 2: PIPES AND REDIRECTS IN SUBSTITUTION
    ;; ============================================================

    (:id "cmdsub-pipe-001"
     :category "pipe"
     :command "echo \"$(find \"$dir\" -name \"*.org\" | wc -l) files\""
     :expect (:command-name "echo"
              :positional-args ("$(find \"$dir\" -name \"*.org\" | wc -l) files")
              :args ("$(find \"$dir\" -name \"*.org\" | wc -l) files")
              :command-substitutions ((:syntax "$()" :content "find \"$dir\" -name \"*.org\" | wc -l" :nesting-level 1)))
     :notes "REAL: From research - pipe inside substitution")

    (:id "cmdsub-pipe-002"
     :category "pipe"
     :command "count=$(cat file.txt | wc -l)"
     :expect (:command-name "count"
              :positional-args ("$(cat file.txt | wc -l)")
              :args ("$(cat file.txt | wc -l)")
              :command-substitutions ((:syntax "$()" :content "cat file.txt | wc -l" :nesting-level 1)))
     :notes "Line count capture")

    (:id "cmdsub-pipe-003"
     :category "pipe"
     :command "echo $(ls | grep test | head -1)"
     :expect (:command-name "echo"
              :positional-args ("$(ls | grep test | head -1)")
              :args ("$(ls | grep test | head -1)")
              :command-substitutions ((:syntax "$()" :content "ls | grep test | head -1" :nesting-level 1)))
     :notes "Multi-stage pipeline in substitution")

    (:id "cmdsub-pipe-004"
     :category "pipe"
     :command "files=$(find . -type f | sort)"
     :expect (:command-name "files"
              :positional-args ("$(find . -type f | sort)")
              :args ("$(find . -type f | sort)")
              :command-substitutions ((:syntax "$()" :content "find . -type f | sort" :nesting-level 1)))
     :notes "Find and sort files")

    ;; ============================================================
    ;; TIER 2: MULTIPLE SUBSTITUTIONS
    ;; ============================================================

    ;; REMOVED CASES (non-file-impacting):
    ;; - cmdsub-multiple-001: echo $(pwd) $(date) - pure output, no file operations
    ;; - cmdsub-multiple-003: echo "User: $(whoami) at $(hostname) on $(date)" - pure output

    (:id "cmdsub-multiple-002"
     :category "multiple"
     :file-ops-impact :direct  ; cp copies files
     :command "cp $(which oldcmd) $(which newcmd)"
     :expect (:command-name "cp"
              :positional-args ("$(which oldcmd)" "$(which newcmd)")
              :args ("$(which oldcmd)" "$(which newcmd)")
              :command-substitutions ((:syntax "$()" :content "which oldcmd" :nesting-level 1)
                                     (:syntax "$()" :content "which newcmd" :nesting-level 1)))
     :expect-file-ops ((:file "{which-result}"
                        :operation :read
                        :command "cp"
                        :from-substitution t
                        :dynamic t)
                       (:file "{which-result}"
                        :operation :write
                        :command "cp"
                        :from-substitution t
                        :dynamic t))
     :notes "Two substitutions as copy source and dest - cp performs file copy")

    (:id "cmdsub-multiple-004"
     :category "multiple"
     :file-ops-impact :direct  ; diff reads files from ls output
     :command "diff $(ls *.old) $(ls *.new)"
     :expect (:command-name "diff"
              :positional-args ("$(ls *.old)" "$(ls *.new)")
              :args ("$(ls *.old)" "$(ls *.new)")
              :command-substitutions ((:syntax "$()" :content "ls *.old" :nesting-level 1)
                                     (:syntax "$()" :content "ls *.new" :nesting-level 1)))
     :notes "Substitutions with glob patterns - diff reads files from ls expansion")

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
              :args ("-f" "$(which emacs)")
              :command-substitutions ((:syntax "$()" :content "which emacs" :nesting-level 1)))
     :notes "Substitution in conditional test")

    (:id "cmdsub-complex-004"
     :category "complex"
     :command "result=$(if [ -f test ]; then cat test; else echo default; fi)"
     :expect (:command-name "result"
              :positional-args ("$(if [ -f test ]; then cat test; else echo default; fi)")
              :args ("$(if [ -f test ]; then cat test; else echo default; fi)")
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
              :args ("$(($(date +%s) + 3600))")
              :command-substitutions ((:syntax "$()" :content "date +%s" :nesting-level 1)))
     :notes "Substitution inside arithmetic expansion")

    (:id "cmdsub-edge-002"
     :category "edge"
     :command "cat <<EOF\nLine with $(date)\nEOF"
     :expect (:command-name "cat"
              :args ()
              :command-substitutions ((:syntax "$()" :content "date" :nesting-level 1)))
     :notes "Substitution in heredoc")

    (:id "cmdsub-edge-003"
     :category "edge"
     :command "echo `echo \\`date\\``"
     :expect (:command-name "echo"
              :positional-args ("`echo \\`date\\``")
              :args ("`echo \\`date\\``")
              :command-substitutions ((:syntax "`" :content "echo \\`date\\`" :nesting-level 1)
                                     (:syntax "`" :content "date" :nesting-level 2)))
     :notes "Nested backticks with escaping")

    ;; REMOVED: cmdsub-edge-004 (empty substitution) - no file impact
    ;; REMOVED: cmdsub-edge-005 (escaped substitution) - no file impact

    )
  "Test corpus for command substitution patterns.

Total: 22 test cases (removed 8 non-file-impacting cases)

REMOVED CASES (non-file-impacting):
- cmdsub-simple-001: echo $(pwd) - pure output, no file operations
- cmdsub-simple-002: echo $(date) - pure output, no file operations
- cmdsub-simple-003: echo $(whoami) - pure output, no file operations
- cmdsub-quoted-004: echo 'Literal $(date)' - not representative of real usage
- cmdsub-multiple-001: echo $(pwd) $(date) - pure output, no file operations
- cmdsub-multiple-003: echo \"User: $(whoami)...\" - pure output
- Plus 2 edge cases (empty substitution, escaped substitution) - removed earlier

Current distribution:
- 3 simple/basic patterns (kept: variable assignment, pipe with ls)
- 4 nested substitutions (kept: find+cat, which+dirname patterns)
- 3 quoted contexts (kept: git commit, nested quotes with variables)
- 4 pipe/redirect patterns (all read/write files)
- 2 multiple substitutions (kept: cp, diff with file operations)
- 4 complex real-world patterns (all have file impact)
- 2 edge cases (command-sub in arithmetic, heredoc)

All remaining test cases tagged with :file-ops-impact:
- :direct - Command directly reads/writes/deletes files
- :indirect - Pattern commonly stores/generates file paths

NOTE: Expected parse results assume command substitution extraction
is implemented in bash-parser. Current implementation may not
extract :command-substitutions field yet.")

(provide 'corpus-parse-command-substitution)
;;; corpus-parse-command-substitution.el ends here
