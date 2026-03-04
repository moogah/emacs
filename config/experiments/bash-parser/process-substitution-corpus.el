;;; process-substitution-corpus.el --- Test corpus for process substitution parsing -*- lexical-binding: t; -*-

;; Test corpus for process substitution with <(...) syntax
;; Covers: basic diff, single input, pipes, real-world patterns, and edge cases

(defvar jf/bash-process-substitution-corpus
  '(
    ;; ============================================================
    ;; TIER 1: BASIC PROCESS SUBSTITUTION (pedagogical)
    ;; ============================================================

    (:id "procsub-basic-001"
     :category "basic"
     :command "diff <(cmd1) <(cmd2)"
     :expect (:command-name "diff"
              :positional-args ("<(cmd1)" "<(cmd2)")
              :process-substitutions ((:syntax "<()" :content "cmd1" :position 1)
                                     (:syntax "<()" :content "cmd2" :position 2)))
     :notes "Simplest case: two process substitutions for comparison")

    (:id "procsub-basic-002"
     :category "basic"
     :command "grep pattern <(find . -name \"*.txt\")"
     :expect (:command-name "grep"
              :positional-args ("pattern" "<(find . -name \"*.txt\")")
              :process-substitutions ((:syntax "<()" :content "find . -name \"*.txt\"" :position 2)))
     :notes "Single process substitution as input to grep")

    (:id "procsub-basic-003"
     :category "basic"
     :command "cat <(echo \"line1\") <(echo \"line2\")"
     :expect (:command-name "cat"
              :positional-args ("<(echo \"line1\")" "<(echo \"line2\")")
              :process-substitutions ((:syntax "<()" :content "echo \"line1\"" :position 1)
                                     (:syntax "<()" :content "echo \"line2\"" :position 2)))
     :notes "Concatenate outputs from multiple process substitutions")

    (:id "procsub-basic-004"
     :category "basic"
     :command "diff <(sort file1) <(sort file2)"
     :expect (:command-name "diff"
              :positional-args ("<(sort file1)" "<(sort file2)")
              :process-substitutions ((:syntax "<()" :content "sort file1" :position 1)
                                     (:syntax "<()" :content "sort file2" :position 2)))
     :notes "Compare sorted files without creating temp files")

    (:id "procsub-basic-005"
     :category "basic"
     :command "wc -l <(ls -1)"
     :expect (:command-name "wc"
              :flags ("-l")
              :positional-args ("<(ls -1)")
              :process-substitutions ((:syntax "<()" :content "ls -1" :position 1)))
     :notes "Count lines from command output via process substitution")

    ;; ============================================================
    ;; TIER 2: PROCESS SUBSTITUTION WITH PIPES
    ;; ============================================================

    (:id "procsub-pipe-001"
     :category "pipe"
     :command "diff <(cat file | grep pattern) <(cat file2 | grep pattern)"
     :expect (:command-name "diff"
              :positional-args ("<(cat file | grep pattern)" "<(cat file2 | grep pattern)")
              :process-substitutions ((:syntax "<()" :content "cat file | grep pattern" :position 1)
                                     (:syntax "<()" :content "cat file2 | grep pattern" :position 2)))
     :notes "Process substitution with pipes inside each substitution")

    (:id "procsub-pipe-002"
     :category "pipe"
     :command "diff <(ls -1 | sort) <(find . -type f | sort)"
     :expect (:command-name "diff"
              :positional-args ("<(ls -1 | sort)" "<(find . -type f | sort)")
              :process-substitutions ((:syntax "<()" :content "ls -1 | sort" :position 1)
                                     (:syntax "<()" :content "find . -type f | sort" :position 2)))
     :notes "Multiple commands piped inside each process substitution")

    (:id "procsub-pipe-003"
     :category "pipe"
     :command "diff <(cmd1 | cmd2 | cmd3) <(cmd4)"
     :expect (:command-name "diff"
              :positional-args ("<(cmd1 | cmd2 | cmd3)" "<(cmd4)")
              :process-substitutions ((:syntax "<()" :content "cmd1 | cmd2 | cmd3" :position 1)
                                     (:syntax "<()" :content "cmd4" :position 2)))
     :notes "Complex pipeline in one substitution, simple command in other")

    (:id "procsub-pipe-004"
     :category "pipe"
     :command "comm <(sort file1 | uniq) <(sort file2 | uniq)"
     :expect (:command-name "comm"
              :positional-args ("<(sort file1 | uniq)" "<(sort file2 | uniq)")
              :process-substitutions ((:syntax "<()" :content "sort file1 | uniq" :position 1)
                                     (:syntax "<()" :content "sort file2 | uniq" :position 2)))
     :notes "Compare unique sorted lines from two files")

    (:id "procsub-pipe-005"
     :category "pipe"
     :command "paste <(cut -f1 file1) <(cut -f2 file2)"
     :expect (:command-name "paste"
              :positional-args ("<(cut -f1 file1)" "<(cut -f2 file2)")
              :process-substitutions ((:syntax "<()" :content "cut -f1 file1" :position 1)
                                     (:syntax "<()" :content "cut -f2 file2" :position 2)))
     :notes "Merge columns from different files")

    ;; ============================================================
    ;; TIER 3: REAL-WORLD PATTERN
    ;; ============================================================

    (:id "procsub-real-001"
     :category "real-world"
     :command "diff <(tail -15 config/experiments/bash-parser/test/test-results.txt) <(tail -15 config/experiments/bash-parser/test-results.txt) && echo \"✓ Identical\""
     :expect (:command-name "diff"
              :positional-args ("<(tail -15 config/experiments/bash-parser/test/test-results.txt)"
                              "<(tail -15 config/experiments/bash-parser/test-results.txt)")
              :process-substitutions ((:syntax "<()" :content "tail -15 config/experiments/bash-parser/test/test-results.txt" :position 1)
                                     (:syntax "<()" :content "tail -15 config/experiments/bash-parser/test-results.txt" :position 2))
              :chain t
              :chain-operator "&&")
     :notes "REAL: From research - Compare last 15 lines of test results files")

    (:id "procsub-real-002"
     :category "real-world"
     :command "diff <(git log --oneline main) <(git log --oneline branch)"
     :expect (:command-name "diff"
              :positional-args ("<(git log --oneline main)" "<(git log --oneline branch)")
              :process-substitutions ((:syntax "<()" :content "git log --oneline main" :position 1)
                                     (:syntax "<()" :content "git log --oneline branch" :position 2)))
     :notes "Compare git logs between branches")

    ;; ============================================================
    ;; TIER 4: EDGE CASES
    ;; ============================================================

    (:id "procsub-edge-001"
     :category "edge"
     :command "some-cmd <(cmd1) <(cmd2) <(cmd3)"
     :expect (:command-name "some-cmd"
              :positional-args ("<(cmd1)" "<(cmd2)" "<(cmd3)")
              :process-substitutions ((:syntax "<()" :content "cmd1" :position 1)
                                     (:syntax "<()" :content "cmd2" :position 2)
                                     (:syntax "<()" :content "cmd3" :position 3)))
     :notes "Three process substitutions as inputs")

    (:id "procsub-edge-002"
     :category "edge"
     :command "diff file.txt <(cmd)"
     :expect (:command-name "diff"
              :positional-args ("file.txt" "<(cmd)")
              :process-substitutions ((:syntax "<()" :content "cmd" :position 2)))
     :notes "Mixed regular file and process substitution")

    (:id "procsub-edge-003"
     :category "edge"
     :command "cat <(grep pattern <(other-cmd))"
     :expect (:command-name "cat"
              :positional-args ("<(grep pattern <(other-cmd))")
              :process-substitutions ((:syntax "<()" :content "grep pattern <(other-cmd)" :position 1)
                                     (:syntax "<()" :content "other-cmd" :position 1 :nesting-level 2)))
     :notes "Nested process substitution (process substitution inside another)")

    (:id "procsub-edge-004"
     :category "edge"
     :command "diff <(cat <<EOF\nline1\nline2\nEOF\n) <(echo -e \"line1\\nline2\")"
     :expect (:command-name "diff"
              :positional-args ("<(cat <<EOF\nline1\nline2\nEOF\n)" "<(echo -e \"line1\\nline2\")")
              :process-substitutions ((:syntax "<()" :content "cat <<EOF\nline1\nline2\nEOF\n" :position 1)
                                     (:syntax "<()" :content "echo -e \"line1\\nline2\"" :position 2))
              :heredoc-delimiter "EOF"
              :heredoc-content "line1\nline2\n")
     :notes "Process substitution containing heredoc")

    (:id "procsub-edge-005"
     :category "edge"
     :command "diff <(cmd1 $(nested-cmd)) <(cmd2)"
     :expect (:command-name "diff"
              :positional-args ("<(cmd1 $(nested-cmd))" "<(cmd2)")
              :process-substitutions ((:syntax "<()" :content "cmd1 $(nested-cmd)" :position 1)
                                     (:syntax "<()" :content "cmd2" :position 2))
              :command-substitutions ((:syntax "$()" :content "nested-cmd" :nesting-level 1)))
     :notes "Process substitution with command substitution inside")

    )
  "Test corpus for process substitution patterns.

Total: 17 test cases
- 5 basic patterns (simple diff, grep, cat, sort comparison, line count)
- 5 pipe patterns (pipes inside process substitutions, complex pipelines)
- 2 real-world patterns (test result comparison, git log comparison)
- 5 edge cases (three inputs, mixed with files, nested, heredoc, command substitution)

Categories by tier:
- Tier 1 (basic): 5 tests - Core process substitution mechanics
- Tier 2 (pipe): 5 tests - Pipelines inside process substitutions
- Tier 3 (real-world): 2 tests - Actual patterns from research
- Tier 4 (edge): 5 tests - Boundary conditions and complex interactions

Real-world examples: 1 test marked with 'REAL:' extracted from coverage
analysis of actual Claude Code session commands. Only 1 real example
found in entire corpus - process substitution is rare in practice.

NOTE: Process substitution <(...) creates a temporary named pipe (FIFO)
that allows command output to be used where a filename is expected.
This is a bash-specific feature (not POSIX sh).

Expected parse structure includes:
- :command-name - The main command using process substitutions
- :process-substitutions - List of <(cmd) patterns with:
  - :syntax \"<()\" - The process substitution syntax marker
  - :content - The command inside the process substitution
  - :position - Argument position (1-indexed)
  - :nesting-level - For nested process substitutions (optional)
- :command-substitutions - If any $(cmd) inside the <() blocks
- :heredoc-* - If heredoc appears inside process substitution

NOTE: Expected parse results assume process substitution extraction
is implemented in bash-parser. Current implementation may not
extract :process-substitutions field yet.")

(provide 'process-substitution-corpus)
;;; process-substitution-corpus.el ends here
