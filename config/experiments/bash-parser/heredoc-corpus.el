;;; heredoc-corpus.el --- Test corpus for heredoc parsing -*- lexical-binding: t; -*-

;; Test corpus for heredoc patterns with various delimiters, quoting, and contexts
;; Covers: basic heredoc, quoted delimiters, indented heredoc, in contexts, and complex patterns

(defvar jf/bash-heredoc-corpus
  '(
    ;; ============================================================
    ;; TIER 1: BASIC HEREDOC (pedagogical)
    ;; ============================================================

    (:id "heredoc-basic-001"
     :category "basic"
     :command "cat <<EOF\nHello World\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Hello World\n"
              :heredoc-quoted nil)
     :notes "Simplest heredoc: unquoted delimiter with variable expansion")

    (:id "heredoc-basic-002"
     :category "basic"
     :command "cat <<'EOF'\nLiteral $VAR text\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Literal $VAR text\n"
              :heredoc-quoted t)
     :notes "Single-quoted delimiter prevents expansion")

    (:id "heredoc-basic-003"
     :category "basic"
     :command "cat <<\"EOF\"\nDoubled quoted $USER\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Doubled quoted $USER\n"
              :heredoc-quoted nil)
     :notes "Double-quoted delimiter allows expansion")

    (:id "heredoc-basic-004"
     :category "basic"
     :command "cat <<EOF\nLine with $VAR expansion\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Line with $VAR expansion\n"
              :heredoc-quoted nil
              :variable-expansions ("$VAR"))
     :notes "Variable expansion in unquoted heredoc")

    (:id "heredoc-basic-005"
     :category "basic"
     :command "cat <<END\nEND"
     :expect (:command-name "cat"
              :heredoc-delimiter "END"
              :heredoc-content ""
              :heredoc-quoted nil)
     :notes "Empty heredoc (delimiter immediately follows)")

    (:id "heredoc-basic-006"
     :category "basic"
     :command "cat <<MARKER\nMultiple\nLines\nOf\nContent\nMARKER"
     :expect (:command-name "cat"
              :heredoc-delimiter "MARKER"
              :heredoc-content "Multiple\nLines\nOf\nContent\n"
              :heredoc-quoted nil)
     :notes "Multi-line heredoc with custom delimiter")

    ;; ============================================================
    ;; TIER 2: HEREDOC IN CONTEXTS
    ;; ============================================================

    (:id "heredoc-context-001"
     :category "context"
     :command "git commit -m \"$(cat <<'EOF'\nAdd feature X\n\nDetailed description here.\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\nEOF\n)\""
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :command-substitutions ((:syntax "$()" :content "cat <<'EOF'\nAdd feature X\n\nDetailed description here.\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\nEOF\n" :nesting-level 1))
              :heredoc-delimiter "EOF"
              :heredoc-content "Add feature X\n\nDetailed description here.\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\n"
              :heredoc-quoted t)
     :notes "REAL: Git commit with heredoc inside command substitution")

    (:id "heredoc-context-002"
     :category "context"
     :command "cat <<EOF\nCurrent date: $(date)\nUser: $(whoami)\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Current date: $(date)\nUser: $(whoami)\n"
              :heredoc-quoted nil
              :command-substitutions ((:syntax "$()" :content "date" :nesting-level 1)
                                     (:syntax "$()" :content "whoami" :nesting-level 1)))
     :notes "Command substitution inside heredoc content")

    (:id "heredoc-context-003"
     :category "context"
     :command "cat <<EOF | grep test\nLine 1\ntest line\nLine 3\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Line 1\ntest line\nLine 3\n"
              :heredoc-quoted nil
              :pipeline t)
     :notes "Heredoc with pipe to downstream command")

    (:id "heredoc-context-004"
     :category "context"
     :command "cat <<EOF1\nFirst doc\nEOF1\ncat <<EOF2\nSecond doc\nEOF2"
     :expect (:commands ((:command-name "cat"
                          :heredoc-delimiter "EOF1"
                          :heredoc-content "First doc\n")
                        (:command-name "cat"
                          :heredoc-delimiter "EOF2"
                          :heredoc-content "Second doc\n")))
     :notes "Multiple heredocs in sequence")

    (:id "heredoc-context-005"
     :category "context"
     :command "mysql -u root <<SQL\nSELECT * FROM users;\nDROP TABLE temp;\nSQL"
     :expect (:command-name "mysql"
              :flags ("-u")
              :positional-args ("root")
              :heredoc-delimiter "SQL"
              :heredoc-content "SELECT * FROM users;\nDROP TABLE temp;\n"
              :heredoc-quoted nil)
     :notes "Heredoc feeding input to database command")

    (:id "heredoc-context-006"
     :category "context"
     :command "python3 <<PYTHON\nprint('Hello')\nfor i in range(5):\n    print(i)\nPYTHON"
     :expect (:command-name "python3"
              :heredoc-delimiter "PYTHON"
              :heredoc-content "print('Hello')\nfor i in range(5):\n    print(i)\n"
              :heredoc-quoted nil)
     :notes "Heredoc with inline Python script")

    (:id "heredoc-context-007"
     :category "context"
     :command "cat <<'EOF' > output.txt\nNo expansion here: $HOME\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "No expansion here: $HOME\n"
              :heredoc-quoted t
              :redirects ((:type "file" :operator ">" :target "output.txt")))
     :notes "Heredoc with file redirection")

    (:id "heredoc-context-008"
     :category "context"
     :command "git add -A && git commit -m \"$(cat <<'EOF'\nStreamline CLAUDE.md: reduce redundancy by 73%\n\nConsolidated overlapping sections and removed verbose explanations\nwhile preserving all essential information.\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\nEOF\n)\" && git status"
     :expect (:chain t
              :commands ((:command-name "git" :subcommand "add" :flags ("-A"))
                        (:command-name "git" :subcommand "commit"
                         :heredoc-delimiter "EOF"
                         :heredoc-quoted t
                         :command-substitutions ((:syntax "$()" :nesting-level 1)))
                        (:command-name "git" :subcommand "status")))
     :notes "REAL: Chained git commands with heredoc in middle command")

    ;; ============================================================
    ;; TIER 3: COMPLEX PATTERNS
    ;; ============================================================

    (:id "heredoc-complex-001"
     :category "complex"
     :command "cat <<-\tEOF\n\t\tIndented line\n\t\tAnother line\n\tEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "\t\tIndented line\n\t\tAnother line\n"
              :heredoc-quoted nil
              :heredoc-indented t)
     :notes "Indented heredoc with <<- strips leading tabs")

    (:id "heredoc-complex-002"
     :category "complex"
     :command "cat <<'EOF'\nQuotes \"inside\" the 'heredoc'\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Quotes \"inside\" the 'heredoc'\n"
              :heredoc-quoted t)
     :notes "Nested quotes inside heredoc content")

    (:id "heredoc-complex-003"
     :category "complex"
     :command "cat <<EOF > file.txt 2>&1\nContent with redirect\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Content with redirect\n"
              :heredoc-quoted nil
              :redirects ((:type "file" :operator ">" :target "file.txt")
                         (:type "fd" :operator "2>&1")))
     :notes "Heredoc with stdout redirect and stderr merge")

    (:id "heredoc-complex-004"
     :category "complex"
     :command "cat << 'EOF' | sed 's/foo/bar/g' | grep bar\nfoo test\nmore foo\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "foo test\nmore foo\n"
              :heredoc-quoted t
              :pipeline t
              :pipeline-stages 3)
     :notes "Heredoc in multi-stage pipeline")

    (:id "heredoc-complex-005"
     :category "complex"
     :command "git commit -m \"$(cat <<'EOF'\nConvert OpenSpec to Beads\n\nCreated 5 self-contained beads:\n- emacs-t9l: Add bash validation\n- emacs-9q2: Implement bash tools\n- emacs-aqw: Example preset\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\nEOF\n)\""
     :expect (:command-name "git"
              :subcommand "commit"
              :flags ("-m")
              :heredoc-delimiter "EOF"
              :heredoc-content "Convert OpenSpec to Beads\n\nCreated 5 self-contained beads:\n- emacs-t9l: Add bash validation\n- emacs-9q2: Implement bash tools\n- emacs-aqw: Example preset\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>\n"
              :heredoc-quoted t
              :command-substitutions ((:syntax "$()" :nesting-level 1)))
     :notes "REAL: Multi-line commit message with bullet points")

    (:id "heredoc-complex-006"
     :category "complex"
     :command "cat <<EOF\nLine with backticks: \\`date\\`\nLine with dollars: \\$VAR\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "Line with backticks: \\`date\\`\nLine with dollars: \\$VAR\n"
              :heredoc-quoted nil)
     :notes "Escaped special characters in heredoc")

    (:id "heredoc-complex-007"
     :category "complex"
     :command "cat << 'EOF' \n## Bead Dependency Graph\n\n```\nemacs-t9l (P1)\n    Add validation\n```\nEOF"
     :expect (:command-name "cat"
              :heredoc-delimiter "EOF"
              :heredoc-content "## Bead Dependency Graph\n\n```\nemacs-t9l (P1)\n    Add validation\n```\n"
              :heredoc-quoted t)
     :notes "REAL: Heredoc containing markdown with code fences")

    ;; ============================================================
    ;; TIER 3: EDGE CASES
    ;; ============================================================

    (:id "heredoc-edge-001"
     :category "edge"
     :command "cat <<''\nContent with empty delimiter\n"
     :expect (:command-name "cat"
              :heredoc-delimiter ""
              :heredoc-content "Content with empty delimiter\n"
              :heredoc-quoted t)
     :notes "Empty string as delimiter (unusual but valid)")

    (:id "heredoc-edge-002"
     :category "edge"
     :command "cat <<END_OF_FILE\nContent\nEND_OF_FILE"
     :expect (:command-name "cat"
              :heredoc-delimiter "END_OF_FILE"
              :heredoc-content "Content\n"
              :heredoc-quoted nil)
     :notes "Delimiter with underscores (common convention)")

    (:id "heredoc-edge-003"
     :category "edge"
     :command "cat <<'E!O@F#'\nSpecial $chars @everywhere\nE!O@F#"
     :expect (:command-name "cat"
              :heredoc-delimiter "E!O@F#"
              :heredoc-content "Special $chars @everywhere\n"
              :heredoc-quoted t)
     :notes "Delimiter with special characters")

    (:id "heredoc-edge-004"
     :category "edge"
     :command "cat <<EOF1 <<EOF2\nThis is invalid\nEOF1\nEOF2"
     :expect (:error t
              :error-type "multiple-heredoc-redirects")
     :notes "Invalid: multiple heredoc redirects to same command")

    )
  "Test corpus for heredoc patterns.

Total: 25 test cases
- 6 basic heredoc patterns (simple, quoted, empty, multi-line)
- 8 heredoc in contexts (git commit, command substitution, pipes, redirects, sequences)
- 7 complex patterns (indented, nested quotes, multi-stage pipelines, real examples)
- 4 edge cases (empty delimiter, special chars, invalid syntax)

Categories by tier:
- Tier 1 (basic): 6 tests - Core heredoc mechanics
- Tier 2 (context): 8 tests - Heredoc combined with other features
- Tier 3 (complex): 7 tests - Real-world patterns from research
- Edge cases: 4 tests - Boundary conditions and errors

Real-world examples: 5 tests marked with 'REAL:' extracted from coverage
analysis of actual Claude Code session commands.

NOTE: Expected parse results assume heredoc extraction is implemented
in bash-parser. Current implementation may not extract :heredoc-*
fields yet.")

(provide 'heredoc-corpus)
;;; heredoc-corpus.el ends here
