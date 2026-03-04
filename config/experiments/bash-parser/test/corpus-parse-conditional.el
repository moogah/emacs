;;; corpus-parse-conditional.el --- Test corpus for conditional parsing -*- lexical-binding: t; -*-

;; Test corpus for conditional patterns (if/then/else, test operators, [[ ]] syntax)
;; Covers: simple conditionals, test operators, complex real-world patterns, and edge cases
;;
;; CORPUS REFINEMENT (2026-03-04): REVIEWED - Already refined (24→16 tests)
;; Previous refinement removed 8 string/numeric/arithmetic tests with no file impact
;; All remaining tests check file properties (-f, -d, -e, -r, -w, -x) or use file-reading commands

(defvar jf/bash-conditional-corpus
  '(
    ;; ============================================================
    ;; TIER 1: SIMPLE CONDITIONALS (pedagogical)
    ;; ============================================================

    (:id "cond-simple-001"
     :category "simple"
     :command "if [ -f file ]; then echo \"exists\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-f"
                               :test-args ("file"))
              :then-branch ((:command-name "echo"
                             :positional-args ("exists")))
              :else-branch nil)
     :notes "Simplest conditional: file existence test")

    (:id "cond-simple-002"
     :category "simple"
     :command "if [ -d dir ]; then echo \"dir\"; else echo \"no dir\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-d"
                               :test-args ("dir"))
              :then-branch ((:command-name "echo"
                             :positional-args ("dir")))
              :else-branch ((:command-name "echo"
                             :positional-args ("no dir"))))
     :notes "If/then/else: directory test with both branches")

    (:id "cond-simple-003"
     :category "simple"
     :command "test -f file && echo \"found\""
     :expect (:command-name "test"
              :test-operator "-f"
              :positional-args ("file")
              :logical-operator "&&"
              :next-command (:command-name "echo"
                             :positional-args ("found")))
     :notes "Test command with logical AND continuation")

    (:id "cond-simple-004"
     :category "simple"
     :command "if [[ -f file ]]; then cat file; fi"
     :expect (:command-name "if"
              :test-condition (:operator "[["
                               :test-operator "-f"
                               :test-args ("file"))
              :then-branch ((:command-name "cat"
                             :positional-args ("file")))
              :else-branch nil)
     :notes "[[ ]] syntax: modern test expression")

    (:id "cond-simple-005"
     :category "simple"
     :command "[ -x /usr/bin/emacs ] && emacs"
     :expect (:command-name "["
              :test-operator "-x"
              :positional-args ("/usr/bin/emacs")
              :logical-operator "&&"
              :next-command (:command-name "emacs"))
     :notes "Executable test with command continuation")

    (:id "cond-simple-006"
     :category "simple"
     :command "if [ -e path ]; then ls -la path; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-e"
                               :test-args ("path"))
              :then-branch ((:command-name "ls"
                             :flags ("-la")
                             :positional-args ("path")))
              :else-branch nil)
     :notes "Path exists test with ls command")

    ;; ============================================================
    ;; TIER 2: TEST OPERATORS (file, string, numeric, compound)
    ;; ============================================================
    ;; SIMPLIFIED: Removed string/numeric operator variations (no file impact)
    ;; Kept: Compound tests that demonstrate file operations and logical combinations

    ;; REMOVED: cond-testop-001 to -006 (string/numeric tests)
    ;; Reason: For file impact detection, we only care about file test operators (-f, -d, -e, -r, -w, -x)
    ;; String/numeric tests don't affect files, so parser doesn't need detailed operator handling

    (:id "cond-testop-007"
     :category "test-operators"
     :command "if [[ -f file && -r file ]]; then cat file; fi"
     :expect (:command-name "if"
              :test-condition (:operator "[["
                               :compound t
                               :logical-operator "&&"
                               :left-test (:test-operator "-f"
                                           :test-args ("file"))
                               :right-test (:test-operator "-r"
                                            :test-args ("file")))
              :then-branch ((:command-name "cat"
                             :positional-args ("file")))
              :else-branch nil)
     :notes "Compound test: file exists AND readable")

    (:id "cond-testop-008"
     :category "test-operators"
     :command "if [[ -d dir || -f file ]]; then echo \"found\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "[["
                               :compound t
                               :logical-operator "||"
                               :left-test (:test-operator "-d"
                                           :test-args ("dir"))
                               :right-test (:test-operator "-f"
                                            :test-args ("file")))
              :then-branch ((:command-name "echo"
                             :positional-args ("found")))
              :else-branch nil)
     :notes "Compound test: directory OR file with [[ ]]")

    ;; ============================================================
    ;; TIER 3: COMPLEX REAL-WORLD PATTERNS (from research)
    ;; ============================================================

    (:id "cond-complex-001"
     :category "complex"
     :command "if [ -d \"openspec/changes/archive/2026-03-03-bash-script-execution\" ]; then\n  echo \"ERROR: Archive directory already exists\"\n  exit 1\nelse\n  echo \"Archive path is available\"\nfi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-d"
                               :test-args ("openspec/changes/archive/2026-03-03-bash-script-execution"))
              :then-branch ((:command-name "echo"
                             :positional-args ("ERROR: Archive directory already exists"))
                            (:command-name "exit"
                             :positional-args ("1")))
              :else-branch ((:command-name "echo"
                             :positional-args ("Archive path is available"))))
     :notes "REAL: From research - Multi-line conditional with exit in then branch")

    (:id "cond-complex-002"
     :category "complex"
     :command "if [ -d \"openspec/changes/archive/2026-02-08-bootstrap-gptel-scope-spec\" ]; then echo \"EXISTS\"; else echo \"AVAILABLE\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-d"
                               :test-args ("openspec/changes/archive/2026-02-08-bootstrap-gptel-scope-spec"))
              :then-branch ((:command-name "echo"
                             :positional-args ("EXISTS")))
              :else-branch ((:command-name "echo"
                             :positional-args ("AVAILABLE"))))
     :notes "REAL: From research - Single-line conditional with long path")

    (:id "cond-complex-003"
     :category "complex"
     :command "if [ -d \"openspec/changes/archive/2026-03-03-bash-parser-file-ops\" ]; then echo \"ERROR: Archive already exists\"; exit 1; else mv openspec/changes/bash-parser-file-ops openspec/changes/archive/2026-03-03-bash-parser-file-ops && echo \"Archive complete\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-d"
                               :test-args ("openspec/changes/archive/2026-03-03-bash-parser-file-ops"))
              :then-branch ((:command-name "echo"
                             :positional-args ("ERROR: Archive already exists"))
                            (:command-name "exit"
                             :positional-args ("1")))
              :else-branch ((:command-name "mv"
                             :positional-args ("openspec/changes/bash-parser-file-ops"
                                               "openspec/changes/archive/2026-03-03-bash-parser-file-ops"))
                            (:command-name "echo"
                             :positional-args ("Archive complete"))))
     :notes "REAL: From research - Complex else branch with mv and echo chained")

    (:id "cond-complex-004"
     :category "complex"
     :command "mkdir -p openspec/changes/archive && if [ -d \"openspec/changes/archive/2026-02-08-bootstrap-gptel-scope-spec\" ]; then echo \"EXISTS\"; else echo \"AVAILABLE\"; fi"
     :expect (:commands ((:command-name "mkdir"
                          :flags ("-p")
                          :positional-args ("openspec/changes/archive"))
                         (:command-name "if"
                          :test-condition (:operator "["
                                           :test-operator "-d"
                                           :test-args ("openspec/changes/archive/2026-02-08-bootstrap-gptel-scope-spec"))
                          :then-branch ((:command-name "echo"
                                         :positional-args ("EXISTS")))
                          :else-branch ((:command-name "echo"
                                         :positional-args ("AVAILABLE"))))))
     :notes "REAL: From research - Conditional chained after mkdir setup")

    (:id "cond-complex-005"
     :category "complex"
     :command "if [ -f $(which emacs) ]; then echo \"Found Emacs\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-f"
                               :test-args ("$(which emacs)")
                               :command-substitutions ((:syntax "$()"
                                                        :content "which emacs"
                                                        :nesting-level 1)))
              :then-branch ((:command-name "echo"
                             :positional-args ("Found Emacs")))
              :else-branch nil)
     :notes "Conditional with command substitution in test")

    (:id "cond-complex-006"
     :category "complex"
     :command "if grep -q pattern file; then echo \"Found\"; else echo \"Not found\"; fi"
     :expect (:command-name "if"
              :test-condition (:command-name "grep"
                               :flags ("-q")
                               :positional-args ("pattern" "file"))
              :then-branch ((:command-name "echo"
                             :positional-args ("Found")))
              :else-branch ((:command-name "echo"
                             :positional-args ("Not found"))))
     :notes "Conditional using command exit status (not test operator)")

    (:id "cond-complex-007"
     :category "complex"
     :command "if [ -f config.el ]; then\n  source config.el\n  echo \"Config loaded\"\nfi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-f"
                               :test-args ("config.el"))
              :then-branch ((:command-name "source"
                             :positional-args ("config.el"))
                            (:command-name "echo"
                             :positional-args ("Config loaded")))
              :else-branch nil)
     :notes "Multi-command then branch with newlines")

    ;; ============================================================
    ;; TIER 4: EDGE CASES
    ;; ============================================================

    (:id "cond-edge-001"
     :category "edge"
     :command "if [ -f file ]; then echo \"a\"; elif [ -d file ]; then echo \"b\"; else echo \"c\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :test-operator "-f"
                               :test-args ("file"))
              :then-branch ((:command-name "echo"
                             :positional-args ("a")))
              :elif-branches (((:test-condition (:operator "["
                                                 :test-operator "-d"
                                                 :test-args ("file"))
                                :then-branch ((:command-name "echo"
                                               :positional-args ("b"))))))
              :else-branch ((:command-name "echo"
                             :positional-args ("c"))))
     :notes "Elif chain with three branches")

    (:id "cond-edge-002"
     :category "edge"
     :command "if [ ! -f file ]; then echo \"does not exist\"; fi"
     :expect (:command-name "if"
              :test-condition (:operator "["
                               :negation t
                               :test-operator "-f"
                               :test-args ("file"))
              :then-branch ((:command-name "echo"
                             :positional-args ("does not exist")))
              :else-branch nil)
     :notes "Negation operator: ! (NOT)")

    ;; REMOVED: cond-edge-003 (arithmetic test) - no file impact
    ;; REMOVED: cond-edge-004 (null command no-op) - no file impact

    (:id "cond-edge-005"
     :category "edge"
     :command "if true; then echo \"always\"; fi"
     :expect (:command-name "if"
              :test-condition (:command-name "true")
              :then-branch ((:command-name "echo"
                             :positional-args ("always")))
              :else-branch nil)
     :notes "Conditional using true command (always succeeds)")

    )
  "Test corpus for conditional patterns (if/then/else, test operators).

Total: 16 test cases (removed 8 tests with no file impact)
- 6 simple conditionals (basic if/then, if/else, test command, [[ ]] syntax)
- 2 compound test operators (removed 6 string/numeric tests - no file impact)
- 7 complex real-world patterns (from research with command substitution, chaining, multi-line)
- 3 edge cases (elif chains, negation, true/false) (removed: arithmetic, null command)

SIMPLIFIED from 24 to 16 tests:
- Removed: 6 string/numeric test operator variations (cond-testop-001 to -006)
  Reason: String/numeric tests don't affect file operations
- Removed: 2 edge cases (arithmetic test, null command)
  Reason: No file impact

Categories by tier:
- Tier 1 (simple): 6 tests - Basic conditional mechanics
- Tier 2 (test-operators): 2 tests - Compound file tests (kept: && and || combinations)
- Tier 3 (complex): 7 tests - Real-world patterns from coverage analysis
- Tier 4 (edge): 3 tests - Boundary conditions (elif, negation, true command)

Test operator coverage (file-impact focused):
- File tests: -f (regular file), -d (directory), -e (exists), -x (executable), -r (readable)
- Compound tests: && (AND), || (OR) within [[ ]]
- Special: ! (negation)

Real-world examples: 4 tests marked with 'REAL:' extracted from conditional-examples.tsv
showing actual patterns from Claude Code session commands.

NOTE: Expected parse results assume conditional extraction is implemented
in bash-parser. Current implementation may not extract :test-condition,
:then-branch, :else-branch fields yet.")

(provide 'corpus-parse-conditional)
;;; corpus-parse-conditional.el ends here
