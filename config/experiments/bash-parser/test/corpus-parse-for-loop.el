;;; corpus-parse-for-loop.el --- Test corpus for for-loop parsing -*- lexical-binding: t; -*-

;; Test corpus for for-loop patterns with various iteration lists, bodies, and contexts
;; Covers: basic iteration, expansions, command substitution, nested patterns, and real-world examples
;;
;; CORPUS REFINEMENT (2026-03-04): Removed 2 test cases with zero file impact
;; Goal: Focus on loops that perform file operations or iterate over file-related lists
;; Removed: Pure echo loops with no file context

(defvar jf/bash-for-loop-corpus
  '(
    ;; ============================================================
    ;; TIER 1: SIMPLE FOR-LOOPS (pedagogical)
    ;; ============================================================

    ;; REMOVED CASES (non-file-impacting):
    ;; - forloop-simple-001: for x in a b c; do echo $x; done - pure output, no file context
    ;; - forloop-simple-002: for i in {1..5}; do echo $i; done - pure output, no file context

    (:id "forloop-simple-003"
     :category "simple"
     :file-ops-impact :direct  ; cat reads each file
     :command "for file in file1 file2 file3; do cat $file; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("file1" "file2" "file3")
              :loop-body "cat $file")
     :notes "File list iteration with simple body command - cat reads files")

    (:id "forloop-simple-004"
     :category "simple"
     :command "for name in alice bob charlie; do echo \"Hello $name\"; done"
     :expect (:command-name "for"
              :loop-variable "name"
              :loop-list ("alice" "bob" "charlie")
              :loop-body "echo \"Hello $name\"")
     :notes "Simple iteration with quoted output")

    (:id "forloop-simple-005"
     :category "simple"
     :file-ops-impact :direct  ; ls reads directory structure
     :command "for dir in src test lib; do ls $dir; done"
     :expect (:command-name "for"
              :loop-variable "dir"
              :loop-list ("src" "test" "lib")
              :loop-body "ls $dir")
     :notes "Directory iteration with ls command - reads directory contents")

    ;; ============================================================
    ;; TIER 2: FOR-LOOPS WITH EXPANSIONS
    ;; ============================================================

    (:id "forloop-expansion-001"
     :category "expansion"
     :command "for file in *.txt; do echo $file; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("*.txt")
              :loop-body "echo $file"
              :glob-pattern t)
     :notes "Glob pattern expansion for txt files")

    (:id "forloop-expansion-002"
     :category "expansion"
     :command "for dir in */; do ls $dir; done"
     :expect (:command-name "for"
              :loop-variable "dir"
              :loop-list ("*/")
              :loop-body "ls $dir"
              :glob-pattern t)
     :notes "Directory glob with trailing slash")

    (:id "forloop-expansion-003"
     :category "expansion"
     :command "for file in $(find . -name \"*.el\"); do echo $file; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("$(find . -name \"*.el\")")
              :loop-body "echo $file"
              :command-substitutions ((:syntax "$()" :content "find . -name \"*.el\"" :nesting-level 1)))
     :notes "Command substitution generates iteration list")

    (:id "forloop-expansion-004"
     :category "expansion"
     :command "for dir in */; do echo \"=== $(basename \"$dir\") ===\"; done"
     :expect (:command-name "for"
              :loop-variable "dir"
              :loop-list ("*/")
              :loop-body "echo \"=== $(basename \"$dir\") ===\""
              :glob-pattern t
              :command-substitutions ((:syntax "$()" :content "basename \"$dir\"" :nesting-level 1)))
     :notes "Glob list with command substitution in body")

    (:id "forloop-expansion-005"
     :category "expansion"
     :command "for file in *.{txt,md}; do cat $file; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("*.{txt,md}")
              :loop-body "cat $file"
              :glob-pattern t
              :brace-expansion t)
     :notes "Combined glob and brace expansion")

    (:id "forloop-expansion-006"
     :category "expansion"
     :command "for num in {1..10..2}; do echo $num; done"
     :expect (:command-name "for"
              :loop-variable "num"
              :loop-list ("{1..10..2}")
              :loop-body "echo $num"
              :brace-expansion t)
     :notes "Brace expansion with step increment")

    (:id "forloop-expansion-007"
     :category "expansion"
     :command "for file in $(ls *.org | sort); do echo $file; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("$(ls *.org | sort)")
              :loop-body "echo $file"
              :command-substitutions ((:syntax "$()" :content "ls *.org | sort" :nesting-level 1)))
     :notes "Command substitution with pipe in list generation")

    (:id "forloop-expansion-008"
     :category "expansion"
     :command "for dir in */; do echo \"$(find \"$dir\" -name \"*.org\" | wc -l) files\"; done"
     :expect (:command-name "for"
              :loop-variable "dir"
              :loop-list ("*/")
              :loop-body "echo \"$(find \"$dir\" -name \"*.org\" | wc -l) files\""
              :glob-pattern t
              :command-substitutions ((:syntax "$()" :content "find \"$dir\" -name \"*.org\" | wc -l" :nesting-level 1)))
     :notes "Complex command substitution in loop body with pipe")

    ;; ============================================================
    ;; TIER 3: COMPLEX REAL-WORLD PATTERNS
    ;; ============================================================

    (:id "forloop-real-001"
     :category "complex-real"
     :command "for bead in emacs-ags emacs-1n7 emacs-blx emacs-eii emacs-crn emacs-ckg emacs-rqp emacs-dw4 emacs-48j emacs-s75; do echo \"=== $bead ===\"; bd show \"$bead\" 2>/dev/null | head -5; done"
     :expect (:command-name "for"
              :loop-variable "bead"
              :loop-list ("emacs-ags" "emacs-1n7" "emacs-blx" "emacs-eii" "emacs-crn" "emacs-ckg" "emacs-rqp" "emacs-dw4" "emacs-48j" "emacs-s75")
              :loop-body "echo \"=== $bead ===\"; bd show \"$bead\" 2>/dev/null | head -5"
              :pipeline t
              :redirects ((:type "fd" :operator "2>" :target "/dev/null")))
     :notes "REAL: From research - Multiple beads with compound body and redirection")

    (:id "forloop-real-002"
     :category "complex-real"
     :command "for id in emacs-aa89 emacs-jv63 emacs-nldf emacs-b01z emacs-8oyz emacs-gaah emacs-wxn1 emacs-85i3; do\n  echo \"=== $id ===\"\n  bd show $id --format '{{.Description}}' 2>&1\n  echo \"\"\ndone"
     :expect (:command-name "for"
              :loop-variable "id"
              :loop-list ("emacs-aa89" "emacs-jv63" "emacs-nldf" "emacs-b01z" "emacs-8oyz" "emacs-gaah" "emacs-wxn1" "emacs-85i3")
              :loop-body "echo \"=== $id ===\"\n  bd show $id --format '{{.Description}}' 2>&1\n  echo \"\""
              :multiline t
              :redirects ((:type "fd" :operator "2>&1")))
     :notes "REAL: From research - Multi-line body with stderr merge")

    (:id "forloop-real-003"
     :category "complex-real"
     :command "for commit in 00950fb 7abad63 b7d3bec d93c732 9862675 6d84cfd; do\n  echo \"=== $commit ===\"\n  git show --stat --format=\"%s%n%b\" $commit | head -30\n  echo \"\"\ndone"
     :expect (:command-name "for"
              :loop-variable "commit"
              :loop-list ("00950fb" "7abad63" "b7d3bec" "d93c732" "9862675" "6d84cfd")
              :loop-body "echo \"=== $commit ===\"\n  git show --stat --format=\"%s%n%b\" $commit | head -30\n  echo \"\""
              :multiline t
              :pipeline t)
     :notes "REAL: From research - Git commit iteration with formatted output")

    (:id "forloop-real-004"
     :category "complex-real"
     :command "for change in scoped-bash-command-execution gptel-preset-upstream-alignment gptel-preset-refactor; do\n  echo \"=== $change ===\"\n  if [ -d \"openspec/changes/$change/specs\" ]; then\n    find \"openspec/changes/$change/specs\" -name \"spec.md\" -type f | sed 's|^openspec/changes/[^/]*/specs/||' || echo \"  (no spec.md files)\"\n  else\n    echo \"  (no specs directory)\"\n  fi\ndone"
     :expect (:command-name "for"
              :loop-variable "change"
              :loop-list ("scoped-bash-command-execution" "gptel-preset-upstream-alignment" "gptel-preset-refactor")
              :loop-body "echo \"=== $change ===\"\n  if [ -d \"openspec/changes/$change/specs\" ]; then\n    find \"openspec/changes/$change/specs\" -name \"spec.md\" -type f | sed 's|^openspec/changes/[^/]*/specs/||' || echo \"  (no spec.md files)\"\n  else\n    echo \"  (no specs directory)\"\n  fi"
              :multiline t
              :conditional t
              :pipeline t)
     :notes "REAL: From research - Nested conditional with find and sed pipeline")

    (:id "forloop-real-005"
     :category "complex-real"
     :command "for bead_id in emacs-7jj emacs-7xb emacs-qwu emacs-9lu emacs-9hs emacs-oyg emacs-u5z; do\n  bd show \"$bead_id\" 2>&1 | grep -E \"^\\S\" | head -1\ndone"
     :expect (:command-name "for"
              :loop-variable "bead_id"
              :loop-list ("emacs-7jj" "emacs-7xb" "emacs-qwu" "emacs-9lu" "emacs-9hs" "emacs-oyg" "emacs-u5z")
              :loop-body "bd show \"$bead_id\" 2>&1 | grep -E \"^\\S\" | head -1"
              :multiline t
              :pipeline t
              :redirects ((:type "fd" :operator "2>&1")))
     :notes "REAL: From research - Multi-stage pipeline with regex grep")

    (:id "forloop-real-006"
     :category "complex-real"
     :command "for change in scoped-bash-command-execution gptel-preset-upstream-alignment gptel-preset-refactor; do echo \"=== $change ===\"; find \"openspec/changes/$change/specs\" -name \"spec.md\" 2>/dev/null | head -5; done"
     :expect (:command-name "for"
              :loop-variable "change"
              :loop-list ("scoped-bash-command-execution" "gptel-preset-upstream-alignment" "gptel-preset-refactor")
              :loop-body "echo \"=== $change ===\"; find \"openspec/changes/$change/specs\" -name \"spec.md\" 2>/dev/null | head -5"
              :pipeline t
              :redirects ((:type "fd" :operator "2>" :target "/dev/null")))
     :notes "REAL: From research - Single-line body with find and pipeline")

    (:id "forloop-real-007"
     :category "complex-real"
     :command "for preset in explore executor research plan; do echo \"=== $preset.md ===\"; grep -E \"^(use-tools|include-tool-results):\" /Users/jefffarr/emacs/config/gptel/presets/$preset.md 2>/dev/null || echo \"NOT FOUND\"; done"
     :expect (:command-name "for"
              :loop-variable "preset"
              :loop-list ("explore" "executor" "research" "plan")
              :loop-body "echo \"=== $preset.md ===\"; grep -E \"^(use-tools|include-tool-results):\" /Users/jefffarr/emacs/config/gptel/presets/$preset.md 2>/dev/null || echo \"NOT FOUND\""
              :redirects ((:type "fd" :operator "2>" :target "/dev/null"))
              :logical-operator "||")
     :notes "REAL: From research - Grep with fallback using OR operator")

    (:id "forloop-real-008"
     :category "complex-real"
     :command "cd openspec/changes/bash-parser-file-ops/specs && for dir in */; do\n  echo \"Copying ${dir%/}...\"\n  cp -r \"$dir\" \"../../../specs/\"\ndone && echo \"Sync complete\""
     :expect (:command-name "for"
              :loop-variable "dir"
              :loop-list ("*/")
              :loop-body "echo \"Copying ${dir%/}...\"\n  cp -r \"$dir\" \"../../../specs/\""
              :multiline t
              :glob-pattern t
              :parameter-expansion t
              :file-operations ((:operation "copy" :flags ("-r"))))
     :notes "REAL: From research - Directory sync with parameter expansion and file ops")

    ;; ============================================================
    ;; TIER 3: NESTED AND COMPOUND PATTERNS
    ;; ============================================================

    (:id "forloop-nested-001"
     :category "nested"
     :command "for file in $(find . -name \"*.el\" -not -name \"gptel.el\" | sort); do echo \"=== $file ===\"; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("$(find . -name \"*.el\" -not -name \"gptel.el\" | sort)")
              :loop-body "echo \"=== $file ===\""
              :command-substitutions ((:syntax "$()" :content "find . -name \"*.el\" -not -name \"gptel.el\" | sort" :nesting-level 1)))
     :notes "Find with exclusion and sort feeding for loop")

    (:id "forloop-nested-002"
     :category "nested"
     :command "for dir in openspec/changes/*/; do echo \"=== $(basename \"$dir\") ===\"; done"
     :expect (:command-name "for"
              :loop-variable "dir"
              :loop-list ("openspec/changes/*/")
              :loop-body "echo \"=== $(basename \"$dir\") ===\""
              :glob-pattern t
              :command-substitutions ((:syntax "$()" :content "basename \"$dir\"" :nesting-level 1)))
     :notes "Nested glob path with basename substitution")

    (:id "forloop-compound-001"
     :category "compound"
     :command "for file in *.txt; do cat $file; echo \"---\"; done"
     :expect (:command-name "for"
              :loop-variable "file"
              :loop-list ("*.txt")
              :loop-body "cat $file; echo \"---\""
              :glob-pattern t
              :compound-body t)
     :notes "Multiple commands in body separated by semicolon")

    (:id "forloop-compound-002"
     :category "compound"
     :command "for i in {1..3}; do echo \"Start $i\"; sleep 1; echo \"End $i\"; done"
     :expect (:command-name "for"
              :loop-variable "i"
              :loop-list ("{1..3}")
              :loop-body "echo \"Start $i\"; sleep 1; echo \"End $i\""
              :brace-expansion t
              :compound-body t)
     :notes "Three-command body with sleep")

    ;; ============================================================
    ;; TIER 4: EDGE CASES
    ;; ============================================================

    (:id "forloop-edge-001"
     :category "edge"
     :command "for x in; do echo $x; done"
     :expect (:command-name "for"
              :loop-variable "x"
              :loop-list ()
              :loop-body "echo $x")
     :notes "Empty iteration list (loop body never executes)")

    (:id "forloop-edge-002"
     :category "edge"
     :command "for item in single; do echo $item; done"
     :expect (:command-name "for"
              :loop-variable "item"
              :loop-list ("single")
              :loop-body "echo $item")
     :notes "Single iteration")

    (:id "forloop-edge-003"
     :category "edge"
     :command "for i in {1..5}; do echo $i; break; done"
     :expect (:command-name "for"
              :loop-variable "i"
              :loop-list ("{1..5}")
              :loop-body "echo $i; break"
              :brace-expansion t
              :loop-control "break")
     :notes "Loop with break statement (exits after first iteration)")

    (:id "forloop-edge-004"
     :category "edge"
     :command "for i in {1..5}; do [ $i -eq 3 ] && continue; echo $i; done"
     :expect (:command-name "for"
              :loop-variable "i"
              :loop-list ("{1..5}")
              :loop-body "[ $i -eq 3 ] && continue; echo $i"
              :brace-expansion t
              :loop-control "continue"
              :conditional t)
     :notes "Loop with continue statement (skips iteration when i=3)")

    )
  "Test corpus for for-loop patterns.

Total: 24 test cases (removed 2 non-file-impacting cases)

REMOVED CASES (non-file-impacting):
- forloop-simple-001: for x in a b c; do echo $x; done - pure output, no file context
- forloop-simple-002: for i in {1..5}; do echo $i; done - pure output, no file context

Current distribution:
- 3 simple patterns (kept: file iteration with cat, ls on directories)
- 8 expansion patterns (globs, brace expansion, command substitution - all with file context)
- 8 complex real-world patterns (extracted from research data)
- 3 nested/compound patterns (multi-command bodies, nested substitution)
- 2 edge cases (empty list, single item, loop control)

Categories by tier:
- Tier 1 (simple): 5 tests - Basic for-loop mechanics with literal lists
- Tier 2 (expansion): 8 tests - Glob patterns, brace expansion, command substitution
- Tier 3 (complex-real): 8 tests - Real examples from research with multi-line bodies, pipelines, conditionals
- Tier 3 (nested/compound): 3 tests - Nested command substitution and compound bodies
- Tier 4 (edge): 2 tests - Boundary conditions and loop control statements

Real-world examples: 8 tests marked with 'REAL:' extracted from for-loop-examples.tsv
research data, representing actual Claude Code session commands with coverage
gaps ranging from 36.8% to 90.9%.

Expected parse structure fields:
- :command-name - Always \"for\"
- :loop-variable - Iterator variable name (e.g., \"x\", \"file\", \"dir\")
- :loop-list - List of items to iterate over (may contain globs, expansions, substitutions)
- :loop-body - Commands executed in do...done block (may be multi-line)
- :command-substitutions - Command substitution list if present in list or body
- :glob-pattern - t if loop list contains glob patterns (*, ?, [])
- :brace-expansion - t if loop list uses brace expansion ({1..5}, {a,b,c})
- :multiline - t if body spans multiple lines with \\n
- :pipeline - t if body contains pipes (|)
- :conditional - t if body contains if/then/else/fi
- :compound-body - t if body has multiple commands with semicolons
- :redirects - List of redirections (2>/dev/null, 2>&1, etc.)
- :parameter-expansion - t if uses parameter expansion (${var%pattern})
- :file-operations - List of file operations (copy, move, delete)
- :loop-control - \"break\" or \"continue\" if present

NOTE: Expected parse results assume for-loop extraction is implemented
in bash-parser. Current implementation may not extract all :loop-* fields yet.")

(provide 'corpus-parse-for-loop)
;;; corpus-parse-for-loop.el ends here
