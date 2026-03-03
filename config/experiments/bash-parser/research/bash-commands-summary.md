# Bash Commands from Claude Code Sessions

**Analysis Date:** 2026-03-03 19:08:42

## Overview

- **Total sessions analyzed:** 96
- **Sessions with bash commands:** 90 (93.8%)
- **Total commands extracted:** 1552
- **Unique command patterns:** 883
- **Average complexity score:** 2.22/10
- **High complexity commands (≥5):** 288 (18.6%)

## Command Categories

| Category | Count | Percentage |
|----------|-------|------------|
| other | 583 | 37.6% |
| git | 398 | 25.6% |
| filesystem | 227 | 14.6% |
| custom-scripts | 144 | 9.3% |
| search | 124 | 8.0% |
| file-reading | 41 | 2.6% |
| make | 20 | 1.3% |
| emacs | 9 | 0.6% |
| text-processing | 5 | 0.3% |
| npm/node | 1 | 0.1% |

## Top 20 Most Common Command Patterns

| Rank | Pattern | Count |
|------|---------|-------|
| 1 | `git status` | 76 |
| 2 | `git commit -m <string>` | 46 |
| 3 | `.<path> config<path>` | 42 |
| 4 | `openspec status --change <string> --json` | 36 |
| 5 | `git log --oneline -<num>` | 31 |
| 6 | `openspec status --change <string>` | 29 |
| 7 | `ls -la <path>` | 27 |
| 8 | `ls -la openspec<path>` | 26 |
| 9 | `openspec list --json` | 21 |
| 10 | `bd list --label scoped-bash-command-execution` | 14 |
| 11 | `mkdir -p openspec<path>` | 13 |
| 12 | `git status --short` | 12 |
| 13 | `mkdir -p <path>` | 9 |
| 14 | `git diff --stat` | 9 |
| 15 | `git log -<num> --stat` | 8 |
| 16 | `ls -la config<path>` | 8 |
| 17 | `bd create --title <string> --type task --priority P0 --labels <string> --description <string>` | 8 |
| 18 | `bd create --title <string> --type task --priority P1 --labels <string> --description <string>` | 8 |
| 19 | `git diff config<path> \| head -<num>` | 7 |
| 20 | `git add config<path> config<path>` | 7 |

## Example Commands by Category

### Custom-Scripts (144 commands)

- `./bin/emacs-isolated.sh -batch \
    -l config/experiments/bash-parser/bash-parser.el \
    -l config/experiments/bash-parser/test/test-glob-matching.el \
    -l config/experiments/bash-parser/test/test-command-semantics.el \
    -l config/experiments/bash-parser/test/test-file-operations.el \
    -l config/experiments/bash-parser/test/test-parser-extension.el \
    -l config/experiments/bash-parser/test/test-security-validator.el \
    -f ert-run-tests-batch-and-exit`
  - Run bash-parser test suite to establish baseline
- `./bin/tangle-org.sh config/experiments/bash-parser/bash-parser-core.org`
  - Tangle and validate bash-parser-core.org
- `./bin/tangle-org.sh config/experiments/bash-parser/bash-parser-glob.org`
  - Tangle and validate bash-parser-glob.org
- `./bin/tangle-org.sh config/experiments/bash-parser/bash-parser-security.org`
  - Tangle and validate bash-parser-security.org
- `./bin/tangle-org.sh config/experiments/bash-parser/bash-parser-semantics.org && ./bin/tangle-org.sh config/experiments/bash-parser/bash-parser-variables.org`
  - Tangle semantics and variables modules

*...and 139 more*

### Emacs (9 commands)

- `emacs --batch --eval "(progn (load-file \"config/gptel/scope/scope-core.el\") (princ \"Loaded successfully\n\"))" 2>&1 \| head -20`
  - Test loading the updated scope-core.el
- `emacs --batch -l config/experiments/bash-parser/bash-parser.el --eval "(message \"Loaded successfully\")" 2>&1 \| tail -2`
  - Test if file loads without errors
- `emacs --batch --eval "(progn (require 'tree-sitter) (message \"tree-sitter: %s\" (version)))"`
  - Check if tree-sitter is available
- `emacs --batch -l config/experiments/bash-parser/bash-parser.el --eval '(princ (pp-to-string (jf/bash-parse "sudo rm -rf /tmp/test")))'`
  - Test parsing of sudo command
- `emacs -batch -l bash-parser.el -l test-corpus.el -l bash-parser-test.el -f ert-run-tests-batch-and-exit 2>&1 \| head -100`
  - Run bash parser test suite

*...and 4 more*

### File-Reading (41 commands)

- `head -30 openspec/specs/gptel/scope-presets.md`
  - Check what scope-presets.md covers
- `cat << 'EOF'
## Bead Dependency Graph

```
emacs-t9l (P1)
    Add bash validation type to scope-core
    │
    ├─► emacs-9q2 (P1)
    │       Rewrite scope-bash-tools with run_bash_command
    │       │
    │       └─► emacs-efp (P1)
    │               Test scoped bash command execution
    │               │
    │               └─► emacs-2pu (P1)
    │                       Deploy to production
    │
    └─► emacs-aqw (P2)
            Create example preset with bash_tools
            │
            └─► emacs-efp (P1)
                    (also blocks testing)

emacs-7jx (P2) - Update agent definitions
    (No dependencies - can be done anytime)
```

## Implementation Order

**Critical Path (must be sequential):**
1. emacs-t9l - Add validation infrastructure
2. emacs-9q2 - Implement bash tools
3. emacs-efp - Integration testing
4. emacs-2pu - Production deployment

**Parallel Track:**
- emacs-aqw - Preset example (after t9l, before efp)
- emacs-7jx - Agent updates (independent, P2)

## Work Flow

Start with **emacs-t9l**, then do **emacs-9q2** and **emacs-aqw** in parallel (or sequentially). Once both complete, do **emacs-efp** testing. Finally deploy with **emacs-2pu**. Update agents (**emacs-7jx**) anytime.
EOF
`
  - Show dependency visualization
- `head -5 ~/.claude/history.jsonl`
  - Check format of history file
- `head -2 ~/.claude/projects/-Users-jefffarr-emacs/31a29ec9-bcc3-45f6-90b2-0faa1e6ba719.jsonl \| jq -r '.[0] \| keys'`
  - Check session file structure
- `head -1 ~/.claude/projects/-Users-jefffarr-emacs/03768c10-d67d-4ce7-9c98-7fff335aefb3.jsonl \| jq '. \| keys' 2>/dev/null \| head -20`
  - Examine session message structure

*...and 36 more*

### Filesystem (227 commands)

- `ls -la openspec/specs/preset-registration/spec.md openspec/specs/scope-profiles/spec.md 2>&1`
  - Check if main spec files exist
- `ls -la openspec/specs/`
  - Check specs directory structure
- `mkdir -p openspec/specs/preset-registration openspec/specs/scope-profiles`
  - Create directories for new specs
- `ls -la openspec/specs/gptel/`
  - Check current gptel specs structure
- `mv openspec/specs/preset-registration/spec.md openspec/specs/gptel/preset-registration.md && rmdir openspec/specs/preset-registration && mv openspec/specs/scope-profiles/spec.md openspec/specs/gptel/scope-profiles.md && rmdir openspec/specs/scope-profiles`
  - Move specs to gptel directory and remove empty directories

*...and 222 more*

### Git (398 commands)

- `git status`
  - Show working tree status
- `git diff HEAD`
  - Show staged and unstaged changes
- `git log --oneline -10`
  - Show recent commit messages
- `git add .beads/issues.jsonl && git commit -m "$(cat <<'EOF'
Convert scoped-bash-command-execution OpenSpec to Beads issues

Created 5 self-contained beads from OpenSpec design artifacts:
- emacs-t9l: Add bash validation type to scope-core
- emacs-9q2: Rewrite scope-bash-tools with run_bash_command
- emacs-aqw: Create example preset with bash_tools configuration
- emacs-7jx: Update agent definitions to use run_bash_command
- emacs-efp: Test scoped bash command execution in isolated session

Each bead contains complete implementation context, code snippets,
design rationale, and acceptance criteria. Dependencies configured
to enforce implementation order: t9l → 9q2/aqw → efp.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
EOF
)" && git status`
  - Commit beads database and show status
- `git status`
  - Show working tree status

*...and 393 more*

### Make (20 commands)

- `make test-pattern PATTERN='^test-script-execution-' 2>&1 \| tail -50`
  - Run script execution tests to verify status
- `make test-bash-parser 2>&1 \| head -50`
  - Test the new Makefile-based test invocation
- `make test-bash-parser 2>&1 \| tail -30`
  - Check test completion and results
- `make emacs-test-eval EVAL_CMD="(jf/test-run-pattern-batch \"^test-glob-literal\")" 2>&1 \| grep -E "(Running\|PASSED\|FAILED\|^Ran)"`
  - Test direct make invocation
- `make test-directory DIR=config/gptel/tools/bash-parser 2>&1 \| tail -100`
  - Run bash-parser tests to see current failures

*...and 15 more*

### Npm/Node (1 commands)

- `npm list -g openspec 2>&1 \| head -5`
  - Check openspec package info

### Other (583 commands)

- `bd list --label scoped-bash-command-execution`
  - List beads with scoped-bash-command-execution label
- `bd list --label scoped-bash-command-execution --status open`
  - Check which beads are still open
- `bd show emacs-ags emacs-1n7 emacs-blx emacs-eii emacs-crn emacs-ckg emacs-rqp emacs-dw4 emacs-48j emacs-s75 \| grep -E "(^ID\|^Status)"`
  - Check status of agent-worked beads
- `for bead in emacs-ags emacs-1n7 emacs-blx emacs-eii emacs-crn emacs-ckg emacs-rqp emacs-dw4 emacs-48j emacs-s75; do echo "=== $bead ==="; bd show "$bead" 2>/dev/null \| head -5; done`
  - Check status of first 10 beads
- `bd close emacs-1n7 --comment "Key normalization added to scope-core YAML parser - commit 425b6c5" && bd close emacs-blx --comment "Review complete - created 9 new beads for identified issues" && bd close emacs-ckg --comment "Review complete - created 5 new beads for identified issues"`
  - Close three beads that agents completed

*...and 578 more*

### Search (124 commands)

- `grep -n "preset\.md" openspec/specs/gptel/persistent-agent.md openspec/specs/gptel/sessions-branching.md 2>/dev/null \| head -20`
  - Check for preset.md references in remaining files
- `grep -n "preset\.md\\|scope\.yml" openspec/specs/gptel/sessions-branching.md \| head -20`
  - Check for preset.md and scope.yml references in sessions-branching
- `grep -A 20 "PersistentAgent" openspec/changes/gptel-preset-upstream-alignment/design.md \| head -40`
  - Check PersistentAgent design decisions
- `grep -B 5 -A 15 "agent directories\\|agent scope\\|PersistentAgent scope" openspec/changes/gptel-preset-upstream-alignment/design.md`
  - Check how agent directories are handled
- `grep -r "preset\.md" openspec/specs/gptel/ 2>/dev/null \|\| echo "No preset.md references found"`
  - Verify no remaining preset.md references

*...and 119 more*

### Text-Processing (5 commands)

- `sed -n '779,900p' config/gptel/scope/scope-core.org \| grep -A 5 "======="`
  - Find separator in third conflict
- `sed -n '127,132p' /Users/jefffarr/emacs/config/gptel/gptel.el`
  - Check lines around scope-core loading
- `sed -n '375,385p' config/experiments/bash-parser/bash-parser.el \| cat -A`
  - Show lines around 382 with visible characters
- `sed -n '375,385p' config/experiments/bash-parser/bash-parser.el`
  - Show lines around error
- `sed -n '359,361p' config/experiments/bash-parser/bash-parser.el`
  - Check if-let parens

