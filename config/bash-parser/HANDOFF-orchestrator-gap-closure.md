# Handoff: Close Orchestrator Gaps Surfaced by Legacy Wrapper Deprecation

## Context

The `unconditional-redirection-extraction` OpenSpec change introduced a two-layer architecture for `jf/bash-extract-semantics`:
- **Layer 0 (Grammar)**: Decomposes compounds into simple commands, extracts redirections
- **Layer 1 (Handlers)**: Receives each simple command, extracts domain-specific operations
- **Merge**: Combines results, claims tokens, calculates coverage

We verified the implementation against contract tests and fixed many issues. As the final step, we deprecated the legacy wrapper `jf/bash-extract-file-operations` by making it delegate to the orchestrator instead of the old recursive engine (`jf/bash--extract-file-operations-impl`). This correctly surfaced gaps where the orchestrator doesn't yet cover extraction cases the old recursive engine handled.

## Current State

```
Branch: gptel-bash-parser-contract-integration-tests
ERT:       623 ran, 611 passed (3 unexpected failures + 9 expected failures)
Buttercup: 508 ran, 504 passed (4 failures)
Total:     1131 ran, 1115 passed (16 failures, of which 9 are expected)
```

**Starting point was**: 1052 passed (79 failures)
**Progress so far**: 63 tests fixed across 8 commits

## What Was Done

### Session 4 Changes (this session, 1 commit)

1. **Non-destructive alist operations in chain decomposer** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Root cause: `assq-delete-all` destructively mutated shared list structure — when a later cd/pushd/popd in the chain removed PWD, it corrupted the var-context already stored in earlier entries
   - Added `jf/bash--alist-remove-key` (non-destructive, uses `cl-remove-if`) replacing all `assq-delete-all` calls
   - Fixed `test-cd-with-operations-between` (4-command chain with interleaved cd + operations)

2. **Self-executing path resolution against PWD** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Self-execution detection now resolves `./script.sh`, `../bin/runner` against PWD via `jf/bash--resolve-path-variables`
   - Fixed 5 tests: `test-cd-and-run-script`, `test-deploy-script-pattern`, `test-relative-path-dot-slash-script-execution`, `test-relative-path-dot-slash-with-args`, `test-relative-path-parent-script-execution`

3. **cd in conditional condition flows to then-branch** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Added `jf/bash--extract-cd-from-condition` to detect cd commands in `if` conditions
   - Then-branch receives updated var-context with new PWD (static analysis assumes success)
   - Fixed `test-cd-in-if-statement`

4. **Relative path resolution in for-loop globs and test conditions** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - For-loop glob source now resolves relative paths (e.g., `./src/*.txt` → `/base/dir/src/*.txt`)
   - Test condition file paths resolve without requiring `$` (was previously gated on variable presence)
   - Fixed `test-cd-for-loop-files`, `test-nested-for-loop-relative-pattern`, `test-nested-conditional-relative-test`

### Session 3 Changes (4 commits)

1. **Variable resolution metadata propagation** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - `jf/bash--resolve-handler-filesystem-ops` now detects unresolved variables even without var-context
   - Propagates `:unresolved t` and `:unresolved-vars` to output operations
   - Preserves `:high` confidence for redirections (source-aware degradation)
   - Uses `jf/bash-resolve-variables` (pure variable detection) for nil-context case,
     `jf/bash--resolve-path-variables` (full path resolution) for with-context case
   - Fixed 9 tests

2. **Pattern flow tracking** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Post-merge step links outer commands to `:match-pattern` ops from substitution entries
   - Uses `jf/bash--infer-operation-from-command` to determine consumer operation type
   - Attaches `:pattern-source` metadata with producer command info and search-scope
   - Fixed 9 tests (7 pattern-flow + 2 recursive substitution tests)

3. **Deduplication and exec-block metadata** (`config/bash-parser/analysis/bash-parser-orchestrator.org`, `config/bash-parser/commands/find.el`)
   - Orchestrator calls `jf/bash--deduplicate-operations` on filesystem domain after merge
   - Scoped to `:filesystem` domain only (avoids deduping auth/network ops)
   - Find handler marks exec-block ops with `:indirect t`
   - Fixed 4 tests

4. **Find exec-type metadata** (`config/bash-parser/commands/find.el`)
   - Find handler propagates `:type` from parser exec-blocks as `:exec-type`
   - Fixed 1 test

### Session 2 Changes (3 commits)

1. **Fix find handler exec-blocks** — Handler reads `:exec-blocks` from parser output (7 tests)
2. **Command substitution processing** — Recursive decomposition with `:from-substitution t` (15 tests)
3. **Self-execution detection** — Path-based commands detected as `:execute` operations (8 tests)

### Session 1 Changes (see git log)

See commits b4b5672 through 981f509 for the original two-layer implementation, handler fixes, metadata propagation, and legacy wrapper deprecation.

## Remaining Failures: Analysis

### 4 Buttercup Failures (complex integration corpus)

| Test | Gap |
|------|-----|
| integration-001 | Command substitutions in loop sources + nested conditionals |
| integration-002 | Command substitutions in positional args (`cat $(find ...)`) |
| integration-003 | Loop glob + conditional + directory ops (partial) |
| integration-004 | Heredoc + while loop with dynamic file writes |

### 3 ERT Unexpected Failures (by category)

**PWD edge cases** (2 tests): `test-pwd-assignment-inline`, `test-pwd-substitution-nested`
- `PWD=/new/path cat file.txt` (inline environment variable not applied to single command)
- `cat $(basename $(pwd))/file.txt` (nested command substitution static evaluation)

**Command-based conditionals** (1 test): `test-conditional-command-based`
- `if grep -q pattern file.txt; then ...` — non-bracket command-based conditions

**Script execution edge cases** (3 tests): `test-script-execution-nested-python`, `test-script-execution-nested-self-executing`, `test-script-execution-corpus`
- Nested execution via `bash -c 'python script.py'` (wrapper command parsing)
- Need to detect that `python`, `bash`, `node` etc. take script files as arguments

**Corpus integration ERT** (4 tests): `test-corpus-integration-001..004`
- Same as Buttercup integration tests above

**Parser extensions** (2 tests): `test-parser-extension-single-nesting-depth`, `test-parser-extension-double-nesting-depth`
- Nesting depth analysis for parser extensions

### 9 ERT Expected Failures (known limitations, `:expected-result :failed`)

- 4 **xargs** tests: No handler for xargs yet
- 4 **dynamic redirect** tests: `$(cmd)` or `$VAR` in redirect destinations
- 1 **nested backtick** test: Backtick-style command substitutions

## Recommended Next Steps (priority order)

1. **Script execution wrapper commands** (~3 tests): Need handler or detection for `python script.py`, `bash -c 'cmd'`, `node app.js` etc. These are commands that take script files as arguments and execute them.

2. **Command-based conditionals** (~1 test): `if grep -q pattern file.txt` — condition parser needs to handle non-bracket commands (currently only handles `[ -f file ]` style).

3. **PWD inline assignment** (~1 test): `PWD=/new/path cat file.txt` — inline environment variables scoped to single command.

4. **Nested command substitution evaluation** (~1 test): `$(basename $(pwd))` needs recursive static evaluation of deterministic commands.

5. **xargs handler** (~4 expected-failure tests): Create `config/bash-parser/commands/xargs.el` handler. Would convert 4 expected failures to passes.

6. **Dynamic redirect handling** (~4 expected-failure tests): Redirections with `$(cmd)` or `$VAR` in destinations need resolution.

## Known Issue: Assignment extractor misidentifies `cd`

`jf/bash--extract-assignments-from-command` treats `cd /dir` as a variable assignment `cd=/dir` because:
- `cd` matches the valid variable name pattern `^[A-Za-z_][A-Za-z0-9_]*$`
- `cd` doesn't have a registered handler in `jf/bash-command-handlers`
- The command has exactly one positional arg

This is harmless because the chain decomposer's cd detection runs afterward and correctly sets PWD. The spurious `(cd . "/dir")` entry in the context is inert. However, it could be cleaned up by adding a builtin exclusion list to the split-assignment heuristic.

## Key Files

- **Orchestrator**: `config/bash-parser/analysis/bash-parser-orchestrator.{org,el}` ← EDIT THE .org FILE
- **Handler registry**: `config/bash-parser/semantics/bash-parser-semantics.el`
- **Command handlers**: `config/bash-parser/commands/*.el`
- **Old recursive engine**: `config/bash-parser/plugins/bash-parser-file-ops.{org,el}` (contains `jf/bash--extract-file-operations-impl`)
- **Wrapper**: `jf/bash-extract-file-operations` in file-ops — now delegates to orchestrator
- **Contract tests**: `config/bash-parser/test/integration/two-layer-contracts-spec.el`
- **Self-execution detection**: `jf/bash--command-executes-self-p` in file-ops
- **Pattern flow**: `jf/bash--extract-pattern-flow-operations` in `bash-parser-recursive.el`
- **Deduplication**: `jf/bash--deduplicate-operations` in file-ops
- **Variable resolution**: `jf/bash-resolve-variables` in `bash-parser-variables.org` (pure), `jf/bash--resolve-path-variables` in file-ops (full pipeline)

## Important: Literate Programming Workflow

**Always edit `.org` files, never `.el` directly.** The `.el` files are generated.

```bash
# Edit the org file, then tangle:
./bin/tangle-org.sh config/bash-parser/analysis/bash-parser-orchestrator.org
# This tangles AND validates parens
```

Commit both `.org` and `.el` files together. Commit incrementally as batches of tests pass.
