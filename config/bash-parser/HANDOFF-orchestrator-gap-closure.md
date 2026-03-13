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
ERT:       623 ran, 601 passed (13 unexpected failures + 9 expected failures)
Buttercup: 508 ran, 504 passed (4 failures)
Total:     1131 ran, 1105 passed (26 failures, of which 9 are expected)
```

**Starting point was**: 1052 passed (79 failures)
**Progress so far**: 53 tests fixed across 7 commits

## What Was Done

### Session 3 Changes (this session, 4 commits)

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

### 13 ERT Unexpected Failures (by category)

**PWD/cd directory context** (6 tests): `test-cd-and-run-script`, `test-cd-for-loop-files`, `test-cd-in-if-statement`, `test-cd-with-operations-between`, `test-pwd-assignment-inline`, `test-pwd-substitution-nested`
- cd context not flowing through conditionals (if-statement), loops (for-loop-files), and between operations
- PWD from inline assignments and command substitutions

**Self-executing relative paths with PWD** (3 tests): `test-relative-path-dot-slash-script-execution`, `test-relative-path-dot-slash-with-args`, `test-relative-path-parent-script-execution`
- `./script.sh` should resolve to `/base/dir/script.sh` when PWD context is available
- Orchestrator creates `:execute` ops with raw path, doesn't resolve against PWD

**Script execution edge cases** (3 tests): `test-script-execution-nested-python`, `test-script-execution-nested-self-executing`, `test-script-execution-corpus`
- Nested execution via `bash -c 'python script.py'` (wrapper command parsing)
- Need to detect that `python`, `bash`, `node` etc. take script files as arguments

**Nested structures** (2 tests): `test-nested-conditional-relative-test`, `test-nested-for-loop-relative-pattern`
- Relative path resolution in nested conditional and for-loop contexts

**Misc** (3 tests): `test-conditional-command-based`, `test-cmdsub-nested-backticks`, `test-deploy-script-pattern`
- Command-based conditionals (non-test-bracket): `if grep -q pattern file.txt; then ...`
- Backtick-style command substitutions
- Deploy script pattern (complex cd + operations)

**Corpus integration ERT** (4 tests): `test-corpus-integration-001..004`
- Same as Buttercup integration tests above

**Parser extensions** (2 tests): `test-parser-extension-single-nesting-depth`, `test-parser-extension-double-nesting-depth`
- Nesting depth analysis for parser extensions

### 9 ERT Expected Failures (known limitations, `:expected-result :failed`)

- 4 **xargs** tests: No handler for xargs yet
- 4 **dynamic redirect** tests: `$(cmd)` or `$VAR` in redirect destinations
- 1 **nested backtick** test: Backtick-style command substitutions

## Recommended Next Steps (priority order)

1. **PWD/cd context in nested structures** (~6 tests): The chain decomposer tracks cd correctly for sequential chains, but cd inside conditionals/loops doesn't flow to subsequent commands within the same scope. May need to pass directory context through conditional/loop decomposition.

2. **Relative path resolution for self-executing commands** (~3 tests): The orchestrator's self-execution detection creates `:execute` ops but doesn't resolve `./script.sh` against the current PWD context. Add resolution step for `:execute` ops similar to how handler ops get variable resolution.

3. **Script execution wrapper commands** (~3 tests): Need handler or detection for `python script.py`, `bash -c 'cmd'`, `node app.js` etc. These are commands that take script files as arguments and execute them.

4. **xargs handler** (~4 expected-failure tests): Create `config/bash-parser/commands/xargs.el` handler. Would convert 4 expected failures to passes.

5. **Dynamic redirect handling** (~4 expected-failure tests): Redirections with `$(cmd)` or `$VAR` in destinations need resolution.

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
