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
ERT:       623 ran, 619 passed (4 unexpected failures + 9 expected failures)
Buttercup: 508 ran, 506 passed (2 failures)
Total:     1131 ran, 1125 passed (6 failures, of which 9 are expected = -3 net unexpected)
```

**Starting point was**: 1052 passed (79 failures)
**Progress so far**: 73 tests fixed across 11 commits

## What Was Done

### Session 5 Changes (this session, 3 commits)

1. **Shell wrapper -c decomposition** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - `bash -c 'cmd'`, `sh -c 'cmd'`, etc. now recursively decompose the inner command string
   - Metadata tracks `:indirect t` and `:nesting-depth` (supports nested wrappers like `bash -c 'sh -c "rm file"'`)
   - Uses key-deduplicating metadata build to prevent stale values in nested wrappers
   - Fixed 3 tests: `test-script-execution-nested-python`, `test-script-execution-nested-self-executing`, `test-script-execution-corpus`

2. **Command-based conditional fallback** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Conditions like `if grep -q pattern file.txt` are parsed as regular commands with `:test-condition t` metadata
   - Falls back to this when bracket-style test extraction (`[ -f file ]`) finds nothing
   - Fixed 1 test: `test-conditional-command-based`

3. **Parser extension nesting depth** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Double-nested wrappers correctly propagate `:nesting-depth 2`
   - Fixed 2 tests: `test-parser-extension-single-nesting-depth`, `test-parser-extension-double-nesting-depth`

4. **Self-execution unresolved variable metadata** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - `./$SCRIPT_NAME` commands now get `:unresolved t` and `:unresolved-vars` when var-context is nil
   - Fixed 1 corpus case: `exec-variable-003`

5. **Inline environment variable support** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - `PWD=/new/path cat file.txt` now applies inline `:env-vars` to command context
   - Parser already extracted env-vars; orchestrator now merges them per-command
   - Fixed 1 test: `test-pwd-assignment-inline`

6. **Glob resolution and pattern-source enrichment** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - For-loop globs without var-context get `/` prefix (e.g., `*/` → `/*/`)
   - Raw `$(cmd)` file paths filtered from handler results (pattern-flow provides resolved versions)
   - Test condition ops now get `:pattern t` when file path contains glob characters
   - Pattern-source enriched with `:substitution-content` and `:pattern` for traceability
   - Fixed 3 tests: `test-corpus-integration-002`, `test-corpus-integration-003`, corresponding Buttercup specs

### Session 4 Changes (1 commit)

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

1. **Variable resolution metadata propagation** — Fixed 9 tests
2. **Pattern flow tracking** — Fixed 9 tests (7 pattern-flow + 2 recursive substitution)
3. **Deduplication and exec-block metadata** — Fixed 4 tests
4. **Find exec-type metadata** — Fixed 1 test

### Session 2 Changes (3 commits)

1. **Fix find handler exec-blocks** — 7 tests
2. **Command substitution processing** — 15 tests
3. **Self-execution detection** — 8 tests

### Session 1 Changes (see git log)

See commits b4b5672 through 981f509 for the original two-layer implementation, handler fixes, metadata propagation, and legacy wrapper deprecation.

## Remaining Failures: Analysis

### 2 Buttercup Failures (complex integration corpus)

| Test | Gap |
|------|-----|
| integration-001 | `{dynamic}` placeholder for `$(basename "$file")` in redirects |
| integration-004 | `{dynamic}` placeholder for `$line` in redirect paths |

### 4 ERT Unexpected Failures

**Complex corpus** (2 tests): `test-corpus-integration-001`, `test-corpus-integration-004`
- Same as Buttercup integration tests above
- Both require `{dynamic}` placeholder representation for unresolvable variables/substitutions in redirect destinations

**Nested command substitution evaluation** (1 test): `test-pwd-substitution-nested`
- `cat $(basename $(pwd))/file.txt` needs recursive static evaluation of `basename`
- Evaluation chain: `$(pwd)` → PWD value → `$(basename PWD)` → last component → resolve path

### 9 ERT Expected Failures (known limitations, `:expected-result :failed`)

- 4 **xargs** tests: No handler for xargs yet
- 4 **dynamic redirect** tests: `$(cmd)` or `$VAR` in redirect destinations
- 1 **nested backtick** test: Backtick-style command substitutions

## Recommended Next Steps (priority order)

1. **Dynamic redirect placeholder** (~3 tests, 2 ERT + 2 Buttercup): Implement `{dynamic}` placeholder for redirect destinations containing unresolvable `$(cmd)` or `$VAR` references. This would fix integration-001, integration-004, and corresponding Buttercup specs.

2. **Nested command substitution evaluation** (~1 test): `$(basename $(pwd))` needs recursive static evaluation of deterministic commands like `basename`, `dirname`. The `jf/bash--static-dirname` function already exists for `dirname`; needs equivalent `basename` support.

3. **xargs handler** (~4 expected-failure tests): Create `config/bash-parser/commands/xargs.el` handler. Would convert 4 expected failures to passes.

4. **Dynamic redirect handling** (~4 expected-failure tests): Redirections with `$(cmd)` or `$VAR` in destinations need resolution.

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
