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
ERT:       623 ran, 578 passed (45 failures + 9 expected failures)
Buttercup: 508 ran, 504 passed (4 failures)
Total:     1131 ran, 1082 passed (49 failures)
```

**Starting point was**: 1052 passed (79 failures)
**Progress so far**: 30 tests fixed across 3 commits

## What Was Done This Session

### Session 2 Changes (this session, 3 commits)

1. **Fix find handler exec-blocks** (`config/bash-parser/commands/find.el`)
   - Handler now reads `:exec-blocks` from parser output instead of reconstructing from positional-args
   - Fixed 7 tests (3 Buttercup find corpus + 4 ERT find/exec tests)

2. **Command substitution processing** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - `jf/bash--make-entry` now recursively decomposes `:command-substitutions` with `:from-substitution t` metadata
   - For-loop substitution source handling: decomposes substituted commands and binds loop variable from find's `-name` pattern
   - Added `jf/bash--extract-find-name-pattern` helper
   - Fixed 15 tests

3. **Self-execution detection** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Orchestrator now detects path-based commands (`./script.sh`, `/path/to/tool`, `../bin/runner`) as `:execute` operations
   - Uses existing `jf/bash--command-executes-self-p` from file-ops module
   - Fixed 8 tests

### Session 1 Changes (prior session, see git log)

See commits b4b5672 through 981f509 for the original two-layer implementation, handler fixes, metadata propagation, and legacy wrapper deprecation.

## Remaining Failures: Analysis and Gap Categories

### 4 Buttercup Failures

All are complex integration corpus tests via `jf/bash-extract-semantics`:

| Test | Gap |
|------|-----|
| integration-001 | Command substitutions in loop sources + nested conditionals |
| integration-002 | Command substitutions in positional args (`cat $(find ...)`) |
| integration-003 | Loop glob + conditional + directory ops (partial) |
| integration-004 | Heredoc + while loop with dynamic file writes |

### 45 ERT Failures (by category)

**Pattern flow** (7 tests): `test-pattern-flow-*`
- When `cat $(find . -name '*.txt')` resolves, the old engine attached `:pattern-source` metadata linking the cat read to the find match-pattern
- Pattern flow function exists (`jf/bash--extract-pattern-flow-operations`) but isn't called from the orchestrator

**Variable resolution** (7 tests): `test-variable-*`, `test-corpus-variable-*`, `test-confidence-degradation-*`
- Unresolved variable handling differences: the orchestrator's variable resolution for handler ops doesn't produce `:unresolved t` / `:unresolved-vars` on operations when variables can't be resolved
- Empty variable context behavior differences

**PWD/cd directory context** (6 tests): `test-cd-*`, `test-pwd-*`, `test-deploy-script-pattern`
- cd context tracking in conditionals, loops, complex chains
- PWD from variable assignments and substitutions

**Recursive engine features** (5 tests): `test-recursive-*`
- `test-recursive-single-substitution`, `test-recursive-substitution-grep` — command substitution ops expected with specific metadata (partially working, may need `:command` field on sub ops)
- `test-recursive-deduplication` — orchestrator doesn't deduplicate operations
- `test-recursive-with-exec-blocks` — find exec-blocks via old recursive path

**Self-executing relative paths** (3 tests): `test-relative-path-*`
- PWD resolution for self-executing relative paths (need to resolve `./script.sh` to `/project/bin/script.sh` when PWD is set)

**xargs handling** (4 tests): `test-extraction-xargs-*`
- No handler for xargs patterns

**Dynamic redirects** (4 tests): `test-extraction-dynamic-redirect-*`, `test-extraction-heredoc-dynamic-redirect`
- Redirections with `$(cmd)` or `$VAR` in destinations need variable/substitution resolution

**find exec integration** (2 tests): `test-find-exec-rm-pattern`, `test-find-multiple-exec-blocks`
- These test `jf/bash-extract-file-operations` path with older expectations — may need count/metadata adjustments

**Conditional** (1 test): `test-conditional-command-based`
- Command-based conditionals (non-test-bracket) — e.g., `if grep -q pattern file.txt; then ...`

**Nested backtick substitution** (1 test): `test-cmdsub-nested-backticks`
- Backtick-style command substitutions

**Script execution edge cases** (3 tests): `test-script-execution-nested-*`, `test-script-execution-variable-unresolved`
- Nested execution via `bash -c 'python script.py'` (wrapper command parsing)
- Variable in script path

**Parser extension** (2 tests): `test-parser-extension-*`
- Nesting depth analysis for extensions

### Gap Categories Summarized (priority order)

1. **Variable resolution for handler ops** (~7 tests): The orchestrator's `jf/bash--resolve-handler-filesystem-ops` needs to set `:unresolved t` and `:unresolved-vars` when variables can't be resolved, and handle `:confidence :medium` degradation.

2. **Pattern flow tracking** (~7 tests): When command substitutions produce `:match-pattern` ops, the outer command's ops should get `:pattern-source` metadata. The function `jf/bash--extract-pattern-flow-operations` exists in `bash-parser-recursive.el` and needs to be integrated into the orchestrator.

3. **PWD/directory context** (~6 tests): The orchestrator's chain decomposer already tracks cd/pushd/popd. Failures may be about context not flowing correctly through nested structures or variable assignments.

4. **xargs handler** (~4 tests): Register handler for `xargs` that produces operations based on piped input patterns.

5. **Dynamic redirect handling** (~4 tests): Redirections with `$(cmd)` or `$VAR` in destinations.

6. **Deduplication** (~1 test): The orchestrator's merge step should deduplicate operations.

7. **Script execution edge cases** (~3 tests): Wrapper command handling (`bash -c`), variable in script path.

8. **Remaining integration/misc** (~6 tests): find exec metadata, conditionals, parser extensions, nested backticks.

## Recommended Next Steps

1. **Variable resolution** — Fix `jf/bash--resolve-handler-filesystem-ops` to propagate `:unresolved` metadata. High-impact, localized fix.

2. **Pattern flow** — Integrate `jf/bash--extract-pattern-flow-operations` from the recursive module into the orchestrator's post-merge step.

3. **xargs handler** — Create `config/bash-parser/commands/xargs.el` handler.

4. **Deduplication** — Add dedup in the orchestrator's merge step (function `jf/bash--deduplicate-operations` exists in file-ops).

5. **Dynamic redirects** — Extend `jf/bash-extract-operations-from-redirections` to handle variable/substitution destinations.

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

## Important: Literate Programming Workflow

**Always edit `.org` files, never `.el` directly.** The `.el` files are generated.

```bash
# Edit the org file, then tangle:
./bin/tangle-org.sh config/bash-parser/analysis/bash-parser-orchestrator.org
# This tangles AND validates parens
```

Commit both `.org` and `.el` files together. Commit incrementally as batches of tests pass.
