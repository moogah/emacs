# Handoff: Close Orchestrator Gaps Surfaced by Legacy Wrapper Deprecation

## Context

The `unconditional-redirection-extraction` OpenSpec change introduced a two-layer architecture for `jf/bash-extract-semantics`:
- **Layer 0 (Grammar)**: Decomposes compounds into simple commands, extracts redirections
- **Layer 1 (Handlers)**: Receives each simple command, extracts domain-specific operations
- **Merge**: Combines results, claims tokens, calculates coverage

We verified the implementation against contract tests and fixed many issues. As the final step, we deprecated the legacy wrapper `jf/bash-extract-file-operations` by making it delegate to the orchestrator instead of the old recursive engine (`jf/bash--extract-file-operations-impl`). This correctly surfaced **72 ERT test failures** — gaps where the orchestrator doesn't yet cover extraction cases the old recursive engine handled.

## Current State

```
Branch: gptel-bash-parser-contract-integration-tests
ERT:       623 ran, 551 passed (72 failures + 9 expected failures)
Buttercup: 508 ran, 501 passed (7 failures)
Total:     1131 ran, 1052 passed (79 failures)
```

**Baseline snapshot**: `config/bash-parser/test-results.txt` — use `git diff config/bash-parser/test-results.txt` to track progress as gaps are closed.

## What Was Done This Session

### Implementation Changes

1. **Orchestrator metadata propagation** (`config/bash-parser/analysis/bash-parser-orchestrator.org`)
   - Added `jf/bash--annotate-ops-with-metadata` — propagates entry metadata (`:conditional`, `:branch`, `:loop-context`, `:subshell-context`) onto operations during merge
   - Added `jf/bash--normalize-var-context` — normalizes string keys to symbols for variable resolution
   - Added `jf/bash--extract-test-condition-ops` — extracts `:read-metadata` from conditional `:condition-text`
   - Updated for-loop decomposer to emit `:match-pattern` for glob sources and include `:loop-variable` in metadata

2. **Handler claimed-token-ids** (`config/bash-parser/commands/{cat,rm,cp}.el`)
   - All three now compute and return `:claimed-token-ids` by scanning tokens for `:command-name` and `:positional-arg` types

3. **Find handler** (`config/bash-parser/commands/find.el`)
   - Added `jf/bash-find-exec-command-ops` mapping for `-exec` block operations
   - Added exec-block extraction logic (NOTE: currently non-functional because parser doesn't expose exec-block args in `:positional-args` — see parser gap below)
   - Added claimed-token-ids computation

4. **Legacy wrapper deprecation** (`config/bash-parser/plugins/bash-parser-file-ops.org`)
   - `jf/bash-extract-file-operations` now delegates to `jf/bash-extract-semantics` and returns `:filesystem` domain ops
   - Removed "DEPRECATED" framing

5. **Test fixes**
   - Token-claiming spec: Updated to use `jf/bash--claim-tokens-for-results` (correct function name and signature)
   - Semantic pipeline spec: Fixed chain dispatch test — chains correctly dispatch handlers for ALL decomposed simple commands
   - Deleted `config/bash-parser/test/integration/legacy-wrapper-spec.el`

### Tests Fixed (27 → 7 Buttercup failures)
- 4 token-claiming tests (function renamed)
- 1 semantic pipeline chain test (wrong expectation)
- 3 handler coverage tests (claimed-token-ids)
- 6 grammar extraction tests (metadata propagation + conditional/loop)
- 3 variable corpus tests (var-context normalization)
- 2 subshell tests (metadata propagation)
- 1 integration-005 corpus test (conditional test condition extraction)

## Remaining Failures: Analysis and Gap Categories

### 7 Buttercup Failures

All are corpus tests via `jf/bash-extract-semantics`:

| Test | Gap |
|------|-----|
| find-001, find-002, find-003 | Parser doesn't expose `-exec` block args in `:positional-args` (stored in `:exec-blocks` but handler doesn't use it) |
| integration-001 | Command substitutions in loop sources + nested conditionals |
| integration-002 | Command substitutions in positional args (`cat $(find ...)`) |
| integration-003 | Loop glob + conditional + directory ops (partial) |
| integration-004 | Heredoc + while loop with dynamic file writes |

### 72 ERT Failures (by file, 81 counting duplicates)

These are all tests that call `jf/bash-extract-file-operations` and relied on the old recursive engine. They represent real orchestrator gaps:

| File (failure count) | Gap Category |
|---------------------|--------------|
| `test-bash-parser-recursive.el` (15) | Recursive engine features: command substitution extraction, nested analysis, pattern flow |
| `test-corpus-file-operations.el` (10) | Same corpus gaps as Buttercup: find exec-blocks, integration cases, variable edge cases |
| `test-pattern-flow.el` (9) | Pattern source tracking through command substitutions (`cat $(find ...)`) |
| `test-corpus-script-execution.el` (9) | Script execution detection (bash, python, perl script paths) |
| `test-file-operations.el` (9) | Variable resolution, duplicate dedup, xargs, dynamic redirects, self-execution |
| `test-pwd-directory-context.el` (6) | PWD-relative path resolution in nested constructs |
| `test-directory-changing-commands.el` (6) | cd context in conditionals, loops, complex chains |
| `test-find-exec-patterns.el` (3) | find -exec patterns (parser gap) |
| `test-parser-extension.el` (2) | Nesting depth analysis for extensions |
| `test-loop-context.el` (2) | Loop variable from command substitution sources |
| `test-conditional-context.el` (1) | Command-based conditionals (non-test-bracket) |

### Gap Categories Summarized

1. **Command substitution handling** (~25 tests): The orchestrator doesn't process `$(cmd)` inside arguments. The old engine recursively parsed substitution content and extracted operations with `:from-substitution t`.

2. **find -exec block parsing** (~6 tests): Parser exposes `:exec-blocks` key on find commands but the data format isn't documented. The handler needs to use `:exec-blocks` instead of trying to extract from positional-args.

3. **Script execution detection** (~9 tests): No handler for `bash`, `python`, `perl`, etc. that extracts `:execute` operations for script paths.

4. **xargs handling** (~4 tests): No handler for xargs patterns.

5. **Dynamic redirect/heredoc handling** (~4 tests): Redirections with variables/substitutions in destinations.

6. **Variable resolution edge cases** (~5 tests): Unresolved variable confidence degradation, empty context, partial resolution behavior differences.

7. **Deduplication** (~1 test): Old engine deduped operations; orchestrator doesn't.

8. **Pattern source tracking** (~9 tests): When `cat $(find . -name '*.txt')` resolves, the old engine attached `:pattern-source` metadata linking the cat read to the find match-pattern.

## Recommended Next Steps

1. **Investigate `:exec-blocks` parser key** — The diagnostic showed the parser produces `:exec-blocks` in the keys. Read the parser code to understand its format, then update the find handler to use it.

2. **Add command substitution processing** — This is the biggest gap (~25 tests). The orchestrator's Layer 0 decomposition needs to recursively process `$(...)` and `` `...` `` content, producing operations with `:from-substitution t`.

3. **Add script execution handlers** — Register handlers for `bash`, `sh`, `python`, `perl`, `ruby`, `node` that extract `:execute` operations for script paths.

4. **Add xargs handler** — Register handler for `xargs` that produces operations based on piped input patterns.

5. **Handle dynamic redirects** — Redirections with `$(cmd)` or `$VAR` in destinations need variable/substitution resolution.

6. **Add deduplication** — The orchestrator's merge step should deduplicate operations.

7. **Pattern source tracking** — When resolving handler ops through variables that came from substitutions, attach `:pattern-source` metadata.

Each of these could be a separate bead. The test failures are the specification — each one documents exactly what behavior the orchestrator needs to produce.

## Key Files

- **Orchestrator**: `config/bash-parser/analysis/bash-parser-orchestrator.{org,el}`
- **Handler registry**: `config/bash-parser/semantics/bash-parser-semantics.el`
- **Command handlers**: `config/bash-parser/commands/*.el`
- **Old recursive engine**: `config/bash-parser/plugins/bash-parser-file-ops.{org,el}` (still contains `jf/bash--extract-file-operations-impl`)
- **Wrapper**: `jf/bash-extract-file-operations` in file-ops — now delegates to orchestrator
- **Contract tests**: `config/bash-parser/test/integration/two-layer-contracts-spec.el`
