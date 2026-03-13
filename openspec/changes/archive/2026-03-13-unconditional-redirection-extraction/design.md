## Context

The command-based semantic handlers refactoring introduced a filesystem plugin as a gateway to the recursive compound-walking engine. This plugin uses a predicate to decide whether to enter the recursive engine, gating grammar-level extraction (redirections, compound decomposition) behind a domain-specific check. This causes silent failures for:
1. Commands not in the filesystem whitelist that use redirections (`echo hello > file.txt`)
2. Compound structures (pipelines, chains) where the top-level has no `:command-name` and no redirection tokens (`rm file.txt && echo done`)

The old `jf/bash-extract-file-operations` API still works correctly because it enters the recursive engine unconditionally.

**Current architecture:**
```
jf/bash-extract-semantics
  ├─ Plugin loop (filesystem plugin gated by predicate) ◄── PROBLEM
  │     └─ jf/bash--extract-file-operations-impl
  │           └─ jf/bash-analyze-file-operations-recursive  ◄── CORRECT ENGINE
  │                 └─ jf/bash--extract-from-single-command
  │                       ├─ Redirection extraction (unconditional within engine)
  │                       └─ Positional arg extraction (handler-based)
  ├─ Command handler dispatch (top-level only)
  └─ Merge results
```

**Key finding:** The recursive engine (`bash-parser-recursive.el`) already implements the correct architecture internally. `jf/bash-analyze-file-operations-recursive` walks all compound structures and for each simple command calls `jf/bash--extract-from-single-command`, which does both redirection extraction (grammar-level) and handler dispatch (command-level). The only problem is how `jf/bash-extract-semantics` enters this engine — through a gated plugin instead of unconditionally.

**Second finding:** The plugin system itself is redundant. Command handlers already solve domain dispatch — each handler declares what domains it produces. The predicate-gated plugin loop is a parallel mechanism doing the same job. Cloud-auth detection can be a command handler registered for `aws`, `gcloud`, `az`, etc. that emits `:authentication` / `:network` domain ops. No separate plugin infrastructure needed.

## Goals / Non-Goals

**Goals:**
- Make `jf/bash-extract-semantics` enter the recursive engine unconditionally, eliminating the filesystem plugin gate
- Remove the entire plugin system (registry, registration, predicates, plugin-result struct) — command handlers already solve domain dispatch
- Migrate cloud-auth detection from plugin to command handlers for `aws`, `gcloud`, `az`
- Ensure all compound structures (pipelines, chains, loops, conditionals, subshells, substitutions) produce correct operations through the new API
- Migrate corpus tests to exercise `jf/bash-extract-semantics`
- Preserve `jf/bash-extract-file-operations` as a deprecated wrapper

**Non-Goals:**
- Restructuring the recursive engine internals — it's already correct
- Splitting `bash-parser-file-ops.el` into multiple modules — not needed for this change
- Changes to the parser, coverage system, or gptel scope validation

## Decisions

### Decision 1: Unconditional entry into recursive engine from orchestrator

**Choice:** Replace the filesystem plugin invocation in `jf/bash-extract-semantics` with a direct, unconditional call to `jf/bash--extract-file-operations-impl`.

**Rationale:** The recursive engine already handles compound decomposition, redirection extraction, and per-simple-command handler dispatch. The filesystem plugin was a thin wrapper adding a predicate gate and token claiming. By calling the internal implementation directly, we get the same extraction with no gating.

**Alternative considered — Rewrite recursive engine as pure Layer 0:** Would separate grammar extraction from handler dispatch inside the recursive engine. Rejected because the engine already works correctly when entered — the problem is only how it's entered. Separating the layers inside the engine would require major refactoring with no behavioral benefit.

**Alternative considered — Make filesystem plugin predicate always return t:** Minimal change, but keeps dead abstraction (a plugin that always runs isn't a plugin). Rejected.

### Decision 2: Remove entire plugin system

**Choice:** Remove the entire plugin infrastructure: `jf/bash-semantic-plugins` registry, `jf/bash-register-plugin`, `jf/bash-plugin-result` struct, the plugin loop in the orchestrator, and all plugin implementations (filesystem plugin, cloud-auth plugin).

**Rationale:** The plugin system is redundant with command handlers. Both systems solve the same problem — given a parsed command, produce domain-specific semantic operations. Command handlers already have per-command dispatch, domain declaration, and are called per-simple-command by the recursive engine. The plugin system adds a parallel predicate-gated dispatch mechanism that provides no additional capability. Cloud-auth detection migrates to command handlers registered for `aws`, `gcloud`, `az`, etc.

**Alternative considered — Keep plugin system for cloud-auth only:** Would preserve the plugin infrastructure for one remaining plugin. Rejected because a plugin system with one consumer is dead abstraction, and command handlers already handle the dispatch correctly.

### Decision 3: Token claiming from recursive engine output

**Choice:** After the recursive engine returns operations, build claimed-token-ids by matching operation file paths against the token inventory. Reuse the existing `jf/bash-plugin-filesystem--find-token-for-path` logic inline in the orchestrator.

**Rationale:** The recursive engine already returns operations with full metadata (`:file`, `:operation`, `:source`). We can derive claimed tokens from these operations without the plugin wrapper. This preserves coverage calculation accuracy.

**Alternative considered — Have the recursive engine return claimed tokens directly:** Would require modifying the recursive engine's return type. Rejected as unnecessary scope increase — post-hoc token matching is sufficient and keeps the engine unchanged.

### Decision 4: Orchestrator restructure — two-layer architecture

**Choice:** Restructure `jf/bash-extract-semantics` into a two-layer architecture with no plugin system:

```
jf/bash-extract-semantics (restructured)
  1. Layer 0: Grammar extraction (unconditional)
     Call jf/bash--extract-file-operations-impl unconditionally
     → Returns flat list of file operations from redirections AND command handlers
     → Recursive engine already dispatches to handlers per-simple-command internally
  2. Layer 1: Command handler dispatch on top-level command
     Call jf/bash-extract-command-semantics for non-filesystem domains
     → Captures cloud-auth, network, and other domain ops for simple commands
  3. Merge all results
     → Grammar ops (:filesystem from Layer 0)
     → Handler ops (all domains from Layer 1)
     → Build claimed-token-ids, calculate coverage, build final result
```

**Rationale:** The plugin system is eliminated entirely. Grammar extraction runs unconditionally (Layer 0). Command handlers provide all domain-specific semantics (Layer 1). Cloud-auth detection moves from a predicate-gated plugin to command handlers for `aws`, `gcloud`, `az`. This means cloud-auth benefits from per-simple-command dispatch in the recursive engine for compounds, fixing the compound dispatch bug that the old plugin approach had.

**Note on Layer 1 for compounds:** The top-level `jf/bash-extract-command-semantics` call may produce empty results for compounds (no top-level command-name). However, the recursive engine in Layer 0 already calls handlers per-simple-command internally. Non-filesystem handler domains for compound subcommands are captured there if the recursive engine is updated to return all domain results (see Risks).

### Decision 5: Corpus test migration approach

**Choice:** Create new Buttercup spec file `config/bash-parser/test/corpus/runners/corpus-file-operations-spec.el` that:
1. Reuses the existing `jf/bash-file-operations-test-corpus` data (require from `test-corpus-file-operations.el`)
2. Calls `jf/bash-extract-semantics` → extracts `:filesystem` domain operations
3. Runs the same assertions against the same expected operations

Keep the old ERT corpus tests during transition. Remove after migration verified.

**Rationale:** Reusing corpus data ensures identical test coverage. Buttercup provides better test organization. The migration validates that the new pipeline produces identical results to the old one.

### Decision 6: Legacy wrapper implementation

**Choice:** `jf/bash-extract-file-operations` becomes:
```elisp
(defun jf/bash-extract-file-operations (parsed-command &optional var-context)
  "DEPRECATED: Use jf/bash-extract-semantics instead."
  (let ((result (jf/bash-extract-semantics parsed-command)))
    (alist-get :filesystem (plist-get result :domains))))
```

**Rationale:** Single code path. All extraction goes through the new pipeline. The wrapper just extracts the filesystem domain from the result.

**Risk:** The old API accepted `var-context` as a parameter. The new `jf/bash-extract-semantics` doesn't take var-context. Need to verify whether var-context is used by any callers outside tests, and if so, thread it through to the recursive engine.

## Risks / Trade-offs

### [Risk] Cloud-auth command handler migration
Cloud-auth detection moves from a plugin to command handlers for `aws`, `gcloud`, `az`. The handler implementations must produce the same `:authentication` and `:network` domain operations as the old plugin.

→ **Mitigation:** Write tests for the new cloud-auth handlers that verify identical domain output. The recursive engine's per-simple-command dispatch means cloud-auth now works correctly for compounds (an improvement over the old plugin approach).

### [Risk] Non-filesystem domains in compound subcommands
The recursive engine (`jf/bash--extract-file-operations-impl`) currently returns only filesystem operations. Non-filesystem domain results from command handlers called per-simple-command inside the engine are not captured in the return value. The top-level `jf/bash-extract-command-semantics` call handles non-filesystem domains for simple commands, but for compounds it gets no command-name.

→ **Mitigation:** For this change, non-filesystem domains for compound subcommands remain a known limitation. A follow-up change can update the recursive engine to return all domain results, not just filesystem. The important thing is that the architecture now makes this possible — command handlers are the single dispatch mechanism.

### [Risk] var-context parameter threading
The old `jf/bash-extract-file-operations` accepts `var-context`. The new `jf/bash-extract-semantics` doesn't. If the wrapper drops var-context, callers passing it will silently lose variable resolution.

→ **Mitigation:** Audit callers of `jf/bash-extract-file-operations` for var-context usage. If needed, add var-context as optional parameter to `jf/bash-extract-semantics` and thread it to the recursive engine.

### [Risk] Token claiming accuracy after removing filesystem plugin
The filesystem plugin's token-claiming helpers were carefully designed to claim the right tokens. Replacing with post-hoc token matching from operations may be less precise.

→ **Mitigation:** Compare claimed-token-ids before and after the change using existing coverage tests. The coverage calculation tests will detect any regressions.

### [Risk] Corpus test migration completeness
The ERT corpus has 60+ test cases with detailed expected operations. The Buttercup migration must cover all of them.

→ **Mitigation:** Both test suites (old ERT, new Buttercup) run during transition. Remove ERT version only when Buttercup version covers all cases and passes.

## Migration Plan

### Phase 1: Remove plugin system and restructure orchestrator
1. Remove entire plugin infrastructure from `bash-parser-plugins.el`: `jf/bash-semantic-plugins`, `jf/bash-register-plugin`, `jf/bash-plugin-result` struct, plugin loop
2. Remove filesystem plugin and cloud-auth plugin implementations
3. Restructure `jf/bash-extract-semantics` into two-layer architecture (grammar + command handlers)
4. Add unconditional call to `jf/bash--extract-file-operations-impl`
5. Add token claiming from operations output
6. Update `jf/bash-extract-file-operations` to delegate to `jf/bash-extract-semantics`
7. Tangle `bash-parser-plugins.org`

### Phase 1b: Migrate cloud-auth to command handlers
1. Create command handlers for `aws`, `gcloud`, `az` that emit `:authentication` / `:network` domain ops
2. Register handlers via the existing command handler registry
3. Test that cloud-auth detection works for simple commands and compound subcommands

### Phase 2: New grammar extraction tests
1. Create `grammar-extraction-spec.el` — compound decomposition + dispatch verification
2. Create `grammar-redirection-spec.el` — redirection extraction in isolation
3. Create `semantic-pipeline-spec.el` — end-to-end two-layer verification
4. Run and verify all pass

### Phase 3: Corpus test migration
1. Create `corpus-file-operations-spec.el` in Buttercup, reusing corpus data
2. Run both old ERT and new Buttercup corpus tests — verify identical results
3. Keep both during stabilization

### Phase 4: Regression verification
1. Run all bash-parser tests: `./bin/run-tests.sh -d config/bash-parser --report`
2. Run all scope integration tests: `./bin/run-tests.sh -d config/gptel/scope/test/integration --report`
3. Run full scope test suite: `./bin/run-tests.sh -d config/gptel/scope --report`

### Rollback
Git revert of the orchestrator change. The old filesystem plugin code is in git history. No data migration involved.

## Open Questions

1. **var-context threading:** Does `jf/bash-extract-semantics` need a var-context parameter? Need to audit callers.
2. **Old ERT corpus tests removal timing:** When are we confident enough to remove the old tests? After one full test cycle? After a release?
3. **Recursive engine multi-domain return:** Should the recursive engine be updated to return all domain results (not just filesystem) from per-simple-command handler dispatch? This would fix non-filesystem domains for compound subcommands. Deferred as follow-up.
