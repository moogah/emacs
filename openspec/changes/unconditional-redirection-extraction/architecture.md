## Components

### Grammar Extraction Layer (Layer 0)
- **Responsibility**: Unconditionally decompose compound bash structures into simple commands, extract shell-level I/O (redirections) from each, and dispatch each simple command to command handlers.
- **Replaces**: The filesystem plugin's role as gateway to `jf/bash-analyze-file-operations-recursive`, AND the entire plugin system (registry, predicates, plugin-result struct)
- **Key Functions**:
  - Entry point in `jf/bash-extract-semantics` (replaces plugin loop entirely)
  - Reuses existing recursive engine (`jf/bash-analyze-file-operations-recursive`) for compound walking
  - Reuses existing `jf/bash-extract-operations-from-redirections` for redirection extraction
- **Outputs**: List of file operations from redirections and command handlers (`:filesystem` domain)

### Command Handler Layer (Layer 1)
- **Responsibility**: For each simple command, execute registered command handlers to produce domain-specific operations from positional arguments, flags, and command patterns
- **Unchanged from current design**: `jf/bash-extract-command-semantics` + handler registry
- **Key change**: Called per-simple-command by the recursive engine (inside Layer 0), AND once on the top-level command for non-filesystem domains. Cloud-auth detection moves here from the removed plugin system.
- **New handlers**: `aws`, `gcloud`, `az` command handlers emit `:authentication` / `:network` domain ops (migrated from cloud-auth plugin)
- **Outputs**: Per-command `(:domains ... :claimed-token-ids ...)` results

### Result Merge
- **Responsibility**: Combine grammar-level operations and handler operations across all simple commands. Calculate coverage. Produce final `jf/bash-extract-semantics` return structure.
- **Key Functions**: Aggregation logic in `jf/bash-extract-semantics`
- **Outputs**: Final `(:parse-complete ... :coverage ... :domains ...)` structure

### Legacy Wrapper
- **Responsibility**: Preserve `jf/bash-extract-file-operations` API as thin wrapper
- **Implementation**: Calls `jf/bash-extract-semantics`, extracts `:filesystem` domain operations
- **Status**: Deprecated, preserved for gradual migration

## Interfaces

### Layer 0 → Layer 1 Contract
```
Input:  simple-command plist (with :command-name, :positional-args, :tokens, :redirections, accumulated var-context)
Output: (:domains ((domain . ops) ...) :claimed-token-ids (...))
```
Layer 0 calls Layer 1 for each simple command encountered during recursive decomposition. The simple command includes accumulated context (variable assignments, directory changes from prior chain commands).

### Public API (preserved)
```elisp
(jf/bash-extract-semantics parsed-command)
  => (:parse-complete t/nil
      :parse-errors (...)
      :coverage (:total-tokens N :claimed-tokens M :coverage-ratio R ...)
      :domains ((:filesystem . ops) (:authentication . ops) (:network . ops) ...))
```

Note: `:plugin-results` key is removed from the return structure since the plugin system is eliminated. `:cloud-auth` domain key is replaced by `:authentication` and `:network` domain keys from command handlers.

### Data Flow
```
Bash String
    ↓
jf/bash-parse
    ↓
Parsed Command (simple or compound)
    ↓
jf/bash-extract-semantics
    ↓
┌─ Layer 0: Grammar Extraction (unconditional) ─────────────────────┐
│                                                                    │
│  Compound?  ──yes──► Recursive decomposition                      │
│      │               (pipelines, chains, loops, conditionals,      │
│      no               subshells, substitutions, nested commands)   │
│      │                      │                                      │
│      ▼                      ▼                                      │
│  Simple command        Simple commands (with accumulated context)  │
│      │                      │                                      │
│      ├──────────────────────┘                                      │
│      ▼                                                             │
│  For each simple command:                                          │
│    1. Extract redirections → :filesystem ops (:source :redirection)│
│    2. Dispatch to command handlers (per-simple-command) ──┐       │
│                                                            │       │
└────────────────────────────────────────────────────────────┼───────┘
                                                             │
┌─ Layer 1: Command Handlers ────────────────────────────────┼───────┐
│                                                            ▼       │
│  jf/bash-extract-command-semantics(simple-cmd)                     │
│    → handler results by domain (:filesystem, :authentication, etc.)│
│                                                                    │
│  Top-level dispatch (for non-filesystem domains of simple commands)│
│    → captures domains the recursive engine doesn't return          │
│                                                                    │
└────────────────────────────────────────────────────────────────────┘
                         │
┌─ Merge ────────────────┼───────────────────────────────────────────┐
│                        ▼                                           │
│  Combine: grammar ops + handler ops                                │
│  Group by domain, collect claimed tokens, calculate coverage       │
│  Produce final (:parse-complete :coverage :domains ...)            │
│                                                                    │
└────────────────────────────────────────────────────────────────────┘
```

## Boundaries

### In Scope
- Remove entire plugin system (registry, registration, predicates, plugin-result struct)
- Restructure `jf/bash-extract-semantics` to implement two-layer architecture (grammar + command handlers)
- Make grammar extraction unconditional (no predicate gating)
- Migrate cloud-auth detection from plugin to command handlers for `aws`, `gcloud`, `az`
- Dispatch command handlers per-simple-command from Layer 0
- Migrate corpus tests from `jf/bash-extract-file-operations` to `jf/bash-extract-semantics`
- Preserve `jf/bash-extract-file-operations` as deprecated wrapper

### Out of Scope
- Changes to the parser (`jf/bash-parse`) — unchanged
- Changes to individual command handlers (cat.el, rm.el, etc.) — unchanged
- Changes to coverage calculation algorithm — unchanged
- Updating recursive engine to return multi-domain results — follow-up work
- Changes to gptel scope validation — it already consumes `jf/bash-extract-semantics`

### Internal vs External
- **Internal**: Layer 0 decomposition logic, Layer 0→Layer 1 dispatch mechanism, merge logic
- **External (stable)**: `jf/bash-extract-semantics` return structure, command handler registration API

## Testing Approach

### Test Framework
**Buttercup** — BDD framework with `describe`/`it`/`expect` syntax. Preferred for all new tests per project convention. Provides:
- `before-each`/`after-each` for handler registry setup/teardown
- Spy system for verifying Layer 0→Layer 1 dispatch
- Nested `describe` blocks for organizing by layer and command type

### Test Organization
**Location**: `config/bash-parser/test/unit/core/` for grammar layer tests (co-located with existing core unit tests like `token-emission-spec.el`).

New test files:
- `grammar-extraction-spec.el` — Layer 0: compound decomposition, redirection extraction, dispatch
- `grammar-redirection-spec.el` — Layer 0: redirection extraction in isolation (all operator types, variable paths, compound contexts)
- `semantic-pipeline-spec.el` — End-to-end: two-layer pipeline verification (Layer 0 through merge)

Migrated test file:
- `config/bash-parser/test/corpus/runners/corpus-file-operations-spec.el` — Buttercup version of corpus tests exercising `jf/bash-extract-semantics`

### Naming Conventions
- **Files**: `*-spec.el` suffix (Buttercup convention)
- **Describes**: `(describe "Grammar extraction"` / `(describe "Semantic pipeline"`
- **Its**: `(it "decomposes pipeline into simple commands"` — behavior-focused

### Running Tests
```bash
# Grammar layer tests
./bin/run-tests.sh -d config/bash-parser/test/unit/core -f buttercup

# Migrated corpus tests
./bin/run-tests.sh -d config/bash-parser/test/corpus -f buttercup

# All bash-parser tests (regression)
./bin/run-tests.sh -d config/bash-parser --report

# Integration tests (should pass unchanged)
./bin/run-tests.sh -d config/gptel/scope/test/integration -f buttercup
```

### Test Patterns

**Handler registry isolation**: Each spec uses `before-each` to clear and re-register only the handlers needed for that test, preventing cross-test leakage.

**Layer 0 dispatch verification**: Use Buttercup spies on `jf/bash-extract-command-semantics` to verify Layer 0 dispatches each simple command independently:
```elisp
(spy-on 'jf/bash-extract-command-semantics :and-call-through)
(jf/bash-extract-semantics (jf/bash-parse "cat f1.txt && rm f2.txt"))
(expect 'jf/bash-extract-command-semantics :to-have-been-called-times 2)
```

**Grammar extraction isolation**: Test redirection extraction directly on simple command plists, independent of handler dispatch, to verify unconditional behavior.

**Corpus migration pattern**: Each corpus test case calls `jf/bash-extract-semantics` and extracts the `:filesystem` domain operations for assertion, matching the old API's output format:
```elisp
(let* ((result (jf/bash-extract-semantics (jf/bash-parse command)))
       (fs-ops (alist-get :filesystem (plist-get result :domains))))
  (expect (length fs-ops) :to-equal expected-count))
```

### Scenario Mapping

| Spec Requirement | Test File | Pattern |
|---|---|---|
| Unconditional compound decomposition | `grammar-extraction-spec.el` | One `describe` per compound type (pipeline, chain, conditional, loop, subshell, substitution) |
| Unconditional redirection extraction | `grammar-redirection-spec.el` | One `it` per operator type + compound context scenarios |
| Simple command dispatch | `grammar-extraction-spec.el` | Spy on handler layer, verify call count and arguments |
| Result merge across layers | `semantic-pipeline-spec.el` | End-to-end tests with known handlers registered (no plugin system) |
| Filesystem operations (migrated corpus) | `corpus-file-operations-spec.el` | Parameterized corpus data, same test cases as ERT version |

## Dependencies

### Internal Dependencies
- `bash-parser-core` — Parser (unchanged)
- `bash-parser-recursive` — Recursive compound walker (reused by Layer 0)
- `bash-parser-semantics` — Command handler registry and extraction (unchanged, called per-simple-command)
- `bash-parser-plugins` — Orchestrator only (plugin infrastructure removed, module retains orchestrator function)
- `bash-parser-file-ops` — Redirection extraction functions (reused by Layer 0, old API becomes wrapper)
- `bash-parser-coverage` — Coverage calculation (unchanged)

### External Dependencies
- None — architecture is self-contained within bash-parser

### Test Dependencies
- `buttercup` — BDD test framework
- Existing command handler files (cat.el, rm.el, cp.el, etc.) for end-to-end tests

## Constraints

### Compatibility
- `jf/bash-extract-semantics` return structure MUST be preserved (gptel scope validation depends on it), except `:plugin-results` key is removed
- `jf/bash-extract-file-operations` MUST remain callable (deprecated wrapper)
- Command handler registration API unchanged

### Performance
- Layer 0 decomposition reuses existing recursive engine — no performance regression expected
- Per-simple-command handler dispatch adds N handler lookups (vs 1 top-level lookup) — negligible for typical command counts (<10 in a chain)

### Migration
- Old ERT corpus tests preserved during transition, removed after Buttercup migration verified
- `jf/bash-extract-file-operations` callers should migrate to `jf/bash-extract-semantics` over time
- Any code referencing the plugin system (`jf/bash-semantic-plugins`, `jf/bash-register-plugin`, `jf/bash-plugin-result`) must be updated
