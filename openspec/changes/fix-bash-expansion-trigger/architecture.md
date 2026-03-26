## Components

### 1. `gptel-make-scoped-tool` macro (scope-core.el)
**Responsibility**: Single point of validation, expansion triggering, and retry for ALL tool types. Wraps tool functions with permission checking before body execution.

**Change**: No structural change needed. The macro already has the correct async expansion flow. The fix is in what `validate-bash-tool` returns.

### 2. `validate-bash-tool` (scope-core.el or scope-shell-tools.el)
**Responsibility**: Validates bash commands through the 7-stage semantic pipeline and returns `:allowed nil` on failure.

**Change**: Rewrite to call `jf/gptel-scope--validate-command-semantics` instead of returning `:allowed t`. Must propagate error fields so `build-violation-info` can construct violation-info.

**Location**: Stays in scope-core.el alongside `validate-path-tool` and `validate-pattern-tool`. The macro owns all validation dispatch — all validators live with the macro. It calls `validate-command-semantics` (defined in scope-shell-tools.el) at runtime; no `require` needed since scope-shell-tools loads during init before any bash tool is invoked.

### 3. `run_bash_command` tool definition (config/gptel/tools/bash-tools.el — NEW file)
**Responsibility**: Define the `run_bash_command` tool with execution logic. No validation.

**Change**: Move tool definition from scope-shell-tools.el to `config/gptel/tools/bash-tools.el` (alongside all other tool definitions). Remove `validate-command-semantics` call — the body only contains execution logic (timeout, output truncation, result formatting). The `request_scope_expansion` meta-tool also moves here.

### 3b. `scope-shell-tools.el` (validation pipeline library)
**Responsibility**: Provide the 7-stage semantic validation pipeline, schema loading, glob matching, and file operation validation as library functions called by scope-core's validators.

**Change**: Remove tool definitions and execution logic. This becomes a pure validation library.

### 4. `build-violation-info` (scope-core.el)
**Responsibility**: Transform validation error plists into violation-info format for expansion UI.

**Change**: Already handles bash error types (path_out_of_scope, command_denied, cloud_auth_denied). No change needed — it just wasn't being reached before because `validate-bash-tool` never returned errors.

### 5. Expansion UI actions (scope-expansion.el)
**Responsibility**: Handle user choices (deny/add-to-scope/allow-once).

**Change**: Update bash "add to scope" to use v4 path-based model. When bash denial is path_out_of_scope, add path to `paths.read` or `paths.write` based on operation type. Remove v3 category-based code.

## Interfaces

### Macro → Validator contract
```
check-tool-permission(config, tool-name, args, metadata)
  → (:allowed t)                          ;; pass
  → (:allowed nil :error E :message M ...) ;; fail — triggers expansion
```

All validators (path, pattern, bash) MUST return this format. The macro makes decisions solely on `:allowed`.

### Validator → Validation pipeline
```
validate-bash-tool(tool-name, args, config, metadata)
  calls → validate-command-semantics(command, directory, config)
    → nil                                  ;; pass
    → (:error E :message M :path P ...)    ;; fail
  wraps result in → (:allowed t/nil ...)
```

### Macro → Expansion UI
```
trigger-inline-expansion(check-result, tool-name, wrapper-callback)
  calls → build-violation-info(check-result, tool-name)
    → (:tool T :resource R :reason M :validation-type V :operation O)
  calls → prompt-expansion(violation-info, callback, patterns, tool-name)
```

### Expansion UI → Scope updater
For bash path denials:
```
add-to-scope dispatches on :validation-type
  'bash + path resource → add-path-to-scope(scope-file, path, operation)
  'bash + command_denied → not addable (allow-once or deny only)
  'bash + cloud_auth_denied → update cloud.allowed_providers
```

## Boundaries

### In scope
- `validate-bash-tool` rewrite (the core fix)
- Move tool definitions from scope-shell-tools.el to `config/gptel/tools/bash-tools.el`
- `run_bash_command` tool body simplification (remove validation)
- Expansion "add to scope" update for bash v4 paths
- Integration tests for bash validation → expansion flow
- Spec updates for all three modified capabilities

### Out of scope
- Changes to the macro structure itself (it already works correctly)
- Changes to path or pattern validators
- Changes to the expansion UI transient menu
- Changes to allow-once mechanics

## Testing Approach

### The Gap in Current Tests

Every existing test calls validators and expansion helpers **directly** — none invoke a tool through the `gptel-make-scoped-tool` macro. The tests prove each link works in isolation but never verify the chain is connected. This is why the suite passed while expansion was broken for bash tools.

### New Test Layer: Macro-Level Contract Tests

We need tests that **test the macro directly** using minimal test tools to verify three contract layers:

1. **Routing contract** — the macro dispatches to the correct validator for the tool's category. Currently `tool-routing-spec.el` tests the lookup (category → validation type), but nothing tests the dispatch (that `check-tool-permission` actually calls `validate-bash-tool` for bash tools, not `validate-path-tool`).

2. **Validation contract** — the validator returns the correct `:allowed` result, and the macro acts on it (passes through on `:allowed t`, triggers expansion on `:allowed nil`).

3. **Expansion contract** — on denial, the macro triggers expansion UI, and handles approval (retry + execute body) or denial (return error).

**Key pattern**: Create trivially simple test tools via the macro — tool bodies are one-liners like `(list :success t :result "executed")`. This isolates the macro's behavior from tool-specific complexity (no bash-parser, no filesystem, no mocking overhead).

**Transient automation**: Mock `jf/gptel-scope-prompt-expansion` with `:and-call-fake` that immediately invokes the callback (simulating user choice). This is already proven in existing tests — transient suffix functions are just regular elisp that get state from `(transient-scope)`, so we can test the entire flow without displaying a UI.

### Test Framework
Buttercup (BDD framework) — consistent with all recent test additions in this codebase.

### Test Organization
- **New**: `config/gptel/scope/test/expansion/macro-expansion-contract-spec.el` — tests that tools invoke expansion through the macro for ALL validation types (path, pattern, bash)
- **Update**: `config/gptel/tools/test/run-bash-command-spec.el` — update for tool body changes
- **Existing**: `config/gptel/scope/test/expansion/` — keep existing unit/integration tests

### Naming Conventions
- Files: `*-spec.el`
- Suites: `(describe "Macro expansion contract" ...)`
- Tests: `(it "triggers expansion UI when bash validation denies command" ...)`

### Running Tests
```bash
./bin/run-tests.sh -d config/gptel/scope/test/expansion   # All expansion tests
./bin/run-tests.sh -d config/gptel/tools                   # Tool contract tests
./bin/run-tests.sh -d config/gptel                         # All gptel tests
```

### Test Patterns

**Macro contract tests use minimal test tools** — tool bodies are one-liners like `(list :success t)`. This isolates the macro's behavior from tool-specific complexity (no bash-parser, no filesystem). Three test layers:

**Layer 1: Routing contract** — verify correct validator dispatched for each category:
```elisp
;; Create minimal bash-category tool via macro
;; Spy on validate-bash-tool AND validate-path-tool
;; Invoke tool through macro
;; Assert: validate-bash-tool WAS called
;; Assert: validate-path-tool was NOT called
```

**Layer 2: Validation + Expansion contract** — mock `check-tool-permission`, verify macro behavior:
```elisp
;; Create minimal async tool via macro
;; Mock check-tool-permission → (:allowed nil :error "path_out_of_scope" ...)
;; Mock prompt-expansion to auto-approve (invoke callback immediately)
;; Invoke tool through macro
;; Assert: prompt-expansion WAS called (expansion triggered)
;; Assert: tool body DID execute (after approval)
```

**Layer 3: Validator unit tests** — test validator functions directly:
```elisp
;; Call validate-bash-tool with restricted config
;; Mock bash-parser to return semantics with out-of-scope path
;; Assert: returns (:allowed nil :error "path_out_of_scope" ...)
```

### Scenario Mapping

**Layer 1: Routing** (spy on validators, verify correct one called):

| Scenario | Mocking |
|---|---|
| Bash-category tool dispatches to bash validator | Spy on all validators. Assert only bash called |
| Path-category tool dispatches to path validator | Spy on all validators. Assert only path called |
| Pattern-category tool dispatches to pattern validator | Spy on all validators. Assert only pattern called |
| Meta-category tool bypasses all validation | Spy on all validators. Assert none called, body runs |

**Layer 2: Expansion** (mock check-tool-permission, test macro flow):

| Scenario | Mocking |
|---|---|
| Async tool: denial triggers expansion UI | Mock check-tool-permission → denied. Spy on prompt-expansion |
| Async tool: approval runs body | Mock prompt-expansion auto-approve. Verify body executes |
| Async tool: denial skips body | Mock prompt-expansion auto-deny. Verify body NOT called |
| Sync tool: denial returns error immediately | Mock check-tool-permission → denied. Verify no expansion UI |

**Layer 3: Validator units** (direct calls, no macro):

| Scenario | Mocking |
|---|---|
| validate-bash-tool denies out-of-scope path | Mock bash-parser |
| validate-bash-tool allows in-scope command | Mock bash-parser |
| validate-bash-tool rejects missing bash_tools config | None |

## Dependencies

- `bash-parser-core` — tree-sitter bash parsing (already required by scope-shell-tools)
- `bash-parser-orchestrator` — semantic extraction pipeline (already required by scope-shell-tools)
- `jf-gptel-scope-expansion` — expansion UI (already required by scope-shell-tools)
- `transient` — UI framework for expansion menu

## Constraints

- **Macro expansion timing**: The macro is expanded at compile/load time. Changes to `validate-bash-tool` behavior don't require macro changes since it's called at runtime via `check-tool-permission`.
- **Backward compatibility**: `request_scope_expansion` meta-tool must continue working for pre-emptive expansion requests.
- **Transient in batch mode**: Transient menus cannot display in batch Emacs (no display). Tests MUST mock `jf/gptel-scope-prompt-expansion` rather than attempting to render the transient menu.
- **Macro-level tests are the regression gate**: The existing unit and integration tests for individual components remain valuable, but the macro-level contract tests are what prevent this class of bug. If a validator returns `:allowed t` incorrectly, only a test that invokes the tool through the macro will catch it.
