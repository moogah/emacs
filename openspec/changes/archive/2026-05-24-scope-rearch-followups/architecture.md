## Components

Three isolated fix sites, each in one existing module. No new modules. No interface changes beyond what the delta specs require.

- **`config/gptel/scope/scope-shell-tools.org`** — owner of `request_scope_expansion`. Gains a lookup from `tool_name` to `:validation-type` (consulting the tool's `:category` or the registered wrapper's recorded operation).
- **`config/gptel/scope/scope-expansion.org`** — owner of `jf/gptel-scope--add-bash-to-scope`. Bare-command-name branch gains a callback invocation. Writer helpers and docstrings drop `:org-roam-patterns`.
- **`config/gptel/scope-profiles.org`** — empty-scope fallback writer drops the `:org-roam-patterns` section. Docstrings trimmed.
- **`config/gptel/preset-registration.org`** — `jf/gptel-preset--extract-scope` drops `:org-roam-patterns` from its extraction list and emits a one-line warning when legacy keys are present.

## Interfaces

No new public interfaces. Behavior corrections only:

- `request_scope_expansion` tool schema is unchanged; the internal violation-info construction now branches on the resolved validation-type.
- `jf/gptel-scope--add-bash-to-scope` retains its signature; the bare-command-name branch now funcalls the callback with a structured `:success nil` payload (matching the Deny-style shape) instead of a user-facing `message`.
- `jf/gptel-preset--scope-defaults` entries no longer carry `:org-roam-patterns`; consumers that key off that field (if any remain) will silently stop receiving it.

## Boundaries

**In scope:**
- The three numbered fixes from `proposal.md`.
- Regression tests per fix.
- Spec deltas for `gptel/scope-expansion` and `gptel/scope-profiles`.

**Out of scope:**
- Any change to validator semantics, pipeline stages, error codes, or the scoped tool macro.
- Restoring pattern-based (org-roam) validation. The rearch removed it intentionally; this change finishes the removal, it does not revisit the decision.
- Introducing a "regression/" test subdir. The team's answer to the placement question was one subdir per bug, close to the code.

## Testing Approach

### Test Framework

**Buttercup** for all three regression tests. New tests in this codebase default to Buttercup per `CLAUDE.md`; the existing `config/gptel/scope/test/` tree is already Buttercup-first. ERT is not used for new work.

### Test Organization

One subdir per bug, next to the module under test:

- **Bug 1** (`request_scope_expansion` validation-type) → `config/gptel/scope/test/tool-wrapper/` (the tool is registered via `gptel-make-tool` but the violation-info construction is the wrapper-facing contract).
- **Bug 2** (`add-bash-to-scope` callback on bare command name) → `config/gptel/scope/test/expansion/` (this is a scope-expansion action handler).
- **Bug 3** (residual `org_roam_patterns` in empty-scope fallback) → `config/gptel/scope/test/yaml/` (the writer round-trip lives here, and the empty-scope output is a YAML writer concern).

### Naming Conventions

Per the existing pattern under `config/gptel/scope/test/`:
- Files are `*-spec.el`
- `(describe "<module or scenario>" (it "<specific behavior>" ...))`
- One top-level `describe` per file, matching the spec scenario group being regressed

Proposed filenames:
- `config/gptel/scope/test/tool-wrapper/request-scope-expansion-validation-type-spec.el`
- `config/gptel/scope/test/expansion/add-bash-to-scope-callback-spec.el`
- `config/gptel/scope/test/yaml/empty-scope-fallback-spec.el`

### Running Tests

Existing conventions apply:

```
./bin/run-tests.sh -d config/gptel/scope/test/tool-wrapper   # Bug 1
./bin/run-tests.sh -d config/gptel/scope/test/expansion      # Bug 2
./bin/run-tests.sh -d config/gptel/scope/test/yaml           # Bug 3
./bin/run-tests.sh -d config/gptel/scope                     # All scope tests
```

Snapshot captures via `--snapshot` are available; not required for these regressions.

### Test Patterns

Match what `config/gptel/scope/test/` already does:

- **Setup/teardown**: `before-each` / `after-each` for buffer-local state (`let*`-bound temp buffer, set `jf/gptel--branch-dir`, write a throwaway `scope.yml` in a temp directory created with `make-temp-file`).
- **Mocks**: `(spy-on 'symbol :and-return-value VALUE)` for gptel-owned surfaces (e.g., the async callback). Avoid mocking internal scope functions — the whole point is to catch drift between the handler and its callback contract.
- **Fixtures**: `config/gptel/scope/test/helpers-spec.el` already provides shared matchers and mocks; reuse `jf/gptel-scope-test--with-temp-scope-yml` (or the equivalent already-present helper) rather than writing new setup code.
- **Assertions**: `expect ... :to-equal`, `:to-be`, `:to-have-been-called-with`.

### Scenario Mapping

Each scenario from the delta specs maps to one Buttercup `it` block.

| Spec scenario | Test case |
|---|---|
| "Validation-type resolves from the requested tool" | `request-scope-expansion-validation-type-spec.el` — `it "resolves path for filesystem tools"` |
| "Validation-type resolves to bash for bash-backed tools" | same file — `it "resolves bash for run_bash_command"` |
| "Unknown tool names are rejected" | same file — `it "returns :success nil for unknown tool"` |
| "Bash file-op denials route to path sections" | `add-bash-to-scope-callback-spec.el` — `it "writes and invokes callback for path-shaped resource"` |
| "Bare command name refusal invokes the callback" | same file — `it "invokes callback with :success nil for bare command name"` |
| "Empty-scope fallback writes only validator-consumed sections" | `empty-scope-fallback-spec.el` — `it "omits org_roam_patterns from written YAML"` and `it "emits only paths/cloud/security sections"` |

## Dependencies

No new external libraries. Relies only on what is already in-tree:
- `buttercup` (already required for testing)
- `yaml` (for YAML round-trip assertions in Bug 3)
- `config/gptel/scope/test/helpers-spec.el` (shared fixtures)

## Constraints

- **No backwards-compatibility shims.** Bug 3's removal of `:org-roam-patterns` from the extracted keys is final. If a preset still carries it, the warning tells the user to delete the key from their preset — there is no silent migration.
- **Callback invocation is exactly-once.** Bug 2's fix must not introduce a path where the callback fires twice. Every branch in `jf/gptel-scope--add-bash-to-scope` must end in exactly one funcall.
- **No validator changes.** If a fix appears to need a validator change, escalate — the rearch is a stable surface and this change must stay at the edges.
