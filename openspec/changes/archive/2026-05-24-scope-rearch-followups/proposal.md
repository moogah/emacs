## Why

Three bugs surfaced while verifying specs against the rearchitected gptel scope validation code. They are not in the rearch itself — the rearch is clean — but are pre-existing or newly-exposed gaps that the verification uncovered. Tracking them together gives us a single reviewable unit for the cleanup.

This change is a non-traditional use of the OpenSpec workflow: the proposal is a **findings document**, not a feature pitch. The specs delta describes behavior the current code fails to honor (not new behavior being introduced), and the design is a fix plan, not an architecture proposal. We are trying this format to see if OpenSpec holds up as a container for post-verification reconciliation work.

## What Changes

- **Bug 1 — `request_scope_expansion` takes the wrong primary argument.** The pre-emptive expansion tool's LLM-facing signature is `(tool_name, patterns, justification)`. It then has to recover `:validation-type` (`'path` or `'bash`) from the `tool_name` string to construct a routable violation-info — and today it can't, so it hardcodes `:validation-type 'path` and mis-routes bash scenarios. The fix is not to add tool-name → validation-type lookup machinery (that approach was attempted in commit `d00949f`, then reverted in `9ed38c6` after author-blind review showed the lookup was an elaborate workaround for the wrong primary argument). Scope is fundamentally `(operation, paths)` — every other consumer of `:validation-type` derives it from operation directly (`scope-validation.el:779-785`). The right fix is to refactor `request_scope_expansion`'s signature to `(operation, patterns, justification)`, deriving `:validation-type` the same way the validation pipeline does.
- **Bug 2 — `jf/gptel-scope--add-bash-to-scope` drops the callback for bare command names.** When the resource is not path-shaped (no `/`, no `~`, no glob characters), the handler emits a `message` and returns without funcalling the callback. The pending invocation's continuation never fires — the wrapper hangs rather than receiving a structured denial.
- **Bug 3 — Residual `org_roam_patterns` code in three modules.** The rearch removed pattern-based validation from the validation pipeline and from all profile YAML files, but `scope-expansion.org`, `scope-profiles.org`, and `preset-registration.org` still reference `org-roam-patterns` / `org_roam_patterns`. This is dead code that can surprise future readers and can write a phantom section into `scope.yml` under the empty-scope fallback path.

## Capabilities

### New Capabilities

_(none — this change fixes behavior, it does not introduce new capabilities)_

### Modified Capabilities

- `gptel/scope-expansion`: Refactor `request_scope_expansion`'s LLM-facing signature so its primary argument is `operation` (closed-set: `read`, `write`, `modify`, `execute`, or `bash`) rather than `tool_name`; `:validation-type` then derives trivially from operation. Correct `jf/gptel-scope--add-bash-to-scope` so the callback is always invoked (including on the bare-command-name refusal path).
- `gptel/scope-profiles`: Explicitly remove `org-roam-patterns` from the extracted key set and from the empty-scope fallback writer; the empty `scope.yml` should only contain sections the validators actually consume.

## Impact

**Code:**
- `config/gptel/scope/scope-shell-tools.org` — replace `request_scope_expansion`'s `tool_name` arg with `operation` (closed-enum); derive `:validation-type` from operation in the violation-info constructor
- `config/gptel/scope/scope-expansion.org` — fix the bare-command-name branch in `jf/gptel-scope--add-bash-to-scope` to funcall the callback; remove `:org-roam-patterns` references from the writer helpers
- `config/gptel/scope-profiles.org` — remove `:org-roam-patterns` from the empty-scope fallback and from docstrings that imply it's a supported section
- `config/gptel/preset-registration.org` — drop `:org-roam-patterns` from the key-extraction list

**Tests:**
- Regression coverage under `config/gptel/scope/test/tool-wrapper/` or `test/expansion/` for bugs 1 and 2
- A small assertion test under `config/gptel/scope/test/yaml/` or similar that the empty-scope fallback's written YAML contains no `org_roam_patterns` section

**Specs:**
- Delta on `scope-expansion.md` to specify `request_scope_expansion`'s new `(operation, patterns, justification)` signature, the operation→validation-type derivation, the unknown/invalid-operation rejection contract, and the callback contracts the fixes restore
- Delta on `scope-profiles.md` to drop `:org-roam-patterns` from the integration-with-preset-registration scenario

**LLM-facing API:** Bug 1 changes `request_scope_expansion`'s argument schema (`tool_name` → `operation`). Existing model prompts that called the tool with `tool_name` will need to switch to `operation`. No data migration; the tool is a transient LLM affordance, not a persisted format.

**Not affected:** no change to validator semantics, pipeline stages, error codes, or the scoped tool macro. The rearch's invariants are preserved; this change only makes the current code honor them.
