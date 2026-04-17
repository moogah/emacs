## Why

Three bugs surfaced while verifying specs against the rearchitected gptel scope validation code. They are not in the rearch itself — the rearch is clean — but are pre-existing or newly-exposed gaps that the verification uncovered. Tracking them together gives us a single reviewable unit for the cleanup.

This change is a non-traditional use of the OpenSpec workflow: the proposal is a **findings document**, not a feature pitch. The specs delta describes behavior the current code fails to honor (not new behavior being introduced), and the design is a fix plan, not an architecture proposal. We are trying this format to see if OpenSpec holds up as a container for post-verification reconciliation work.

## What Changes

- **Bug 1 — `request_scope_expansion` hardcodes `:validation-type path`.** The pre-emptive expansion tool constructs a violation-info plist with `:validation-type 'path` even when the LLM is asking about a bash-backed operation. Section-targeted writes and the routing contract in the expansion UI both key off `:validation-type`, so pre-emptive expansion cannot correctly target bash scenarios.
- **Bug 2 — `jf/gptel-scope--add-bash-to-scope` drops the callback for bare command names.** When the resource is not path-shaped (no `/`, no `~`, no glob characters), the handler emits a `message` and returns without funcalling the callback. The pending invocation's continuation never fires — the wrapper hangs rather than receiving a structured denial.
- **Bug 3 — Residual `org_roam_patterns` code in three modules.** The rearch removed pattern-based validation from the validation pipeline and from all profile YAML files, but `scope-expansion.org`, `scope-profiles.org`, and `preset-registration.org` still reference `org-roam-patterns` / `org_roam_patterns`. This is dead code that can surprise future readers and can write a phantom section into `scope.yml` under the empty-scope fallback path.

## Capabilities

### New Capabilities

_(none — this change fixes behavior, it does not introduce new capabilities)_

### Modified Capabilities

- `gptel/scope-expansion`: Correct the `request_scope_expansion` contract so `:validation-type` is derived from the tool it is requesting for, and correct `jf/gptel-scope--add-bash-to-scope` so the callback is always invoked (including on the bare-command-name refusal path).
- `gptel/scope-profiles`: Explicitly remove `org-roam-patterns` from the extracted key set and from the empty-scope fallback writer; the empty `scope.yml` should only contain sections the validators actually consume.

## Impact

**Code:**
- `config/gptel/scope/scope-shell-tools.org` — rework `request_scope_expansion` violation-info construction
- `config/gptel/scope/scope-expansion.org` — fix the bare-command-name branch in `jf/gptel-scope--add-bash-to-scope` to funcall the callback; remove `:org-roam-patterns` references from the writer helpers
- `config/gptel/scope-profiles.org` — remove `:org-roam-patterns` from the empty-scope fallback and from docstrings that imply it's a supported section
- `config/gptel/preset-registration.org` — drop `:org-roam-patterns` from the key-extraction list

**Tests:**
- Regression coverage under `config/gptel/scope/test/tool-wrapper/` or `test/expansion/` for bugs 1 and 2
- A small assertion test under `config/gptel/scope/test/yaml/` or similar that the empty-scope fallback's written YAML contains no `org_roam_patterns` section

**Specs:**
- Delta on `scope-expansion.md` to clarify the `:validation-type` and callback contracts the fixes restore
- Delta on `scope-profiles.md` to drop `:org-roam-patterns` from the integration-with-preset-registration scenario

**Not affected:** no change to validator semantics, pipeline stages, error codes, or the scoped tool macro. The rearch's invariants are preserved; this change only makes the current code honor them.
