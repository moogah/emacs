---
name: fix-add-bash-to-scope-callback-hang
description: "Replace the bare (message ...) at scope-expansion.el line 640 (bare-command-name refusal branch of jf/gptel-scope--add-bash-to-scope) with a structured funcall of the async callback. Today every other branch funcalls; this one drops the callback and the wrapper hangs forever."
change: scope-rearch-followups
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/scope-expansion.org` (modify) — the `jf/gptel-scope--add-bash-to-scope` function (heading + babel block; `.el` line 618-641 is the tangled output).
- `config/gptel/scope/test/expansion/add-bash-to-scope-callback-spec.el` (new) — Buttercup regression spec.

## Why

Every branch of `jf/gptel-scope--add-bash-to-scope` must funcall the wrapper's async callback exactly once — that is the documented contract for scope-expansion action handlers. The bare-command-name branch currently violates this: when the resource is not path-shaped, the handler emits a `message` and returns nil, leaving the pending tool invocation's continuation never fired.

Symptom: the wrapper hangs rather than receiving a structured denial. Bug 2 in the proposal.

## Implementation steps

1. **Read** the current implementation at `config/gptel/scope/scope-expansion.el:618-641` (and the corresponding `.org` block) to confirm the bare-command-name branch's exact code and to inspect what the wrapper expects from `callback`.
2. **Check the function signature.** `(jf/gptel-scope--add-bash-to-scope resource tool &optional denied-operation)` does NOT receive `callback` as an argument today. **Investigate the call site** in `jf/gptel-scope-prompt-expansion` / the expansion-UI action handlers (search for `add-bash-to-scope` in `scope-expansion.el`). The fix may require:
   - Adding `callback` as an argument to this function and threading it from the call site, OR
   - The call site already wraps a callback and the bare-command-name branch needs to signal a structured nil to the wrapper rather than the literal callback.
   Whichever is the case, the function ultimately needs to deliver the design D2 payload to the wrapper's continuation. **Document the call-site shape in `## Observations`** and pick the simpler of the two approaches.
3. **Write the failing regression spec FIRST** at `config/gptel/scope/test/expansion/add-bash-to-scope-callback-spec.el`. Two `it` blocks under `describe "jf/gptel-scope--add-bash-to-scope callback contract"`:
   - `it "writes and invokes callback :success t for path-shaped resource"` — call with a path-shaped resource (e.g. `"/tmp/foo"`), assert the underlying writer is called AND the callback receives a `:success t` payload.
   - `it "invokes callback with :success nil for bare command name"` — call with `"brew"` (or another bare name), assert the writer is NOT called AND the callback receives a JSON string with `:success false`, `:error "command_name_not_expandable"`, and a human-readable message naming the offending command.
   Reuse spy patterns from `add-bash-to-scope-routing-spec.el` and the helpers in `helpers-spec.el`.
4. **Run the new spec — confirm it fails** (the bare-command-name branch hangs / never delivers the callback).
5. **Apply the fix** per design D2 in `design.md:59-76`:
   ```elisp
   (funcall callback
            (json-serialize
             `(:success :false
               :error "command_name_not_expandable"
               :message ,(format "Cannot expand scope for command name '%s'. Request expansion for a specific file operation (path) instead."
                                 resource)
               :user_denied t)))
   ```
   Place this where the `(message ...)` currently sits. Remove the `message` call (it's redundant once the LLM gets structured feedback).
6. **Re-tangle** with `./bin/tangle-org.sh config/gptel/scope/scope-expansion.org`.
7. **Run the new spec — confirm it passes**.
8. **Run the broader scope suite** with `./bin/run-tests.sh -d config/gptel/scope --report` to confirm no regressions.

## Verification

```bash
./bin/tangle-org.sh config/gptel/scope/scope-expansion.org
./bin/run-tests.sh -d config/gptel/scope/test/expansion
./bin/run-tests.sh -d config/gptel/scope --report
```

Expected: new spec passes; the callback fires exactly once on every branch (confirm with spy call counts in both `it` blocks); scope suite counts match baseline or improve.

## Out-of-scope

- Refactoring the path-router / bash-router split.
- Inventing a new error code class — `"command_name_not_expandable"` is emitted by the expansion-UI handler, not a validator, and lives in a separate vocabulary from the canonical validator error codes (per design D2's note on this).
- Bug 1 (`request_scope_expansion` refactor) — separate parallel task; no shared file conflicts.
- Bug 3 (`:org-roam-patterns` cleanup) — separate parallel task; no shared file conflicts.

## Context

- Design D2 in `openspec/changes/scope-rearch-followups/design.md:59-76` — exact payload shape.
- Spec scenario: `openspec/changes/scope-rearch-followups/specs/gptel/scope-expansion.md` — Requirement "Section-targeted writes", Scenario "Bare command name refusal invokes the callback".
- Wrapper contract: `jf/gptel-scope-prompt-expansion` docstring (currently in `scope-expansion.org`, ~line 643 of `.el`).

## Observations

_(implementor fills during execution)_

## Discoveries

_(implementor fills during execution)_
