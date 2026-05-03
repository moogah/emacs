---
name: add-snapshot-coverage-to-persistent-agent-creation-spec
description: The persistent-agent creation spec was not updated when the wiring task added snapshot keys to its session.org drawer. The PA path's snapshot emission is currently structurally untested — a regression where `preset-spec` is accidentally nil would not be caught. Add the `:GPTEL_MODEL:`/`:GPTEL_SYSTEM:`-absence assertions, mirroring the pattern applied to the three commands-path tests, and refresh the stale scenario comment.
change: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:wire-snapshot-into-session-creation
---

## Files to modify

- `config/gptel/tools/test/persistent-agent/creation-spec.el` — extend the "writes session.org with a self-describing :PROPERTIES: drawer" test (currently lines 83–129) with snapshot-key assertions; refresh its leading scenario-mapping comment

## Why

**Finding 1 (blocking).** The wire-snapshot task's brief (Implementation step 8) explicitly required the PA creation spec to assert `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, AND the snapshot keys, with `:GPTEL_SYSTEM:` absent. The diff updated three commands-path test files with this pattern but missed the persistent-agent file. `grep` of `creation-spec.el` returns zero matches for `GPTEL_MODEL`, `GPTEL_TOOLS`, `GPTEL_BACKEND`, `GPTEL_TEMPERATURE`, `GPTEL_SYSTEM`, or `snapshot`. The `with-mock-preset` fixture registers a non-trivial preset (with `:backend` and `:model`) so `gptel-get-preset` returns a non-nil spec — the production path *does* emit snapshot keys into the drawer; nothing verifies it.

**Finding 2 (advisory).** The leading scenario comment on the same test reads: *"The drawer carries `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, and the agent's `:GPTEL_SCOPE_*:` keys (Mode 2a — `register/boundary/scope-profile-applicator`)."* It does not mention the chat-mode snapshot keys or the `:GPTEL_SYSTEM:` exclusion, so it diverges from the cited register entry as updated in cycle-5. A reader following the comment will hit a contradiction.

## Implementation steps

1. In `config/gptel/tools/test/persistent-agent/creation-spec.el`, extend the "writes session.org with a self-describing :PROPERTIES: drawer" `it` block (around lines 83–129) with assertions:
   - `(expect (org-entry-get nil "GPTEL_MODEL") :not :to-be nil)` — snapshot model present
   - `(expect (org-entry-get nil "GPTEL_SYSTEM") :to-be nil)` — system key excluded
   - Optionally one or two more snapshot keys (e.g. `:GPTEL_BACKEND:`, `:GPTEL_TEMPERATURE:`) to broaden coverage
   - Mirror the structural-assertion style used in the three sibling files updated in cycle-5 (`session-creation-spec.el`, `session-org-creation-spec.el`, `activity-session-chat-spec.el`).
2. Update the leading scenario-mapping comment to mention the chat-mode snapshot keys and the `:GPTEL_SYSTEM:` exclusion, citing Decision 2 and `register/invariant/drawer-system-key-write-exclusion` alongside the existing `register/boundary/scope-profile-applicator` reference. Match the comment style of the three updated commands-path tests.
3. Re-run `./bin/run-tests.sh -d config/gptel/tools` and confirm the new assertions pass with the post-cycle-5 production code, and would fail if `preset-spec` were forced nil.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/tools
grep -nE "GPTEL_MODEL|GPTEL_SYSTEM" config/gptel/tools/test/persistent-agent/creation-spec.el
```

Expect: `creation-spec.el` now matches the snapshot-key contract; tests pass.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/wire-snapshot-into-session-creation.md` (Findings 1, 2).

Cited register entries: `interfaces.org#register-shape-drawer-text-block`, `interfaces.org#register-boundary-scope-profile-applicator`, `interfaces.org#register-invariant-drawer-system-key-write-exclusion`.
