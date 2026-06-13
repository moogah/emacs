---
name: update-tests-for-synchronous-flush
description: Update workspaces specs to assert synchronous flush instead of debounce/timer behavior, and add coverage for the new sync scenarios.
change: workspaces-synchronous-persistence-flush
status: done
relations:
  - "blocked-by:remove-debounce-wire-synchronous-flush"
---

# Update tests for synchronous flush

Bring the workspaces test suite in line with the synchronous-flush behavior:
remove/rewrite anything that asserted the debounce timer, and add coverage
proving deliberate commands and tab-switch now write to disk immediately.

Depends on `remove-debounce-wire-synchronous-flush` (the implementation must
land first; this task is verification + regression coverage).

## Files to modify

Buttercup specs under `config/workspaces/test/` (`*-spec.el`). Identify the
exact files by grep (step 1) — likely candidates:

- `config/workspaces/test/persistence-spec.el`
- `config/workspaces/test/persistence-v3-spec.el`
- any layout/tab-switch autosave spec

## Implementation steps

1. **Find debounce-coupled tests.** Grep the test tree for the removed symbols
   and any timer manipulation:
   ```bash
   grep -rn "workspace-save-state\|workspace--save-timer\|workspace-save-idle-delay\|run-with-idle-timer\|cl-letf.*timer\|current-idle-time" config/workspaces/test/
   ```
   For each hit, determine whether the test asserts *debounce timing* (must be
   rewritten) or merely *that a write eventually happened* (may already pass
   under synchronous flush).

2. **Rewrite timer assertions as synchronous-flush assertions.** A test that
   armed the idle timer and then simulated idle to force the write should
   instead assert the file on disk reflects the change **immediately after the
   command returns** — no timer simulation needed. Pattern: invoke the command,
   then read the state file (via the same temp-dir override the suite already
   uses — `workspace-state-directory-override`) and assert the expected
   workspace/layout/slot is present.

3. **Add coverage for the new scenarios** (from specs/workspaces/spec.md):
   - A deliberate command (`workspace-new`, `workspace-save-layout`,
     `workspace-switch-layout`) writes to disk synchronously — assert the state
     file contains the result right after the call, with no idle wait.
   - Workspace context switch (tab switch) writes the outgoing workspace's
     `:working-state` to disk synchronously.
   - Layout switch writes the outgoing layout's `:working-state` to disk
     synchronously.
   - (Regression) anti-save predicate still suppresses the autosave AND its
     flush on tab switch / idle tick.
   - (Regression) `workspace--persistence-blocked` still suppresses every write.

4. **Keep the existing invariant tests passing** (autosave-never-writes-saved-state,
   explicit-save-clears-working-state, restore-precedence-working-over-saved,
   corruption-safety). They should be unaffected; if any fail, that signals an
   implementation regression in the dependency task, not a test-update need.

5. **Run the suite:**
   ```bash
   ./bin/run-tests.sh -d config/workspaces
   ```
   All specs green.

## Design rationale

Only the flush *timing* changed; capture semantics and invariants did not. So
test churn should be confined to assertions that specifically exercised the
debounce timer. See design.md "Risks / Trade-offs" (tests asserting
timer/debounce behavior).

## Verification

```bash
# No test still references the removed debounce machinery:
grep -rn "workspace-save-state\|workspace--save-timer\|workspace-save-idle-delay" config/workspaces/test/
# expect: no matches
./bin/run-tests.sh -d config/workspaces
# expect: all specs pass
```

## Context pointers

- specs/workspaces/spec.md — the synchronous-flush scenarios this task covers.
- design.md D2/D3 — synchronous flush entry point and the idle already-idle fix.
- The implementation task: remove-debounce-wire-synchronous-flush.
