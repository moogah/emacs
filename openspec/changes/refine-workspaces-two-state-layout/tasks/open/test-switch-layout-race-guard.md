---
name: test-switch-layout-race-guard
description: Add a buttercup spec that fires two workspace-switch-layout calls back-to-back and asserts the older deferred restore closure no-ops via the workspace--restore-generation check. Closes the test gap surfaced by on-touch Architect Finding 1 (advisory remainder after inline-fix commit bc64784 moved the cl-incf into workspace--restore-frameset).
change: refine-workspaces-two-state-layout
status: ready
relations:
  - "discovered-from:bookmark-reincarnation"
discovered_by: architect
discovered_class: invariant-gap
---

## Files to modify

- `config/workspaces/test/buffer-reincarnation-spec.el` (modify) — add a new `describe` block adjacent to the existing race-guard test, exercising `workspace-switch-layout` as the entry point instead of `workspace--apply-saved-layout`.

## Implementation steps

1. **Locate the existing race-guard test** in `buffer-reincarnation-spec.el` (around line 180 in the bookmark-reincarnation merge). It pre-loads a workspace, calls `workspace--apply-saved-layout` twice in quick succession (with `run-at-time` mocked to capture the closure), then asserts only the second closure runs `window-state-put`.

2. **Add a sibling test** in the same file that:
   - Sets up two layouts on a single workspace.
   - Calls `workspace-switch-layout` to layout A, then immediately to layout B (no intervening idle).
   - Drains the timers and asserts the layout-A closure no-ops (its captured `gen` no longer equals the live `workspace--restore-generation`), while the layout-B closure proceeds.

3. **Keep the existing test untouched.** The new test pins the boundary contract's claim "every entry into `workspace--restore-frameset` participates in the race guard". The existing test pins the same invariant via `workspace--apply-saved-layout`; both belong.

## Design rationale

The race guard moved into `workspace--restore-frameset` itself in commit `bc64784` (inline fix for Finding 1). That move closes the structural bug; the test gap (Finding 1's secondary recommendation) remains. Adding the test cements `register/boundary/buffer-reincarnation-pipeline`'s stage 4 contract for both entry paths, and prevents a future refactor that re-introduces the per-callsite increment from silently regressing the guard.

## Verification

```bash
./bin/run-tests.sh -d config/workspaces
```

Specific assertion:

- The new `it` block exercises `workspace-switch-layout` (not `workspace--apply-saved-layout`) as the trigger.
- Test count: 86 → 87 (or higher, if multiple scenarios are added).

## Context

- Finding ID: `arch-cycle-20260524-200631-on-touch-bookmark-reincarnation-1`
- Findings file: `.orchestrator/cycles/cycle-20260524-200631/findings/on-touch-bookmark-reincarnation.md`
- Resolved-in-part by: commit `bc64784` (cl-incf relocation).
- Remaining acceptance for `register/boundary/buffer-reincarnation-pipeline` to transition from `divergent` → `reconciled` at integrate.
