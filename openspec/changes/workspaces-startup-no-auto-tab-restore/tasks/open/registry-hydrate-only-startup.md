---
name: registry-hydrate-only-startup
description: Startup hydrates the registry without creating tabs; remove the lazy-restore machinery in full
change: workspaces-startup-no-auto-tab-restore
status: ready
relations: []
---

## Files to modify

- config/workspaces/persistence.org (modify) → tangle to persistence.el
- config/workspaces/data-model.org (modify) → tangle to data-model.el
- config/workspaces/test/persistence-spec.el (modify)
- config/workspaces/test/persistence-v3-spec.el (modify)
- config/workspaces/test/tabs-spec.el (modify)

## Implementation steps

1. **persistence.org — `workspace--restore`** (the "Startup restore entry
   point" subtree): remove the `(workspace--restore-tabs)` call. The body
   becomes: read state, and when non-nil, `workspace--deserialize-state`.
   Update the docstring to say it hydrates the registry only (no tabs).

2. **persistence.org — delete `workspace--restore-tabs`** entirely (the
   "Restore on startup" subtree's tab-creation defun). No other caller
   exists.

3. **persistence.org — delete the lazy-activation machinery**:
   - `workspace--activate-pending-workspace` (defun).
   - `workspace--persistence-after-tab-switch` (defun) AND its two
     `(advice-add 'tab-bar-switch-to-tab :after …)` /
     `(advice-add 'tab-bar-select-tab :after …)` forms (the
     "Hook into tab switch advice" subtree).
   - Do NOT touch `workspace--persistence-before-tab-switch` or its
     `:before` advice — that is the outgoing-workspace `:working-state`
     autosave and is unrelated.

4. **persistence.org — `workspace--deserialize-state`**: stop wrapping
   entries in `workspace--mark-restore-pending`. In the `t` cond arm,
   `puthash` the workspace directly (with the `:broken` tag still applied
   via `workspace--mark-broken` when the home dir is missing). Update the
   docstring to drop the `:restore-pending` paragraph.

5. **persistence.org — `workspace--apply-saved-layout`**: remove the
   trailing block that clears `:restore-pending`
   (`(when (workspace--restore-pending-p ws) (puthash … (workspace--clear-restore-pending ws) …))`).
   The function now only applies the effective layout. It remains the
   shared helper for `workspace-restore` / `workspace-revert` /
   `workspace-switch-layout`. Update its docstring (drop the
   "Also clears any `:restore-pending'" sentence).

6. **persistence.org — `workspace--persistence-serialize-workspace`**: no
   code change needed (it already whitelists slots), but update the
   docstring to drop the `:restore-pending` mention; it now only notes
   that `:broken` is runtime-only and stripped by the whitelist.

7. **data-model.org — remove the `:restore-pending` helpers**: delete
   `workspace--restore-pending-p`, `workspace--mark-restore-pending`, and
   `workspace--clear-restore-pending`, plus the surrounding prose subtree
   that documents the flag. (Confirm no remaining callers:
   `grep -rn restore-pending config/workspaces/*.org` should return
   nothing after this task.)

8. **Tangle both**: `./bin/tangle-org.sh config/workspaces/persistence.org`
   and `./bin/tangle-org.sh config/workspaces/data-model.org` (each
   auto-validates parens).

9. **Tests — persistence-spec.el**:
   - Rework the "Workspaces survive restart" → "restores tabs from the
     persisted file" spec: after `workspace--restore`, assert the registry
     is hydrated (alpha + beta present) AND that **no workspace tab was
     created** by restore (tab count unchanged from the pre-restore
     baseline). Then assert an explicit `(workspace-restore "alpha")`
     materializes a tab named `alpha`.
   - Reframe the "Restart restores working-state, not saved-state, when
     both present" scenario (if present here) to drive through
     `(workspace-restore "code")` rather than selecting a startup tab —
     matching the delta spec.

10. **Tests — persistence-v3-spec.el**: remove the
    "the :restore-pending runtime tag is also filtered" spec (lines ~251)
    — the tag no longer exists. (The sibling `:broken`-filtered assertion,
    if present, stays.)

11. **Tests — tabs-spec.el**: remove the
    "workspace--persistence-after-tab-switch no-ops on non-workspace tabs"
    spec (and the file-header comment referencing that `:after` advice) —
    the function and advice are gone.

12. **Add new coverage** (persistence-spec.el): a spec asserting startup
    hydration creates no tabs — write a v3 file with two healthy
    workspaces, baseline the tab count, `workspace--restore`, then expect
    both in the registry, tab count unchanged, and both present in
    `(workspace--registered-names)` (the `workspace-restore` candidate
    list).

## Design rationale

The lazy-on-first-selection layout path is the only unreliable restore
path; the two explicit paths (`workspace-restore`, `workspace-switch-
layout`) apply layout synchronously and work. Dropping startup tab
creation removes the tab-explosion AND dissolves the lazy-restore bug
(no startup tabs left to fail). Full removal (not dormant) is the explicit
decision — orphan helpers and a `:restore-pending` keyword nothing sets
are exactly the misleading machinery being cleared. The whole registry is
re-serialized on every flush, so unmaterialized workspaces round-trip
losslessly; hydrating-without-tabs risks no persisted data.

## Verification

- `./bin/tangle-org.sh config/workspaces/persistence.org` and
  `… data-model.org` both validate.
- `grep -rn 'restore-pending\|restore-tabs\|activate-pending\|persistence-after-tab-switch' config/workspaces/persistence.org config/workspaces/data-model.org`
  returns nothing.
- `./bin/run-tests.sh -d config/workspaces` is fully green.
- Production state file is untouched by the run (md5 unchanged) — the
  test-isolation fix already on the branch guarantees this.

## Context

design.md § Decisions D1 (drop restore-tabs) and D2 (remove lazy machinery
in full); specs/workspaces/spec.md MODIFIED "Per-machine persistence and
restoration".
