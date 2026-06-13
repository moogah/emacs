---
name: registry-hydrate-only-startup
description: Startup hydrates the registry without creating tabs; remove the lazy-restore machinery in full
change: workspaces-startup-no-auto-tab-restore
status: done
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

## Observations

- Implemented exactly as the task body prescribed. No departures from the
  step list.
- Net spec delta: baseline 333 -> 332. Removed two specs that encoded
  now-dead behavior (`tabs-spec.el`'s "after-tab-switch no-ops on
  non-workspace tabs" — the function and `:after` advice are gone;
  `persistence-v3-spec.el`'s ":restore-pending runtime tag is also
  filtered" — the tag no longer exists). Reworked one in place
  ("Workspaces survive restart": now asserts hydrate-only + no startup
  tabs + explicit `workspace-restore` materialization). Added one
  ("startup hydration creates no tabs for healthy workspaces"). Net = -2 +1.
- The two deleted specs are NOT a behavior-coverage loss: the no-startup-
  tabs invariant they implicitly leaned on is now asserted directly and
  more strongly by the reworked + new persistence specs, and the
  `:broken` serializer-omission contract (the live sibling of the deleted
  `:restore-pending` byte-filter spec) stays green in `persistence-v3-spec.el`.
- Wider blast radius was smaller than the design feared. Of the named
  at-risk specs, only `tabs-spec.el` and `persistence-v3-spec.el`
  referenced the removed symbols. `broken-home-load-spec`,
  `broken-home-runtime-spec`, `save-restore-spec`, `buffer-reincarnation-spec`,
  `workspace-delete-purge-spec`, and `data-model-spec` already drove through
  `workspace--restore` (registry-only assertions) or `workspace-restore`
  (explicit materialization) and required no edit — they were already
  aligned with hydrate-only restore. `save-restore-spec`'s
  `tab-bar-select-tab 1` calls are plain navigation, not lazy-activation
  triggers, so they were unaffected by removing the `:after` advice.
- The `:before` autosave advice (`workspace--persistence-before-tab-switch`)
  on BOTH `tab-bar-select-tab` and `tab-bar-switch-to-tab` is intact and
  still fires; verified by grep against the tangled `persistence.el`.

## Discoveries
- discovery_id: disc-registry-hydrate-only-startup-1
  class: dead-branch
  description: |
    Retired the :restore-pending runtime tag entirely. The persistence
    loader no longer marks deserialized workspaces restore-pending, the
    tab-switch :after activation hook (workspace--persistence-after-tab-switch)
    and its handler (workspace--activate-pending-workspace) are deleted, and
    the three data-model helpers (workspace--restore-pending-p /
    --mark-restore-pending / --clear-restore-pending) are removed.
    workspace--apply-saved-layout no longer clears the flag (it now only
    applies the effective layout). This is the vocabulary that
    register/vocabulary/workspace-state-slot enumerates as a runtime tag.
  affected_register_entry: register/vocabulary/workspace-state-slot
  recommendation: |
    At integrate, reconcile workspace-state-slot to drop :restore-pending
    from its runtime-tag vocabulary. The persisted-slot closed set
    (:saved-state, :working-state) is unchanged. The :broken runtime tag
    stays. The candidate invariant
    register/invariant/runtime-tags-routed-through-helpers (mentioned in the
    deleted data-model prose) now has a closed set of one (:broken) — note
    that if the invariant is ever promoted.
- discovery_id: disc-registry-hydrate-only-startup-2
  class: dead-branch
  description: |
    :restore-pending was a runtime-only tag stripped by the serializer
    whitelist (:name :home :recent-layout-group :buffer-files :layout-groups).
    Removing it did NOT alter the persisted-slot whitelist (unchanged) nor
    the :broken runtime tag (retained, still stripped). The serializer
    docstring was updated to state the whitelist explicitly and to drop the
    :restore-pending mention. The byte-equivalence / :broken-omission spec
    in persistence-v3-spec.el still pins the whitelist behavior.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    At integrate, reconcile workspace-plist-v3 to drop :restore-pending from
    the runtime-tag list. No change to the persisted-slot whitelist.
- discovery_id: disc-registry-hydrate-only-startup-3
  class: interface-drift
  description: |
    CONFIRMED (no drift): the autosave-guard-pipeline boundary note matched
    the code exactly. workspace--persistence-after-tab-switch was the
    lazy-restore path (deleted, with its two :after advice forms on
    tab-bar-switch-to-tab / tab-bar-select-tab). The autosave stage-1 wrap
    surface — workspace--persistence-before-tab-switch (:before advice on
    BOTH tab-bar-select-tab and tab-bar-switch-to-tab) plus
    workspace--kill-emacs-flush and workspaces-mode--idle-tick — is fully
    untouched. The :before advice still fires on both tab-switch entry
    points (verified via grep against tangled persistence.el; idle-tick and
    kill-emacs specs green). The effective-state derivation in
    workspace--apply-saved-layout (working-over-saved precedence) was not
    altered — only the trailing :restore-pending-clearing block was removed;
    the data-model precedence cases stay green.
  affected_register_entry: register/boundary/autosave-guard-pipeline
  recommendation: |
    No reconciliation needed for the boundary itself — the note was accurate.
    The :after lazy-restore path it distinguished from the :before autosave
    path simply no longer exists; the boundary's autosave surfaces are
    unchanged.
