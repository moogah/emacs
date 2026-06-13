---
name: restore-home-builder-fallback
description: workspace-restore opens home.org for a workspace with no saved layout instead of a bare tab
change: workspaces-startup-no-auto-tab-restore
status: ready
relations:
  - "blocked-by:registry-hydrate-only-startup"
---

## Files to modify

- config/workspaces/persistence.org (modify) → tangle to persistence.el
- config/workspaces/test/persistence-spec.el (modify)

## Implementation steps

1. **persistence.org — `workspace-restore`** (the "Explicit save / restore
   commands" subtree): in the "no live tab" branch, after
   `(tab-bar-new-tab)` + `(tab-bar-rename-tab name)`, decide whether a
   saved layout exists before applying:
   - Compute the effective layout the same way `workspace--apply-saved-
     layout` does: recent group → `workspace--group-recent-layout` →
     `workspace--layout-effective-state`.
   - **If non-nil** (saved layout present) → call
     `(workspace--apply-saved-layout name)` (unchanged behavior).
   - **If nil** (no saved layout — `:layout-groups` empty/nil) → call
     `(funcall workspace-home-builder name)` instead, the same call shape
     `workspace-new` uses, so the tab opens `<home>/home.org`. Guard with
     `(and (boundp 'workspace-home-builder) (functionp workspace-home-builder))`
     to mirror `workspace--new-default-path`.
   - Do NOT attempt `window-state-put` in the no-layout case.
   - The existing-tab switch branch and the broken-state `user-error`
     guard are unchanged.
   - Update the `workspace-restore` docstring to describe the no-layout
     fallback.

2. **Tangle**: `./bin/tangle-org.sh config/workspaces/persistence.org`.

3. **Tests — persistence-spec.el**: add a spec mirroring the delta
   scenario "Restore a workspace with no saved layout opens home.org":
   - Register a workspace (via `workspace-new`, real scaffold, under the
     spec's temp parent) so `<home>/home.org` exists, then clear its
     layout groups (set `:layout-groups` nil) to simulate a layout-less
     recovered workspace; remove its live tab so `workspace-restore` takes
     the create branch.
   - Spy on `workspace-home-builder` (or assert the current buffer is the
     workspace's `home.org` after restore), and assert `window-state-put`
     is NOT called (e.g. `spy-on 'window-state-put`).
   - Add/confirm a companion assertion that a workspace WITH a saved
     layout still goes through `workspace--apply-saved-layout` (not the
     home-builder) — guards against the branch inverting.

## Design rationale

A workspace recovered from its on-disk directory alone (or never
explicitly saved) has no `:saved-state`/`:working-state`, so the prior
behavior left a bare tab. Running the home-builder opens `home.org`,
matching `workspace-new` and giving a useful landing surface. The builder
is NOT run when a layout exists, because layouts already encode their own
windows/buffers and running the builder first would flash an extra
`home.org` window and fight `window-state-put`.

## Verification

- `./bin/tangle-org.sh config/workspaces/persistence.org` validates.
- `./bin/run-tests.sh -d config/workspaces` is fully green.
- Manual sanity (optional): with a registry entry whose `:layout-groups`
  is nil, `M-x workspace-restore` opens `<home>/home.org` in a fresh tab.

## Context

design.md § Decisions D3 (workspace-restore home-builder fallback);
specs/workspaces/spec.md MODIFIED "Explicit restore command", scenario
"Restore a workspace with no saved layout opens home.org".

## Observations

- The saved-layout presence test was implemented as the design specified:
  recent group → `workspace--find-group` → `workspace--group-recent-layout`
  → `workspace--layout-effective-state`, and gates on that effective state
  being non-nil. This is the exact derivation `workspace--apply-saved-layout`
  uses internally, so "has a layout" cannot diverge from the
  restore-precedence invariant (no fork of `:saved-state`-only checking).
- The broken-state `user-error` guard remains at the top of the function
  body (after registry lookup), strictly before the create/switch dispatch
  and therefore before any tab creation OR home-builder invocation. A broken
  workspace can never reach the home builder. Verified by the existing
  broken-restore specs staying green.
- `window-state-put` is NOT called directly by the layout-applying path —
  it is deferred via `run-at-time` inside `workspace--restore-frameset`
  (layouts.el). A synchronous spy on `window-state-put` therefore can never
  observe a call within the test's dynamic extent regardless of branch. To
  make the "no layout → no window-state-put attempted" assertion meaningful
  I additionally spy on `workspace--restore-frameset` (the synchronous choke
  point that *schedules* the deferred put) and assert it is not called; the
  literal `window-state-put` spy from the task body is kept as a belt-and-
  suspenders check. The companion "layout exists" spec asserts
  `workspace--apply-saved-layout` IS called (synchronous, reliable) rather
  than relying on the deferred put, guarding against branch inversion.
- No `workspace--set-layout-groups` setter exists in data-model.el; the
  test sets `:layout-groups nil` via `plist-put` on a `copy-sequence` of the
  workspace plist (mirroring the spec's `:layout-groups is nil` precondition)
  and clears the recent-group pointer via the existing
  `workspace--set-recent-group`.
- `workspace--tab-index-for` returns a 1-based index (per its docstring), so
  the test closes the materialized tab with `(tab-bar-close-tab idx)` (not
  `(1+ idx)`).

## Discoveries

- discovery_id: disc-restore-home-builder-fallback-1
  class: invariant-gap
  description: |
    The cited register entry register/invariant/restore-precedence-working-
    over-saved is fully upheld: the new saved-layout-presence check reuses
    workspace--layout-effective-state, so it cannot diverge from the
    precedence rule. No invariant gap found — the natural presence-check did
    NOT diverge, so no push-back was warranted on that axis.
  affected_register_entry: register/invariant/restore-precedence-working-over-saved
  recommendation: |
    Confirm-no-drift. No reconciliation needed.
- discovery_id: disc-restore-home-builder-fallback-2
  class: spec-signal
  description: |
    The spec scenario "Restore a workspace with no saved layout opens
    home.org" asserts "no window-state-put is attempted". Because
    workspace--restore-frameset defers the actual window-state-put via
    run-at-time, the user-visible contract "no layout is applied" is more
    precisely "workspace--restore-frameset is never reached" — the deferred
    put can never fire if its scheduler is never called. The spec wording is
    fine for a behavioral contract, but the test asserts at the
    workspace--restore-frameset boundary (the synchronous gate) to be robust;
    a future reader should not expect a synchronous window-state-put call to
    exist on either branch.
  affected_register_entry: register/boundary/home-org-read-pipeline
  recommendation: |
    No change required. If the register ever documents the restore-apply
    path's deferral, note that window-state-put is asynchronous and the
    synchronous observable is workspace--restore-frameset invocation.
