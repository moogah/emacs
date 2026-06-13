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
