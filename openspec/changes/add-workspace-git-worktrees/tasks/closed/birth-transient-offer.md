---
name: birth-transient-offer
description: workspace-new pops the workspace-menu transient after creation (fresh/anchored-scaffolded only)
change: add-workspace-git-worktrees
status: ready
---

## Files to modify
- config/workspaces/workspaces.org (modify) — the creation flow, and tangled workspaces.el
- config/workspaces/test/transient-menu-spec.el (modify) OR a new
  config/workspaces/test/birth-offer-spec.el

## Implementation steps
1. In the workspace creation flow (the fresh-scaffold path and the
   anchored-scaffolded path), AFTER `workspace--dispatch-create-integrations`
   has run and the new workspace's tab is selected (it is already current at
   this point), invoke the front-door transient: `(workspace-menu)`.
   - Gate strictly on context: pop the transient ONLY for `fresh` and
     `anchored-scaffolded`. For `anchored-existing` (adopted) do NOT pop it —
     never auto-open a menu against a directory the user already owned.
   - The new workspace is current, so the transient's Integrations group builds
     its `:menu` payload against it. `workspace-new` names NO integration — it
     just raises the generic menu, preserving directionality.
2. Guard the call so it is a no-op in non-interactive/batch contexts (tests):
   e.g. only call when `(not noninteractive)`, or factor the pop into a small
   helper `workspace--offer-birth-menu` that tests can spy/stub. Prefer the
   helper — it makes the behavior unit-testable without launching a real
   transient.
3. Tangle + validate: `./bin/tangle-org.sh config/workspaces/workspaces.org`.
4. Add specs: spy the birth-offer helper (or `workspace-menu`) and assert it is
   called once for a fresh creation and once for anchored-scaffolded, and NOT
   called for anchored-existing; assert declining/dismissing leaves a valid
   registered workspace with a live tab.

## Design rationale
The interactive add-worktree command already exists as the integration `:menu`
entry, so "offer worktrees at birth" is simply "show the menu". Reusing the
transient avoids a second interactive surface, naturally supports adding
several repos (the transient stays open across invocations), and is consistent
with the `C-x w` menu the user already knows. Adopted workspaces are skipped
because we must never inject artifacts into a directory the user already owned
(mirrors the gptel on-create `anchored-existing` skip).

## Design pattern
See config/workspaces/workspaces-transient.org for `workspace-menu` and its
registry-driven Integrations group. See
config/gptel/sessions/workspace-integration.org for the established
`anchored-existing` skip pattern.

## Verification
- `./bin/tangle-org.sh config/workspaces/workspaces.org` validates
- `./bin/run-tests.sh -d config/workspaces` — birth-offer specs pass
- Acceptance (spec scenarios): birth offers registered menu commands for
  fresh/anchored-scaffolded; declining leaves a valid empty workspace; adopted
  workspaces get no birth offer.

## Context
design.md § Decisions 'D4 — Birth reuses the existing transient, not a new surface'
specs/workspaces/spec.md (ADDED Birth-time integration offer)
