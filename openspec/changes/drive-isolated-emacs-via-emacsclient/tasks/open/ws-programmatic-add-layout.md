---
name: ws-programmatic-add-layout
description: "workspace-add-layout / workspace-remove-layout: change a named workspace's layouts without switching, with R1–R3 protection rules"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:ws-public-api"
  - "blocked-by:ws-layout-metadata"
  - "blocked-by:ws-persistence-v4"
  - "enables:ws-agent-tool-add-layout"
---

## Files to modify
- config/workspaces/layouts.org (modify) — workspace-add-layout
- config/workspaces/test/add-layout-spec.el (new)

## Implementation steps
1. Specs first:
   - adding layout N with state S to workspace W stores S as `:saved-state`,
     `:working-state` nil, metadata as given; W's `:recent-layout-group` unchanged;
     selected frame / current tab / window configuration unchanged; flush called once;
   - works for a workspace with no open tab;
   - refuses (user-error naming the conflict, no state change, no flush) when N
     exists with origin `user` (R2);
   - refuses when N exists with origin `agent` and `workspace-layout-on-screen-p` (R3);
   - replaces an off-screen agent layout, discarding its `:working-state`;
   - after adding, a user rearrangement + autosave + `workspace-revert` restores S (R1);
   - `workspace-remove-layout` removes an off-screen agent layout (frame/tab/windows
     unchanged, flush once); refuses user-origin, on-screen, and `home`; reassigns
     the recent pointer like `workspace-delete-layout` when it named the removed one.
2. Implement `workspace-add-layout (ws-name layout-name state &rest metadata)` using
   `workspace--upsert-group`, `workspace--layout-make`, the metadata helper, and
   `workspace--flush-state`. Do NOT call `workspace--set-recent-group`.
   Implement `workspace-remove-layout (ws-name layout-name)` over the same pure
   helpers `workspace-delete-layout` uses, plus the origin/on-screen/home checks.
3. Tangle; run tests.

## Design rationale
This is the core MVP operation for demonstrations. Autosave is kept as is (the user
relies on tab-switch autosave); programmatic writes avoid it by never switching tabs,
and the R1–R3 rules define how they coexist with the user's working state. Refusing
the on-screen case is the MVP choice; versioned names or an "updated" flag are
deferred with W7.

## Verification
- ./bin/tangle-org.sh config/workspaces/layouts.org
- ./bin/run-tests.sh -d config/workspaces

## Context
- design.md § Decisions 'D16. ...' (W2, W4 R1–R3)
- specs/workspaces/spec.md § 'Programmatic layout addition'
- specs/workspaces/spec.md § 'Protection of user layouts from programmatic writes'
- specs/workspaces/spec.md § 'Programmatic layout removal'
