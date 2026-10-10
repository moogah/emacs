---
name: ws-public-api
description: "Public name-addressed workspaces API (list/get/layouts/on-screen); migrate org-graph off the internal registry"
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "enables:ws-programmatic-add-layout"
---

## Files to modify
- config/workspaces/workspaces.org (modify) — public accessors section
- config/workspaces/test/public-api-spec.el (new)
- config/org-graph/discovery.org (modify) — use the public accessor instead of `workspace--registry`

## Implementation steps
1. Write specs first (`public-api-spec.el`) against a synthetic `workspace--registry`
   (follow the fixtures in `config/workspaces/test/layouts-spec.el`):
   - `workspace-names` returns all registered names;
   - `workspace-get NAME` returns the workspace plist or nil;
   - `workspace-layouts NAME` returns a list of `(:name N :origin O :description D :source S)`
     for each layout-group (origin defaults to `user` until ws-layout-metadata lands —
     read via `workspace--layout-origin` if fbound, else `user`);
   - `workspace-layout-on-screen-p WS LAYOUT` is t only when LAYOUT is WS's
     `:recent-layout-group` AND WS has an open tab (`workspace--tab-index-for` over
     *all* frames — extend lookup beyond the selected frame);
   - none of the functions change frames, tabs, windows, buffers, or call
     `workspace--flush-state` (spy on it).
2. Implement the four functions in `workspaces.org` as documented public API
   (no `--` in names), each a thin read over the registry.
3. Replace org-graph's direct `(gethash name workspace--registry)` in
   `discovery.org` (~line 119–126) with `workspace-get` / `workspace-names`, keeping
   its soft-dependency guard (`fboundp`).
4. Tangle both files; run both test dirs.

## Design rationale
Agents must address workspaces by name; today every accessor derives "which
workspace" from the selected frame's current tab and all are internal
(`data-model.org` says none is public API). org-graph already reaches into
`workspace--registry`; formalizing the read API fixes that coupling too. The
on-screen predicate is what the R3 protection rule needs: a layout is on screen when
it is the recent layout of a workspace with an open tab, because tab-bar restores
that tab's own window configuration and an autosave would bury a replacement.

## Verification
- ./bin/tangle-org.sh config/workspaces/workspaces.org
- ./bin/tangle-org.sh config/org-graph/discovery.org
- ./bin/run-tests.sh -d config/workspaces
- ./bin/run-tests.sh -d config/org-graph
- `grep -n 'workspace--registry' config/org-graph/discovery.org` shows only the `defvar` forward declaration, if anything

## Context
- design.md § Decisions 'D16. Workspaces improvements this change depends on' (W1)
- specs/workspaces/spec.md § 'Public name-addressed workspace API'
