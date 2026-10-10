---
name: ws-layout-metadata
description: "Layout metadata in :etc (origin/description/source); explicit saves take ownership"
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "enables:ws-persistence-v4"
  - "enables:ws-programmatic-add-layout"
---

## Files to modify
- config/workspaces/data-model.org (modify) — metadata accessors
- config/workspaces/layouts.org (modify) — workspace-save-layout sets origin user
- config/workspaces/persistence.org (modify) — workspace-save sets origin user
- config/workspaces/test/layout-metadata-spec.el (new)

## Implementation steps
1. Specs first:
   - `workspace--layout-origin` returns `user` for a layout whose `:etc` lacks `:origin`;
   - `workspace--layout-with-metadata LAYOUT &key origin description source` returns a
     new layout (non-destructive, like `workspace--upsert-group`) with those keys in `:etc`;
   - after `workspace-save` with an agent-origin recent layout, origin is `user`;
   - same for `workspace-save-layout`.
2. Implement accessors in `data-model.org` over the reserved `:etc` slot (keep it an
   alist/plist consistent with `workspace--layout-make`).
3. Make both explicit-save paths set origin `user` on the layout they write (inside
   the single save funnel `workspace--autosave-current-layout` when SLOT is
   `:saved-state`, so every explicit save path is covered once).
4. Tangle; run tests.

## Design rationale
Agent-made layouts must be distinguishable from the user's so programmatic writes
can never clobber user work (R2) and so demo layouts can be listed and cleaned up.
R4: an explicit save over an agent layout transfers ownership to the user, after
which R2 protects it. Layouts loaded from v3 files have no metadata and must count
as the user's.

## Verification
- ./bin/tangle-org.sh config/workspaces/data-model.org
- ./bin/tangle-org.sh config/workspaces/layouts.org
- ./bin/tangle-org.sh config/workspaces/persistence.org
- ./bin/run-tests.sh -d config/workspaces

## Context
- design.md § Decisions 'D16. ...' (W4 R4, W5)
- specs/workspaces/spec.md § 'Layout metadata'
