---
name: int-workspaces-org-graph
description: Create the project note at workspace birth and record its ID, mark it done on purge, install display-name and home-builder from the note, register the session schema, add the adopt-project-note command.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-scaffold-and-lint
  - blocked-by:ws-inversion-points
  - blocked-by:ws-scaffold-and-anchor
  - blocked-by:og-edge-writer-and-project-note
  - blocked-by:og-connected-dblock
  - enables:int-workspace-commands
  - enables:int-migration-command
---

## Files to modify
- config/integrations/workspaces-org-graph.org (modify; tangle)
- config/integrations/test/workspaces-org-graph-spec.el (new)

## Implementation steps
1. Register integration `org-graph-project` with `workspace-register-integration`
   **before** any other integration in this layer (registration order is
   dispatch order; the gptel birth session must observe `:node-id`).
   - `:on-create (payload)`: if `(plist-get payload :node-id)` resolves via
     `vulpea-db-get-by-id`, return `skipped`; else
     `(org-graph/create-project-note (plist-get payload :name))` and
     `(workspace-set-node-id (gethash name workspace--registry) id)`; return `ok`.
     Non-interactive; errors propagate to the dispatcher's guard.
   - `:on-purge (payload)`: if `:node-id` resolves, set the note's `status`
     field to `done` through the same mechanism `create-project-note` used
     (vulpea meta or property) under the coordinator lock; never delete the
     note.
2. Install `workspace-display-name-function` → function that returns
   `(vulpea-note-title (vulpea-db-get-by-id node-id))` when `:node-id` is
   non-nil and resolves, else nil (workspaces falls back to the name).
3. Install `workspace-home-builder` → function that resolves the note's path
   (`vulpea-note-path`), `find-file`s it in a single window, and if the
   buffer is unmodified runs `org-dblock-update-all` (or updates the first
   `org-graph-connected` block) and saves; if the note cannot be resolved,
   call `workspace-default-home-builder`.
4. Register the `session` schema:
   `(vulpea-schema-define 'org-graph-session :predicate (org-graph-schemas--tag-predicate "gptel_session") :fields nil)`
   and add `session` to `org-graph-note-types` (or whatever the taxonomy
   list is) so finders and the dblock grouping know it. Do this inside the
   guard, after org-graph's own schema registration.
5. `workspace-adopt-project-note`: interactive; `vulpea-find`-style
   selection restricted to `project` notes; then prompts for a directory
   (`read-directory-name`) and calls `workspace-new` with that directory and
   the note's ID (prefix-arg anchor semantics). Register it as a `:menu`
   entry too (`a`).
6. Specs (stub vulpea DB, temp registry): on-create creates a note and sets
   node-id; on-create with a resolvable node-id is `skipped` and creates
   nothing; on-purge sets status done and leaves the file; display-name
   returns the title or nil; home builder opens the note and regenerates
   only when unmodified; `session` schema present after load; adopt calls
   `workspace-new` with the chosen ID.

## Design rationale
The project note is the workspace's canonical identity; it must exist from
birth and survive purge (notes outlive worktrees — that was the whole
point). Display name and home view are read from the note through
workspaces' inversion points so workspaces never reads a file. The
`session` schema lives here because its predicate names gptel's filetag;
org-graph must not know gptel. Adopt-project-note is the cross-machine
path: the vault syncs, the registry does not.

## Design pattern
Integration registration shape from the archived
`config/org-graph/workspace-integration.org` (handlers return
`ok`/`skipped`); vulpea access via `vulpea-db-get-by-id` /
`vulpea-note-title` / `vulpea-note-path` as in `discovery.org`'s seed
function.

## Verification
- `./bin/tangle-org.sh config/integrations/workspaces-org-graph.org` validates.
- `./bin/run-tests.sh -d config/integrations` — green.
- `grep -n 'workspace-register-integration\|workspace-display-name-function\|workspace-home-builder\|org-graph-session' config/integrations/workspaces-org-graph.el` → all present.

## Context
design.md § D8 (workspaces-org-graph.org), § D3, § Risks (dynamic-block regeneration)
specs/workspace-graph-integration/spec.md § 'Project note as workspace identity', 'Display name and home layout come from the project note', 'Virtual home via the connected-notes block'
