---
name: int-workspace-commands
description: Add find-in-workspace (edge-derived note set with path expansion for ID'd headings) and new-in-workspace, and contribute the Workspace group to the graph menu.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-workspaces-org-graph
  - blocked-by:og-menu-extra-groups
  - enables:configure-migrate-and-verify
  - blocked-by:disposition-edge-drawer-meta-and-placement
---

## Files to modify
- config/integrations/workspaces-org-graph.org (modify; tangle — same submodule)
- config/integrations/test/workspaces-org-graph-spec.el (modify)

## Implementation steps
1. `jf-integrations--current-node-id ()`: current tab's workspace →
   `workspace-node-id`; `user-error "No workspace owns this tab"` when nil.
2. `jf-integrations--workspace-note-ids (node-id)`: rows from
   `(org-graph-query/incoming node-id 'part-of)` → set of from-ids; resolve
   each to its path (`vulpea-db-get-by-id` → `vulpea-note-path`); expand: the
   result set contains every note whose `vulpea-note-path` is in that path
   set (`vulpea-db-query` with a path predicate, or query all notes once and
   filter). This admits ID'd headings inside linked sessions, which inherit
   the file's `part-of` via path membership rather than needing their own
   edge (design OQ-B accepts the over-inclusion).
3. `org-graph/find-in-workspace` (name it under the integration prefix but
   keep the user-facing command name as in the spec): `vulpea-find
   :filter-fn (lambda (n) (member (vulpea-note-id n) ids)) :require-match t`.
   Compute `ids` once per invocation.
4. `org-graph/new-in-workspace`: call `org-graph/find-or-create` (authoring.org)
   with the prompted title; after creation, `org-graph/write-edge` on the
   resulting note's file with `part-of` → node-id. If find-or-create
   returned an existing note, still write the edge (idempotent).
5. Contribute the group: push `("Workspace" . (("f" "Find in workspace" org-graph/find-in-workspace) ("n" "New in workspace" org-graph/new-in-workspace) ("a" "Adopt project note" workspace-adopt-project-note)))`
   onto `org-graph-menu-extra-groups` inside the guard (idempotent — check
   before pushing). Also register `f`/`n` as workspace integration `:menu`
   entries so they appear in the workspaces Integrations menu too.
6. Specs: with stubbed incoming rows and stub notes (a file note A, a
   heading note H sharing A's path, an unrelated C), the id set is {A, H};
   find passes a filter accepting A and H and rejecting C (invoke the
   filter-fn captured via a spy on `vulpea-find`); new-in-workspace writes
   the edge with the created note's file; both commands `user-error` off a
   workspace tab; the extra-groups alist contains the Workspace group once
   after loading twice.

## Design rationale
This is the co-located workflow without the co-location: one keystroke to
see only this project's notes, one to start a note already linked to the
project. Association comes from the edge index alone — no per-workspace
filetag (org tags cannot carry hyphens, and tags break on rename). Path
expansion is what makes an ID'd topic heading inside a chat show up in the
project finder, which was the user's stated reason for indexing sessions.

## Design pattern
`vulpea-find :filter-fn` usage as in `config/org-graph/finders.org`;
menu contribution via `org-graph-menu-extra-groups` (og-menu-extra-groups);
workspace `:menu` registration shape from integrations.org.

## Verification
- `./bin/tangle-org.sh config/integrations/workspaces-org-graph.org` validates.
- `./bin/run-tests.sh -d config/integrations` — green.
- `grep -n 'find-in-workspace\|new-in-workspace\|org-graph-menu-extra-groups' config/integrations/workspaces-org-graph.el` → present.

## Context
design.md § D8 (find-in-workspace, new-in-workspace), § Open Questions OQ-B
specs/workspace-graph-integration/spec.md § 'Workspace-scoped note commands'
specs/org-graph-menu/spec.md § 'Graph Menu Prefix' (Workspace group)

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/invariant/edge-drawer-immediately-after-property-drawer`: speculated → DIVERGENT (ask-cycle-1789334430-8). `vulpea-db--extract-meta` (vulpea-db-extract.el:807-840) takes the first description list before the first headline and descends into drawers, so the EDGES drawer `write-edge` places right after the property drawer SHADOWS any META the note carries. Step 4's `new-in-workspace` writes onto arbitrary vault notes that may carry META → BLOCKED (`blocked-by:disposition-edge-drawer-meta-and-placement`). If option (a)+dedupe is chosen, integrate will re-point this task's blocked-by to `og-edge-writer-meta-safe-and-dedupe`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md`.
- `register/invariant/write-edge-idempotent`: speculated → DIVERGENT (ask-cycle-1789334430-13). Dedupe looks only at the drawer on the line after `:END:`; a hand-authored EDGES drawer elsewhere yields a second drawer and duplicate tuples. Step 4's "still write the edge (idempotent)" holds only for writer-created drawers until this is decided. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-write-edge-idempotent.md`.
- `register/boundary/edge-writer-api`: speculated → reconciled. `(org-graph/write-edge FILE TYPE TARGET &optional NODE-ID)` returns the item text or nil on the no-op and REFUSES an ID-less node (`user-error`). **Implication**: `org-graph/find-or-create` must yield an ID'd note before step 4's write; treat a nil return as "already linked". See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-edge-writer-api.md`.
- `register/shape/workspace-plist-v3`: reconciled — read the id with `workspace-node-id` (data-model.el:178); `workspace--current-name` (tabs.el:47) is the tab→name resolver step 1 leans on. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-plist-v3.md`.
- `register/vocabulary/relation-types`: confirmed — `'part-of` is normalised by the writer; the query side (`org-graph-query/incoming node-id 'part-of`) takes the same symbol. See `.orchestrator/cycles/cycle-1789334430/reconciliations/vocabulary-relation-types.md`.

### Meta-discoveries
- `vulpea-api-verify-against-source`: read `vulpea-find` (`:filter-fn`, `:require-match`) and `vulpea-db-query` in `runtime/straight/repos/vulpea` before writing steps 2-3; the path-expansion query shape is a guess until checked.
- `stub-bounded-regression`: step 6 is fully stubbed; add ONE un-stubbed scenario (temp vault with a file note, an ID'd heading in a linked session file, an unrelated note; real index) asserting the id set is {A, H}.
