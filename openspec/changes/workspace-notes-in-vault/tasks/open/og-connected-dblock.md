---
name: og-connected-dblock
description: Add the org-graph-connected dynamic block that renders a note's incoming typed neighbourhood grouped by note type with branch lineage.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:og-menu-extra-groups
  - enables:int-workspaces-org-graph
---

## Files to modify
- config/org-graph/dblock.org (new; tangle to dblock.el)
- config/org-graph/org-graph.org (modify; tangle — loader slot after `query.el`)
- config/org-graph/test/dblock-spec.el (new)
- config/org-graph/test/module-load-spec.el (modify — order now includes `dblock`)

## Implementation steps
1. Create `dblock.org` with the standard header (`#+property: header-args:emacs-lisp :tangle dblock.el`,
   `:comments no` on the lexical-binding block). Define
   `org-dblock-write:org-graph-connected (params)`:
   - `:id` param, default: the file-level node's ID (`org-id-get` at
     `point-min` via `save-excursion`); `:types` optional list of relation
     symbols (default: all).
   - Rows: `org-graph-query/incoming id` (filtered by `:types` when given).
     Resolve each `from-id` with `vulpea-db-get-by-id`; skip unresolved.
   - Group: for each note, the first type in `org-graph-schemas--note-types`
     order whose schema predicate (`org-graph/note-of-type-p`) matches;
     `untyped` last. Within a group sort by title.
   - Lineage: for a note with outgoing `branched-from` edges, render it
     indented under its parent when the parent is in the same block; a note
     whose parent is absent renders at top level. Keep it to one pass over
     the rows (build parent → children map).
   - Emit plain org: `- <Type>` heading lines? No — emit a description list
     per group: `- topic ::` then `  - [[id:X][Title]]` items, lineage as
     deeper indentation. Deterministic ordering so regeneration is a no-op
     when nothing changed.
2. Register a `#+BEGIN: org-graph-connected` skeleton snippet? Not needed;
   `org-graph/create-project-note` emits the block.
3. Loader: add `dblock.el` after `query.el` (dblock reads query + schemas).
4. Specs: stub `org-graph-query/incoming` and `vulpea-db-get-by-id` (as
   `query-spec.el` / `finders-spec.el` do): grouping puts a `topic` and a
   `session` (predicate on a stub tag) in separate groups; `branched-from`
   child renders indented under its parent; updating twice yields identical
   buffer text; unknown `:id` renders an empty block without error.

## Design rationale
The "virtual home" is the project note itself: an org dynamic block that
regenerates the connected-notes listing on demand replaces the
hand-curated session headings the old activities notes had (write-only
anchors that drifted). Rendering into the note keeps the listing greppable
in the vault, needs no new mode, and lets the home layout stay "open the
project note". Grouping by schema predicate reuses the taxonomy; branch
lineage comes from `branched-from` edges so the session tree is visible
without reading YAML sidecars.

## Design pattern
Standard `org-dblock-write:` function convention (see org's
`org-dblock-write:clocktable`); query stubbing as in
`config/org-graph/test/query-spec.el`. Module header/loader slot pattern
as in `authoring.org` / `menu.org` additions.

## Verification
- `./bin/tangle-org.sh config/org-graph/dblock.org` and `org-graph.org` validate.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -n 'dblock.el' config/org-graph/org-graph.el` → loader slot present.

## Context
design.md § D6 (Dynamic block bullet); § Risks (regeneration edits the note)
specs/org-graph/spec.md § 'Connected-notes dynamic block'
