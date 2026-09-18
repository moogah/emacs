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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/invariant/org-graph-loader-ordered-sequence`: speculated → reconciled. The loader is the ten-module contiguous block org-graph.el:104-113 (schemas, extractor, coordinator, query, finders, authoring, edge-type, tools, discovery, menu) and the enforcement is now a structural audit of the TANGLED loader, module-load-spec.el:127-143 (`module-load-spec--canonical-order`, count 10, line span 9). **Implication**: step 3's `dblock.el` slot after `query.el` must also update `module-load-spec--canonical-order`, the `(expect (length numbers) :to-equal 10)` and the span `9` → eleven modules / span 10, or the audit fails. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-org-graph-loader-ordered-sequence.md`.
- `register/boundary/project-note-creation`: speculated → reconciled. `org-graph/create-project-note` (authoring.el:192) stamps the literal `org-graph-authoring--connected-block` = `#+BEGIN: org-graph-connected\n#+END:` (authoring.el:185-189) — no params. **Implication**: step 1's `:id` default (file-level node at point-min) is the path every stamped block will take; step 2 holds as written. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-project-note-creation.md`.
- `register/invariant/edge-drawer-immediately-after-property-drawer`: speculated → DIVERGENT (ask-cycle-1789334430-8): an EDGES drawer at the pinned spot is parsed by `vulpea-db--extract-meta` as the note's META. The dblock reads rows from the typed-edge index (`org-graph-query/incoming`), which the divergence does not affect for session notes (no META today); keep the dblock index-only — do not read `vulpea-note-meta` for grouping. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md`.

### Meta-discoveries
- `vulpea-api-verify-against-source`: every speculated vulpea claim this cycle was wrong until read against `runtime/straight/repos/vulpea`; verify `vulpea-db-get-by-id`, `vulpea-note-title` / `-path` / `-tags` signatures there before writing step 1's resolver, not from memory.
- `stub-bounded-regression`: step 4 stubs the query and the DB; add ONE un-stubbed end-to-end scenario (temp vault, real `vulpea-db-sync`, a real `org-graph/write-edge part-of` from a session note, then `org-dblock-update` on the project note lists it) so the stubbed shapes are pinned to the real index.
