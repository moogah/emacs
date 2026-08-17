---
name: extractor-union
description: Wire both pure scanners (drawer + inline) into the vulpea extractor wrapper, unioning their rows into the typed_edges table (OV-5/LD-5).
change: org-graph-spike
status: ready
relations:
  - blocked-by:edges-drawer
  - blocked-by:parse-rel-links
cites_register_entries:
  - register/boundary/parser-extractor-db
  - register/shape/typed-edge-tuple
  - register/invariant/typed-edge-extraction-scope
  - register/invariant/enclosing-node-attribution
---

> Cycle cycle-1786458912: contract point RESOLVED — the note-granularity
> filter shipped with `edges-drawer` (a `seq-filter` on from-id = note-id
> in `org-graph-extractor/extract`; the whole-file scanner made it
> non-deferrable). See
> .orchestrator/cycles/cycle-1786458912/reconciliations/parser-extractor-db.md.
> This task inherits it pinned and reduces to: add the `parse-rel-links`
> leg + union/dedup.

> Revises the (closed) `vulpea-extractor-plugin` wrapper to run two scanners
> instead of one. `typed_edges` schema is unchanged. See design.md
> § Open-Vocabulary Typed Edges, OV-5.

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/extractor.org`
  (the `make-vulpea-extractor` wrapper only — not the pure parsers)
- `config/org-graph/test/extractor-spec.el`

## Implementation steps
1. In the extractor wrapper, call both `org-graph-extractor/parse-drawer-edges`
   (edge drawer) and `org-graph-extractor/parse-rel-links` (inline) over the
   note's parsed tree, then **union** the tuple lists before the DB write path.
2. Preserve the existing invariants: the scope gate still restricts to
   `org-graph-roam-root`; both scanners resolve attribution internally via
   the shared enclosing-node walk (LD-4), so the wrapper adds no attribution
   logic; the per-note from-id filter stays (it applies to the unioned
   stream). Storage shape (`rel-type` as a SYMBOL) and the priority-50
   registration are unchanged — note the extractor `:version` is already 2
   (edges-drawer bumped it); bump to 3 ONLY if this task changes scanner
   output for identical file content (adding the rel-links leg does: bump).
3. De-duplicate identical `(from rel to)` tuples that could arise from the
   same edge asserted on both surfaces (union semantics, not multiset).
4. Extend `extractor-spec.el`: a note with **both** an edge-drawer item and
   an inline `rel:` edge yields both rows; a note outside the roam root yields
   none; the storage-as-symbol and registration assertions still hold, with
   `emacsql`/`vulpea-db` stubbed.

## Design rationale
Both surfaces are pure functions emitting the same tuple contract (OV-5), so
the wrapper's only new job is to run both and union. Keeping the union in the
wrapper (not the parsers) leaves each parser independently testable.

## Verification
- `./bin/tangle-org.sh config/org-graph/extractor.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — extractor spec passes,
  including the dual-surface union case.
- `grep -n 'parse-rel-links\|parse-drawer-edges' config/org-graph/extractor.el`
  shows both called from the wrapper.

## Context
design.md § Open-Vocabulary Typed Edges (OV-5) and § Links-Drawer Edge
Surface (LD-4/LD-5); architecture.md § Components (`extractor`,
dual-scanner union).

## Cycle updates (cycle-1786458912)

- **Stage-0 precondition (register/boundary/rel-link-path-syntax,
  reconciled):** an unregistered link type parses as `fuzzy` — the
  rel-links leg extracts NOTHING until the `rel:` type is registered in
  `org-link-parameters` at parse time. Two consequences: (a) the
  dual-surface spec case MUST register the link type in its fixture
  (reuse the pattern in `parse-rel-links-spec.el`); (b) sequencing —
  landing this before `rel-link-type` means the live rel-links leg unions
  an always-empty stream (green-on-empty, eoc-4). Prefer implementing
  with-or-after `rel-link-type`, or accept the staged emptiness knowingly
  in Observations.
- The wrapper currently runs the drawer leg + from-id filter only;
  `parse-rel-links` is shipped but unwired (intentional staging).
- Scanner reads the link-type name via the fail-closed
  `org-graph-extractor--edge-link-type` (nil until the defcustom lands
  with `rel-link-type`).
