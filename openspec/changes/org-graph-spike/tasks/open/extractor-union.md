---
name: extractor-union
description: Wire both pure scanners (drawer + inline) into the vulpea extractor wrapper, unioning their rows into the typed_edges table (OV-5).
change: org-graph-spike
status: blocked
relations:
  - blocked-by:open-vocab-drawer
  - blocked-by:parse-rel-links
---

> Revises the (closed) `vulpea-extractor-plugin` wrapper to run two scanners
> instead of one. `typed_edges` schema is unchanged. See design.md
> § Open-Vocabulary Typed Edges, OV-5.

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/extractor.org`
  (the `make-vulpea-extractor` wrapper only — not the pure parsers)
- `config/org-graph/test/extractor-spec.el`

## Implementation steps
1. In the extractor wrapper, call both `org-graph-extractor/parse-typed-edges`
   (drawer) and `org-graph-extractor/parse-rel-links` (inline) over the note's
   parsed tree, then **union** the tuple lists before the DB write path.
2. Preserve the existing invariants: the scope gate still restricts to
   `org-graph-roam-root`; drawer edges stay attributed to the note's own
   PROPERTIES drawer; inline edges use their enclosing node (already resolved
   inside `parse-rel-links`). Storage shape (`rel-type` as a SYMBOL) and the
   priority-50 registration are unchanged.
3. De-duplicate identical `(from rel to)` tuples that could arise from the
   same edge asserted on both surfaces (union semantics, not multiset).
4. Extend `extractor-spec.el`: a note with **both** a `REL_` drawer edge and
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
- `grep -n 'parse-rel-links\|parse-typed-edges' config/org-graph/extractor.el`
  shows both called from the wrapper.

## Context
design.md § Open-Vocabulary Typed Edges (OV-5); architecture.md § Components
(`extractor`, dual-scanner union).
