---
name: parse-rel-links
description: "Add the pure inline-link scanner parse-rel-links that extracts rel: links and attributes each to its nearest ID-bearing ancestor node (OV-3/OV-4/OV-5)."
change: org-graph-spike
status: blocked
relations:
  - enables:extractor-union
  - blocked-by:edges-drawer
cites_register_entries:
  - register/shape/typed-edge-tuple
  - register/vocabulary/relation-types
  - register/boundary/rel-link-path-syntax
  - register/invariant/enclosing-node-attribution
---

> Batch sequencing (cycle-1786458912 plan): blocked-by `edges-drawer` so the
> extractor.org restructure and the shared ancestor-walk helper
> (`org-graph-extractor--enclosing-note-id`, LD-4) land once; this task then
> reuses the helper rather than racing to define it.

> Second authoring surface for typed edges. Pure function only — the `rel:`
> link *runtime* (follow/complete/face) is a separate task (`rel-link-type`).
> See design.md § Open-Vocabulary Typed Edges, OV-3/OV-4/OV-5.

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/extractor.org`
  (new pure scanner alongside `parse-drawer-edges`)
- `config/org-graph/test/parse-rel-links-spec.el` (new)

## Implementation steps
1. Implement `org-graph-extractor/parse-rel-links (element-tree)` as a PURE
   function returning `(from-id rel-type to-id)` tuples. It maps over `link`
   elements with `:type "rel"`, splitting each path `<type>:<target-id>` on
   the first `:` → `rel-type` (interned symbol) and `to-id`.
2. **Enclosing-node attribution (OV-4/LD-4):** `from-id` is the nearest
   ancestor carrying an `:ID:`, resolved by walking `org-element-lineage` up
   to the enclosing headline, falling back to the file-level (top) node's
   `:ID:`. Implement the walk as a shared helper (e.g.
   `org-graph-extractor--enclosing-note-id`) — the drawer scanner
   (`edges-drawer` task) uses the same rule. A `rel:` link with **no**
   ID-bearing ancestor yields **no** tuple (drop, never mis-attribute).
3. Handle: malformed path (missing `:` separator, empty type or target) →
   skip gracefully, no error.
4. Write `parse-rel-links-spec.el` first, using `org-graph-test/build-tree`
   (extend the helper to place `rel:` links in body/headings as needed):
   - link at file top → attributed to the file node's `:ID:`.
   - link under a subheading with its **own** `:ID:` → that heading's id.
   - link under a heading with **no** `:ID:`, nested inside one that has
     → walks up to the ID-bearing ancestor.
   - link with no ID-bearing ancestor at all → dropped.
   - a bare `[[id:x]]` link in the same prose → produces no edge.
   - malformed `rel:` path → no error, no row.

## Design rationale
Enclosing-node resolution (OV-4/LD-4) is the bulk of this task's test
surface, and it is no longer inline-only: LD-4 makes it the single
attribution model for both surfaces, so the ancestor walk built here is
shared with the drawer scanner. Keeping it a pure function mirrors D4/OV-5
so the extractor wrapper can union both scanners.

## Design pattern
Pure-function-over-AST with synthetic-tree tests, same contract as
`parse-drawer-edges`: `((from rel to) ...)`.

## Verification
- `./bin/tangle-org.sh config/org-graph/extractor.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — parse-rel-links spec passes
  all attribution cases.
- Function never signals on malformed input.

## Context
design.md § Open-Vocabulary Typed Edges (OV-3, OV-4, OV-5); spec.md § Typed
Semantic Edges (inline `rel:` links, enclosing-node attribution).
