---
name: typed-edge-query
description: Implement outgoing, incoming, and connected typed-edge queries on top of the vulpea typed_edges table, test-first with vulpea mocked.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:vulpea-extractor-plugin
  - blocked-by:test-helpers
---

## Files to modify
- `config/org-graph/query.el` ← via `config/org-graph/org-graph.org`
  (Query section)
- `config/org-graph/test/typed-edges-spec.el` (new)

## Implementation steps
1. Implement the typed-edge query API against the `typed_edges` table:
   - `org-graph-query/outgoing (from-id &optional rel-type)` — edges where
     `from-id` matches, optionally filtered by relation type.
   - `org-graph-query/incoming (to-id &optional rel-type)` — edges where
     `to-id` matches (this is how "incoming" relations resolve, since edges
     are stored directionally and NOT auto-symmetrized — see Non-Goals).
   - `org-graph-query/connected (note-id)` — union of outgoing and incoming.
2. Back them with `vulpea-db-query` (or a direct emacsql select against
   `typed_edges`). Return plists or structs with `:from`, `:rel`, `:to` and,
   where useful, the resolved target `vulpea-note` (via `vulpea-db-get-by-id`)
   so callers/agents get titles, not just ids.
3. Keep relation-type filtering a simple predicate on the normalized symbol.
4. Write `typed-edges-spec.el` with `org-graph-test/with-stubbed-vulpea`:
   fixture `typed_edges` rows; assert outgoing/incoming/connected return the
   right subsets, and that `rel-type` filtering narrows correctly. No live DB.

## Design rationale
Edges are explicitly authored and directional — there is no auto-derived
symmetry (design.md Non-Goals), so "incoming" is a real query against `to-id`,
not a mirror of outgoing. This API is the read half of the agent-facing graph
surface; keeping it a thin, mockable layer over `vulpea-db-query` matches the
behavioral-test convention and isolates us from vulpea internals.

## Design pattern
Mock at the vulpea API boundary (`vulpea-db-query`, `vulpea-db-get-by-id`) per
design.md Testing Approach. Return resolved notes so the gptel tool layer can
present human-readable edges.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — query specs pass for
  outgoing, incoming, connected, and rel-type-filtered cases.
- Manual: against a roam note carrying `:IMPLEMENTS:`,
  `org-graph-query/outgoing` returns the edge and resolves the target title.

## Context
design.md § Components (org-graph-query); design.md § Non-Goals
(no auto-symmetry); architecture.md § Interfaces.
</content>
