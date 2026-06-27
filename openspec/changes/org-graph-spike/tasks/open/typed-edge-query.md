---
name: typed-edge-query
description: Implement outgoing, incoming, and connected typed-edge queries on top of the vulpea typed_edges table, test-first with vulpea mocked.
change: org-graph-spike
status: ready
relations:
  - blocked-by:vulpea-extractor-plugin
  - blocked-by:test-helpers
cites_register_entries:
  - register/boundary/typed-edge-query-api
  - register/shape/typed-edge-tuple
  - register/vocabulary/relation-types
  - register/boundary/parser-extractor-db
---

## Files to modify
- `config/org-graph/query.el` ← via a **new** `config/org-graph/query.org`
  (own literate module, matching the one-org-per-el convention; NOT a section
  in `org-graph.org`)
- `config/org-graph/org-graph.org` (loader) — append the `query` module to the
  existing require/load sequence; **append-only**, do not reorder existing loads
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

## Cycle 1782551613 updates (cycle-1782551613)
> Unblocked: blocker `vulpea-extractor-plugin` is done. Status flipped blocked → ready.

Absorb the cycle's register diff before implementing:
- **`register/shape/typed-edge-tuple` (confirmed):** the `typed_edges` row is
  `(from-id rel-type to-id)` with **`rel-type` stored as a SYMBOL** (emacsql
  prin1/read round-trips symbols). Query predicates MUST match on the symbol,
  not a string — e.g. `[:where (= rel-type 'implements)]`, not `"implements"`.
- **`register/vocabulary/relation-types` (confirmed):** the relation set is
  single-sourced; reuse `org-graph-relation-types` / the extractor's mapping
  rather than re-listing symbols.
- **`register/boundary/parser-extractor-db` (DIVERGENT — open user decision):**
  the typed graph is currently **file-level-only** (the extractor emits edges
  only for the file-level node; see follow-up `scope-extractor-edges-per-note`).
  Queries read whatever is stored, so they are unaffected by the model decision,
  but write your tests against file-level `from-id`s to match current behaviour.
- The extractor exposes registration as `org-graph-extractor-register` (function,
  not load-time); the `typed_edges` table exists only after the loader calls it —
  mock the DB boundary in specs as the foundation specs do (`vulpea-db` stubbed).

## Cycle 1782561220 updates (cycle-1782561220)
This task is on the **critical path** (it unblocks `gptel-tools` →
`workspace-integration` → `module-load-smoke`). Plan-phase decisions:
- **New module `query.org`.** `query.el`/`finders.el` did not exist; the repo
  convention is one `.org` per `.el`. Create `config/org-graph/query.org`
  tangling to `query.el`. Its **first** babel block MUST use `:comments no` so
  `;;; query.el --- ... -*- lexical-binding: t; -*-` lands on line 1
  (`register/invariant/lexical-binding-line-1`). Loader registration in
  `org-graph.org` is **append-only** (three tasks touch the loader this cycle —
  do not reorder).
- **Cite `register/boundary/typed-edge-query-api` (speculated, this cycle).**
  It is the forward contract for this task: `outgoing`/`incoming`/`connected`
  return edge plists `(:from :rel :to [resolved-note])`, `:rel` a SYMBOL, no
  auto-symmetry. Pressure-test it; it confirms when your specs land. The query
  surface is **agnostic** to the file-level-vs-note-granular attribution
  decision in `scope-extractor-edges-per-note` (also this batch) — you read
  stored rows by id, you do not re-attribute. Build fixtures directly; do not
  drive them through the extractor.
- Stage source/test files explicitly when committing — do NOT `git add -A`
  (foundation-cycle implementors swept worktree artifacts into commits).
