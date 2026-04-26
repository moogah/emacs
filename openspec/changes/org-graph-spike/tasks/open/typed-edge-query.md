---
name: typed-edge-query
description: Implement org-graph-query/{outgoing,incoming,connected} on top of vulpea-db-query, test-first with vulpea mocked.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:test-helpers"
  - "blocked-by:vulpea-extractor-plugin"
---

## Files to modify

- `config/org-graph/test/query/typed-edges-spec.el` (new) — Buttercup spec, written first.
- `config/org-graph/org-graph.org` (modify) — fill the `Query` subtree.

## Implementation steps

1. Write the spec first. `describe "org-graph-query"` with nested `describe`s for `outgoing`, `incoming`, `connected`. For each:
   - returns rows matching the requested filter.
   - filters by `rel-type` when provided.
   - returns empty list when no edges exist.
   - dedupes when the same `(from rel-type to)` would otherwise repeat.

   Stub the underlying vulpea query function via `org-graph-test/with-stubbed-vulpea` so tests are deterministic without a real DB. Capture the query payload passed to `vulpea-db-query` and assert it shapes correctly (column projection, where clause).

2. In the `Query` subtree, implement:
   - `org-graph-query/outgoing (from-id &optional rel-type)` — selects rows from `typed_edges` where `from-id` matches and (optionally) `rel-type` matches.
   - `org-graph-query/incoming (to-id &optional rel-type)` — selects rows where `to-id` matches.
   - `org-graph-query/connected (note-id)` — union of outgoing and incoming for `note-id`, deduped.

3. Each function returns a list of `(:from FROM :rel REL :to TO :to-title TITLE)` plists. Resolve `:to-title` via `vulpea-db-get-by-id` for each `to-id`. If a target has been deleted, the row may have already been cascaded away; defensive nil-check the title.

4. Use `vulpea-db-query` with structured predicates per the vulpea v2 API. Read the vulpea source (`vulpea-db-query.el`) at implementation time to match the actual call signature.

5. Run tests until green: `./bin/run-tests.sh -d config/org-graph/test/query`.

## Design rationale

The query API is the surface AI agents call most. Returning structured plists with resolved titles (`:to-title`) means agents don't need a second round-trip to render edges as something a human can read. Dedup in `connected` matters because reciprocal edges (A implements B, B relates-to A) could otherwise show A→B twice in one call.

Mocking `vulpea-db-query` at the function boundary (architecture.md §Test Patterns) keeps the spec fast and isolates the test from vulpea's schema evolution. If vulpea changes its query API, this is the only file in the test suite that needs updating.

## Verification

- `./bin/run-tests.sh -d config/org-graph/test/query` — green.
- `grep -nE "defun org-graph-query/(outgoing|incoming|connected)" config/org-graph/org-graph.el` — 3 matches.
- Manual: with the spike loaded and a few `:IMPLEMENTS:` properties added to `~/org/roam/` notes, `(org-graph-query/outgoing "<some-id>")` returns the expected rows.

## Context

- specs/org-graph/spec.md §Typed Semantic Edges (query-API requirements)
- architecture.md §Components §org-graph-query
- design.md §D4
