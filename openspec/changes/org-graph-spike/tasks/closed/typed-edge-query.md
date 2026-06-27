---
name: typed-edge-query
description: Implement outgoing, incoming, and connected typed-edge queries on top of the vulpea typed_edges table, test-first with vulpea mocked.
change: org-graph-spike
status: done
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

## Observations

- **`vulpea-db-query` does not read `typed_edges`.** The task body and
  `register/boundary/typed-edge-query-api` both describe the query API as
  "a thin layer over `vulpea-db-query`". Factually it cannot be:
  `vulpea-db-query` loads and elisp-filters the `notes` table only
  (`runtime/straight/build/vulpea/vulpea-db-query.el:153`). `typed_edges`
  is org-graph's own side table, so the genuine read boundary is the same
  emacsql connection the extractor *writes* through —
  `(emacsql (vulpea-db) [:select ... :from typed_edges ...])`. Implemented
  accordingly; see Discoveries for the register reconciliation.
- **Test-helper gap.** `org-graph-test/with-stubbed-vulpea` exposes no
  keyword for `vulpea-db-get-by-id` (note resolution) and none for a raw
  `emacsql`/`typed_edges` select, so it cannot mock this module's boundary.
  I followed the established `extractor-spec.el` convention instead
  (function-scoped `cl-letf` over `vulpea-db` + `emacsql`), plus a local
  `cl-letf` over `vulpea-db-get-by-id`. Recommend extending the shared
  helper with `:get-by-id` (and possibly an edge-select stub) — filed as a
  discovery, not done here to avoid touching a helper three cycle tasks
  share.
- **Layered mocking.** The public `outgoing`/`incoming`/`connected` specs
  stub the internal `org-graph-query--select` seam to isolate plist shape,
  far-end resolution, rel passthrough, and union; `--select` itself is
  tested at the emacsql boundary (query vector + symbol-bound args). This
  keeps tests from re-implementing SQLite filtering in a stub (the fragile
  alternative) while still asserting query construction is correct.
- **`:note` resolves the FAR end.** The register's `output_shape` says
  "the resolved target note". I resolved the note at the end *opposite* the
  query subject (destination for outgoing, source for incoming) since that
  is the note an agent answering "what is this connected to?" needs.
  `:note` is always present (may be nil) for a stable consumer shape.
- **No symmetry / no dedup in `connected`.** Per design Non-Goals, edges
  are directional; `connected` is a plain `append` of outgoing then
  incoming. A self-edge (FROM=TO=NOTE-ID) legitimately appears in both
  halves — documented, not special-cased.

## Discoveries

- discovery_id: disc-typed-edge-query-1
  class: interface-drift
  description: |
    register/boundary/typed-edge-query-api (and the task body) frame the
    query API as "a thin, vulpea-mockable layer over vulpea-db-query".
    vulpea-db-query only queries/filters the `notes` table; it has no
    knowledge of the `typed_edges` side table. The implemented (and only
    viable) boundary is a raw `(emacsql (vulpea-db) [:select ... :from
    typed_edges ...])`, mirroring how the extractor's writes go through
    `emacsql` (see extractor.el / register/shape/typed-edge-tuple) — NOT
    vulpea-db-query. Everything else in the entry held exactly: three
    functions with the stated signatures, edge plists `(:from :rel :to
    :note)`, `:rel` a SYMBOL, rel-type filtering matches the symbol, no
    auto-symmetry, resolution via vulpea-db-get-by-id.
  affected_register_entry: register/boundary/typed-edge-query-api
  recommendation: |
    On confirm, amend the entry's purpose/output_shape wording: the read
    boundary is `emacsql (vulpea-db)` against `typed_edges`, with
    `vulpea-db-get-by-id` for note resolution — drop the "layer over
    vulpea-db-query" phrasing. The producers list (config/org-graph/query.el,
    outgoing|/incoming|/connected) is correct. Add `:note` semantics: the
    resolved note is the FAR end relative to the query subject, always
    present (nil when unresolved). Recommend status speculated -> confirmed
    with this wording fix.

- discovery_id: disc-typed-edge-query-2
  class: shape-fragmentation
  description: |
    The output edge plist shape `(:from :rel :to :note)` is introduced by
    this task but has no register shape entry of its own — it is only
    described prose-style inside register/boundary/typed-edge-query-api's
    output_shape. The downstream gptel-tools task (next cycle) consumes this
    shape to present human-readable edges, so it is a real cross-module
    contract that would benefit from a dedicated `register/shape/*` entry
    (parallel to register/shape/typed-edge-tuple for the storage triple).
  affected_register_entry: register/boundary/typed-edge-query-api
  recommendation: |
    Consider adding register/shape/typed-edge-result (plist: :from string,
    :rel symbol, :to string, :note vulpea-note|nil = far end relative to
    subject) so gptel-tools binds to a named shape rather than re-reading
    prose. Low priority; the boundary entry currently carries it.

- discovery_id: disc-typed-edge-query-3
  class: invariant-gap
  description: |
    `org-graph-test/with-stubbed-vulpea` cannot mock this module's boundary:
    it has no keyword for `vulpea-db-get-by-id` and none for a raw emacsql
    `typed_edges` select. New query specs had to fall back to ad-hoc
    `cl-letf` (consistent with extractor-spec.el, which also stubs
    `emacsql`/`vulpea-db` directly). As more modules read the side table,
    this gap will be re-hit.
  affected_register_entry: register/boundary/typed-edge-query-api
  recommendation: |
    Extend the shared helper macro (config/org-graph/test/helpers-spec.el)
    with a `:get-by-id` keyword shadowing `vulpea-db-get-by-id`, and consider
    a reusable typed_edges-select stub. Append-only; coordinate since
    multiple cycle tasks touch test helpers.

- discovery_id: disc-typed-edge-query-4
  class: deviation
  description: |
    Loader registration in org-graph.org: rather than adding a load to the
    (currently empty) Submodules block, I replaced the Query section's
    `;; implemented in typed-edge-query` placeholder with the actual
    `(jf/load-module ".../query.el")`. This keeps the edit inside this
    task's own section (append-only, no reordering, no collision with the
    other two loader-touching tasks this cycle). Note: the sibling extractor
    section still carries its placeholder comment — the submodules are not
    yet wired into init wholesale (wire-into-init task), but tangling the
    Query-section load line does make query.el load when org-graph.el is
    loaded.
  affected_register_entry: register/boundary/typed-edge-query-api
  recommendation: |
    During integrate, confirm the three loader-touching tasks each edited
    their own section (no shared-block conflict). If a single canonical load
    sequence is wanted, consolidate into Submodules at wire-into-init time.
