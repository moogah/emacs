---
name: refresh-architecture-md-against-re-eval
description: Rewrite the stale sections of architecture.md to match design.md RE-1..RE-6 and the implemented loader; remove the SUPERSEDED banner once done.
change: org-graph-spike
status: ready
relations:
  - discovered-from:vulpea-extractor-plugin
cites_register_entries:
  - register/vocabulary/relation-types
discovered_by: architect
discovered_class: interface-drift
---

## Files to modify
- `openspec/changes/org-graph-spike/architecture.md`

## Why
Foundation Architect finding `arch-cycle-1782551613-01` (blocking, routed to
user; risk neutralised by an inline SUPERSEDED banner). architecture.md still
describes the pre-re-eval design: org-node, filetag finders, `~/work`
recursive eager-scan, `org-graph-watched-roots` /
`org-graph-typed-graph-root` defcustoms, and a nested test layout. All are
superseded by design.md RE-1..RE-6 and the implemented loader
(`config/org-graph/org-graph.org`).

## Implementation steps
1. Rewrite the **Components**, **Interfaces**, **Dependencies**,
   **Constraints**, and **Testing Approach > Test Organization** sections to
   match the implemented loader:
   - vulpea-only; no org-node / org-mem.
   - Defcustoms: `org-graph-roam-root`, `org-graph-relation-types`,
     `org-graph-watch-workspace-homes`, `org-graph-note-types`,
     `org-graph-coordinator-timeout`. No `org-graph-watched-roots` /
     `org-graph-typed-graph-root`.
   - Discovery = registry-driven vulpea sync + `org-id-locations` DB seed.
   - Finders = schema-aware (`vulpea-schema`).
   - Flat test layout: `config/org-graph/test/*-spec.el`.
2. Preserve the still-valid claims (do NOT delete): `make-vulpea-extractor` +
   `typed_edges` + `notes(id)` FK `:on-delete :cascade`; pure-parser tuple
   shape; `with-file-lock` signature; Buttercup; PROPERTIES-drawer convention.
3. Remove the SUPERSEDED banner at the top once the body is consistent.

## Verification
- `grep -n "org-node\|watched-roots\|typed-graph-root\|directory-files-recursively" openspec/changes/org-graph-spike/architecture.md`
  returns nothing (or only the historically-accurate Re-evaluation framing).
- Cross-check defcustom names against `config/org-graph/org-graph.org`.

## Context
Finding `.orchestrator/cycles/cycle-1782551613/findings/arch-cycle-1782551613-01.md`;
design.md § Re-evaluation (RE-1..RE-6); config/org-graph/org-graph.org.
Not part of the cycle-1782551613 execute batch (deferred doc hygiene; do
before archiving the change).

## Observations

Contradictions resolved while reconciling old architecture.md, design.md
RE-*, and the implemented code:

1. **`org-graph-coordinator-timeout` is NOT a loader defcustom.** The task
   body lists it among the loader's defcustoms. In the implementation it is
   defined in `config/org-graph/coordinator.org` (the coordinator
   sub-module), not in `org-graph.org`. The loader owns four defcustoms
   (`org-graph-roam-root`, `org-graph-relation-types`,
   `org-graph-watch-workspace-homes`, `org-graph-note-types`); the timeout
   belongs to the coordinator. architecture.md now attributes it correctly.

2. **Query layer does NOT use `vulpea-db-query`.** The superseded
   architecture.md claimed the query API is "built on `vulpea-db-query`
   against the `typed_edges` table." That is false: `query.org` explicitly
   notes `typed_edges` is org-graph's own side table (not a vulpea-managed
   `notes` table), so `vulpea-db-query` cannot read it. The three public
   queries route through raw `(emacsql (vulpea-db) ...)` via a single
   `org-graph-query--select` seam. Corrected in Components + Interfaces.

3. **Phantom discovery API.** The superseded Interfaces listed
   `(org-graph/eager-discover)` and `(org-graph/watched-roots)` — neither
   exists. The real discovery surface is `org-graph/index-roots`,
   `org-graph/configure-sync`, and `org-graph/seed-org-id-locations`.
   Replaced.

4. **gptel-tools + workspace-integration are not yet implemented.** The
   loader (`org-graph.org`) carries placeholder sections for both, and no
   `tools.el` / workspace-integration file exists. The original
   architecture.md described `org-graph-tools` and the gptel tool surface
   as present. I rewrote both as *planned* (loader placeholders, pending
   their own tasks) rather than deleting them, since they remain part of
   the spike's intended structure (RE-5). Not stale, just not landed.

5. **Partial loader wiring (could-not-fully-reconcile, but not stale).**
   The `discovery`, `extractor`, and `coordinator` sub-modules exist as
   tangled files and are tested, but the loader's `Submodules` block
   currently loads only `schemas.el` + `finders.el` (and `query.el` from
   its own section). The full ordered load sequence is owned by the pending
   `wire-into-init` task. architecture.md describes components by their
   implemented modules (accurate to the code) and flags tools/
   workspace-integration as pending; I did not assert a load order the
   loader does not yet enforce.

## Discoveries

- discovery_id: disc-refresh-architecture-md-1
  class: interface-drift
  description: |
    The typed-edge query layer reads `typed_edges` via raw emacsql on the
    shared `(vulpea-db)` connection through `org-graph-query--select`, NOT
    via `vulpea-db-query` (which only loads/filters the vulpea `notes`
    table). The superseded architecture.md asserted `vulpea-db-query`. Any
    register entry describing the parser→extractor→DB→query boundary should
    record that the read seam is `org-graph-query--select` over emacsql, and
    that `rel-type` is matched as a SYMBOL scalar (emacsql prin1/read
    round-trip), consistent with `register/shape/typed-edge-tuple`.
  affected_register_entry: register/boundary/parser-extractor-db
  recommendation: |
    Confirm the register's parser-extractor-db boundary entry names
    `org-graph-query--select` (raw emacsql) as the read seam, not
    `vulpea-db-query`, so future consumers do not re-introduce the wrong
    accessor.

- discovery_id: disc-refresh-architecture-md-2
  class: spec-signal
  description: |
    Task body lists `org-graph-coordinator-timeout` among the loader
    defcustoms, but it is owned by the coordinator sub-module
    (coordinator.org), not org-graph.org. Minor doc-source drift in the task
    framing; the defcustom itself is correct and present.
  recommendation: |
    No action required beyond architecture.md attributing the timeout to the
    coordinator (done). Note for whoever audits the loader defcustom set:
    the loader owns four, the coordinator owns the fifth.
