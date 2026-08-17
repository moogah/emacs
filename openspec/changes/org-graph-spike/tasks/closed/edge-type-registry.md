---
name: edge-type-registry
description: "Add the optional in-vault edge-type registry: an :edge-type: selector, a metadata loader, a finder, and four seed registry notes (OV-6)."
change: org-graph-spike
status: done
relations:
  - enables:rel-link-type
  - enables:query-inverse-symmetric
merge_commit: a9e9a933
cites_register_entries:
  - register/boundary/edge-type-registry-lookup
  - register/vocabulary/relation-types
---

> The folksonomy→taxonomy layer. Types work with zero registration; a note
> tagged `:edge-type:` graduates one by declaring metadata. See design.md
> § Open-Vocabulary Typed Edges, OV-6/OV-7.

## Files to modify
- `config/org-graph/edge-type.el` ← via `config/org-graph/edge-type.org` (new module)
- `config/org-graph/org-graph.el` ← via `config/org-graph/org-graph.org` (load new module)
- `config/org-graph/test/edge-type-spec.el` (new)
- Seed notes under `org-graph-roam-root` (see step 4)

## Implementation steps
1. Define the `:edge-type:` selector predicate (filetag membership), mirroring
   the schemas-module pattern (RE-3). Registry notes are ordinary vault notes
   with an `:ID:` and the `edge-type` filetag.
2. Implement `org-graph/edge-types` — read all `:edge-type:` notes and build a
   lookup keyed by relation symbol, carrying `label` (a `LABEL` property or
   the title), `inverse` (`:INVERSE:` → symbol or nil), `symmetric`
   (`:SYMMETRIC:` → boolean), and `description`. Cache per session; a note
   change invalidates via the normal vulpea reindex path.
3. Implement `org-graph/find-edge-type` — an interactive finder restricted to
   `:edge-type:` notes (same shape as the type finders).
4. Ship four **seed registry notes** — `implements` (`:INVERSE: implemented-by`),
   `contradicts` (`:SYMMETRIC: t`), `supersedes` (`:INVERSE: superseded-by`),
   `relates-to` (`:SYMMETRIC: t`) — as starter data, replacing the hardcoded
   closed list.
5. Guarantee **graceful absence**: `org-graph/edge-types` returns an empty
   lookup when no registry notes exist; every consumer treats a missing entry
   as "render the raw symbol." No extraction path depends on the registry.
6. `edge-type-spec.el`: predicate selects only tagged notes; metadata load
   parses label/inverse/symmetric/description; missing type → nil lookup;
   seed notes load; finder filter admits only registry notes.

## Design rationale
The registry is data-in-the-vault (git-versioned, indexed, discoverable),
not code — so a new type is coined by usage and graduated by writing a note,
at any time (OV-6). Inverse/symmetric live here because a hardcoded list
can't carry per-type semantics once the vocabulary is open. Storage stays
canonical; the registry only enriches reads (OV-7).

## Verification
- `./bin/tangle-org.sh config/org-graph/edge-type.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — edge-type spec passes.
- `grep -n 'edge-type' config/org-graph/org-graph.el` shows the module loaded
  in canonical order.

## Context
design.md § Open-Vocabulary Typed Edges (OV-6, OV-7); spec.md § Typed
Semantic Edges (optional edge-type registry); architecture.md § Components
(`edge-type` registry).

## Observations

- **Seed notes shipped as data + installer, not vault writes.** The task's
  "Files to modify" lists seed notes under `org-graph-roam-root` — a runtime
  vault (`~/org/roam/`) outside this repository, which an implementation
  worktree must not write into. Implemented instead as
  `org-graph-edge-type-seed-definitions` (the four seeds with their
  `:INVERSE:`/`:SYMMETRIC:` metadata) plus the interactive, idempotent
  `org-graph-edge-type-install-seeds` (defaults to `org-graph-roam-root`,
  writes `edge-type-<rel>.org` only when missing, never clobbers user edits).
  **User-side step required:** run `M-x org-graph-edge-type-install-seeds`
  once; the notes then enter the lookup via normal vulpea indexing.
- **Cache invalidation mechanism.** vulpea 2.4 exposes no post-reindex hook
  (verified against `vulpea-db-sync.el`), so "invalidated via the normal
  vulpea reindex path" is implemented by keying the session cache on the
  vulpea DB file's modification time — a reindex rewrites the DB file, so
  the next read repopulates without any hook or advice. An explicit
  `org-graph-edge-type-invalidate-cache` covers tests and manual refresh.
- **"Replacing the hardcoded closed list" is split across parallel tasks.**
  The `org-graph-relation-types` defcustom and the extractor's closed-list
  helpers (`--rel-key`/`--key->rel`, still present in `extractor.el` at
  branch time) are owned by the parallel edges-drawer/rel-link work per the
  orchestrator brief; this task deliberately did not touch them. The seeds
  functionally replace the list; its demotion/deletion lands with those
  tasks.
- **Description source pinned.** The register entry says only "description
  (free text)". Implemented as `DESCRIPTION` property first, then the
  `description` vulpea meta entry — a body-only description is not loadable
  because vulpea does not index note bodies.
- **Loader edits kept minimal** per the merge-conflict heads-up: one
  `jf/load-module` line (after `finders`, before `tools`) plus the same
  insertion in the canonical-order prose sentence.

## Discoveries

- discovery_id: disc-edge-type-registry-1
  class: vocabulary-mismatch
  description: |
    The change artifacts disagree on the canonical relation-normalization
    helper's name: register/vocabulary/relation-types speculates
    org-graph-extractor--normalize-rel, while architecture.md names
    org-graph-extractor--tag->rel as "the only allowed translation site".
    Neither exists yet (the edges-drawer task ships it in parallel).
    edge-type.el keys registry entries by normalized note title, so it must
    share that site: org-graph-edge-type--normalize-rel delegates to
    org-graph-extractor--tag->rel when fbound (the architecture name; the
    extractor loads first in the loader sequence) and falls back to an
    identical local trim/downcase/[ _]+->-/intern chain so the module stays
    standalone-loadable in unit specs.
  affected_register_entry: register/vocabulary/relation-types
  recommendation: |
    At integrate, pin canonical_mapping_function to the name the
    edges-drawer task actually shipped; if it is not
    org-graph-extractor--tag->rel, repoint edge-type.el's fboundp guard in
    the same reconciliation so the registry cannot silently fork
    normalization via its fallback.

- discovery_id: disc-edge-type-registry-2
  class: deviation
  description: |
    register/boundary/edge-type-registry-lookup (speculated) said the
    lookup is "cached per session; invalidated via the normal vulpea
    reindex path". vulpea 2.4 has no reindex hook to attach to, so the
    implementation keys the cache on the vulpea DB file's mtime (the
    reindex path rewrites that file) plus an explicit
    org-graph-edge-type-invalidate-cache command. The behavioural contract
    otherwise held exactly: enrich-only, empty lookup on zero registry
    notes, nil as a normal consult result. One addition: the stage-2
    "consult" operation got a named accessor, org-graph/edge-type (rel) ->
    plist | nil, alongside the stage-1 producer org-graph/edge-types.
  affected_register_entry: register/boundary/edge-type-registry-lookup
  recommendation: |
    Reconcile stage-1 notes to state the mtime-keyed session cache (+
    explicit invalidation command) as the invalidation mechanism, and add
    org-graph/edge-type as the named stage-2 consult function in producers.
