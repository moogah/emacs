---
name: edge-type-registry
description: "Add the optional in-vault edge-type registry: an :edge-type: selector, a metadata loader, a finder, and four seed registry notes (OV-6)."
change: org-graph-spike
status: ready
relations:
  - enables:rel-link-type
  - enables:query-inverse-symmetric
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
