---
name: query-inverse-symmetric
description: Add registry-driven inverse-label rendering and symmetric-aware reads to the query layer, derived at read time with no stored inverse rows (OV-7).
change: org-graph-spike
status: blocked
relations:
  - blocked-by:edge-type-registry
cites_register_entries:
  - register/boundary/typed-edge-query-api
  - register/invariant/no-materialized-inverse-rows
  - register/boundary/edge-type-registry-lookup
---

> Revises the (closed) `typed-edge-query` read API. Storage stays canonical
> and directional; this is a read/render concern only. See design.md
> § Open-Vocabulary Typed Edges, OV-7.

## Files to modify
- `config/org-graph/query.el` ← via `config/org-graph/query.org`
- `config/org-graph/test/typed-edges-spec.el`

## Implementation steps
1. **Inverse rendering:** add a resolver that, given a stored `(from rel to)`
   edge and the target's perspective, labels it with the registry `:INVERSE:`
   for `rel` (e.g. a stored `implements` edge shows as `implemented-by` on the
   target). Prefer a display helper (e.g. `org-graph-query/edge-label EDGE
   PERSPECTIVE`) so the underlying rows/API stay unchanged. Unregistered or
   inverse-less types render the raw symbol.
2. **Symmetric-aware reads:** for a type whose registry entry is
   `:SYMMETRIC: t`, `org-graph-query/connected` (and optionally
   `incoming`/`outgoing`) surface the relation in both directions without an
   extra stored row.
3. Do **not** write inverse rows and do **not** change the `typed_edges`
   schema — derive at read time from `org-graph/edge-types`.
4. Extend `typed-edges-spec.el`: a stored `implements` edge renders as
   `implemented-by` from the target's side when the registry declares the
   inverse; a `:SYMMETRIC: t` type surfaces bidirectionally in `connected`;
   an unregistered type renders its raw symbol. Stub
   `org-graph-query--select` and `org-graph/edge-types`.

## Design rationale
Deriving inverse/symmetry at read time keeps `typed_edges` canonical and
avoids double-write divergence (OV-7). The registry is the single source of
per-type semantics, consistent with the folksonomy→taxonomy model (OV-6).

## Verification
- `./bin/tangle-org.sh config/org-graph/query.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — typed-edges spec passes the
  inverse and symmetric cases.
- `grep -n 'INVERSE\|symmetric\|edge-types' config/org-graph/query.el` shows
  read-time derivation, and no `INSERT`/write of inverse rows.

## Context
design.md § Open-Vocabulary Typed Edges (OV-7); spec.md § Typed Semantic
Edges (inverse/symmetry derived at query/display time).
