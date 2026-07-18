---
name: open-vocab-drawer
description: Replace the closed-set allowlist in the drawer typed-edge parser with the REL_ namespace discriminator, making the relation vocabulary open (OV-1/OV-2).
change: org-graph-spike
status: ready
relations:
  - enables:extractor-union
---

> Revises the (closed) `parse-typed-edges` work. Clean break — no
> backward-compat with bare `:IMPLEMENTS:` keys (near-zero existing edges,
> user-confirmed). See design.md § Open-Vocabulary Typed Edges, OV-1/OV-2.

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/extractor.org`
  (relation-key translation + pure drawer parser)
- `config/org-graph/org-graph.el` ← via `config/org-graph/org-graph.org`
  (`org-graph-relation-types` docstring/meaning)
- `config/org-graph/test/parse-typed-edges-spec.el`

## Implementation steps
1. Redefine the discriminator. A drawer property is a typed edge **iff its
   key matches `^REL_\(.+\)$`**. `org-graph-extractor--key->rel` returns the
   captured group lowercased with `_`→`-` interned as a symbol
   (`REL_RELATES_TO` → `relates-to`), else `nil`. `--rel-key` becomes the
   inverse: `REL_` + upcase(`_`←`-`) (`falsifies` → `REL_FALSIFIES`). These
   two remain the only translation sites.
2. Delete the allowlist membership test entirely — no lookup against
   `org-graph-relation-types`. Any well-formed `REL_<TYPE>` extracts; a bare
   `REL_` (empty type) is ignored.
3. Redefine `org-graph-relation-types` as a **non-gating completion seed
   list** (docstring update only): it no longer gates extraction; it seeds
   candidate suggestions for the authoring surfaces. Keep the four starter
   symbols as its default value.
4. Update `parse-typed-edges-spec.el`:
   - `:REL_IMPLEMENTS: [[id:abc]]` → one `implements` row.
   - `:REL_FALSIFIES: [[id:abc]]` (novel, unregistered) → one `falsifies`
     row, proving open vocabulary.
   - `:SOURCE: [[id:abc]]` and `:IMPLEMENTS: [[id:abc]]` (no `REL_`) →
     **no** rows (discriminator + no back-compat).
   - Multi-valued `:REL_RELATES_TO: [[id:a]] [[id:b]]` → two rows.
   - Malformed / empty value / bare `REL_` → no error, no spurious rows.

## Design rationale
The old closed list did double duty — vocabulary AND edge-discriminator.
Opening the vocabulary (OV-1) breaks the second job, so the `REL_` namespace
becomes the sole discriminator (OV-2). Underscore delimiter matches org
property-key convention and the existing hyphen→underscore transform.

## Verification
- `./bin/tangle-org.sh config/org-graph/extractor.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — parse-typed-edges spec
  passes all cases above.
- `grep -n 'REL_' config/org-graph/extractor.el` shows the prefix in
  `--rel-key`/`--key->rel` only.
- `grep -n 'org-graph-relation-types' config/org-graph/extractor.el` returns
  no membership-gate use (seed/suggestion only).

## Context
design.md § Open-Vocabulary Typed Edges (OV-1, OV-2); spec.md § Typed
Semantic Edges (open vocabulary + `REL_` discriminator).
