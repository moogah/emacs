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
1. Introduce `org-graph-edge-property-prefix` as a `defcustom` (string,
   group `org-graph`, default `"REL_"`) in `org-graph.org`. This single knob
   holds the namespace + delimiter so the ergonomics can be retuned in one
   place (OV-2 / OV-Q4). Do **not** hardcode the literal anywhere else.
2. Redefine the discriminator in terms of that defcustom. A drawer property
   is a typed edge **iff its key begins with the prefix and has a non-empty
   remainder**. `org-graph-extractor--key->rel` strips the prefix and returns
   the remainder lowercased with `_`→`-` interned as a symbol (with the
   default: `REL_RELATES_TO` → `relates-to`), else `nil`. `--rel-key` is the
   inverse: prefix + upcase(`_`←`-`) (`falsifies` → `REL_FALSIFIES`). These
   two remain the only translation sites, and both read the defcustom (build
   the match dynamically via `regexp-quote`, not a baked-in `^REL_` regex).
3. Delete the allowlist membership test entirely — no lookup against
   `org-graph-relation-types`. Any well-formed prefixed key extracts; a bare
   prefix with an empty remainder is ignored.
4. Redefine `org-graph-relation-types` as a **non-gating completion seed
   list** (docstring update only): it no longer gates extraction; it seeds
   candidate suggestions for the authoring surfaces. Keep the four starter
   symbols as its default value.
5. Update `parse-typed-edges-spec.el` (drive cases off the defcustom's
   default, not a literal, so a prefix change doesn't silently break tests):
   - `:REL_IMPLEMENTS: [[id:abc]]` → one `implements` row.
   - `:REL_FALSIFIES: [[id:abc]]` (novel, unregistered) → one `falsifies`
     row, proving open vocabulary.
   - `:SOURCE: [[id:abc]]` and `:IMPLEMENTS: [[id:abc]]` (no prefix) →
     **no** rows (discriminator + no back-compat).
   - Multi-valued `:REL_RELATES_TO: [[id:a]] [[id:b]]` → two rows.
   - Malformed / empty value / bare prefix → no error, no spurious rows.
   - One case that rebinds `org-graph-edge-property-prefix` to a different
     value and confirms the discriminator follows it.

## Design rationale
The old closed list did double duty — vocabulary AND edge-discriminator.
Opening the vocabulary (OV-1) breaks the second job, so the `REL_` namespace
becomes the sole discriminator (OV-2). Underscore delimiter matches org
property-key convention and the existing hyphen→underscore transform.

## Verification
- `./bin/tangle-org.sh config/org-graph/extractor.org` and
  `./bin/tangle-org.sh config/org-graph/org-graph.org` validate.
- `./bin/run-tests.sh -d config/org-graph/test` — parse-typed-edges spec
  passes all cases above, including the rebound-prefix case.
- `grep -n 'org-graph-edge-property-prefix' config/org-graph/extractor.el`
  shows `--rel-key`/`--key->rel` read the defcustom; `grep -n '"REL_"'
  config/org-graph/extractor.el` returns **nothing** (literal lives only in
  the defcustom default in `org-graph.el`).
- `grep -n 'org-graph-relation-types' config/org-graph/extractor.el` returns
  no membership-gate use (seed/suggestion only).

## Context
design.md § Open-Vocabulary Typed Edges (OV-1, OV-2); spec.md § Typed
Semantic Edges (open vocabulary + `REL_` discriminator).
