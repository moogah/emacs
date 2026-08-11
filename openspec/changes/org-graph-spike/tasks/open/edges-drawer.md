---
name: edges-drawer
description: Replace the REL_-prefixed properties surface with the dedicated edge drawer — parse-drawer-edges pure scanner over description-list items, drawer-name discriminator, unified enclosing-node attribution (LD-1..LD-6).
change: org-graph-spike
status: ready
relations:
  - enables:extractor-union
  - enables:parse-rel-links
cites_register_entries:
  - register/shape/typed-edge-tuple
  - register/vocabulary/relation-types
  - register/boundary/parser-extractor-db
  - register/invariant/edge-drawer-discriminator
  - register/invariant/enclosing-node-attribution
---

> Replaces `open-vocab-drawer` (properties surface, never started). Clean
> break — no notes authored against `REL_` keys. See design.md
> § Links-Drawer Edge Surface (LD-1..LD-6); research note
> `~/org/roam/20260810132157-emacs_org_typed_edges.org`.

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/extractor.org`
  (new pure drawer scanner; delete the closed-set parser, REL_ translation
  sites, and properties-ownership scoping)
- `config/org-graph/org-graph.el` ← via `config/org-graph/org-graph.org`
  (`org-graph-edge-drawer` defcustom, export exclusion,
  `org-graph-relation-types` docstring)
- `config/org-graph/test/parse-drawer-edges-spec.el` (new; replaces
  `parse-typed-edges-spec.el`)

## Implementation steps
1. Introduce `org-graph-edge-drawer` as a `defcustom` (string, group
   `org-graph`, default `"EDGES"`) in `org-graph.org`, replacing
   `org-graph-edge-property-prefix` (delete it). Do not hardcode the drawer
   name anywhere else. Warn on a customization org cannot parse as a drawer
   name (word chars, `-`, `_` only).
2. Implement `org-graph-extractor/parse-drawer-edges (element-tree)` as a
   PURE function returning `(from-id rel-type to-id)` tuples: map over
   `drawer` elements whose `:drawer-name` equals the configured name
   (case-insensitive); within each, walk description-list `item`s. The
   relation symbol is the item `:tag` trimmed, lowercased, with spaces and
   `_` mapped to `-`, interned (`- follows up ::` → `follows-up`); each
   `id:` link object in the item yields one tuple. Non-item drawer content,
   empty tags, and non-`id:` links are skipped; malformed input never
   signals.
3. **Attribution (LD-4):** `from-id` is the nearest ancestor carrying an
   `:ID:` (enclosing headline chain, else the file-level node), via the
   same `org-element-lineage` walk `parse-rel-links` specifies — extract it
   as a shared helper (e.g. `org-graph-extractor--enclosing-note-id`) if
   `parse-rel-links` has landed first, otherwise create it here for both to
   share. No ID-bearing ancestor → the drawer contributes nothing.
4. Delete the properties-surface machinery: `parse-typed-edges`, the
   allowlist membership test, `--rel-key` / `--key->rel`, and the per-note
   PROPERTIES-drawer ownership scoping (`--note-property-drawer` /
   `--edges-from-note`). Redefine `org-graph-relation-types` as a
   non-gating completion seed list (docstring update; keep the four starter
   symbols).
5. In the loader, extend `org-export-with-drawers`' exclusion list with the
   configured drawer name when that variable still has its default shape
   (LD-6); document the customized-variable override story in the defcustom
   docstring.
6. Write `parse-drawer-edges-spec.el` (drive cases off the defcustom, not a
   literal):
   - `- implements :: [[id:abc]]` in the edge drawer → one `implements`
     row.
   - `- falsifies :: [[id:abc]]` (novel, unregistered) → one row, proving
     open vocabulary.
   - `- follows up :: [[id:abc]]` → `follows-up` (multi-word tag
     normalization).
   - `- relates-to :: [[id:a]] [[id:b]]` → two rows.
   - `:SOURCE: [[id:abc]]` property and a bare body `[[id:x]]` link →
     no rows (discriminator).
   - A differently-named drawer with identical items → no rows.
   - Drawer under an ID-bearing subheading → that heading's id; drawer
     under a heading with no `:ID:` nested inside an ID-bearing ancestor →
     walks up; no ID-bearing ancestor at all → dropped.
   - Empty drawer / empty tag / non-item content / malformed → no error,
     no spurious rows.
   - One case rebinding `org-graph-edge-drawer` to a different name and
     confirming the discriminator follows it.

## Design rationale
Verified org-element behavior (research note): links in PROPERTIES values
are raw strings — no link object in the AST, no completion while authoring;
links in a named drawer are first-class objects with the relation available
structurally as the item `:tag`, so follow/completion/org-roam backlink
visibility come free and the drawer-name discriminator replaces the `REL_`
namespace hack. Attribution unifies with `parse-rel-links` (LD-4), deleting
the properties-specific ownership machinery instead of porting it.

## Verification
- `./bin/tangle-org.sh config/org-graph/extractor.org` and
  `./bin/tangle-org.sh config/org-graph/org-graph.org` validate.
- `./bin/run-tests.sh -d config/org-graph/test` — parse-drawer-edges spec
  passes all cases above, including the rebound-drawer-name case.
- `grep -n 'org-graph-edge-drawer' config/org-graph/extractor.el` shows the
  scanner reads the defcustom; `grep -n '"EDGES"' config/org-graph/extractor.el`
  returns nothing (literal lives only in the defcustom default in
  `org-graph.el`).
- `grep -n 'REL_\|edge-property-prefix\|parse-typed-edges' config/org-graph/extractor.el config/org-graph/org-graph.el`
  returns nothing.

## Context
design.md § Links-Drawer Edge Surface (LD-1..LD-6); spec.md § Typed
Semantic Edges (edge-drawer items); architecture.md § Components
(`extractor`); research note
`~/org/roam/20260810132157-emacs_org_typed_edges.org`.
