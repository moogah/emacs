---
name: edges-drawer
description: Replace the REL_-prefixed properties surface with the dedicated edge drawer — parse-drawer-edges pure scanner over description-list items, drawer-name discriminator, unified enclosing-node attribution (LD-1..LD-6).
change: org-graph-spike
status: needs_review
relations:
  - enables:extractor-union
  - enables:parse-rel-links
merge_commit: 901756eb
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

## Observations

- `org-graph-edge-property-prefix` never existed to delete: the
  superseded `open-vocab-drawer` task was never started, so step 1's
  "replacing org-graph-edge-property-prefix (delete it)" was a no-op.
  The defcustom was introduced fresh.
- The wrapper needed the per-note from-id filter NOW, not at
  `extractor-union`: the new scanner walks the whole-file AST and
  attributes internally, while vulpea invokes `:extract-fn` once per
  ID-bearing note — without the filter every multi-note file
  immediately reproduces the cycle-1782551613 N-fold duplication.
  Implemented `(seq-filter (from-id = note-id))` in
  `org-graph-extractor/extract`; `extractor-union` inherits it pinned
  and reduces to adding the `parse-rel-links` leg + union/dedup.
- Bumped the extractor `:version` 1→2 (parser-epoch discipline): the
  properties→drawer swap changes scanner output for identical file
  content, so vulpea's content-hash cache must be invalidated or
  unchanged files would keep stale `typed_edges` rows. Added a spec
  asserting version 2. (Slight scope expansion beyond the task's step
  list, but required for correctness of the swap itself.)
- Also deleted `org-graph-extractor--ids-in-value` (regexp id-link
  extraction over property-value strings) — not named in step 4's
  delete list but dead once the properties surface is gone; drawer
  links are first-class AST link objects.
- extractor-spec's "vocabulary single-sourcing" describe was deleted
  along with `org-graph-extractor--default-relation-types` /
  `--relation-types`: with the allowlist gone there is no second
  vocabulary declaration left to drift.
- Test-infrastructure growth: `helpers-spec.el` gained `:body` support
  in `org-graph-test/build-tree` (file-level and per-heading raw org
  text) and a shared `org-graph-test/edge-drawer-text` builder whose
  drawer name defaults to the live `org-graph-edge-drawer` value — both
  parse-drawer-edges-spec and extractor-spec build fixtures through it,
  so neither hardcodes the drawer literal.
- Empty-tag robustness comes from org itself: `- :: [[id:x]]` parses as
  an UNTAGGED item (`:tag` nil), so the "empty tag skipped" contract
  falls out of the parser; `--normalize-rel` still guards
  whitespace-only tags.
- `config/org-graph/docs/spike-eval.org:267` still carries the
  closed-set framing ("extend org-graph-relation-types") — owned by
  `spike-eval-doc-update`, left untouched.
- LD-6 verified end-to-end in the real load path: after `(require 'ox)`
  under the loader, `org-export-with-drawers` = `(not "LOGBOOK"
  "EDGES")`; the defcustom `:set` warning fires on an invalid name
  (e.g. `"BAD NAME"`) while still setting the value (warn, not reject).

## Discoveries

- discovery_id: disc-edges-drawer-1
  class: vocabulary-mismatch
  description: |
    The normalization rule's whitespace class is now pinned. The entry's
    canonical_mapping_function used "[ _]+" (spaces and underscores
    only) and noted "exact whitespace-run collapsing pinned at
    reconciliation". Implementation pins "[[:space:]_]+": any
    whitespace run (spaces, tabs) plus underscores collapses to a
    single hyphen. Trim/downcase/intern unchanged.
  affected_register_entry: register/vocabulary/relation-types
  recommendation: |
    Reconcile canonical_mapping_function (and validator) to
    "[[:space:]_]+"; helper landed as
    org-graph-extractor--normalize-rel in extractor.el, the single
    normalization site both scanners must share.

- discovery_id: disc-edges-drawer-2
  class: spec-signal
  description: |
    Stage 2's SPECULATED note-granularity filter is confirmed and
    already landed: the wrapper keeps only tuples whose from-id equals
    the note vulpea is processing. It could not wait for
    extractor-union — whole-file scanning + per-note :extract-fn
    invocation reproduces the cycle-1782551613 duplication divergence
    without it (extractor-spec's multi-note cases prove the filter).
  affected_register_entry: register/boundary/parser-extractor-db
  recommendation: |
    Pin the stage-2 filter as implemented in
    org-graph-extractor/extract; retarget extractor-union to "add
    parse-rel-links leg + union/dedup of the two streams" only.
    Also note stage 4's registration is no longer verbatim-unchanged:
    extractor :version bumped 1→2 for the surface swap (parser-epoch
    cache invalidation; spec-asserted).

- discovery_id: disc-edges-drawer-3
  class: spec-signal
  description: |
    The enclosing-node walk reads ancestors' ids structurally: org-element
    (>= 9.6) caches each node's own PROPERTIES-drawer entries as element
    properties, so the shared helper checks (org-element-property :ID
    node) on `headline` lineage nodes and on the root `org-data` (which
    carries the file-level drawer's :ID) — no manual drawer digging.
    Verified by batch probe on the bundled Org. Semantics match the
    invariant exactly (nearest ID-bearing ancestor, else file node,
    else drop); org-element-lineage appears in exactly one helper,
    org-graph-extractor--enclosing-note-id.
  affected_register_entry: register/invariant/enclosing-node-attribution
  recommendation: |
    Reconcile the entry with the mechanism (cached :ID element
    properties incl. org-data; Org >= 9.6 floor) so parse-rel-links
    reuses the landed helper without re-deriving the walk.

- discovery_id: disc-edges-drawer-4
  class: interface-drift
  description: |
    The tuple entry's producer note held: parse-drawer-edges landed with
    signature (ELEMENT-TREE), from-id resolved internally, tuple shape
    (string symbol string) unchanged — extractor-spec storage-shape and
    parse-drawer-edges-spec assert it. The parse-rel-links producer leg
    remains speculated (that task has not landed; the wrapper currently
    runs the drawer scanner only).
  affected_register_entry: register/shape/typed-edge-tuple
  recommendation: |
    Flip the parse-drawer-edges producer annotation speculated →
    confirmed; leave parse-rel-links speculated until its task lands.

## Merge notes (orchestrator)

- Conflict in `org-graph.org` (Submodule map region) between this branch and
  the already-merged `org-graph-loader-cleanup` pruning: resolved by keeping
  the pruned single-paragraph form; this branch's edits inside the deleted
  placeholder sections were prose-only and are subsumed. `.el` regenerated by
  re-tangle after resolution. No code dropped.
