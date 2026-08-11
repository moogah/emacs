---
name: parse-rel-links
description: "Add the pure inline-link scanner parse-rel-links that extracts rel: links and attributes each to its nearest ID-bearing ancestor node (OV-3/OV-4/OV-5)."
change: org-graph-spike
status: in_progress
relations:
  - enables:extractor-union
  - blocked-by:edges-drawer
cites_register_entries:
  - register/shape/typed-edge-tuple
  - register/vocabulary/relation-types
  - register/boundary/rel-link-path-syntax
  - register/invariant/enclosing-node-attribution
---

> Batch sequencing (cycle-1786458912 plan): blocked-by `edges-drawer` so the
> extractor.org restructure and the shared ancestor-walk helper
> (`org-graph-extractor--enclosing-note-id`, LD-4) land once; this task then
> reuses the helper rather than racing to define it.

> Second authoring surface for typed edges. Pure function only — the `rel:`
> link *runtime* (follow/complete/face) is a separate task (`rel-link-type`).
> See design.md § Open-Vocabulary Typed Edges, OV-3/OV-4/OV-5.

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/extractor.org`
  (new pure scanner alongside `parse-drawer-edges`)
- `config/org-graph/test/parse-rel-links-spec.el` (new)

## Implementation steps
1. Implement `org-graph-extractor/parse-rel-links (element-tree)` as a PURE
   function returning `(from-id rel-type to-id)` tuples. It maps over `link`
   elements with `:type "rel"`, splitting each path `<type>:<target-id>` on
   the first `:` → `rel-type` (interned symbol) and `to-id`.
2. **Enclosing-node attribution (OV-4/LD-4):** `from-id` is the nearest
   ancestor carrying an `:ID:`, resolved by walking `org-element-lineage` up
   to the enclosing headline, falling back to the file-level (top) node's
   `:ID:`. Implement the walk as a shared helper (e.g.
   `org-graph-extractor--enclosing-note-id`) — the drawer scanner
   (`edges-drawer` task) uses the same rule. A `rel:` link with **no**
   ID-bearing ancestor yields **no** tuple (drop, never mis-attribute).
3. Handle: malformed path (missing `:` separator, empty type or target) →
   skip gracefully, no error.
4. Write `parse-rel-links-spec.el` first, using `org-graph-test/build-tree`
   (extend the helper to place `rel:` links in body/headings as needed):
   - link at file top → attributed to the file node's `:ID:`.
   - link under a subheading with its **own** `:ID:` → that heading's id.
   - link under a heading with **no** `:ID:`, nested inside one that has
     → walks up to the ID-bearing ancestor.
   - link with no ID-bearing ancestor at all → dropped.
   - a bare `[[id:x]]` link in the same prose → produces no edge.
   - malformed `rel:` path → no error, no row.

## Design rationale
Enclosing-node resolution (OV-4/LD-4) is the bulk of this task's test
surface, and it is no longer inline-only: LD-4 makes it the single
attribution model for both surfaces, so the ancestor walk built here is
shared with the drawer scanner. Keeping it a pure function mirrors D4/OV-5
so the extractor wrapper can union both scanners.

## Design pattern
Pure-function-over-AST with synthetic-tree tests, same contract as
`parse-drawer-edges`: `((from rel to) ...)`.

## Verification
- `./bin/tangle-org.sh config/org-graph/extractor.org` validates.
- `./bin/run-tests.sh -d config/org-graph/test` — parse-rel-links spec passes
  all attribution cases.
- Function never signals on malformed input.

## Context
design.md § Open-Vocabulary Typed Edges (OV-3, OV-4, OV-5); spec.md § Typed
Semantic Edges (inline `rel:` links, enclosing-node attribution).

## Observations

- **Registration is a parse-time precondition, verified empirically.**
  org-element (Org 9.7 / Emacs 30.1 probe) only assigns `:type "rel"` /
  `:path "<type>:<target-id>"` to a link when the `rel` type is present
  in `org-link-parameters` at the moment the AST is built; an
  unregistered `[[rel:...]]` parses as a `fuzzy` link carrying the whole
  `"rel:..."` string as its path. The scanner deliberately reads only
  the structural `:type`/`:path` split (no fuzzy-path re-parsing — that
  would duplicate the split rule and could false-positive on fuzzy
  links to headings literally named `rel:...`). See disc-parse-rel-links-1.
- **`org-graph-edge-link-type` consumed ahead of its declaration.** The
  defcustom is declared by the `rel-link-type` task; this task consumes
  it via a fail-closed helper (`org-graph-extractor--edge-link-type`)
  mirroring `--edge-drawer-name` (bare `defvar`; unbound/nil/empty →
  scanner emits nothing). Until `rel-link-type` lands, the production
  scanner is inert — harmless, since without registration no link
  parses as `rel`-typed anyway (both the defcustom and the registration
  arrive together in that task).
- **Test-scoped link-type registration.** The spec registers the link
  type via a rebound `org-link-parameters` + `org-link-make-regexps`,
  restoring the derived global regexps in `unwind-protect` — scoped
  mock at the org boundary, no persistent state. The
  unregistered-type case uses `xrel` (not `rel`) so it stays valid once
  the `rel-link-type` task registers `rel` globally in the test process.
- **Cross-surface interplay for the union task:** a `rel:` link written
  *inside* an edge-drawer item (`- implements :: [[rel:foo:abc]]`) is
  skipped by the drawer scanner (non-`id:` link) but picked up by
  `parse-rel-links` with drawer-item context ignored — the two rows a
  hybrid author might expect collapse to just the inline one. Consistent
  with each surface's contract; worth one line in the extractor-union
  docstring if it ever surprises.

## Discoveries

- discovery_id: disc-parse-rel-links-1
  class: spec-signal
  description: |
    The rel-link path contract has an implicit stage-0 precondition the
    entry does not state: the link-type name must be REGISTERED in
    org-link-parameters at AST-parse time, or org-element never
    produces the stage-2 input at all (the link parses as a fuzzy link
    with the un-stripped "rel:..." path and is invisible to the
    scanner, which skips without signal). Verified by batch probe on
    Org 9.7/Emacs 30.1 and enforced by a spec case. Consequence: any
    extraction/reindex path that parses files BEFORE the rel-link
    runtime registers the type silently yields zero inline edges — a
    load-order dependency from extraction onto the rel-link-type task's
    registration side effect.
  affected_register_entry: register/boundary/rel-link-path-syntax
  recommendation: |
    Reconcile entry: add a stage-0 "register" precondition (producer:
    rel-link-type's org-link-set-parameters call, at module load,
    before any extraction runs) and note the silent-zero failure mode.
    The extractor-union task should note the dependency where it wires
    scanners into the wrapper.
- discovery_id: disc-parse-rel-links-2
  class: interface-drift
  description: |
    The entry names org-graph-edge-link-type as "declared in
    org-graph.org", but no such defcustom exists yet — it is scheduled
    for the rel-link-type task (per that task's body). The scanner-side
    consumer landed first: it reads the variable through the fail-closed
    helper org-graph-extractor--edge-link-type (bare defvar; unbound /
    nil / empty → no link ever matches), mirroring the
    --edge-drawer-name pattern. The split-on-first-colon parse rule
    itself held up exactly as speculated (colons in target ids verbatim;
    missing separator / empty type / empty target → skip, no signal).
  affected_register_entry: register/boundary/rel-link-path-syntax
  recommendation: |
    At reconciliation, record the consumer-side helper
    (org-graph-extractor--edge-link-type, fail-closed) under the entry's
    consumers, and keep the "declared in org-graph.org" clause pointed
    at the rel-link-type task until that defcustom lands.
- discovery_id: disc-parse-rel-links-3
  class: spec-signal
  description: |
    register/invariant/enclosing-node-attribution held exactly as
    speculated: parse-rel-links reuses
    org-graph-extractor--enclosing-note-id verbatim (no second lineage
    walk; grep org-element-lineage in extractor.el resolves to the one
    helper) and all four attribution cases (file-top, ID-bearing
    subheading, ID-less heading walking up, no ID-bearing ancestor →
    dropped) pass in parse-rel-links-spec.el, matching the entry's
    enforcement_mechanism location for this spec.
  affected_register_entry: register/invariant/enclosing-node-attribution
  recommendation: |
    Entry can move speculated → reconciled once parse-rel-links merges:
    both named specs now drive the shared helper and the grep audit
    holds.
