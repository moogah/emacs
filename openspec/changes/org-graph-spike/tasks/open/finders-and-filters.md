---
name: finders-and-filters
description: Implement schema-aware per-type finder commands backed by vulpea-select with a note-type predicate.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:note-type-schemas
  - blocked-by:test-helpers
---

## Files to modify
- `config/org-graph/finders.el` ← via `config/org-graph/org-graph.org`
  (Finders section)
- `config/org-graph/test/finders-spec.el` (new)

## Implementation steps
1. Implement per-type finder commands: `org-graph/find-topic`,
   `org-graph/find-debug`, `org-graph/find-log`, `org-graph/find-reference`,
   `org-graph/find-project`, plus a catch-all `org-graph/find-any`.
2. Each finder calls `vulpea-find` (or `vulpea-select` then `vulpea-visit`)
   with a `:filter-fn` that selects notes of the given type. Reuse the
   note-type schema predicates from `note-type-schemas` as the source of
   truth for "is this note of type X" — do not re-implement filetag matching
   independently. A thin helper `org-graph/note-of-type-p (note type)` should
   delegate to the schema's `:predicate` (or `vulpea-schema-applies-p`).
3. Keep finders interactive (`;;;###autoload` not required for the spike) and
   thin — selection + visit only.
4. Write `finders-spec.el` with `org-graph-test/with-stubbed-vulpea`: stub
   `vulpea-find`/`vulpea-select` to capture the `:filter-fn`, feed a small set
   of `org-graph-test/note-fixture` notes through it, and assert the filter
   admits the right type and rejects others (including an untagged note).

## Design rationale
RE-3: finders become schema-aware. Driving the filter from the same
`vulpea-schema` predicate that validates the type keeps one definition of
"what a topic note is," instead of the original hand-rolled filetag predicate
that could drift from the taxonomy. This replaces the org-node-find-based
finder design in architecture.md.

## Design pattern
`vulpea-find` accepts `:filter-fn` `(note)->bool`. Selection candidates carry
the note id as a text property and report `(metadata (category . vulpea-note))`
— so marginalia/embark/consult annotations work for free. Test by capturing
the passed `:filter-fn` rather than asserting on UI.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — finder specs pass.
- Manual: `M-x org-graph/find-topic` lists only topic notes from `~/org/roam/`;
  `org-graph/find-any` lists all indexed notes.

## Context
design.md § Re-evaluation (RE-3); architecture.md § Components
(org-graph-finders — superseded to schema-aware).
</content>
