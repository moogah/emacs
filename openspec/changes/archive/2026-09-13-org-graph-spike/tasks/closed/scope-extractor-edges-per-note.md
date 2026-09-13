---
name: scope-extractor-edges-per-note
description: Decide and implement the typed-graph granularity model (file-level-only vs note-granular) so the extractor attributes edges to the authoring note, not the whole file.
change: org-graph-spike
status: done
relations:
  - discovered-from:vulpea-extractor-plugin
cites_register_entries:
  - register/shape/typed-edge-tuple
  - register/boundary/parser-extractor-db
discovered_by: reviewer
discovered_class: spec-signal
---

> **User decision (cycle-1782551613, ask-cycle-1782551613-1): NOTE-GRANULAR.**
> Implement option (B). ID'd heading concept-notes may carry their own typed
> edges. Scope extraction to the note actually being processed (parse the note's
> own subtree, or read its vulpea-extracted `:properties` — mind repeated-key
> handling). **Remove** the interim file-level-only guard
> (`org-graph-extractor--file-level-note-p`) shipped in cycle-1782551613, and add
> a multi-note regression test. The guard is a safe-but-incomplete stopgap
> (suppresses heading edges) and MUST be replaced, not kept. Reconcile
> `register/boundary/parser-extractor-db` and `register/shape/typed-edge-tuple`
> divergent → confirmed once the per-note scoping lands with the multi-note test.
> This task is now READY (unblocked by the user model decision).

## Why
Author-blind reviewer finding `arch/review-vulpea-extractor-plugin-1` (major,
fix-and-spec-signal). vulpea invokes the `:extract-fn` **once per ID-bearing
note** — the file-level node AND every ID'd heading
(`vulpea-db-index-heading-level` defaults to `t`). The pure parser
(`org-graph-extractor/parse-typed-edges`) walks the **whole-file AST**, so
without scoping, every note (each heading) is credited with every edge in the
file, inserted once per note — N× duplication + wrong `from-id` attribution.
This violates `register/shape/typed-edge-tuple` ("from-id … always NOTE-ID")
and stage 1 of `register/boundary/parser-extractor-db`.

A **defensive inline fix** already shipped in cycle-1782551613 (commit
92b26816): the extractor now emits edges only for the **file-level note**
(vulpea's file-node plist carries no `:level` key). Under the spike's
file-level concept-note convention (D2/D3) this is correct and removes the
duplication. This task resolves the deeper **model decision** and, if needed,
the note-granular implementation.

## User model decision (BLOCKING — spec-signal)
Pick the typed-graph granularity:

- **(A) File-level-only.** Typed edges live only in the file-level PROPERTIES
  drawer; ID'd headings never carry typed edges. The shipped guard already
  enforces this. Work: document + enforce (optionally reject/ignore relation
  keys found in heading drawers), and mark `register/boundary/parser-extractor-db`
  + `register/shape/typed-edge-tuple` `confirmed` with the file-level scope
  noted.
- **(B) Note-granular.** ID'd heading concept-notes may carry their own typed
  edges. Work: scope extraction to the note actually being processed — either
  parse the note's own subtree/headline, or read edges from `note-data`'s
  vulpea-extracted `:properties` alist (mind repeated-key handling: the
  whole-AST parser preserves repeated relation keys; a properties-alist may
  collapse them). Remove the file-level-only guard; keep the per-note `from-id`
  contract. Update both register entries accordingly.

## Implementation steps (once the model is chosen)
1. If (B): add `org-graph-extractor--edges-from-note` (subtree- or
   `:properties`-scoped) and route `extract` through it; preserve the
   `(FROM-ID REL-TYPE TO-ID)` symbol-rel-type tuple contract.
2. Add a multi-note regression spec: a tree with a file-level relation drawer
   AND an ID'd heading with its own relation drawer; assert each note gets
   ONLY its own edges (no duplication, correct `from-id`). The current
   `org-graph-test/build-tree` only emits a single file-level drawer — extend
   it to build heading nodes.
3. Reconcile `register/boundary/parser-extractor-db` (stage 1/2) and
   `register/shape/typed-edge-tuple` from `divergent` → `confirmed`.

## Verification
- `./bin/run-tests.sh -d config/org-graph` green, including the new multi-note test.
- Manual: a roam note with an `:IMPLEMENTS:` file-level drawer and an unrelated
  ID'd heading produces exactly one edge row attributed to the file id.

## Context
Reviewer finding `.orchestrator/cycles/cycle-1782551613/reviews/vulpea-extractor-plugin.md`;
design.md D2/D3/RE-4; `config/org-graph/extractor.org` (extractor-wrapper);
register entries `parser-extractor-db`, `typed-edge-tuple` (status: divergent).

## Cycle 1782561220 updates (cycle-1782561220)
The model decision is **resolved: NOTE-GRANULAR (B)** — no further user ask;
implement directly. Plan-phase scoping notes:
- Work lands in `config/org-graph/extractor.org` (extractor-wrapper) +
  `org-graph-test/build-tree` extension + the new multi-note spec. You do **not**
  touch the loader (`org-graph.org`) — the three other batch tasks own loader
  edits.
- This task **reconciles** the cycle's only divergent boundary
  (`register/boundary/parser-extractor-db`) and re-confirms
  `register/shape/typed-edge-tuple`'s "from-id always NOTE-ID" contract. At
  integrate these move `divergent → confirmed`; record the reconciliation note.
- `typed-edge-query` (same batch) reads the typed_edges table but builds its
  fixtures directly — your attribution change does not collide with its specs.
- Stage files explicitly (no `git add -A`).

## Observations
- **Scoping mechanism chosen: AST-subtree, not `:properties` alist.** The
  note-granular scope is implemented by `org-graph-extractor--note-property-drawer`,
  which locates the `property-drawer` whose `:ID:` node-property equals the
  note id, and `org-graph-extractor--edges-from-note`, which re-runs the
  *existing pure parser* (`parse-typed-edges`) over that single drawer
  sub-tree. This deliberately avoids reading `note-data`'s `:properties`
  alist: the whole-AST parser preserves *repeated* relation keys (two
  `:IMPLEMENTS:` → two edges), whereas a properties alist collapses a repeated
  key to one pair and would silently drop edges. A regression spec asserts the
  repeated-key case end-to-end through `extract`.
- **`:level` is no longer consulted.** Attribution now follows whichever drawer
  owns the id, uniformly for the file node and heading nodes. The removed guard
  keyed on `(null (plist-member note-data :level))`; matching by `:ID:` is
  strictly more precise and needs no special-casing of the file node.
- **The scope-gate invariant is untouched.** `org-graph-extractor--roam-note-p`
  still runs first in `extract`; note-granular only changes *which node* owns an
  edge, never *whether* non-roam notes are excluded. All five scope-gate specs
  (roam→tuples, non-roam→none, subdir→in, unset→closed, empty→closed) still pass.
- **An ID'd note that owns no drawer contributes nothing.** Matching by `:ID:`
  means a heading vulpea indexed but that authored no relations of its own
  yields no edges and is never mis-credited with the file's edges. The old
  "HEADING note inserts nothing" spec was repurposed to assert exactly this
  (a note whose id matches no drawer), since the file-node-only rationale is gone.
- **`build-tree` gained a `:headings` key** (list of heading plists: `:id`,
  `:title`, `:level`, `:properties`) plus a shared `org-graph-test/--drawer`
  helper, so specs can construct the multi-note file vulpea actually indexes.
  A helper self-test asserts both the file-level and heading drawers carry their
  own id.
- Suite: `./bin/run-tests.sh -d config/org-graph` → **66 specs, 0 failed**
  (was 60 at task entry; +6 = 1 helper self-test, 4 multi-note attribution
  specs, and the repurposed no-drawer spec replacing the file-node-only spec).

## Discoveries

```yaml
discoveries:
  - id: note-granular-via-ast-subtree
    kind: implementation-decision
    summary: |
      Note-granular scoping re-runs the pure AST parser over the single
      property-drawer whose :ID: matches the note, rather than reading
      note-data :properties. Preserves repeated relation keys; keeps the
      parser->extractor->DB contract on one representation (org-element AST).
    affected_register_entry: register/boundary/parser-extractor-db
    rationale: |
      A vulpea :properties alist may collapse a repeated key, dropping edges.
      The AST sub-tree keeps repeated node-property entries distinct.

  - id: reconcile-parser-extractor-db
    kind: register-reconciliation
    affected_register_entry: register/boundary/parser-extractor-db
    recommendation: "divergent -> confirmed"
    summary: |
      The open model decision is resolved (NOTE-GRANULAR). Stage 2 (scope-gate)
      now scopes per-note: extract routes through
      org-graph-extractor--edges-from-note, which parses ONLY the note's own
      :ID:-matched drawer. Multi-note files no longer duplicate or mis-attribute
      edges; from-id is the authoring note. Stages 1 (parse) and 3 (db-insert)
      unchanged and already confirmed. Recommend status -> confirmed with a
      stage-2 note: "scope-gate now also performs per-note attribution via the
      :ID:-matched drawer sub-tree (note-granular); file-level-only guard removed."
      The cross_stage_invariant (typed-edge-extraction-scope) still holds — roam
      exclusion is unchanged.

  - id: reconfirm-typed-edge-tuple
    kind: register-reconciliation
    affected_register_entry: register/shape/typed-edge-tuple
    recommendation: "reconfirmed (stays confirmed)"
    summary: |
      The (from-id rel-type to-id) tuple is unchanged: from-id is ALWAYS the
      NOTE-ID actually being processed (now the heading id for heading notes,
      the file id for the file node), rel-type a SYMBOL, to-id a string. The
      multi-note specs assert from-id = the authoring note's id for both node
      kinds, re-confirming the "from-id always NOTE-ID" clause under the
      note-granular world. No shape change.

  - id: why-foundation-tests-missed-misattribution
    kind: test-gap-analysis
    why_tests_missed: |
      The foundation (cycle-1782551613) extractor specs built only SINGLE-NOTE
      trees via org-graph-test/build-tree, which emitted exactly one file-level
      PROPERTIES drawer and no headings. With one drawer per file, "parse the
      whole-file AST" and "parse this note's drawer" are indistinguishable, so
      the N×-duplication / wrong-from-id defect was structurally unreachable by
      the fixtures. The interim file-node-only guard was likewise validated only
      by a heading note whose id was ABSENT from the single-drawer tree (so it
      got nothing for the trivial reason of no matching drawer, not because of
      level-based suppression). The gap was a fixture-shape gap: no test
      constructed a file with a file-level drawer AND an independent ID'd-heading
      drawer. This task closes it by extending build-tree with :headings and
      adding multi-note attribution specs that fail under whole-file extraction.
```
