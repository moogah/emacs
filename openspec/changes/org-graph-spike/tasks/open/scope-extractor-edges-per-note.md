---
name: scope-extractor-edges-per-note
description: Decide and implement the typed-graph granularity model (file-level-only vs note-granular) so the extractor attributes edges to the authoring note, not the whole file.
change: org-graph-spike
status: blocked
relations:
  - discovered-from:vulpea-extractor-plugin
cites_register_entries:
  - register/shape/typed-edge-tuple
  - register/boundary/parser-extractor-db
discovered_by: reviewer
discovered_class: spec-signal
blocked_by: user-model-decision
---

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
