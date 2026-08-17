---
name: parse-typed-edges
description: Implement the pure typed-edge parser as a function over org-element AST, test-first, returning from-rel-to tuples.
change: org-graph-spike
status: done
relations:
  - blocked-by:test-helpers
---

## Files to modify
- `config/org-graph/extractor.el` ← via `config/org-graph/org-graph.org`
  (Typed-edge parser section) — the pure-function part only
- `config/org-graph/test/parse-typed-edges-spec.el` (new)

## Implementation steps
1. Implement `org-graph-extractor/parse-typed-edges (element-tree note-id)`
   as a PURE function: read the note's PROPERTIES drawer, extract entries
   whose key matches a configured relation type (`org-graph-relation-types`:
   `IMPLEMENTS`, `CONTRADICTS`, `SUPERSEDES`, `RELATES_TO`), parse each
   value's `[[id:...]]` link(s), and return a list of
   `(FROM-ID REL-TYPE TO-ID)` tuples. `FROM-ID` is `note-id`.
2. Handle: single-valued property, multi-valued property (multiple links in
   one value, space-separated), multiple relation properties on one note,
   malformed/empty values (skip gracefully, no error), and a property key that
   is not a configured relation type (ignore).
3. Normalise the relation type to a symbol (`IMPLEMENTS` → `implements`) and
   validate it against `org-graph-relation-types`.
4. No file I/O, no vulpea, no DB. The function takes a parsed tree and an id,
   returns tuples. This is the unit under test.
5. Write `parse-typed-edges-spec.el` first (test-first), using
   `org-graph-test/build-tree` to construct synthetic drawers; cover every
   case in step 2.

## Design rationale
D3/D4 stand under the re-evaluation (RE-4): the PROPERTIES-drawer convention
and a pure parser are still correct. vulpea's native link `:type` is
link-KIND (id/file/https), not semantic relation-KIND, so semantic edges must
be parsed by us. Keeping the parser pure (separable from vulpea's plugin
runtime) matches the codebase's behavioral-test convention and lets the
extractor wrapper change independently if vulpea's plugin API evolves.

## Design pattern
Pure-function-over-AST with synthetic-tree tests, exactly as in design.md D4.
Return value shape is the contract the extractor task consumes:
`((from rel to) ...)`.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — the parser spec passes with
  full case coverage (single, multi-valued, multi-property, malformed, empty,
  non-relation key).
- The function never signals on malformed input — it returns the tuples it
  could parse.

## Context
design.md § Decisions D3, D4; design.md § Re-evaluation (RE-4).
</content>
