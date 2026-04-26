---
name: parse-typed-edges
description: Implement the pure typed-edge parser as a function over org-element AST, test-first, returning (from-id rel-type to-id) tuples.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:test-helpers"
---

## Files to modify

- `config/org-graph/test/extractor/parse-typed-edges-spec.el` (new) — Buttercup spec, written first.
- `config/org-graph/org-graph.org` (modify) — fill the `Extractor` subtree with the pure parser only (vulpea wrapper is a separate task).

## Implementation steps

1. Write the spec first. `describe "org-graph-extractor/parse-typed-edges"` with these `it` blocks:
   - returns one tuple for a single `:IMPLEMENTS: [[id:abc]]` property.
   - returns multiple tuples for a multi-valued property (`:RELATES_TO: [[id:a]] [[id:b]]`).
   - returns tuples for multiple property types in the same drawer.
   - normalizes relation type to lowercase symbol (`IMPLEMENTS` → `implements`, `RELATES_TO` → `relates-to`).
   - returns an empty list for a drawer with no relation properties.
   - ignores non-id links inside relation properties (e.g. `[[file:...]]`) and emits a `warn` (do not raise).
   - skips property values that don't parse as id links (malformed input is non-fatal).

   Use `org-graph-test/build-tree` to construct the input AST; do not write `.org` strings.

2. Define `org-graph-extractor/parse-typed-edges (element-tree note-id)` in the `Extractor` subtree. Implementation outline:
   - Find the top-level property drawer in `element-tree` via `org-element-map`.
   - For each property whose key (uppercased, underscores normalized to dashes) is in `org-graph-relation-types`:
     - Parse the value with `org-link-parse-string-list` or equivalent; collect every `id:` destination.
     - Emit `(note-id rel-type-symbol to-id)` for each.
   - Return the accumulated list. No I/O, no DB, no globals.

3. Helper: `org-graph-extractor--normalize-rel (key)` — uppercase string `"RELATES_TO"` → symbol `relates-to`. Test indirectly via the parser's normalization scenario.

4. Run tests until green: `./bin/run-tests.sh -d config/org-graph/test/extractor`.

5. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale

Factoring extraction as a pure function over `org-element` AST (design.md §D4) lets us iterate on parsing without DB setup or vulpea's plugin runtime — and lets tests run in milliseconds against in-memory ASTs. If vulpea's plugin runtime API changes, only the wrapper (next task) breaks; the parser is insulated. This is the codebase's behavioral-test discipline applied at the right boundary.

The closed initial relation-set (`implements / contradicts / supersedes / relates-to`, design.md §D3) keeps the parser surface tight; expanding it is a one-line `defcustom` change later.

## Verification

- `./bin/run-tests.sh -d config/org-graph/test/extractor` — green, all `it` blocks pass.
- `grep -n "defun org-graph-extractor/parse-typed-edges" config/org-graph/org-graph.el` — matches.
- `grep -nE "vulpea|org-id-find|find-file" config/org-graph/org-graph.el | grep -i extractor` — no matches inside the Extractor subtree (the parser must be I/O-free).

## Context

- design.md §D3, §D4
- architecture.md §Testing Approach §Pure-parser tests
- specs/org-graph/spec.md §Typed Semantic Edges
