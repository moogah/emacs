---
name: note-type-schemas
description: Define vulpea-schema note-type definitions for log, debug, topic, reference, and project with field expectations and validation.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:install-packages
  - blocked-by:test-helpers
---

## Files to modify
- `config/org-graph/schemas.el` ← via `config/org-graph/org-graph.org`
  (Note-type schemas section)
- `config/org-graph/test/schemas-spec.el` (new)

## Implementation steps
1. For each note type in `org-graph-note-types` (`log`, `debug`, `topic`,
   `reference`, `project`), register a `vulpea-schema-define`:
   ```elisp
   (vulpea-schema-define 'org-graph-topic
     :predicate (lambda (note) (member "topic" (vulpea-note-tags note)))
     :fields '((:key "ID" :required t)
               (:key "TITLE" :type string :required t)
               ;; type-specific expectations...
               ))
   ```
   The `:predicate` selects which notes the schema applies to (typically by
   filetag membership). The filetag becomes one validated field among others
   — it is the selector, not the whole taxonomy.
2. Encode type-specific expectations as fields, e.g. `reference` requires a
   source/URL meta key; `project` may expect a status; `log`/`debug` are
   lighter. Keep the initial field sets small and honest — this is a spike.
3. Provide `org-graph/validate-note-type (note)` and
   `org-graph/validate-all-of-type (type)` thin wrappers over
   `vulpea-schema-validate` / `vulpea-schema-validate-all` returning the list
   of `vulpea-violation`s.
4. Write `schemas-spec.el` using `org-graph-test/with-stubbed-vulpea`:
   - a conformant fixture note yields zero violations;
   - a note missing a required field yields a `missing-required` violation;
   - the predicate selects only matching-tag notes.

## Design rationale
RE-3: note-type taxonomy is backed by vulpea 2.4's `vulpea-schema` system
rather than a bare filetag convention. `vulpea-schema-define` /
`vulpea-schema-validate` give declarative, validated note shapes for free, so
finders can be schema-aware and the agent can be told what a well-formed note
of each type looks like. `vulpea-schema` is validation-only — it does NOT
create DB tables (that is the extractor's job) — so this task is independent
of the typed-edge extractor.

## Design pattern
`vulpea-schema-define` signature (from vulpea 2.4 source):
`(cl-defun vulpea-schema-define (name &key predicate fields))`. Field plist
keys: `:key` (string, required), `:type`
(`string`|`number`|`symbol`|`note`|`link`), `:required`, `:one-of`,
`:multiple`, `:validate`. Violation types: `missing-required`, `wrong-type`,
`invalid-reference`, `disallowed-value`, `invalid-value`.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — schema specs pass.
- Manual: `M-x` a command that runs `org-graph/validate-all-of-type` against
  `~/org/roam/` and reports violation counts; confirm it does not error on
  real notes.

## Context
design.md § Re-evaluation (RE-3); proposal.md § What Changes
(schema-backed note-type taxonomy).
</content>
