---
name: note-type-schemas
description: Define vulpea-schema note-type definitions for log, debug, topic, reference, and project with field expectations and validation.
change: org-graph-spike
status: done
relations:
  - blocked-by:install-packages
  - blocked-by:test-helpers
cites_register_entries:
  - register/vocabulary/note-type-taxonomy
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

## Orchestrator brief addenda (cycle-1782551613)

From the foundation Architect audit; cited register entry in `interfaces.org`.

- **`agent-draft` is NOT a member of `register/vocabulary/note-type-taxonomy`.**
  It is a cross-cutting filetag the agent write-tool stamps, not a note type.
  Do NOT define an `org-graph-agent-draft` schema alongside the five types
  (log/debug/topic/reference/project). Conflating them would break both the
  per-type finders and the future draft-review finder.

- **Helper coverage gap.** `org-graph-test/with-stubbed-vulpea` stubs
  `vulpea-schema-validate` but NOT `vulpea-schema-validate-all`. Your
  `validate-all-of-type` wrapper (step 3) will need its own `cl-letf` stub, or
  extend the helper. Prefer a local `cl-letf` in the spec unless you find the
  helper is the right home (if so, that's an additive helper change worth a
  note for the reviewer).

## Observations

- Implemented in a NEW submodule `config/org-graph/schemas.org` (→ `schemas.el`,
  feature `org-graph-schemas`), following the extractor.org/coordinator.org
  pattern. Did NOT touch `org-graph.org` (load-wiring deferred to
  `wire-into-init`/`module-load-smoke`).
- Five schemas registered (`org-graph-{log,debug,topic,reference,project}`) via
  a `org-graph-schemas-register` function called at load. Honest, minimal field
  sets: `reference` requires `source`; `project` requires `status`
  (`:one-of (active paused done)`); `log`→optional `date`, `debug`→optional
  `status` (`:one-of (open resolved)`), `topic`→optional `category`.
- No `org-graph-agent-draft` schema (per addenda) — a spec asserts its absence.
- Wrappers named exactly per task body: `org-graph/validate-note-type (note)`
  (DB-free; loops `org-graph-note-types`, validates against every applying
  schema) and `org-graph/validate-all-of-type (type)` (delegates to
  `vulpea-schema-validate-all`).
- 12 new specs in `config/org-graph/test/schemas-spec.el`; full suite green at
  34 specs (22 baseline + 12), 0 failed.

## Discoveries

- class: register-confirmation
  affected_register_entry: register/vocabulary/note-type-taxonomy
  detail: The speculated taxonomy held exactly as written. The five members
    (log/debug/topic/reference/project) map 1:1 to schemas; the
    consumer_mapping (`config/org-graph/schemas.el`, per-type `:predicate`
    lambda doing filetag membership) is now implemented as described; and the
    "agent-draft is NOT a member" warning was honored. RECOMMEND
    speculated -> confirmed. Note for register accuracy: the consumer_field
    "filetag membership" predicate uses `vulpea-note-tags` (filetags live in
    the note's `tags` slot), whereas schema *fields* read note *metadata* via
    `vulpea-note-meta-get` — so the filetag is encoded ONLY in the predicate
    (the selector), never as a validated field. The task's prose "the filetag
    is one validated field" is imprecise: with vulpea 2.4's engine a filetag
    cannot be a schema field. Implemented faithfully to the engine: predicate =
    filetag, fields = honest meta keys.

- class: api-confirmation
  affected_register_entry: (none)
  detail: Verified against vulpea 2.4 source
    (`runtime/straight/repos/vulpea/vulpea-schema.el`). Signature
    `(cl-defun vulpea-schema-define (name &key predicate fields))` and field
    keys (`:key :type :required :one-of :multiple :validate`) and violation
    types (`missing-required wrong-type invalid-reference disallowed-value
    invalid-value`) match the task body's "Design pattern" exactly. The body's
    example fields `:key "ID"`/`:key "TITLE"` would be read from note METADATA
    (not the ID property / #+title), so I did not encode ID/TITLE as fields —
    they are not honest meta keys in this engine.

- class: deviation
  affected_register_entry: (none)
  detail: Step 4 says "use `org-graph-test/with-stubbed-vulpea`" (which stubs
    `vulpea-schema-define`/`vulpea-schema-validate`). I deliberately did NOT
    stub the validation engine: it is pure/DB-free for our field sets, and
    stubbing it would make the field-spec assertions vacuous (testing the stub,
    not the schema). Instead the specs run the REAL define/validate against
    real fixtures and mock ONLY the DB boundary — a local `cl-letf` on
    `vulpea-db-query` for `validate-all-of-type` (which applies the predicate to
    an in-memory note list). This honors "mock at the API boundary; no real
    DB" while keeping the assertions meaningful. Did NOT extend the shared
    helper — the local `cl-letf` is the right home (only this spec needs it).
