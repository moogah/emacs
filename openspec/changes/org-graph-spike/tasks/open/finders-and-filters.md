---
name: finders-and-filters
description: Implement schema-aware per-type finder commands backed by vulpea-select with a note-type predicate.
change: org-graph-spike
status: ready
relations:
  - blocked-by:note-type-schemas
  - blocked-by:test-helpers
cites_register_entries:
  - register/vocabulary/note-type-taxonomy
---

## Files to modify
- `config/org-graph/finders.el` ← via a **new** `config/org-graph/finders.org`
  (own literate module, matching the one-org-per-el convention; NOT a section
  in `org-graph.org`)
- `config/org-graph/org-graph.org` (loader) — append the `finders` module to the
  existing require/load sequence; **append-only**, do not reorder existing loads
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

## Cycle 1782551613 updates (cycle-1782551613)
> Unblocked: blockers `note-type-schemas` and `test-helpers` are done. Status flipped blocked → ready.

Absorb from `note-type-schemas` (now merged) + the register diff:
- **`register/vocabulary/note-type-taxonomy` (confirmed):** the five schemas
  `org-graph-{log,debug,topic,reference,project}` exist (feature
  `org-graph-schemas`), each selected by a filetag-membership `:predicate`.
  Build finders on these predicates, not a hand-rolled filetag match.
- **agent-draft is NOT a note type** — it is a cross-cutting filetag stamped by
  the write tool. The agent-draft *review* finder (`org-graph/find-agent-drafts`)
  filters on that filetag directly; do not route it through the type schemas.
- The wrappers `org-graph/validate-note-type` / `org-graph/validate-all-of-type`
  already exist; reuse them rather than re-deriving validation.
- **Helper gap:** `org-graph-test/with-stubbed-vulpea` does not stub
  `vulpea-schema-validate-all`; `note-type-schemas` added a local `cl-letf` in
  its spec — follow that pattern (or extend the helper if it's clearly the right
  home, and note it for review).

## Cycle 1782561220 updates (cycle-1782561220)
Plan-phase decisions:
- **New module `finders.org`.** `finders.el` did not exist; the repo convention
  is one `.org` per `.el`. Create `config/org-graph/finders.org` tangling to
  `finders.el`. Its **first** babel block MUST use `:comments no` so
  `;;; finders.el --- ... -*- lexical-binding: t; -*-` lands on line 1
  (`register/invariant/lexical-binding-line-1`). Loader registration in
  `org-graph.org` is **append-only** (three tasks touch the loader this cycle —
  do not reorder).
- **Test-helper contention:** another batch task (`scope-extractor-edges-per-note`)
  is extending the shared `org-graph-test/build-tree` helper. To avoid a merge
  seam, prefer the **local `cl-letf`** pattern in your spec (per the helper-gap
  note above) over editing the shared helpers file.
- Stage source/test files explicitly when committing — do NOT `git add -A`.

## Observations
- Implemented `finders.org` → `finders.el` as a standalone literate module
  (first block `:comments no`, lexical-binding on line 1). Six finders plus a
  thin `org-graph/note-of-type-p` helper and two private filter builders
  (`org-graph-finders--type-filter`, `org-graph-finders--agent-draft-p`).
- `org-graph/note-of-type-p` delegates to `vulpea-schema-applies-p` over the
  schema name from `org-graph-schemas--schema-name`, guarded by
  `vulpea-schema-get` (mirrors `org-graph/validate-note-type`'s guard). No
  hand-rolled filetag matching for the typed finders.
- All finders use `vulpea-find :require-match t` (selection+visit of existing
  notes; finders navigate, they do not create). `find-any` passes no
  `:filter-fn`, falling back to `vulpea-find-default-filter`.
- `org-graph/find-agent-drafts` filters the `agent-draft` filetag directly via
  `vulpea-note-tags`, deliberately bypassing the type schemas (agent-draft is
  cross-cutting, not a taxonomy member).
- Test approach per the helper-gap note: `vulpea-find` is not covered by
  `org-graph-test/with-stubbed-vulpea`, so a local `cl-letf` captures the args
  plist; the real schemas are registered and the genuine
  `vulpea-schema-applies-p` runs through the captured `:filter-fn`. Did not
  touch shared `helpers-spec.el` (avoids the seam with the concurrent
  `build-tree` extension).
- Loader registration is append-only: added one `jf/load-module` line for
  `finders.el` to the Submodules block, with a comment noting the
  schemas-before-finders ordering requirement.
- Verification: `./bin/run-tests.sh -d config/org-graph` green — 73 specs, 0
  failed (baseline 60 + 13 new finder specs).

## Discoveries
- class: invariant-gap
  affected_register_entry: register/vocabulary/note-type-taxonomy
  summary: |
    `finders.el` `(require 'org-graph-schemas)` can ONLY be satisfied if
    schemas.el was already loaded (feature provided), because the file basename
    is `schemas.el`, not `org-graph-schemas.el` — `require` can never auto-load
    it from `load-path`. The loader (`org-graph.org` Submodules block) currently
    contains NO real `jf/load-module` calls for any submodule (schemas, extractor,
    etc. are not wired). I appended the finders load append-only, but there is no
    preceding schemas load, so loading `org-graph.el` today would fail at the
    finders `require`. This is latent (org-graph is not in `jf/enabled-modules`
    yet — vulpea/org-graph wiring is the `wire-into-init` task's job). Flagging so
    wire-into-init establishes the ordered load sequence (schemas → finders) and
    actually wires the submodules. The finder spec is unaffected: it requires
    schemas.el by path before finders.el.
- class: scope-question
  summary: |
    All finders use `:require-match t` (visit-existing semantics). The task said
    "selection + visit only," which I read as no creation; `:require-match t`
    enforces that. If a future finder should support quick-create (vulpea-find's
    create-fn path), that is a deliberate follow-up, not a silent default.
