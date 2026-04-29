---
name: add-drawer-corruption-regression
description: Regression test that an add-to-scope sequence produces exactly one PROPERTIES drawer with no duplication
change: gptel-scope-in-org-properties
status: ready
relations: []
---

## Cites register entries

- `register/invariant/scope-drawer-no-duplication` — this task IS the structural enforcement of that invariant. The existing scaffold under `scaffolding/invariants/scope-drawer-no-duplication.test.el` should either be promoted to its permanent location at `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el` (and dispositioned `promoted` at integrate) OR superseded by the regression spec you write here (and dispositioned `archived` at integrate). Pick one approach and document in `## Discoveries`.
- `register/boundary/scope-pattern-writer` — exercising the writer is the regression path.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el` — the file you may promote.
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-pattern-writer.el`

## Files to modify
- `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el` (new) — regression test exercising the drawer writer through several add-to-scope cycles and asserting the buffer contains exactly one `:PROPERTIES: ... :END:` block.

## Implementation steps

> Cycle 1: original example used `:deny` operation; cycle-1 inline fix (finding-8) removed the `:deny` arm from `--map-operation-to-drawer-key` — `:deny` is no longer a valid input to `--write-pattern-to-drawer`. Replaced with `:execute` to preserve the multi-key-write flow. See `.orchestrator/cycles/cycle-1777460733/findings/arch-cycle-1777460733-8.md`.

1. Create the file with a single `describe` group:

   ```elisp
   ;;; no-duplicate-drawer-spec.el --- Regression: drawer is never duplicated -*- lexical-binding: t; -*-

   (require 'buttercup)
   (require 'org)
   ;; Load helpers and the drawer writer.

   (describe "drawer writer integrity"
     (it "produces exactly one :PROPERTIES: block after multiple add-to-scope writes"
       (jf/gptel-test--with-scope-drawer '((:GPTEL_PRESET . "executor")
                                            (:GPTEL_SCOPE_READ . ("/initial/**")))
         (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/one/**")
         (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/two/**")
         (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :write "/output/**")
         (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :execute "/usr/local/bin/**")
         (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                (drawer-count (cl-count-if
                               (lambda (line) (string-match-p "^[ \t]*:PROPERTIES:[ \t]*$" line))
                               (split-string content "\n"))))
           (expect drawer-count :to-equal 1))))

     (it "preserves :GPTEL_PRESET: across writes"
       (jf/gptel-test--with-scope-drawer '((:GPTEL_PRESET . "executor")
                                            (:GPTEL_SCOPE_READ . ("/initial/**")))
         (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/**")
         (expect (org-entry-get (point-min) "GPTEL_PRESET")
                 :to-equal "executor")))

     (it "is idempotent for duplicate patterns"
       (jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_READ . ("/a/**")))
         (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/a/**")
         (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
                 :to-equal '("/a/**")))))
   ```

2. Run the new spec in isolation: `./bin/run-tests.sh -d config/gptel/scope/test/drawer`.

3. Run the full scope suite to confirm no cross-contamination: `./bin/run-tests.sh -d config/gptel/scope`.

## Design rationale

The `gptel-org-mode-sessions` change resolved a duplicate-`:PROPERTIES:`-drawer corruption (see `handoff-property-drawer-corruption.md`). Loading more state into the same drawer doubles the surface area for any regression of that bug class. This spec is the canary: a sequence that resembles a real add-to-scope chain (initial drawer with preset + scope, then several writer invocations) must end with exactly one drawer.

The other two `it` blocks pin two of the writer's invariants (preserve unrelated keys, idempotency). Both are also covered in `migrate-expansion-tests`, but having them here as a regression spec means a single targeted test failure points directly at writer integrity.

## Design pattern

The test uses `cl-count-if` over split lines to count `:PROPERTIES:` headers. That's slightly unusual — normally you'd just `re-search-forward` and count matches — but the line-counting form is explicit about what it asserts ("exactly one drawer header line"). Either form is acceptable.

## Verification

- `./bin/run-tests.sh -d config/gptel/scope/test/drawer` passes.
- The three `it` blocks all green.
- A deliberate regression (e.g. having the writer call `org-insert-drawer` instead of `org-entry-put`) makes the first `it` block fail with a clear `expected 1, got 2` message.

## Context

design.md § Risks / "Drawer corruption recurrence"
architecture.md § Testing Approach (Edge cases)
specs/gptel/scope-expansion/spec.md § ADDED Requirements / "Drawer writer preserves structure"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/invariant/scope-drawer-no-duplication`: speculated → confirmed. Both producers (`--write-pattern-to-drawer`, `--apply-to-drawer`) preserve drawer-singleton structure. This task is the regression-test layer. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-drawer-no-duplication.md`.
- `register/boundary/scope-pattern-writer`: speculated → confirmed. Writer's contract held; the regression test exercises it through several add-to-scope cycles. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-pattern-writer.md`.

### Already-shipped inline fixes
- `arch-cycle-1777460733-8`: `:deny` arm removed from `--map-operation-to-drawer-key`. **Implication for this task**: the original example in step 1 used `:deny` operation; replaced with `:execute` to preserve the multi-key-write flow (see breadcrumb at top of step 1).

## Cycle 2 updates (cycle-1777470320)

### Scaffold disposition update

The scaffold `scope-drawer-no-duplication.test.el` was set to `archived` at cycle-2 integrate (the runtime invariant is enforced by the single-producer / single-write structure in both creation paths). **Implication for this task**: the "promote vs supersede" choice in `## Cites register entries` is now resolved by cycle-2 — the scaffold is archived, so this task **writes its own regression spec** rather than promoting the scaffold's stub. The existing implementation steps already do this; just record the disposition for clarity.

### Cited entries — confirmed

- `register/invariant/scope-drawer-no-duplication`: speculated → **confirmed** (cycle-2; both creation paths emit one drawer; branching uses byte-copy). See `.orchestrator/cycles/cycle-1777470320/reconciliations/invariant-scope-drawer-no-duplication.md`.
- `register/boundary/scope-pattern-writer`: speculated → **confirmed** (cycle-2). See `.orchestrator/cycles/cycle-1777470320/reconciliations/boundary-scope-pattern-writer.md`.

### Implementation hint — extend the regression to branching

Cycle-2 confirmed that branching uses byte-copy from the parent (rather than re-rendering the drawer for the child). Worth adding a 4th `it` block exercising the branch flow:

```elisp
(it "branching produces child session.org with parent's drawer verbatim and exactly one drawer"
  ;; create parent session via Mode 2a
  ;; create child branch via --copy-truncated-context
  ;; assert child file has exactly one :PROPERTIES: block
  ;; assert org-entry-get values match parent's
  )
```

### Now unblocked

- `implement-drawer-writer` (closed cycle-1)
- `rewire-expansion-writer` (closed cycle-2)
