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

## Cycle 3 updates (cycle-1777478129)

### Cited register entries — disposition flips

- `register/invariant/scope-drawer-no-duplication`: speculated → **confirmed** (cycle-3 integrate). Both creation paths emit single-drawer; cycle-2 byte-copy branching preserves single-drawer; tests in `migrate-session-creation-tests` and `migrate-persistent-agent-tests` exercise this. See `.orchestrator/cycles/cycle-1777478129/reconciliations/invariant-scope-drawer-no-duplication.md`.
- `register/boundary/scope-pattern-writer`: speculated → **confirmed** (cycle-3 integrate). Five action handlers route through `--write-pattern-to-drawer`; harden task added 4 more refusal/no-op branches that all honour the strict-error guard. See `.orchestrator/cycles/cycle-1777478129/reconciliations/boundary-scope-pattern-writer.md`.

### Cycle-3 architect findings absorbed by this task

- **`arch-cycle-1777478129-1` (advisory, shape-fragmentation)**: cycle-3 created three parallel drawer-test helper namespaces. **Implication for this task**: when writing the regression spec, prefer extending `config/gptel/scope/test/helpers-spec.el` (with `jf/gptel-test--render-drawer` and `jf/gptel-test--with-scope-drawer`) over inlining a fourth helper. If a new read-side parser helper is genuinely needed, lift it into `helpers-spec.el` rather than creating a per-file definition.

- **`migrate-expansion-tests` reviewer Finding 2 (advisory, coverage gap)**: the spec scenario "Existing drawer keys are preserved" specifically named `:GPTEL_PRESET:` as a non-scope key that must survive a writer call. The migrate-expansion test fixtured only `:GPTEL_SCOPE_*` keys; the targeted defect class (preset-property corruption — see user's `reference_drawer_corruption_notes.md` and `~/org/roam/20260419111957-gptel_preset_property_corruption.org`) was not pinned. **This task is the natural home for that pin.** Add an `it` block that fixtures `:GPTEL_PRESET . "default"` + `:GPTEL_PARENT_SESSION_ID . "abc-123"` alongside scope keys, runs the writer through several add-to-scope cycles, and asserts (a) exactly one `:PROPERTIES: ... :END:` block (the original purpose of this task) AND (b) the non-scope keys survive verbatim.

### Scaffolding disposition

The cycle-2 scaffold at `scaffolding/invariants/scope-drawer-no-duplication.test.el` was dispositioned `archived` at cycle-3 integrate (the pattern-cohort migrations exercise the invariant via runtime tests; the scaffold can be archived when this regression spec lands). When this task ships, either:
- **Promote** the scaffold to `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el` and write the regression on top of it, OR
- **Supersede** the scaffold by writing the regression directly and deleting the scaffold file.

Pick one approach and document the disposition in `## Discoveries`.

## Observations

- The writer (`jf/gptel-scope--write-pattern-to-drawer`) calls `save-buffer` unconditionally. Test buffers from `jf/gptel-test--with-scope-drawer` are not file-backed, so the spec stubs `save-buffer` to a no-op via `cl-letf` — the same pattern used by the existing "Drawer writer preserves structure" tests in `expansion-ui-spec.el`. This is a benign test-side workaround, not a writer defect; flagging only because future readers may wonder why the new spec mocks `save-buffer`.
- The `helpers-spec.el` self-tests (six `describe`/`it` blocks for `jf/gptel-test--render-drawer` and `jf/gptel-test--with-scope-drawer`) are re-discovered each time `helpers-spec.el` is `(require)`d in a new directory's spec. Running `./bin/run-tests.sh -d config/gptel/scope/test/drawer` therefore reports 10 specs (4 new + 6 helper self-tests), not just 4. This is pre-existing behaviour shared by every scope spec dir; not a defect introduced by this task.

## Discoveries

- discovery_id: disc-add-drawer-corruption-regression-1
  class: deviation
  description: |
    Scaffold disposition: chose **supersede** over **promote**. The new spec
    at `config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el` is a
    fresh regression spec; the cycle-2 scaffold at
    `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el`
    has been deleted in the same commit.

    Cycle-2 integrate already dispositioned the scaffold `archived` (the
    runtime invariant is enforced by the single-producer / single-write
    structure of both creation paths). The cycle-2 update note in the task
    body (lines 99-101) explicitly steers toward writing a fresh regression
    spec rather than promoting the stub. The scaffold's four `it` blocks
    were `(error "speculated; not implemented...")` placeholders, not
    runnable tests, so promoting would have meant rewriting them anyway.
    Supersede is therefore the cleaner outcome: archive the scaffold AND
    delete the file in one step.
  affected_register_entry: register/invariant/scope-drawer-no-duplication
  recommendation: |
    At integrate, mark the cycle-2 scaffold reconciliation as `archived` and
    note that the file has been deleted (not merely dispositioned). The new
    spec is the structural regression-test enforcement of
    `register/invariant/scope-drawer-no-duplication` going forward.

- discovery_id: disc-add-drawer-corruption-regression-2
  class: deviation
  description: |
    Did NOT extend `helpers-spec.el` with a new read-side parser helper.
    Cycle-3 finding-1 advisory (in the task body) said "if a new read-side
    parser helper is genuinely needed, lift it into `helpers-spec.el`
    rather than creating a per-file definition." The four `it` blocks
    needed only (a) `cl-count-if` over split lines (per the task's
    explicit design pattern), and (b) `org-entry-get` /
    `org-entry-get-multivalued-property` for non-scope-key survival
    assertions — both inline forms read directly from the buffer with no
    helper indirection. A `no-duplicate-drawer-spec--count-properties-headers`
    function is defined inside the spec file because it is only used here
    and naming it improves test failure messages; promoting it to
    `helpers-spec.el` would be premature generalisation.
  affected_register_entry: register/boundary/scope-pattern-writer
  recommendation: |
    No action needed. If a future spec also needs to count drawer headers
    in a buffer, lift `--count-properties-headers` into `helpers-spec.el`
    at that point. Until then, keeping the helper local satisfies the
    cycle-3 advisory's "genuinely needed" qualifier.

- discovery_id: disc-add-drawer-corruption-regression-3
  class: invariant-gap
  description: |
    Confirmed the test bites: deliberately changed `:to-equal 1` to
    `:to-equal 99` in the first `it` block, ran
    `./bin/run-tests.sh -d config/gptel/scope/test/drawer`, and observed
    `Expected (no-duplicate-drawer-spec--count-properties-headers (current-buffer))
    to be equal to 99, but instead it was 1`. The failure-mode message is
    clear and points directly at writer integrity, satisfying the task's
    optional verification step. The assertion was restored to `:to-equal 1`
    before commit; `diff` against a pre-change snapshot confirmed the
    spec is byte-identical to the working version.
  affected_register_entry: register/invariant/scope-drawer-no-duplication
  recommendation: |
    No action needed. Recording this so reviewers know the
    deliberate-regression check was performed and the spec's failure
    message is human-readable.

