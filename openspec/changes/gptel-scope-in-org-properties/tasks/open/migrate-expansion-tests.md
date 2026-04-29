---
name: migrate-expansion-tests
description: Rewrite expansion/* test fixtures and assertions from YAML files to drawer-fixture buffers
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-test-helper-with-scope-drawer
  - blocked-by:rewire-expansion-writer
---

## Cites register entries

- `register/boundary/scope-pattern-writer` — assertions move from "scope.yml file mutated" to "buffer drawer mutated (and saved)".
- `register/shape/violation-info` — fixture this shape directly when invoking handlers.
- `register/invariant/scope-add-pattern-idempotent` — every dedup test asserts this invariant.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-pattern-writer.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-add-pattern-idempotent.test.el`

## Files to modify
- `config/gptel/scope/test/expansion/*-spec.el` (modify) — rewrite tests that fixture `scope.yml` or assert on YAML output. The transient/queue/callback tests are unchanged.

## Implementation steps

1. Audit the expansion test directory:

   ```bash
   grep -ln 'scope.yml\|scope-yaml\|write-pattern-to-scope\|--get-scope-file-path' \
        config/gptel/scope/test/expansion/*-spec.el
   ```

2. For each affected spec:

   - **Add-to-scope action tests**: rewrite to fixture a chat buffer with a drawer (`jf/gptel-test--with-scope-drawer`), invoke `--add-to-scope` with a violation-info, and assert the drawer was updated via `org-entry-get-multivalued-property`.
   - **Add-wildcard / Add-custom action tests**: same pattern; the per-action drawer assertions look like:

     ```elisp
     (jf/gptel-test--with-scope-drawer '()
       (jf/gptel-scope--add-to-scope ...)
       (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
               :to-equal '("/expected/pattern")))
     ```

   - **Edit Manually tests**: the action no longer opens `scope.yml`; it switches to the chat buffer. Update the assertion to check that `(switch-to-buffer ...)` was called with the chat buffer, and that the cursor lands in the `:PROPERTIES:` drawer.
   - **Drawer-writer-preserves-structure tests** (NEW per the spec's ADDED requirements): add `it` blocks asserting (a) existing drawer keys are preserved when adding to one key, (b) duplicate patterns are skipped (idempotent), (c) the first add uses the bare key form and subsequent adds use `+:`.

3. Delete tests that assert on YAML round-trip behavior (kebab-to-snake conversion, structure preservation) — those test the YAML writer that's being deleted.

4. Mocked writer tests: switch the spy target from any YAML writer (e.g. `jf/gptel-scope--write-pattern-to-scope`) to `jf/gptel-scope--write-pattern-to-drawer`.

5. Run `./bin/run-tests.sh -d config/gptel/scope/test/expansion` after each spec is migrated.

## Design rationale

The expansion UI's transient menu, queue, and callback shapes are unchanged — those tests don't need rewriting. The fixture and assertion changes concentrate in the action handlers and the writer-helper tests. The new `Drawer writer preserves structure` requirement (in the spec delta) brings new test cases around idempotency and bare-vs-`+` form, which are the writer's behavioral contract.

## Design pattern

Migration follows the same per-spec pattern as `migrate-validation-tests`. Shared helpers live in `helpers-spec.el`; reuse them rather than duplicating fixture builders.

## Verification

- `./bin/run-tests.sh -d config/gptel/scope/test/expansion` passes.
- `grep -n 'scope.yml\|scope-yaml' config/gptel/scope/test/expansion/*-spec.el` returns no results.
- The new `Drawer writer preserves structure` scenarios from the delta spec each have at least one `it` block covering them.
- Snapshot regeneration if used (`expansion/test-results.txt`).

## Context

architecture.md § Testing Approach
specs/gptel/scope-expansion/spec.md § ADDED Requirements / "Drawer writer preserves structure"
specs/gptel/scope-expansion/spec.md § MODIFIED Requirements (all action specs)

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/boundary/scope-pattern-writer`: speculated → confirmed. Writer's contract held; runtime tests for the writer land in this task. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-pattern-writer.md`.
- `register/invariant/scope-add-pattern-idempotent`: speculated → confirmed. Buttercup spec at `config/gptel/scope/test/drawer/write-pattern-spec.el` lands here. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-add-pattern-idempotent.md`.
- `register/shape/violation-info`: unchanged. No cycle-1 implementation modified producers/consumers; deferred to this cycle's exercise.

### User-resolved decisions
- `ask-arch-cycle-1777460733-1`: writer fallback strict-error (B). **Implication for this task**: add tests asserting:
  - feeding `:operation nil` to `--map-operation-to-drawer-key` (or transitively `--write-pattern-to-drawer`) signals an error with the documented message
  - feeding `:operation :foo` (an unknown operation) signals an error
  - the legacy "permissive-route-to-READ" behaviour does NOT survive (negative regression)

### Meta-discoveries
- `vocabulary-cluster/permissive-default-vs-closed-vocabulary`: write-side writer is now strict. **Implication**: tests must reflect this — no `:deny` operation in test inputs, no expectation of silent fallback.
- `other/asymmetric-vocabulary-enforcement-write-vs-read`: enforcement should be symmetric across read/write. **Implication**: a test that catches asymmetry would feed the same closed-set value through both producers and assert behaviour matches.

### Already-shipped inline fixes
- `arch-cycle-1777460733-8`: `:deny` arm removed from `--map-operation-to-drawer-key`. **Implication for this task**: existing tests that constructed violations with `:operation :deny` (if any) must be removed or rewritten.
- `arch-cycle-1777460733-9`: strict-error fallback with explicit `(null operation)` arm. **Implication for this task**: add error-path tests; remove permissive-fallback expectations.
