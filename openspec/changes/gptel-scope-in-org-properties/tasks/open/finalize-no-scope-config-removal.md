---
name: finalize-no-scope-config-removal
description: Migrate 2 failing integration tests, openspec/specs/gptel/scope.md, and config/gptel/scope/interfaces.org off the deleted no_scope_config error code
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:arch-cycle-1777478129-3
  - discovered-from:disposition-empty-drawer-collapse
---

> **Cycle-3 architect finding `arch-cycle-1777478129-3` (blocking, dead-branch).** Cycle-3 disposition Option B removed the `no_scope_config` short-circuit at the loader and dispatcher (commit `76eeecb`), but two integration tests still spy `--load-config :and-return-value nil` and assert `:error "no_scope_config"` (currently failing); the canonical scope spec at `openspec/specs/gptel/scope.md` still describes `no_scope_config` as a live deny outcome (3 references); and `config/gptel/scope/interfaces.org` enumerates it as a macro-level error code. Three orthogonal cleanups; do them as one task to keep the contract change atomic.

## Cites register entries

- `register/boundary/scope-config-loader` — cycle-3 reconciled to Option B (deny-all defaults; no `no_scope_config` short-circuit). This task absorbs the contract change at the test + spec layer.

## Files to modify

- `config/gptel/scope/test/integration/bash-scope-expansion-integration-spec.el` (modify) — lines 465-476: rewrite the test to assert the cycle-3 Option B shape (per-violation deny, not `no_scope_config`).
- `config/gptel/scope/test/integration/filesystem-scope-integration-spec.el` (modify) — lines 494-509: same rewrite, parallel test.
- `openspec/specs/gptel/scope.md` (modify) — lines 37-40: delete "Missing configuration denies with no_scope_config" scenario; replace with the Option B equivalent. Line 408: remove `no_scope_config` from "Macro-level codes outside the validation vocabulary". Lines 424-426: delete "no_scope_config surfaces verbatim" scenario.
- `config/gptel/scope/interfaces.org` (modify) — line 179: remove `no_scope_config (macro, before validation runs)` from the error-code enumeration, OR annotate it as "removed in cycle-3 Option B disposition".

## Implementation steps

1. **Read both failing integration tests** to understand what they're asserting:
   ```bash
   grep -B2 -A20 "no_scope_config" config/gptel/scope/test/integration/bash-scope-expansion-integration-spec.el
   grep -B2 -A20 "no_scope_config" config/gptel/scope/test/integration/filesystem-scope-integration-spec.el
   ```

2. **Rewrite each test** to assert the cycle-3 Option B shape: spy `--load-config` to return `(jf/gptel-scope--deny-all-defaults)` (or just don't spy at all and let the loader compose it from an empty drawer). Assert the call denies with the per-violation `:error` (e.g. `"not-in-scope"` for filesystem; the bash variant per the dispatcher's path). The framing is "the call still denies, but as a per-violation deny, not a config-level deny — and the response goes through `--format-tool-error` like every other denial."

3. **Update `openspec/specs/gptel/scope.md`**:
   - Delete the "Missing configuration denies with no_scope_config" scenario (lines 37-40).
   - Remove `no_scope_config` from the macro-level codes paragraph (line 408): the new wording is "Macro-level codes outside the validation vocabulary: tool_exception."
   - Delete the "no_scope_config surfaces verbatim" scenario (lines 424-426).
   - Add a new scenario near the deleted ones: "Empty drawer denies per-violation" — when a session.org has no `:GPTEL_SCOPE_*` keys, the loader composes deny-all defaults; subsequent tool calls deny with `not-in-scope` (or the path-validation error for the specific operation). The response surfaces through the expansion UI like any other per-violation deny.

4. **Update `config/gptel/scope/interfaces.org`** line 179: remove the `no_scope_config (macro, before validation runs)` row from the error-code table (or annotate "removed cycle-3").

5. **Run targeted suites**:
   ```bash
   ./bin/run-tests.sh -d config/gptel/scope/test/integration
   ./bin/run-tests.sh -d config/gptel/scope/test/validation
   ```
   Expected: both previously-failing integration tests now pass; validation suite unchanged.

## Verification

- `grep -rn "no_scope_config" config/gptel/scope/ openspec/specs/gptel/scope.md` returns only historical-comment hits (e.g. inside reconciliation notes or migration documentation), not live test assertions or spec scenarios.
- `./bin/run-tests.sh -d config/gptel/scope/test/integration` passes (previously had 2 failing tests in this area).
- `openspec/specs/gptel/scope.md` describes the empty-drawer outcome as per-violation deny, NOT as `no_scope_config`.

## Design rationale

Cycle-3 disposition Option B was implemented at the production-code layer in `disposition-empty-drawer-collapse` (commit `76eeecb`). The architect end-of-cycle audit identified that the contract change wasn't fully reconciled at the test + spec + register-prose layer. Each individual reviewer flagged the gap as "out of scope for this task"; the architect-level aggregate is that the cycle-3 batch should absorb the cleanup atomically rather than postponing into cycle-4 verification (where it would block `final-verify-and-archive-prep`).

## Context

- Architect finding: `.orchestrator/cycles/cycle-1777478129/findings/arch-cycle-1777478129-3.md`
- Reconciliation note: `.orchestrator/cycles/cycle-1777478129/reconciliations/boundary-scope-config-loader.md`
- Disposition task (closed): `disposition-empty-drawer-collapse` (commit `76eeecb`)
