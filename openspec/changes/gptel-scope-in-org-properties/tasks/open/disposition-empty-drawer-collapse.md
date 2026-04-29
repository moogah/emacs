---
name: disposition-empty-drawer-collapse
description: Implement Option B — replace Stage 3 collapse-to-nil with a deny-all-defaults composer in --load-config.
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:rewire-validator-config-load
---

> **User decision (2026-04-29):** Option B — revise the implementation in cycle-3 to compose deny-all defaults. Stage 3 of `--load-config` is replaced with a `(cond)` that returns the deny-all-defaults plist when the loaded plist has no `:GPTEL_SCOPE_*` keys. The cycle-1 user disposition for `ask-arch-cycle-1777460733-2` stands; the cycle-1 reconciliation note's "stage-3 collapse" language is amended at cycle-3 integrate.

## Cites register entries

- `register/boundary/scope-config-loader` — currently `divergent` (per `.orchestrator/cycles/cycle-1777470320/reconciliations/boundary-scope-config-loader.md`). Disposition lands in cycle-3 reconciliation as either an entry update (option A) or a code change + entry update (option B).

## The contradiction

Two cycle-1 outputs disagree about what an empty `:PROPERTIES:` drawer should mean:

- **Cycle-1 reconciliation note** (`.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-config-loader.md`): Stage 3 collapses an all-empty plist to `nil`; the dispatcher then denies with `:error "no_scope_config"`. The cycle-1 implementor honored this; the cycle-2 implementor honored this; both reviewers (cycle-1 and cycle-2 author-blind) verified.

- **Cycle-1 user-resolved ask** (`asks_for_user_resolved[1]` in `handshake-cycle-1777460733.json`, option (b)): "empty drawer = valid empty scope; populated with deny-all defaults". The associated `register_changes` says "stage 3 collapse semantics will be revised to 'empty drawer = deny-all defaults' rather than 'no_scope_config'."

Both authorities cannot stand. The implementation matches the reconciliation; the user disposition says the opposite.

## The decision (Option B)

Replace Stage 3 with a "deny-all defaults" composer: when the loaded plist has no `:GPTEL_SCOPE_*` keys, return a plist with empty `:paths` sub-lists and `"deny"` cloud-auth (instead of `nil`). The dispatcher proceeds to validation; per-violation deny is the authorisation outcome, not `no_scope_config`.

**Cost**: one cycle-3 task to revise `--load-config` (small — one `cond` clause swap); `migrate-validation-tests` carries the test pin. The cycle-1 user disposition stands; the cycle-1 reconciliation note's "stage-3 collapse" language is amended.

**Trade-off**: an empty drawer is now a deny-all configuration rather than a config-missing signal. The two cases are technically distinguishable (no drawer at all vs. empty drawer), but downstream behaviour is identical (every read/write/execute denies).

## Implementation steps

1. Read `config/gptel/scope/scope-validation.org` § Dispatcher block (`--load-config`).
2. Replace the Stage 3 collapse logic. Currently:

   ```elisp
   (let ((config (or buffer-result file-result)))
     (and config (--has-any-scope-key-p config) config))
   ```

   Replace with:

   ```elisp
   (let ((config (or buffer-result file-result)))
     (cond
      ((null config) (--deny-all-defaults))
      ((--has-any-scope-key-p config) config)
      (t (--deny-all-defaults))))
   ```

   Where `--deny-all-defaults` returns:

   ```elisp
   '(:paths (:read () :read-metadata () :write () :modify () :execute () :deny ())
     :cloud (:auth-detection "deny" :allowed-providers ()))
   ```

3. Update `--has-any-scope-key-p` to be a pure helper (no side effects); add a docstring noting the new caller semantics.
4. Tangle. Run validation tests; expect `migrate-validation-tests` to land an empty-drawer-deny-all spec in cycle-3.
5. Edit `interfaces.org` § scope-config-loader: replace Stage 3 description with "empty-plist → deny-all defaults composition".
6. Mark this task `done`; close in cycle-3 integrate.
7. Cycle-3 integrate writes a reconciliation note for the entry transitioning from `divergent → reconciled`, with `prior_shape` and `new_shape` capturing the Stage 3 swap.

## Verification

- After resolution, `register/boundary/scope-config-loader` `status` is `reconciled` (Option B applied); not `divergent`.
- A spec in `migrate-validation-tests` (or in this task) asserts that an empty drawer + a tool call against any path produces a deny outcome — not `no_scope_config`.
- The defconst-or-helper `jf/gptel-scope--deny-all-defaults` exists and returns the correct plist shape (empty `:paths` sub-lists, `"deny"` cloud-auth).
- Cycle-3 plan reads `phase_gates.integrate.passed: true` and starts.

## Context

- Reconciliation note: `.orchestrator/cycles/cycle-1777470320/reconciliations/boundary-scope-config-loader.md`
- Architect finding: `.orchestrator/cycles/cycle-1777470320/findings/arch-cycle-1777470320-4.md`
- PM digest: `.orchestrator/cycles/cycle-1777470320/pm-digest.md` § Asks for the user / item 1
- Reviewer surfaced this independently in `.orchestrator/cycles/cycle-1777470320/reviews/rewire-validator-config-load.md` § Finding 1

## Discovered from

- arch-cycle-1777470320-4 (architect end-of-cycle finding)
- rewire-validator-config-load implementor push-back (disc-rewire-validator-config-load-1)
- rewire-validator-config-load author-blind reviewer Finding 1 (convergent)
