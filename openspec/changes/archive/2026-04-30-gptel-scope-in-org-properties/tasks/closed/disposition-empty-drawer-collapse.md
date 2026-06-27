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

## Observations

- The `(null config)` branch in `jf/gptel-scope-authorize-tool-call` and the `no_scope_config` short-circuit in `jf/gptel-scope--final-deny-response` were no longer reachable under Option B (the loader now always returns a plist). I removed both branches rather than leave dead code. `--final-deny-response` now unconditionally formats every denial through `format-tool-error`.
- The `condition-case` error branch in `--load-config` formerly returned `nil`; under Option B I changed it to return `--deny-all-defaults` so the dispatcher contract (always-a-plist) holds even on parse exceptions.
- `config/gptel/scope/interfaces.org` line 179 still lists `"no_scope_config"` in the canonical-codes "lives outside this vocabulary" docstring with note "(macro, before validation runs)". Under Option B no producer emits that code anymore, so this note is now misleading. Out of scope for this task; flagged as discovery for integrate to route (likely to `migrate-validation-tests` or its own follow-up).
- Three integration / dispatcher specs still pin the `no_scope_config` semantic and now fail or no-op:
  - `config/gptel/scope/test/validation/authorize-tool-call-spec.el` — "no scope configuration invokes on-deny with no_scope_config and never validates"
  - `config/gptel/scope/test/integration/filesystem-scope-integration-spec.el` — "no_scope_config error returns JSON through callback (not a signal)"
  - `config/gptel/scope/test/integration/bash-scope-expansion-integration-spec.el` — "no_scope_config returns JSON through callback"
  Per the brief these are owned by the parallel `migrate-validation-tests` task; not touched here. Pre-task baseline run showed 176/182 passing; post-task run shows 175/182 — the single new failure is exactly the dispatcher spec listed above. The other 4 pre-existing failures (parse-completeness permissive mode, timeout/truncation) are unrelated.

## Discoveries

- discovery_id: disc-disposition-empty-drawer-collapse-1
  class: interface-drift
  description: |
    `config/gptel/scope/interfaces.org` § Error Codes (line 179) still
    documents `"no_scope_config"` as a code that "lives outside this
    vocabulary (macro, before validation runs)". Under Option B no
    producer emits that code: `--load-config` always returns a plist,
    `authorize-tool-call`'s `(null config)` short-circuit was deleted,
    and `--final-deny-response` no longer special-cases it. The note is
    a stale boundary-doc reference; an empty drawer now produces
    canonical violation codes (`not-in-scope` / `denied-pattern`)
    through the standard validation path.
  affected_register_entry: register/boundary/scope-config-loader
  recommendation: |
    Integrate-phase reconciliation note for scope-config-loader should
    additionally update `config/gptel/scope/interfaces.org` § Error
    Codes — either remove the `"no_scope_config"` line from the
    "outside this vocabulary" list, or restate it as a
    historical/deprecated code. Either bundle this with
    `migrate-validation-tests` (it is the spec migration's natural
    sibling) or file a small `.tasks/` follow-up.

- discovery_id: disc-disposition-empty-drawer-collapse-2
  class: deviation
  description: |
    The brief instructed to swap only the Stage 3 collapse in
    `--load-config`. In practice the change cascades to two adjacent
    sites that became dead/incoherent under the new contract:
    1. `jf/gptel-scope-authorize-tool-call`'s `(null config)` branch
       returning the synthetic `no_scope_config` deny plist —
       unreachable now (loader never returns nil), and even if reached
       its short-circuit semantics contradict the "empty drawer = deny
       at validation time" intent.
    2. `jf/gptel-scope--final-deny-response`'s `(equal :error
       "no_scope_config")` pass-through branch — also unreachable; the
       function now unconditionally formats through `format-tool-error`.
    Both were removed in this commit so the dispatcher reads cleanly.
    Documenting as `deviation` because the brief was scoped to Stage 3
    and these sit one call-frame away.
  affected_register_entry: register/boundary/scope-config-loader
  recommendation: |
    Integrate-phase: include "removed authorize-tool-call no-config
    short-circuit + final-deny-response no_scope_config branch" in the
    `prior_shape → new_shape` reconciliation note for
    scope-config-loader. No further action required at the code layer.
