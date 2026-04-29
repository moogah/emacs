---
name: rewire-validator-config-load
description: Switch scope-validation's config loader call site from scope-yaml to the drawer reader; remove :security plist reads
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:implement-drawer-reader
---

## Cites register entries

- `register/boundary/scope-config-loader` — switching the call site from `(scope-yaml-load-schema ...)` to `(jf/gptel-scope--load-config)` is the consumer-side completion of this boundary.
- `register/shape/scope-config-plist` — your code reads `:paths` and `:cloud` only; the changes here are the structural enforcement of `:security` removal at every read site.
- `register/invariant/scope-no-security-key-in-plist` — every `(plist-get config :security ...)` you remove satisfies this invariant.
- `register/invariant/scope-parse-complete-is-true` — Stage 1 reads the constant; the "warn when not enforced" branch is removed.
- `register/invariant/scope-coverage-threshold-is-1` — Stage 5 reads the constant.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-config-loader.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-no-security-key-in-plist.test.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-parse-complete-is-true.test.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-coverage-threshold-is-1.test.el`

## Files to modify
- `config/gptel/scope/scope-validation.org` (modify) — replace the `(jf/gptel-scope-yaml--load-schema scope-file)` call site with `(jf/gptel-scope--load-config)`; replace `:security` plist reads with the constants from `add-drawer-encoding-contract`; update Stage 1 (parse completeness) and Stage 5 (coverage threshold) to use the constants directly.
- Tangle: `./bin/tangle-org.sh config/gptel/scope/scope-validation.org`.

## Implementation steps

> Cycle 1: holding name introduced; see `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-config-loader.md`.

1. Locate `jf/gptel-scope-authorize-tool-call` (or whichever entrypoint resolves `scope-file` and calls into the YAML loader). Replace the `(scope-yaml-load-schema scope-file)` call with `(jf/gptel-scope--load-config)`. **Note**: cycle 1 landed the new dispatcher under the holding name `jf/gptel-scope--load-config-from-drawer` to avoid silently rewiring the active validator. As part of this task, either (a) rename the holding-name function to `--load-config` (deleting the legacy YAML-shape `--load-config` body), or (b) replace the legacy `--load-config` body with `(jf/gptel-scope--load-config-from-drawer ...)` delegation. (a) is preferred — the holding name's purpose was deferral, not coexistence.

2. Replace `scope-file` resolution code (anything that builds `(expand-file-name "scope.yml" ...)`) with the `branch-dir` resolution that's already inside `--load-config`. The dispatcher no longer needs to know about the file path — `--load-config` handles buffer-vs-file resolution internally.

3. Update the "no scope config" branch: previously `nil` returned by the loader (file missing) means deny with `:error "no_scope_config"`. Per ask-arch-cycle-1777460733-2 (option b), the user resolved this in favor of "empty drawer = valid empty scope = deny-all defaults" — the loader composes deny-all defaults around the empty plist rather than collapsing to nil. The stage-3 collapse (`--has-any-scope-key-p`) preserves the no_scope_config deny semantic, but the specific deny-all defaults the loader installs are part of THIS task's implementation. See `ask-arch-cycle-1777460733-2` for the deferral.

4. In Stage 1 (parse completeness) of the bash pipeline, replace `(plist-get config :security :enforce-parse-complete)` (or however the current code reads the flag) with the constant `jf/gptel-scope--enforce-parse-complete`. Remove the "warn when not enforced" branch entirely — the constant is always `t`, so the conditional collapses to "always enforce".

5. In Stage 5 (coverage threshold) of the bash pipeline, replace the corresponding `:max-coverage-threshold` read with `jf/gptel-scope--coverage-threshold`. The non-blocking warn branch stays the same.

6. Remove any other `(plist-get config :security ...)` reads in the validation pipeline. Search for them with `grep -n ':security' config/gptel/scope/scope-validation.org`.

7. Tangle. Run `./bin/run-tests.sh -d config/gptel/scope/test/validation` — most tests should pass (the plist shape consumed by the validator is unchanged for `:paths` and `:cloud`). Tests that fixture `scope.yml` will fail; those are migrated in `migrate-validation-tests`.

## Design rationale

This task is the "swap the loader" half of Decision 3. After landing, the validator no longer references `scope.yml` or the `scope-yaml` module; the reader is the new boundary.

The "warn when not enforced" branch of Stage 1 (today: `(if enforce-parse-complete (deny ...) (warn ...))`) collapses because `enforce-parse-complete` is now always `t`. Removing the branch is more than cosmetic — it eliminates a code path that produced different behavior between sessions, which was the original justification for the per-session knob.

`--has-any-scope-key-p` filtering in the loader is what preserves the "missing scope.yml file means no_scope_config" semantics: a `session.org` carrying only `:GPTEL_PRESET:` (no scope keys) is treated as "no scope configured" exactly as a missing `scope.yml` was.

## Design pattern

Follow the dispatcher-then-validate pattern that's already in place. The change is one function (the loader) and a small number of `(plist-get config :security ...)` reads. Avoid restructuring the pipeline — that's not the job of this task.

## Verification

- `./bin/tangle-org.sh config/gptel/scope/scope-validation.org` succeeds.
- `grep -n ':security' config/gptel/scope/scope-validation.el` returns no results.
- `grep -n 'scope-yaml' config/gptel/scope/scope-validation.el` returns no results.
- `grep -n 'scope.yml' config/gptel/scope/scope-validation.el` returns no results.
- `./bin/run-tests.sh -d config/gptel/scope/test/validation` runs (some YAML-fixture tests will fail until `migrate-validation-tests` lands; non-fixture tests pass).

## Context

design.md § Decisions 2, 3 (Buffer-first read; eliminate :security)
design.md § Migration Plan steps 3, 10
specs/gptel/scope/spec.md § MODIFIED Requirements / "Scope configuration loading", "Parse completeness gate", "Coverage threshold warning"
specs/gptel/scope/spec.md § REMOVED Requirements / ":security configuration section"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/boundary/scope-config-loader`: speculated → reconciled. New 3-stage shape: stage 1 always returns non-nil plist (even empty), stage 3 (`--has-any-scope-key-p` collapse) is the no_scope_config gate. The dispatcher landed under holding name `jf/gptel-scope--load-config-from-drawer`. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-config-loader.md`.
- `register/shape/scope-config-plist`: speculated → reconciled. Shape matched byte-for-byte; producers field now lists `--load-config-from-drawer` (holding name) until this rewire renames it. See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-scope-config-plist.md`.
- `register/invariant/scope-no-security-key-in-plist`: speculated → reconciled. Split into L1 (loader output, holds today) and L2 (no readers in `scope-validation.el`, target post-rewire). This task is the L2 transition. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-no-security-key-in-plist.md`.
- `register/invariant/scope-parse-complete-is-true`: speculated → reconciled. Defconst `jf/gptel-scope--enforce-parse-complete` exists at `scope-validation.el:39`; pipeline at line 349 still reads YAML `(plist-get security-config :enforce-parse-complete)`. This task swaps the read. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-parse-complete-is-true.md`.
- `register/invariant/scope-coverage-threshold-is-1`: speculated → reconciled. Defconst `jf/gptel-scope--coverage-threshold` exists at `scope-validation.el:43`; Stage 5 at line 472 still reads YAML `:max-coverage-threshold`. This task swaps the read. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-coverage-threshold-is-1.md`.

### User-resolved decisions
- `ask-arch-cycle-1777460733-2`: empty-paths beacon decision — user chose option (b): relax loader, treat empty drawer as valid empty scope with deny-all defaults. **Implication for this task**: the loader's stage-3 collapse semantics need to change from "empty plist → nil → no_scope_config deny" to "empty plist → populated with deny-all defaults"; specifics of which defaults to install were deferred to cycle-2 and are part of this task's prescribed behaviour.

### Meta-discoveries
- `other/holding-name-producer-pattern`: cycle 1 deliberately landed the new dispatcher under the holding name `--load-config-from-drawer` to defer the active rewire. **Implication**: this task's first concrete decision is rename-vs-delegate at the function-name level (rename the holding name to `--load-config`, deleting the legacy YAML body, vs. keep both and delegate). Rename is preferred per the meta-discovery's intent.
- `invariant-gap-class/deletion-invariant-L1-L2-split`: removing `:security` from the new shape (L1) doesn't remove existing readers in `scope-validation.el` (L2). **Implication**: this task's grep-for-`:security` step (current step 6) is the L2-layer enforcement; treat it as load-bearing, not cleanup.
