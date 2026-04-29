---
name: rewire-validator-config-load
description: Switch scope-validation's config loader call site from scope-yaml to the drawer reader; remove :security plist reads
change: gptel-scope-in-org-properties
status: done
relations: []
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

## Observations

- **Took option (a): rename, not delegate.** Renamed `jf/gptel-scope--load-config-from-drawer` to `jf/gptel-scope--load-config` and deleted the legacy YAML-shape body. Added a `condition-case` wrapper around the body to preserve the YAML loader's swallow-and-log behaviour for unexpected errors (otherwise a crash inside `org-entry-get-multivalued-property` would propagate to every tool call instead of denying with `no_scope_config`). The wrapper is new behaviour relative to cycle-1's `--load-config-from-drawer`, which had no error guard — flagging because `org-mode` errors during `with-temp-buffer` activation (e.g. on filesystems where org's autoload chain hits a missing dependency) would now be silently swallowed where they used to crash. Choose: log-and-deny (current) or propagate-and-die.
- **Defaults interpretation.** The task brief cites `ask-arch-cycle-1777460733-2` resolving to "empty drawer = valid empty scope = deny-all defaults" but also states the stage-3 collapse via `--has-any-scope-key-p` "preserves the no_scope_config deny semantic". These are in tension: if the loader composes deny-all defaults around an empty plist, it would never collapse to nil, and the `no_scope_config` path would die. I read the second statement as authoritative: `--has-any-scope-key-p` still collapses an entirely-empty drawer to nil → `no_scope_config`. The "deny-all defaults" become operative only when *some* scope key is set (the existing `--load-from-buffer` already defaults missing list keys to nil and missing `:auth-detection` to "warn", which is the deny-fail-safe model). I did *not* change the "warn" default to "deny" because that would break `--has-any-scope-key-p`'s "non-default auth indicates intent" check (every empty drawer would suddenly be intent-bearing). Flag for integrate: the cycle-1 reconciliation note and the brief's wording need to agree on which semantic is authoritative.
- **Signature change cascades to two test files.** `--validate-parse-completeness` now takes one arg (parse-result), not two (parse-result + security-config). The two callers in tests are:
  - `config/gptel/scope/test/validation/comprehensive-nil-handling-spec.el:43,50` (validation tests, in scope of `migrate-validation-tests`).
  - `config/gptel/scope/test/integration/error-code-contract-spec.el:129` (integration test, NOT in `migrate-validation-tests`'s declared scope per its file path). This is collateral damage that `migrate-validation-tests` may not pick up — confirm it covers `test/integration/` too, or file a follow-up.
- **`--check-coverage-threshold` signature also changed** (drops the `security-config` arg). It is not directly tested, only invoked from `--validate-command-semantics`, so no test fanout — but if any future caller tries to pass two args they'll get a `wrong-number-of-arguments` error.
- **Pre-existing failures unrelated to this task.** Two tests in `path-validation-spec.el` ("warns when long-running command terminated" and "suggests filters in truncation notice") were already failing before this task started. They appear to assert against bash-tool truncation/timeout messaging unrelated to scope config; left alone.
- **`interfaces.org` register entry under-specified for parse_incomplete producer.** The producer comment for the canonical error code `parse_incomplete` (`config/gptel/scope/interfaces.el:19`/`interfaces.org:161`) still describes the trigger as "and security.enforce_parse_complete is true" — that condition has been promoted to a module constant. Integrate-phase territory; not modified.
- **Test fixtures across the broader scope test suite write `scope.yml` files via `helpers-spec-make-scope-yml`.** Migration is bigger than the validation directory: every test that uses `helpers-spec-load-scope-config` is YAML-bound. Confirm `migrate-validation-tests`'s scope covers the full helper rewrite or file a follow-up.

## Discoveries

- discovery_id: disc-rewire-validator-config-load-1
  class: spec-signal
  description: |
    The cycle-1 reconciliation for `register/boundary/scope-config-loader` and the user resolution of `ask-arch-cycle-1777460733-2` give incompatible directives at the implementation level. The user chose "empty drawer = valid empty scope = deny-all defaults" (the loader returns a populated plist), but the cycle-1 reconciliation note says the stage-3 collapse "preserves the no_scope_config deny semantic" (the loader returns nil). Implementation can satisfy at most one of these literally; I went with stage-3 collapse and treated "deny-all defaults" as the per-key default behaviour for partial drawers. The integrate phase should pick one authoritative semantic and update either the reconciliation note or the user-resolution wording so cycle-3 doesn't relitigate this.
  affected_register_entry: register/boundary/scope-config-loader
  recommendation: |
    Pick one: (a) keep stage-3 collapse and downgrade "deny-all defaults" to mean "per-missing-key fail-safe defaults", or (b) remove `--has-any-scope-key-p` entirely and rely on the validator's own permission-hierarchy code to deny-by-default against an empty plist. Option (b) is the more aggressive reading of `ask-arch-cycle-1777460733-2` but means losing the `:error "no_scope_config"` exit code as a distinct signal — every empty-drawer call would surface as a regular `not-in-scope` violation instead.

- discovery_id: disc-rewire-validator-config-load-2
  class: invariant-gap
  description: |
    `--load-config` now wraps its body in `condition-case` to swallow unexpected errors from the buffer/file readers (e.g. an `org-entry-get-multivalued-property` failure on a malformed drawer), surfacing them as `no_scope_config`. The cycle-1 holding-name dispatcher had no such guard. This silently changes the failure mode for "drawer is malformed in a way that crashes org-mode" from "crash propagates to caller" to "deny with no_scope_config and a message in *Messages*". Whether that is desirable depends on how loud we want failures of the new schema reader to be in production.
  affected_register_entry: register/boundary/scope-config-loader
  recommendation: |
    Decide whether `--load-config`'s error-swallowing is part of the boundary contract. If yes, document it on the boundary register entry (e.g. "errors during stage 1/2 reads are caught and surfaced as no_scope_config"). If no, remove the `condition-case` and let the error propagate — but then the validation pipeline needs an upstream guard or every drawer typo becomes an unhandled error.

- discovery_id: disc-rewire-validator-config-load-3
  class: interface-drift
  description: |
    The signature of `jf/gptel-scope--validate-parse-completeness` and `jf/gptel-scope--check-coverage-threshold` both lost their `security-config` parameter. The validation-pipeline-only caller (`--validate-command-semantics`) is updated, but `config/gptel/scope/test/integration/error-code-contract-spec.el:129` is a non-validation-directory caller that `migrate-validation-tests` may not pick up by its declared scope (validation tests, not integration). The function-signature change is an L2 invariant that the brief did not explicitly call out as separate from the L2-`:security`-removal invariant.
  affected_register_entry: register/invariant/scope-no-security-key-in-plist
  recommendation: |
    Confirm `migrate-validation-tests`'s task body covers `test/integration/error-code-contract-spec.el` too, or file a follow-up task to migrate it (one-line change: drop the second arg from the call).

- discovery_id: disc-rewire-validator-config-load-4
  class: scope-question
  description: |
    `register/vocabulary/drawer-key-set` and the `--load-from-buffer`/`--load-from-file` readers are now the only producers of the validator's input plist, but `config/gptel/sessions/constants.el:54` still defines `jf/gptel-session--scope-file` as `"scope.yml"` and several non-scope-validation modules (`config/gptel/tools/persistent-agent.{org,el}`, `config/gptel/scope/scope-expansion.{org,el}`, `config/gptel/sessions/filesystem.{org,el}`) still consume that constant to build YAML file paths. The validator no longer references it, but the rest of the system keeps the YAML-file mental model alive. Out of scope for this task; flag for whoever owns the broader `gptel-scope-in-org-properties` change.
  affected_register_entry: register/boundary/scope-config-loader
  recommendation: |
    Either tighten `register/boundary/scope-config-loader` to declare that the boundary owns *only* the validator's load path (and other modules retain their own loaders), or expand the change to migrate every `scope.yml` consumer. The current state is partial-rewire — the validator reads drawers, the persistent-agent and scope-expansion modules still read `scope.yml`.
