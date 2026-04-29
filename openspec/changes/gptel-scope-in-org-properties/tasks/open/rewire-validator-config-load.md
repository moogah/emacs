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

1. Locate `jf/gptel-scope-authorize-tool-call` (or whichever entrypoint resolves `scope-file` and calls into the YAML loader). Replace the `(scope-yaml-load-schema scope-file)` call with `(jf/gptel-scope--load-config)`.

2. Replace `scope-file` resolution code (anything that builds `(expand-file-name "scope.yml" ...)`) with the `branch-dir` resolution that's already inside `--load-config`. The dispatcher no longer needs to know about the file path — `--load-config` handles buffer-vs-file resolution internally.

3. Update the "no scope config" branch: previously `nil` returned by the loader (file missing) means deny with `:error "no_scope_config"`. After the change, `--load-config` returns nil when (a) no buffer or file exists, or (b) the resolved drawer carries no scope keys (the `--has-any-scope-key-p` predicate). The deny path stays the same.

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
