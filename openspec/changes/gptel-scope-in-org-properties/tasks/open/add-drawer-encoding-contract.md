---
name: add-drawer-encoding-contract
description: Document drawer key vocabulary in interfaces.org and add the parse-complete / coverage-threshold constants
change: gptel-scope-in-org-properties
status: ready
relations: []
---

## Cites register entries

This task establishes the structural contracts the rest of the cycle implements against. Pressure-test these as you work — flag in `## Discoveries` if any feel wrong.

- `register/vocabulary/drawer-key-set` — the seven `:GPTEL_SCOPE_*` keys and their plist-path mapping. The drawer-key vocabulary section you're adding to `interfaces.org` is this entry made canonical.
- `register/invariant/scope-parse-complete-is-true` — the `defconst` you're adding (`jf/gptel-scope--enforce-parse-complete = t`) is this invariant's structural enforcement.
- `register/invariant/scope-coverage-threshold-is-1` — same for `jf/gptel-scope--coverage-threshold = 1.0`.
- `register/invariant/scope-no-security-key-in-plist` — your removal of `:security` from the canonical config-shape contract in `interfaces.org` is the precondition for this invariant.
- `register/shape/scope-config-plist` — the `:security`-key removal in the canonical shape.

Scaffolds carrying revision licence:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-parse-complete-is-true.test.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-coverage-threshold-is-1.test.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-no-security-key-in-plist.test.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/drawer-key-set.el`

## Files to modify
- `config/gptel/scope/interfaces.org` (modify) — add a "Drawer Key Vocabulary" section; remove the `:security` key from the canonical scope-config-shape contract.
- `config/gptel/scope/scope-validation.org` (modify) — add two `defconst`s near the top of the module: `jf/gptel-scope--enforce-parse-complete` (always `t`) and `jf/gptel-scope--coverage-threshold` (always `1.0`).
- Tangle both: `./bin/tangle-org.sh config/gptel/scope/interfaces.org`, `./bin/tangle-org.sh config/gptel/scope/scope-validation.org`.

## Implementation steps

1. In `interfaces.org`, add a new section `* Drawer Key Vocabulary` (or fold into the existing config-shape section) documenting the key table:

   | Drawer key                       | Type   | Plist field                        |
   |----------------------------------|--------|------------------------------------|
   | `:GPTEL_SCOPE_READ:` / `+:`      | list   | `(:paths (:read ...))`             |
   | `:GPTEL_SCOPE_WRITE:` / `+:`     | list   | `(:paths (:write ...))`            |
   | `:GPTEL_SCOPE_MODIFY:` / `+:`    | list   | `(:paths (:modify ...))`           |
   | `:GPTEL_SCOPE_EXECUTE:` / `+:`   | list   | `(:paths (:execute ...))`          |
   | `:GPTEL_SCOPE_DENY:` / `+:`      | list   | `(:paths (:deny ...))`             |
   | `:GPTEL_SCOPE_CLOUD_AUTH:`       | scalar | `(:cloud (:auth-detection ...))`   |
   | `:GPTEL_SCOPE_CLOUD_PROVIDERS:` / `+:` | list | `(:cloud (:allowed-providers ...))` |

2. In `interfaces.org`, update the canonical scope-config-shape block to remove `:security`. The new shape has only `:paths` and `:cloud`.

3. In `scope-validation.org`, add a new `* Constants` section near the top (before the loader) with:

   ```elisp
   (defconst jf/gptel-scope--enforce-parse-complete t
     "Whether Stage 1 of the bash validation pipeline refuses commands with
   incomplete parses. Fixed at module load; not configurable per session.")

   (defconst jf/gptel-scope--coverage-threshold 1.0
     "Threshold below which the bash semantic-plugin coverage check emits
   a non-blocking warning. Fixed at module load; not configurable per session.")
   ```

4. Do NOT yet wire these into the validator pipeline — that happens in `rewire-validator-config-load`. This task only adds the contract and the constants so subsequent tasks have something to reference.

5. Tangle both files and run `./bin/run-tests.sh -d config/gptel/scope` to confirm nothing regressed (the constants are unused; the contract change is documentation-only at this point).

## Design rationale

The drawer encoding is the change's core public contract. Documenting it in `interfaces.org` mirrors how the canonical error-code set, validation-result shape, and glob test cases already live there — the file is the executable-contracts module. Capturing the key table in one place avoids drift between the reader, the writer, and the spec scenarios.

Adding the constants ahead of the rewire decouples the "remove `:security`" decision from the loader-rewire mechanics. By the time `rewire-validator-config-load` runs, the constants are already in place and that task only has to swap call sites.

## Design pattern

`interfaces.org` already documents canonical sets (error codes, validation-result shape) as tables; follow the same style. Constants follow the existing pattern in `scope-validation.org` for module-level configuration (e.g. how `jf/gptel-scope-yaml--schema-defaults` is structured today, except as `defconst` since these are not user-tunable).

## Verification

- `./bin/tangle-org.sh config/gptel/scope/interfaces.org` succeeds.
- `./bin/tangle-org.sh config/gptel/scope/scope-validation.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/scope` passes (no behavioral regressions; constants are not yet referenced).
- Constants exist: `(boundp 'jf/gptel-scope--enforce-parse-complete)` and `(boundp 'jf/gptel-scope--coverage-threshold)` both return `t` after a fresh load.

## Context

design.md § Decisions 1, 3
architecture.md § Components, Interfaces (Drawer ↔ scope-plist contract)
specs/gptel/scope/spec.md § ADDED Requirements / "Scope drawer encoding"
