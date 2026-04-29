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

## Observations

- The scope-config-shape section in `interfaces.org` previously claimed `jf/gptel-scope-yaml--load-schema` was the producer; that helper still exists but the change moves authoritative config production to the new drawer loader (`jf/gptel-scope--load-from-buffer` / `--load-from-file` / `--load-config`) per the speculated register entry `register/shape/scope-config-plist`. I rewrote the prose generically ("the scope-validation loader") so the contract reads correctly today (where YAML is the source) and will still read correctly after `rewire-validator-config-load` replaces YAML with drawer parsing. The `:security` removal stands either way.
- `scope-validation.el` still contains live `:security` plist reads at lines ~336, ~349, ~468, ~472 (`(plist-get config :security ...)` and `(plist-get security-config :enforce-parse-complete)` / `:max-coverage-threshold`). The task explicitly defers wiring the new constants into the validator pipeline (`rewire-validator-config-load`); this means the freshly-removed `:security` documentation in `interfaces.org` is intentionally a step ahead of the code right now. Tests pass because the YAML loader still produces a `:security` sub-plist — the documentation contract describes the post-rewire state, the code describes the pre-rewire state. The invariant `register/invariant/scope-no-security-key-in-plist` will only become testable after the rewire task lands.
- The baseline for `./bin/run-tests.sh -d config/gptel/scope` is 535 buttercup specs / 2 failures (the two `run_bash_command: Timeout and resource limits` specs already documented in `config/gptel/scope/test-report.txt`). These are pre-existing and unchanged by this task. The orchestrator brief mentioned a wider-suite baseline of 24 buttercup + 10 ERT failures; that's the repo-wide baseline, not the scope-scoped one.
- The orchestrator-supplied baseline file at `.orchestrator/baseline-cycle-1777460733.txt` was not present in the worktree; I used the existing `config/gptel/scope/test-report.txt` instead, which captures the same pre-existing failure set.

## Discoveries

- discovery_id: disc-add-drawer-encoding-contract-1
  class: interface-drift
  description: |
    The canonical scope-config-shape contract in `config/gptel/scope/interfaces.org` named `jf/gptel-scope-yaml--load-schema` as the producer. The change introduces a drawer-based loader (`jf/gptel-scope--load-from-buffer` / `--load-from-file` / `--load-config`) which, per the speculated register entry `register/shape/scope-config-plist`, is the new authoritative producer. I generalised the prose to "the scope-validation loader" so the documentation no longer pins the YAML helper as authoritative. The producer list in the register entry should be considered confirmed as-speculated for this change cycle.
  affected_register_entry: register/shape/scope-config-plist
  recommendation: |
    Once `rewire-validator-config-load` lands, the `interfaces.org` prose under `* Scope Config Shape` should be tightened to name the drawer-loader functions explicitly (rather than the generic "scope-validation loader"). At that point also rewrite the `Scope System Interfaces > Component Overview` ASCII diagram which still shows `scope.yml → scope-yaml.el` as the parsing path — that arrow needs to redirect to the drawer loader inside `scope-validation.el`.

- discovery_id: disc-add-drawer-encoding-contract-2
  class: invariant-gap
  description: |
    The four `:security` plist reads in `config/gptel/scope/scope-validation.el` (at lines 336, 349, 468, 472) remain live after this task. They consume keys (`:enforce-parse-complete`, `:max-coverage-threshold`) the documentation contract has already declared removed. This is intentional staging — the rewire task explicitly owns the call-site swap — but it means `register/invariant/scope-no-security-key-in-plist` will not be enforceable until that task lands. The structural-audit scaffold (`scope-no-security-key-in-plist.test.el`) should not be activated before then or it will fail correctly but uselessly.
  affected_register_entry: register/invariant/scope-no-security-key-in-plist
  recommendation: |
    Sequence the structural-audit invariant test in the same task that does the rewire (or the immediately-following task), not in this one. The invariant entry's `enforcement_mechanism.location` already names that pairing implicitly; the integrate phase should make the dependency explicit by linking the rewire task and the no-security-key invariant test as `enables` / `blocked-by`.

- discovery_id: disc-add-drawer-encoding-contract-3
  class: vocabulary-mismatch
  description: |
    The drawer-key-set scaffolding in `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/drawer-key-set.el` documents the keys without the leading/trailing colons or the `+` multi-value suffix (i.e. `"GPTEL_SCOPE_READ"` not `":GPTEL_SCOPE_READ:"` and not `":GPTEL_SCOPE_READ+:"`). The register entry `register/vocabulary/drawer-key-set` mixes both forms — the `members[].value` field uses the colonised form (`":GPTEL_SCOPE_READ:"`) but the `validator` body checks `memq` against the bare form (`"GPTEL_SCOPE_READ"`). I documented the colonised form in `interfaces.org` (the human-readable surface) because that's what users type in their session.org drawers and that's what `org-element-property` returns; the loader implementation will need to canonicalise to the bare form before any vocabulary check.
  affected_register_entry: register/vocabulary/drawer-key-set
  recommendation: |
    The integrate phase should reconcile the register entry to use one form consistently — bare form for the validator (matching the loader's internal representation) and colonised form for the user-facing description. The current scaffolding's `pcase` arms (bare form) match what the loader implementation should look like; that part is correct. Cross-reference the colonised drawer key form documented in `config/gptel/scope/interfaces.org` § Drawer Key Vocabulary as the authoritative user-surface vocabulary.
