---
name: migrate-validation-tests
description: Rewrite validation/* test fixtures from scope.yml on disk to drawer-fixture buffers
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-test-helper-with-scope-drawer
  - blocked-by:rewire-validator-config-load
---

## Cites register entries

- `register/shape/scope-config-plist` — most validation tests check the loader output's shape; under the new contract, all such assertions check exactly `:paths` and `:cloud`.
- `register/boundary/scope-config-loader` — fixture style switches from "scope.yml on disk" to "drawer in buffer (stage 1)" or "session.org with drawer in tmpdir (stage 2)". Use stage 2 only for tests that exercise the file-fallback branch.
- `register/invariant/scope-no-security-key-in-plist` — drop any test that asserted on `:security`-shaped output; replace with positive shape assertions.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-no-security-key-in-plist.test.el` — positive examples to model your new assertions on.

## Files to modify
- `config/gptel/scope/test/validation/*-spec.el` (modify) — every spec that constructs a `scope.yml` file or feeds raw YAML to `scope-yaml-load-schema` is rewritten to use `jf/gptel-test--with-scope-drawer` (unit) or a tmpdir + real `session.org` (file-fallback only).

## Implementation steps

1. Audit the validation test directory:

   ```bash
   ls config/gptel/scope/test/validation/
   grep -ln 'scope.yml\|scope-yaml' config/gptel/scope/test/validation/*-spec.el
   ```

2. For each spec file, identify fixture style:
   - **Inline YAML string** (e.g. `(let ((yaml "paths:\n  read:\n    - /a\n")) ...)`) — replace with `jf/gptel-test--with-scope-drawer` and an alist (`'((:GPTEL_SCOPE_READ . ("/a")))`).
   - **`scope.yml` on a tmpdir** — if the test exercises the file-fallback path, keep the tmpdir but write `session.org` with a drawer instead of `scope.yml`. If it doesn't need the file path, switch to the in-memory helper.
   - **Mocked loader** (e.g. `(spy-on 'jf/gptel-scope-yaml--load-schema ...)`) — switch the spy target to `jf/gptel-scope--load-config`.

3. Tests that assert on the loaded plist's shape (e.g. checking `:security` keys) need to be updated: drop `:security` assertions; rely on the validator's behavioral assertions instead.

4. Tests that assert on `:enforce-parse-complete` or `:max-coverage-threshold` per-test override behavior need to be deleted — that override no longer exists. Replace with a simpler assertion that the validator uses the constants.

5. Run `./bin/run-tests.sh -d config/gptel/scope/test/validation` after each spec file is migrated to catch regressions early.

6. Snapshot-tracked tests: if `validation/test-results.txt` exists and is git-tracked, update with `./bin/run-tests.sh -d config/gptel/scope --snapshot` after migration is complete; commit the new snapshot together with the test changes.

## Design rationale

Per architecture.md § Testing Approach, unit tests use the in-memory helper (`jf/gptel-test--with-scope-drawer`); the file-fallback path gets dedicated tmpdir tests. Splitting by fixture style keeps unit tests fast and integration tests honest about I/O.

The existing validation tests cover the validator's plist-consumption behavior — that's unchanged across the change. The only thing breaking is fixture style. Most assertions stay the same; only the setup blocks change.

## Design pattern

Per-spec migration pattern:

```elisp
;; Before
(it "denies on a bash command writing outside scope"
  (let ((scope-file (jf/gptel-test--write-scope-yml ...)))
    (let ((result (jf/gptel-scope-validate-bash-tool "rm /etc/passwd"
                    :scope-file scope-file)))
      (expect (plist-get result :allowed) :to-be nil))))

;; After
(it "denies on a bash command writing outside scope"
  (jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_WRITE . ("/tmp/**")))
    (let ((result (jf/gptel-scope-validate-bash-tool "rm /etc/passwd")))
      (expect (plist-get result :allowed) :to-be nil))))
```

## Verification

- `./bin/run-tests.sh -d config/gptel/scope/test/validation` passes.
- `grep -n 'scope.yml\|scope-yaml' config/gptel/scope/test/validation/*-spec.el` returns no results (other than possibly historical comments scheduled for removal).
- Snapshot file (if used) is regenerated and reflects the same passing test count.

## Context

architecture.md § Testing Approach (Test Patterns, Fixture strategy)
design.md § Migration Plan step 8

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/shape/scope-config-plist`: speculated → reconciled. Producers field updated; new shape-producers are `--load-from-buffer` and `--load-from-file`; the dispatcher (`--load-config-from-drawer`, holding name) composes them. See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-scope-config-plist.md`.
- `register/boundary/scope-config-loader`: speculated → reconciled. New 3-stage shape with empty-plist collapse at stage 3 (`--has-any-scope-key-p`). After `rewire-validator-config-load` lands, the loader returns either a populated plist OR nil (collapsed empty); fixtures need to match. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-config-loader.md`.
- `register/invariant/scope-no-security-key-in-plist`: speculated → reconciled. L1 holds (loader output). L2 enforcement (no `:security` reads in `scope-validation.el`) lands when `rewire-validator-config-load` does. **This task should include a buttercup spec asserting L1** (per the scaffolding's pattern), and a structural-audit-style spec asserting L2 (e.g. assert `grep -rn ':security' config/gptel/scope/scope-validation.el` returns no matches).

### Meta-discoveries
- `invariant-gap-class/deletion-invariant-L1-L2-split`: future invariants of the form "X does not exist anywhere" should template both L1 (producer omits X) and L2 (no consumer reads X). **Implication for this task**: ensure both layers are tested — it's not enough to assert the new loader's output omits `:security`; also assert no `(plist-get _ :security ...)` survives in `scope-validation.el`. The scaffolding for this invariant is L1-only.

### User-resolved decisions
- `ask-arch-cycle-1777460733-2` (indirect): empty drawer behaviour changes from "→ no_scope_config deny" to "→ deny-all defaults composed by loader". **Implication**: tests asserting validator behaviour on missing/empty scope need to be updated — the deny path may now have different `:error` keys or different deny-pattern composition.
