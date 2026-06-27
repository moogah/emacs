---
name: migrate-validation-tests
description: Rewrite validation/* test fixtures from scope.yml on disk to drawer-fixture buffers
change: gptel-scope-in-org-properties
status: needs-review
relations: []
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

## Cycle 2 updates (cycle-1777470320)

### Cited register entries — disposition flips

- `register/shape/scope-config-plist`: speculated → **confirmed**. The `:paths` sub-plist now carries six list-valued keys (added `:read-metadata` per ask 10A). When migrating, drawer fixtures should include `:GPTEL_SCOPE_READ_METADATA` examples wherever the legacy YAML fixture set `paths.read_metadata` (often nowhere, since this is net-new). See `.orchestrator/cycles/cycle-1777470320/reconciliations/shape-scope-config-plist.md`.
- `register/boundary/scope-config-loader`: speculated → **divergent**. Cycle-2 implementation honors the cycle-1 reconciliation's stage-3-collapse-to-nil semantic; cycle-1's user-resolved ask (option (b)) said the opposite. The cycle-2 PM digest routes this as `ask-arch-cycle-1777470320-1` to the user. **Implication for this task**: defer the empty-drawer-specific test cases until the user dispositions (in `disposition-empty-drawer-collapse`). All other validation tests (path matching, glob, parse-completeness, coverage threshold) can land first. See `.orchestrator/cycles/cycle-1777470320/reconciliations/boundary-scope-config-loader.md`.
- `register/invariant/scope-no-security-key-in-plist`: speculated → **confirmed**. L1+L2 both hold. The structural-audit-style spec recommended in cycle-1's update is still a good idea — write it as a guard against regression.
- `register/invariant/scope-parse-complete-is-true`: speculated → **confirmed**. The helper now reads `jf/gptel-scope--enforce-parse-complete` defconst directly. Tests should drop any per-call `security-config` parameter and instead test against the defconst's literal value (`t`).
- `register/invariant/scope-coverage-threshold-is-1`: speculated → **confirmed**. Same shape change — drop `security-config` from helper-call sites; the threshold defconst is `1.0`.

### Inline fixes / signature changes to absorb

- The validator helper signatures changed (cycle-2 commit a813d53): `--validate-parse-completeness` and `--check-coverage-threshold` no longer take a `security-config` parameter. Three cycle-2 specs now fail because they call the helpers with the old signature:
  - `handles nil parse-complete gracefully`
  - `handles nil security-config`
  - `parse_incomplete when parse fails and enforce is true`
  - `permissive mode allows incomplete parse with warning`
  - `permissive mode allows partial syntax`
  These five are this task's primary work — drop the parameter and the parameter-shape assertions; rely on the defconst directly.

### Unblocked

- `add-test-helper-with-scope-drawer` (closed cycle-1) — provides `jf/gptel-test--with-scope-drawer`.
- `rewire-validator-config-load` (closed cycle-2) — the loader is rewired and shipped.

### Re-blocked (partial)

- `disposition-empty-drawer-collapse` (cycle-3 user disposition) — only the empty-drawer-specific test cases need to wait. Land everything else first; flag any empty-drawer pin with a `;; depends on disposition-empty-drawer-collapse` comment so the cycle-3 follow-up can find them.

## Observations

- 53 failures → 2 failures in `config/gptel/scope/test/validation/`. The two
  remaining failures are in `resource-limits-spec.el` and were
  already failing pre-task (output-truncation message strings, not
  scope/YAML/drawer-related). Per the task brief contract those count
  as pre-existing pass-throughs.
- The validator's expected scope-config plist shape is much simpler
  than the historical YAML-loaded plist: only `:paths` (with
  `:read`/`:read-metadata`/`:write`/`:modify`/`:execute`/`:deny`) and
  `:cloud` (with `:auth-detection`/`:allowed-providers`). All
  validation tests assert against this shape; constructing the plist
  directly via a new `helpers-spec-make-scope-config` builder is far
  cleaner than round-tripping through a YAML fixture or even a
  drawer fixture for tests that don't exercise the loader itself.
- `cloud-auth` provider matching uses `member` against the
  `:allowed-providers` list. The historical pipeline-behavioral
  fixture passed strings (`"aws"`, `"azure"`, `"gcp"`) for both the
  detected provider and the allowed list, while the unit-level
  detection tests (in the same spec) used keywords (`:aws`, `:azure`,
  `:gcp`). I unified to keywords — the unit-level detection tests are
  the older convention and `cloud-auth-spec--make-cloud-auth-ops`
  takes a keyword `provider` parameter, so keyword discipline is
  internally consistent.
- The `cloud-auth-spec` retained one drawer-fixture test for
  auth-detection round-trip; this exercises the loader's scalar key
  path (`GPTEL_SCOPE_CLOUD_AUTH`) rather than the validator. It's
  the only validation/* test that uses the drawer macro, since the
  remaining tests only need a plist input to validation functions.

## Discoveries

- **Validator helper signatures already drop `security-config`.**
  The brief said five cycle-2 specs broke because helpers no longer
  take `security-config` and the migration should drop the parameter.
  In `comprehensive-nil-handling-spec.el` only two tests actually
  passed `security-config`, not five — the other three named in the
  brief (`parse_incomplete when parse fails and enforce is true`,
  `permissive mode allows incomplete parse with warning`,
  `permissive mode allows partial syntax`) live in
  `parse-completeness-spec.el` and broke because of the
  YAML-fixture/`enforce_parse_complete: false` interaction. The
  permissive-mode tests cover behaviour that no longer exists (the
  override is gone) and were deleted; the strict-mode tests were
  rewritten to use the plist builder. So the brief's count of "5
  specs" maps to "2 helper-signature changes + 3 deleted/rewritten
  YAML+permissive tests."
- **`jf/gptel-scope--load-config` no longer returns nil.** Per the
  cycle-3 disposition (Option B) the loader composes the deny-all
  defaults plist when the drawer is missing/empty. The dispatcher's
  `no_scope_config` short-circuit was removed in
  `--final-deny-response`, so `authorize-tool-call-spec.el`'s "no
  scope configuration" branch had to be rewritten: feeding the
  loader the deny-all-defaults plist drives validation to fail, which
  routes through `--trigger-inline-expansion` like any other scope
  violation.
- **`helpers-spec--convert-vectors-to-lists` is now an orphan.** It
  was only used by the deleted YAML-loader path. I left the function
  in place rather than deleting it because removing more dead helpers
  would expand the diff into territory that other (unmigrated)
  integration/expansion specs may still reference indirectly. Filed
  as a candidate cleanup for the next task that touches this file.
- **Many integration/expansion specs still use the deleted helpers.**
  Files like `expansion-ui-spec.el`, `expansion-roundtrip-spec.el`,
  `bash-parser-contract-layers-spec.el`, etc. call
  `helpers-spec-make-scope-yml`, `helpers-spec-load-scope-config`,
  `helpers-spec--scope-with-paths`, etc. These are outside this
  task's `validation/*` scope. To avoid breaking them at *load* time
  (which would mask any unrelated issues), the helpers are kept as
  stubs: `helpers-spec-make-scope-yml` still creates the temp file;
  the `--load-scope-config` and `--scope-with-paths` helpers signal
  with a clear migration message. Migration of those tests is
  follow-up work tracked by `migrate-integration-tests` and
  `migrate-expansion-tests`.
- **Snapshot tracked at scope level, not validation level.** The
  task's verification step references "validation/test-results.txt"
  but the actually-tracked snapshot is `config/gptel/scope/test-report.txt`
  (one level up). I regenerated that snapshot via `--report` rather
  than `--snapshot`, since the former is what's tracked. The new
  scope-level report shows the validation/ migration's downstream
  effect on the broader scope test suite — many integration/expansion
  specs are still red because they haven't been migrated yet.
