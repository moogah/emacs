---
name: migrate-session-creation-tests
description: Update session-creation-spec.el to assert on session.org drawer content instead of scope.yml
change: gptel-scope-in-org-properties
status: ready
relations: []
---

## Cites register entries

- `register/shape/drawer-text-block` — assertions now read drawer text from the produced `session.org` rather than YAML.
- `register/boundary/scope-profile-applicator` — the producer; tests verify mode 2a's output ends up on disk verbatim.

## Files to modify
- `config/gptel/test/session-creation-spec.el` (modify) — replace `scope.yml`-on-disk assertions with `session.org` drawer assertions; remove tests that asserted on `scope.yml`-specific behavior that no longer exists.

## Implementation steps

1. Read the existing spec; the relevant `it` blocks are documented around `session-creation-spec.el:24` and `:171–:233` (per the file scan in proposal context). Each:
   - Creates a session via the creation entrypoint.
   - Asserts `scope.yml` exists at a particular path.
   - Asserts content via reading `scope.yml`.

2. For each `scope.yml` assertion, change to a `session.org` drawer assertion:

   ```elisp
   ;; Before
   (expect (file-exists-p
            (expand-file-name "branches/main/scope.yml" session-dir))
           :to-be t)

   ;; After
   (let ((session-org (expand-file-name "branches/main/session.org" session-dir)))
     (expect (file-exists-p session-org) :to-be t)
     (with-temp-buffer
       (insert-file-contents session-org)
       (org-mode)
       (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
               :to-equal '("/expected/pattern"))))
   ```

3. Add an "and no scope.yml is written" assertion to confirm the file is not produced anywhere in the session directory:

   ```elisp
   (expect (file-exists-p
            (expand-file-name "branches/main/scope.yml" session-dir))
           :to-be nil)
   ```

   Add this to at least one creation scenario.

4. The `${project_root}` expansion test changes the same way — assert the expanded path appears under `:GPTEL_SCOPE_READ:` (or wherever the profile placed it) in the drawer.

5. The "minimal scope.yml when preset has no scope configuration" test becomes "minimal drawer".

   > Cycle 1: option-a (renderer beacon) was rejected; cycle-1 reconciliation kept renderer minimal and routed deny-all defaults through the loader. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-profile-applicator.md`.
   >
   > Cycle 2: superseded — see § Cycle 2 updates / "empty-drawer behaviour pending user disposition". The cycle-1 description below ("treats this as 'empty drawer = valid empty scope = deny-all defaults'") is *not* what the merged loader does today; the actual behaviour is the inverse. Defer empty-drawer-specific test pins until `disposition-empty-drawer-collapse` resolves.

   Per `--render-drawer-text`, an empty scope plist produces a drawer with only `:GPTEL_PRESET:` (no scope keys). Assert that no `:GPTEL_SCOPE_*` keys are present. **Empty-drawer validator behaviour is pending user disposition** (see `disposition-empty-drawer-collapse`); until that resolves, do not pin behaviour through the validator for empty-drawer cases. The renderer-side assertion ("no `:GPTEL_SCOPE_*` keys when scope is empty") stands. Do NOT add a beacon to `--render-drawer-text` — the cycle-1 reconciliation explicitly dropped the beacon clause.

6. Run `./bin/run-tests.sh -d config/gptel` after migration.

## Design rationale

The session-creation tests are the integration-level proof that the profile applicator and session-creation rewires hooked up correctly. They naturally use tmpdir + real-file fixtures (per architecture.md § Testing Approach: "agent creation → tmpdir + assert file content" — same pattern applies to session creation).

The negative assertion ("no `scope.yml`") is important: it catches regressions where the YAML write step is accidentally re-introduced.

## Design pattern

Tmpdir + assert file content. Each `it` block creates its own session dir under `make-temp-file`, runs creation, asserts on the produced files. No shared session state across tests.

## Verification

- `./bin/run-tests.sh -d config/gptel/test/session-creation-spec.el` passes.
- `grep -n 'scope.yml' config/gptel/test/session-creation-spec.el` returns no results except possibly negative assertions explicitly checking the file doesn't exist.
- At least one test asserts on the drawer content via `org-entry-get-multivalued-property`.
- At least one test asserts `(not (file-exists-p ...scope.yml))`.

## Context

architecture.md § Testing Approach
specs/gptel/sessions-persistence/spec.md § MODIFIED Requirements / "Directory structure initialization", "Scope profile integration"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/shape/drawer-text-block`: speculated → reconciled. Production code emits Shape A (complete, with `:GPTEL_PRESET:`); test assertions should match Shape A. The fixture-helper `jf/gptel-test--render-drawer` produces Shape B (fragment) — do NOT use it to model what production drawers look like in assertions. See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-drawer-text-block.md`.
- `register/boundary/scope-profile-applicator`: speculated → reconciled. Multi-value encoding is single-line space-separated form (`:KEY: v0 v1 v2`); assertions reading multi-value drawer keys must use `org-entry-get-multivalued-property` (not split-by-newline). See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-profile-applicator.md`.

### Meta-discoveries
- `shape-fragmentation-cluster/fragment-vs-complete-shape-ambiguity`: assertions on production-emitted drawers must check Shape A invariants (`:GPTEL_PRESET:` present, exactly one `:PROPERTIES:`/`:END:` pair).

### Already-shipped inline fixes
- `arch-cycle-1777460733-11`: write-side cloud-auth validation in `scope-profiles.el`. **Implication for this task**: any test that constructs a profile with an invalid `:auth-detection` value (e.g. `"warning"`) will now signal at session-creation time, not at first-load time. Tests should construct valid cloud-auth values; if testing the write-side validator, do so explicitly.

## Cycle 2 updates (cycle-1777470320)

### Cited register entries — disposition flips

- `register/shape/drawer-text-block`: speculated → **confirmed** (cycle-1 already reconciled to Shape A; cycle-2 production code keeps emitting Shape A). See `.orchestrator/cycles/cycle-1777470320/reconciliations/shape-drawer-text-block.md`.
- `register/boundary/scope-profile-applicator`: speculated → **confirmed**. Mode 2a (`--render-drawer-text`) is the sole route from a profile-resolved scope-plist to drawer state in both consumer paths; sessions and persistent-agent both compose `(concat drawer-text body)` in a single `with-temp-file` write. See `.orchestrator/cycles/cycle-1777470320/reconciliations/boundary-scope-profile-applicator.md`.
- `register/invariant/scope-drawer-no-duplication`: speculated → **confirmed**. Both creation paths emit exactly one drawer; branching uses byte-copy from parent. See `.orchestrator/cycles/cycle-1777470320/reconciliations/invariant-scope-drawer-no-duplication.md`.

### Empty-drawer behaviour pending user disposition

- `register/boundary/scope-config-loader`: **divergent**. The cycle-1 user-resolved ask said "empty drawer = deny-all defaults"; the cycle-2 implementation keeps the cycle-1 reconciliation's "empty drawer → nil → no_scope_config". Both authorities cannot stand. The user picks via `disposition-empty-drawer-collapse` (cycle-3). **Implication for this task**: any test that pins validator behaviour on an empty-drawer session should be flagged with `;; depends on disposition-empty-drawer-collapse` and skipped (`xit`) until the disposition lands. Renderer-side assertions ("when scope is empty, no `:GPTEL_SCOPE_*` keys are emitted") are not affected and should land normally.

### Cycle-2 implementation details to honour

- The implementor of `rewire-session-creation` (commit `be6b80c`) chose the byte-copy approach for branching (rather than the brief's recommended re-render). This is structurally equivalent IFF the parent was created via Mode 2a, which is now true after cycle-2. **Implication for this task**: branching tests should assert the child's drawer matches the parent's verbatim (via byte-comparison or `org-entry-get-multivalued-property` parity), not by re-rendering.
- The implementor retained `--initial-session-content` for legacy helper-level tests. **Implication for this task**: do not delete `session-org-creation-spec.el` or its references to `--initial-session-content`. The four direct helper-level tests pinning that helper's shape can stay; just update them to assert on drawer-text-block invariants.

### Test-suite delta this cycle

Cycle-2's full-suite count grew to 1707 specs / 92-95 failed. Of those, 5 expected-failure session/test specs are this task's primary work (state.json::tasks[2].test_output_tail).

### Unblocked

- `add-test-helper-with-scope-drawer` (closed cycle-1).
- `rewire-session-creation` (closed cycle-2).

## Observations

- Confirmed cycle-2 `rewire-session-creation` (commit `be6b80c`):
  `jf/gptel--create-session-core` now composes `session.org`'s initial
  content as `(concat drawer-text body)` where `drawer-text` is the
  `register/shape/drawer-text-block` string returned by
  `jf/gptel-scope-profile--create-for-session` (Mode 2a). No `scope.yml`
  is written; the renderer is the sole route from a profile-resolved
  scope-plist to drawer state. The brief's framing matched production.
- Test infrastructure choice: rather than depending on
  `config/gptel/scope/test/helpers-spec.el` (which lives in a sibling
  test directory and pulls in unrelated contract/bash-parser deps), the
  migration introduces two thin local helpers
  (`jf/gptel-test--drawer-multi-value`,
  `jf/gptel-test--drawer-scalar`) that wrap
  `org-entry-get-multivalued-property` / `org-entry-get` against the
  raw `session.org` content captured by `with-captured-io`. This keeps
  `session-creation-spec.el` self-contained on the `persistence-test-
  helpers` infrastructure it already imports.
- Negative assertion coverage: the dedicated `it "does not write
  scope.yml..."` test pairs a hash-table scan (`cl-some
  string-suffix-p` over `(hash-table-keys captured-files)`) with the
  `with-captured-io`-mocked `(file-exists-p ...) :to-be nil` form.
  Both fire from a preset that resolves to a non-empty scope-plist, so
  the case where a regression *would* re-introduce the YAML write is
  the case the test exercises.
- Empty-drawer renderer assertion: with
  `jf/gptel-preset--scope-defaults` bound to nil and the preset
  unregistered, `--resolve` returns nil, `--render-drawer-text` emits
  only `:GPTEL_PRESET:`, and the test verifies absence of every
  `:GPTEL_SCOPE_*:` key (both via the org-mode parser and a raw-text
  regex). This matches the renderer-side invariant pinned in the task
  body and is independent of the loader's deny-all-on-empty-drawer
  resolution from `disposition-empty-drawer-collapse`.
- Pre-existing exact-match test for the `executor` preset's drawer
  (lines 108-125) continues to assert the bare drawer shape
  (`:PROPERTIES:`/`:GPTEL_PRESET: executor`/`:END:`). This holds
  because the test environment has no registered scope-defaults for
  `executor`, so resolution returns nil and the renderer emits no
  `:GPTEL_SCOPE_*:` keys. The "Profile file missing for preset
  executor, profile: coding" warnings observed in test logs confirm
  the resolver's nil branch is the active path.
- Targeted test run (`./bin/run-tests.sh -d config/gptel/test`): 15
  specs, 0 failed (was 5 failed before migration). The 5 previously
  expected-to-fail "Scope profile integration" specs now pass under
  drawer-based assertions.
- Broader gptel run (`./bin/run-tests.sh -d config/gptel`): 1007 ran,
  924 passed. Remaining failures are in unrelated subsystems
  (bash-parser, contract layer, `run_bash_command` integration,
  `PersistentAgent creation flow`); none are in
  `session-creation-spec.el`.

## Discoveries

(none)
