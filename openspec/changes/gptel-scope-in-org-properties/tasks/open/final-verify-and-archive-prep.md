---
name: final-verify-and-archive-prep
description: Run full test suite, manual smoke tests, and opsx-verify; produce an archive-ready change
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-drawer-corruption-regression
  - blocked-by:finalize-no-scope-config-removal
---

## Cites register entries

This task is the omnibus verification before integrate. It cites every register entry the change touched so the integrate-phase reconciliation can enumerate dispositions:

- Shape: `scope-config-plist`, `drawer-text-block`, `violation-info`, `expansion-transient-scope` (cycle-3 add)
- Vocabulary: `drawer-key-set`, `operation-to-drawer-key`
- Boundary: `scope-config-loader`, `scope-profile-applicator`, `scope-pattern-writer`, `scope-expansion-action-handler`
- Invariant: `scope-parse-complete-is-true`, `scope-coverage-threshold-is-1`, `scope-no-security-key-in-plist`, `scope-drawer-no-duplication`, `scope-add-pattern-idempotent`, `expansion-queue-always-progresses` (cycle-3 add)

Verify by running the full suite (`./bin/run-tests.sh -d config/gptel/scope`) and grepping for any remaining `error "speculated; not implemented"` strings — every scaffolded stub must be either satisfied (test passes) or dispositioned (promoted / archived / rejected).

## Files to modify
- (no source code changes — verification and documentation only)
- Possibly snapshot files: `config/gptel/scope/test-results.txt`, `config/gptel/scope-profiles/test-results.txt` if those exist and are git-tracked.

## Implementation steps

1. Run the full test suite:

   ```bash
   ./bin/run-tests.sh
   make test
   ```

   All green. Capture the report:

   ```bash
   ./bin/run-tests.sh --report > /tmp/scope-drawer-final-report.txt
   ```

2. Refresh snapshots if any were tracked:

   ```bash
   ./bin/run-tests.sh -d config/gptel/scope --snapshot
   ./bin/run-tests.sh -d config/gptel/scope-profiles --snapshot
   ```

3. Manual smoke tests in `./bin/emacs-isolated.sh`:

   - **Create a fresh persistent session.** `M-x jf/gptel-persistent-session "smoke-drawer"`. Confirm the resulting `~/.gptel/sessions/smoke-drawer-*/branches/main/session.org` carries a `:PROPERTIES:` drawer with `:GPTEL_PRESET:` and the resolved `:GPTEL_SCOPE_*` keys. Confirm no `scope.yml` exists in the branch directory.

   - **Trigger a scope violation and add-to-scope.** Run a tool call that lands outside the initial scope. The expansion UI fires. Choose "Add to Scope". Confirm the chat buffer's drawer was updated, the buffer is now saved, and the next tool call against the same resource succeeds. Press `C-_` (undo) to confirm the drawer mutation is in the undo ring.

   - **Reopen the file.** Close the buffer and `find-file` the same `session.org`. Confirm the drawer round-trips and the validator sees the same scope.

   - **Create a sub-agent.** Invoke PersistentAgent from the chat buffer with `allowed_paths`. Confirm the agent's `session.org` carries the right drawer keys and the agent dir contains no `scope.yml`.

   - **Edit Manually.** From the expansion UI, choose "Edit Manually". Confirm focus shifts to the chat buffer's `session.org` with the drawer unfolded.

4. Run `openspec verify gptel-scope-in-org-properties` to validate spec scenarios are covered:

   ```bash
   openspec verify gptel-scope-in-org-properties
   ```

   If any scenarios are flagged as uncovered, file follow-up tasks rather than blocking the archive. (Per project convention, "every scenario has at least one test" is the goal; gaps go in `.tasks/` if cross-cutting or in `tasks/open/` if the change owns them.)

5. Update the repo's `CLAUDE.md` if the test directory layout reference (`config/gptel/scope/test/yaml/`) needs to be updated to reflect the deleted directory and the new `drawer/` directory. The existing mention is around `CLAUDE.md` § "Scope validation test organization".

6. Tag the change as ready to archive: confirm `tasks/open/` is empty (all tasks closed), `tasks/closed/` has 16 entries.

7. Run `openspec status --change gptel-scope-in-org-properties` and confirm `isComplete: true`.

8. Commit a final integration commit if changes accumulated across the change's lifecycle haven't already been committed task-by-task.

9. Run `/opsx-archive gptel-scope-in-org-properties` (or note that the change is archive-ready).

## Design rationale

The final-verify task is the explicit gate between "implementation done" and "archived". It catches integration gaps that per-task verification can't see (cross-cutting hook ordering, byte-compile warnings, real-world session lifecycle bugs). Manual smoke tests cover the user-visible surfaces — drawer round-trip, undo affordance, expansion UI, agent creation — that automated tests fixture in isolation.

The CLAUDE.md update keeps onboarding documentation aligned with the codebase. The `openspec verify` step is the formal scenario-coverage check.

## Design pattern

Treat this task as a checklist, not a coding task. Each step is a verification gate; if any step fails, file a fix task (or fold the fix into an existing task and reopen it) rather than band-aiding here.

## Verification

- `./bin/run-tests.sh` passes with no failures.
- All five manual smoke tests succeed (drawer creation, add-to-scope round-trip, reopen, agent creation, edit-manually).
- `openspec verify gptel-scope-in-org-properties` reports no spec-scenario coverage gaps (or any gaps are documented as follow-up tasks).
- `openspec status --change gptel-scope-in-org-properties` reports `isComplete: true`.
- `tasks/open/` is empty (all tasks closed).
- CLAUDE.md test-directory-layout reference matches reality (no `yaml/`; mentions `drawer/`).

## Context

design.md § Migration Plan step 11 (manual smoke)
architecture.md § Testing Approach (Running Tests)
proposal.md § Impact (User impact, Code removed)

## Cycle 1 updates (cycle-1777460733)

### Cited register entries (snapshot at end of cycle 1)
All 13 cited entries reached terminal disposition this cycle:
- 9 reconciled: `scope-config-plist`, `drawer-text-block`, `drawer-key-set`, `operation-to-drawer-key`, `scope-config-loader`, `scope-profile-applicator`, `scope-parse-complete-is-true`, `scope-coverage-threshold-is-1`, `scope-no-security-key-in-plist`
- 3 confirmed: `scope-pattern-writer`, `scope-drawer-no-duplication`, `scope-add-pattern-idempotent`
- 1 unchanged: `violation-info` (deferred — no cycle-1 implementation touched producers/consumers)

See `.orchestrator/handshake-cycle-1777460733.json::register_diff` and the reconciliation notes under `.orchestrator/cycles/cycle-1777460733/reconciliations/`.

### Already-shipped inline fixes (must still hold at verify time)
- `arch-cycle-1777460733-8`: `:deny` arm removed from `--map-operation-to-drawer-key`. **Verify**: `grep -n ':deny' config/gptel/scope/scope-expansion.el` shows no `(eq operation :deny)` arm in the mapper.
- `arch-cycle-1777460733-9`: strict-error fallback with `(null operation)` arm. **Verify**: feeding `:operation nil` to the writer in a smoke test errors loudly with the documented message.
- `arch-cycle-1777460733-11`: write-side cloud-auth validation in `scope-profiles.el`. **Verify**: `grep -n 'validate-cloud-auth\|cloud-auth-values' config/gptel/scope-profiles.el` returns the constant + predicate + two call sites.

### User-resolved decisions to verify landed
- `ask-arch-cycle-1777460733-1` (B applied inline): writer fallback is strict-error. Verify via test output, not just code grep.
- `ask-arch-cycle-1777460733-2` (b deferred): empty drawer = deny-all defaults. Verify by manual smoke (create a session with empty profile; confirm validator denies tool calls outside default deny-all).

### Open asks still routed to user (do NOT block archive on these)
The next plan-phase carries three open asks routed to user (`asks_for_user_open` in handshake):
- `ask-arch-cycle-1777460733-10A` (read-metadata bucket)
- `ask-arch-cycle-1777460733-10B` (match-pattern handling)
- `ask-arch-cycle-1777460733-10C` (delete vs write bucket)

Each blocks `rewire-expansion-writer`. The corresponding disposition tasks (`disposition-read-metadata-bucket`, `disposition-match-pattern-handling`, `disposition-delete-vs-write-bucket`) plus `refuse-add-to-scope-on-nil-operation` exist in `tasks/open/`. **Implication for this task**: the archive-readiness check should treat the four disposition tasks as explicit gates — confirm they are either resolved (closed) or explicitly excluded from this change before flipping to archive.

### Tasks/closed/ count update
> Step 6 currently says `tasks/closed/` has 16 entries. Cycle 1 closed 5 implementor tasks. With the 11 pre-existing open + 4 new disposition follow-ups, total tasks at archive time = 5 (closed cycle-1) + 11 (in-flight) + 4 (disposition follow-ups) = 20, not 16. The "16" number predates the cycle-1 follow-up task creation; reconfirm at archive time.

## Cycle 2 updates (cycle-1777470320)

### Cited register entries — cycle-2 dispositions

- `register/shape/scope-config-plist`: **confirmed** (cycle-2). `:read-metadata` is now part of the `:paths` sub-plist.
- `register/boundary/scope-config-loader`: **divergent** (cycle-2). User must disposition via `disposition-empty-drawer-collapse` before this verify task can pass.
- `register/boundary/scope-pattern-writer`: **confirmed** (cycle-2). Writer is the sole drawer-mutation path.
- `register/boundary/scope-profile-applicator`: **confirmed** (cycle-2). Mode 2a is the sole producer; sessions and persistent-agent both consume.
- `register/vocabulary/operation-to-drawer-key`: **confirmed** (cycle-2). Cycle-2 dispositions 10A/B/C all encoded with strict-error fallbacks.
- `register/vocabulary/drawer-key-set`: **confirmed** (cycle-2). Eight-key set including `GPTEL_SCOPE_READ_METADATA` honored at reader and writer; fixture-locus pending in cycle-3 migrate-* tasks.
- `register/invariant/scope-no-security-key-in-plist`: **confirmed** (cycle-2). L1+L2 hold; this task's verification step is the residual non-validation lint.
- `register/invariant/scope-parse-complete-is-true`: **confirmed** (cycle-2). Defconst is `t`.
- `register/invariant/scope-coverage-threshold-is-1`: **confirmed** (cycle-2). Defconst is `1.0`.
- `register/invariant/scope-add-pattern-idempotent`: **confirmed** (cycle-2). Writer's `(member pattern existing)` short-circuit holds.
- `register/invariant/scope-drawer-no-duplication`: **confirmed** (cycle-2). Both creation paths emit one drawer.
- `register/boundary/scope-expansion-action-handler`: **unchanged** (cycle-2 — deferred). Cycle-3's `harden-add-to-scope-action-handler` is the implementing task; expect this entry to flip to confirmed/reconciled at cycle-3 integrate.

### Cycle-2 architect findings to verify-resolved

- `arch-cycle-1777470320-1` (advisory, dead-branch): folded into `delete-yaml-and-security-residue`. **Verify**: greps for `jf/gptel--scope-file-path` and `jf/gptel-session--scope-file` return no results.
- `arch-cycle-1777470320-2` (advisory, interface-drift): folded into `delete-yaml-and-security-residue`. **Verify**: greps for `scope-yaml`, `scope.yml`, `YAML` in `config/gptel/scope/interfaces.org` and `config/gptel/scope/scope-shell-tools.org` return no results except possibly historical commit references.
- `arch-cycle-1777470320-3` (advisory, invariant-gap): folded into `add-expansion-transient-and-queue-register-entries` (cycle-3). **Verify**: `interfaces.org` carries the new entries `register/shape/expansion-transient-scope` and `register/invariant/expansion-queue-always-progresses`.
- `arch-cycle-1777470320-4` (blocking, interface-drift): user disposition; `disposition-empty-drawer-collapse` resolves it. **Verify**: `register/boundary/scope-config-loader` status is `confirmed` or `reconciled` (not `divergent`).

### Step 6 task-count update (cycle-2)

> Cycle-2 added 2 new tasks (`disposition-empty-drawer-collapse`, `add-expansion-transient-and-queue-register-entries`) and closed 4 (`rewire-validator-config-load`, `rewire-expansion-writer`, `rewire-session-creation`, `rewire-persistent-agent`). Total tasks at archive time, projected: 8 (closed cycle-1) + 4 (closed cycle-2) + 8 (open + 2 new) = 20, give or take cycle-3 follow-ups. Reconfirm at archive time.

### Open asks still routed to user (cycle-2)

- `ask-arch-cycle-1777470320-1` (empty-drawer collapse). The corresponding disposition task is `disposition-empty-drawer-collapse`. This task's archive gate must wait until that disposition is closed.

### Add to manual smoke tests (cycle-2)

After the cycle-2 rewires shipped, add a smoke test for the expansion writer's three new dispositions:

- **`:read-metadata` violation → metadata bucket**: trigger a bash command that reads only metadata (e.g. `[ -f /etc/passwd ]`); add to scope; confirm the drawer's `:GPTEL_SCOPE_READ_METADATA:` carries the path, NOT `:GPTEL_SCOPE_READ:`.
- **`:match-pattern` redirect**: trigger `find /home -name '*.txt'`; add the `:match-pattern` violation; confirm `/home` lands in `:GPTEL_SCOPE_READ:`, NOT `'*.txt'`. (Requires `harden-add-to-scope-action-handler` to be landed first; until then the writer errors loudly — that's the expected behaviour to verify pre-harden.)
- **`:delete` → WRITE bucket**: trigger `rm /tmp/x`; add to scope; confirm `/tmp/x` lands in `:GPTEL_SCOPE_WRITE:`, NOT a separate delete bucket.

## Cycle 3 updates (cycle-1777478129)

Cycle-3 closing batch shipped (8 of 8 cycle-3 tasks done; commits `a8317fc` through `b5f056e`). All 16 cited register entries are dispositioned at integrate:

- **Reconciled (3)**: `scope-config-loader` (Option B applied; `divergent → reconciled`), `expansion-transient-scope` (as-built 5-key shape captured; `speculated → reconciled`), `expansion-queue-always-progresses` (5-handler queue-pump invariant pinned by L1 + L2 tests; `speculated → reconciled`).
- **Confirmed (12)**: every other cited entry; speculation matched implementation. See `.orchestrator/cycles/cycle-1777478129/reconciliations/` for individual notes.
- **Divergent → user-resolution (1)**: `scope-expansion-action-handler` — Stage 2 shipped as refusal-with-guidance, not redirect. Cycle-3 PM digest Ask 1 routes the disposition to the user (default recommendation: Option B — update register entries). See `.orchestrator/cycles/cycle-1777478129/findings/arch-cycle-1777478129-4.md`.

**Implications for this task's verification step**:

1. The Stage 2 `:match-pattern` smoke-test recipe in this body (line ~"`:match-pattern` redirect") is now PARTIALLY OBSOLETED by the harden as-shipped behaviour. The expected outcome is **NOT** "redirect to `/home`" but "user-error directing the user to `c` (custom) or `e` (edit manually)". Update the smoke-test note accordingly when running verification — assert the action handler signals `user-error` and the writer is NOT invoked.

2. The `:GPTEL_SCOPE_READ_METADATA:` path through `--load-from-buffer` was missing from the loader pre-cycle-3 architect-finding-2; cycle-3 commit `b5f056e` added it. The smoke test for `:read-metadata` violations should now round-trip correctly through both the loader and the validator. New regression specs at `path-validation-spec.el:436+` pin the end-to-end path.

3. Three spec-signal user asks remain open (PM digest § "Asks for the user"). Verification should NOT proceed to archive until those asks are resolved or explicitly dispositioned-as-deferred:
   - Ask 1: register entry edit for `:match-pattern` Stage 2 contract (Option B default).
   - Ask 2: provider keyword vs string convention (Option B default — keywords + loader normaliser).
   - Ask 3: drawer emission `+:` continuation form vs single-line (Option B default — amend spec to match writer).

4. Two follow-up cycle-4 tasks remain open: `add-drawer-corruption-regression` (existing) and `finalize-no-scope-config-removal` (new — cycle-3 architect finding 3 follow-up). This task is now `blocked-by` both; remove the legacy blocked-by list of cycle-3-completed tasks (already done at cycle-3 integrate).

5. Test-suite delta: 1707 specs / 95 failed → 1631 specs / 80 failed (net 15 failures resolved; 76 specs deleted with the YAML test directory). The 80 remaining failures are pre-existing in directories/tests outside cycle-3 scope (bash-parser ERT failures, pre-existing buttercup pin-failures unrelated to the scope rewire). Final verify should match this baseline; new failures vs cycle-3 close are regressions.
