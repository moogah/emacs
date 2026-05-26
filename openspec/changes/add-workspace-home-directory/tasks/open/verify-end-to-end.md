---
name: verify-end-to-end
description: Full test run; manual sanity check of scaffold + anchor + delete + purge + broken-home flows; all spec scenarios pass
change: add-workspace-home-directory
status: blocked
relations:
  - blocked-by:workspace-new-anchor-existing
  - blocked-by:workspace-delete-and-purge
  - blocked-by:update-docs-readme
---

## Files to modify
*(verification-only task; no production code changes)*

Possibly: small fix-up commits if regressions surface. Snapshot files
(if `--snapshot` is used) may be updated.

## Implementation steps

1. **Full test suite, all frameworks**:
   ```bash
   ./bin/run-tests.sh
   ```
   Must pass with zero failures across both Buttercup and ERT.

2. **Workspaces-scoped run with snapshot regen** to refresh the
   baseline for future regressions:
   ```bash
   ./bin/run-tests.sh -d config/workspaces --snapshot
   git diff config/workspaces/test-results.txt
   ```
   Inspect the snapshot diff — every changed line should be expected
   (new scenarios passing, no previously-passing scenarios now
   missing).

3. **Spec-scenario coverage audit.** For each scenario in
   `openspec/changes/add-workspace-home-directory/specs/workspaces/spec.md`,
   confirm at least one `it` clause in the corresponding test file
   covers it. Quick way:
   - `grep -c '#### Scenario' openspec/changes/add-workspace-home-directory/specs/workspaces/spec.md`
   - `grep -rc '(it "' config/workspaces/test/`
   - The latter should be ≥ the former for the new behavior.
   - If gaps exist, write the missing `it` clauses now.

4. **Manual end-to-end smoke** in an isolated Emacs
   (`./bin/emacs-isolated.sh`):

   - **Default-path workspace creation**:
     - `M-x workspace-new alpha` → no prompts.
     - Inspect `~/emacs-workspaces/alpha/` from a shell — `.git/`,
       `home.org` (with `#+TITLE: alpha`), `sessions/<date>-initial.org`,
       one git commit titled `Initial workspace`.
     - Verify the tab `alpha` is selected and `home.org` is the
       displayed buffer.

   - **#+TITLE: display override**:
     - Edit `~/emacs-workspaces/alpha/home.org`; change `#+TITLE: alpha`
       to `#+TITLE: Alpha Project`.
     - Verify (wherever the display name surfaces — tab label,
       mode-line, or `M-x workspace-switch` prompt) that
       "Alpha Project" appears, and registry/tab-key remains `alpha`.

   - **Anchor existing repo with home.org** (case 1):
     - `mkdir /tmp/anchor1 && cd /tmp/anchor1 && git init && echo "#+TITLE: anchor1" > home.org`
     - `C-u M-x workspace-new` → choose `/tmp/anchor1`.
     - Verify: no new files, no new commits; tab `anchor1` appears.

   - **Anchor existing repo without home.org** (case 2):
     - `mkdir /tmp/anchor2 && cd /tmp/anchor2 && git init`
     - `C-u M-x workspace-new` → choose `/tmp/anchor2`.
     - Verify: `home.org` and `sessions/<date>-initial.org` appear as
       UNTRACKED in `git status`; no new commits.

   - **Anchor non-repo** (case 3):
     - `mkdir /tmp/anchor3`
     - `C-u M-x workspace-new` → choose `/tmp/anchor3`.
     - Verify: full scaffold + initial commit; tab appears.

   - **Delete is non-destructive**:
     - `M-x workspace-delete alpha`.
     - Inspect `~/emacs-workspaces/alpha/` → still present with
       contents intact.

   - **Re-anchor after delete**:
     - `C-u M-x workspace-new` → choose `~/emacs-workspaces/alpha/`.
     - Verify it registers without scaffolding side effects (case 1
       path) and tab returns.

   - **Purge with safeguard**:
     - `M-x workspace-purge anchor1` → asserts safeguard error
       (anchor1 is outside default parent).
     - `C-u M-x workspace-purge anchor1` → yes-or-no prompt →
       confirm → verify dir is gone.

   - **Purge inside default parent**:
     - `M-x workspace-purge alpha` → yes-or-no prompt → confirm →
       verify `~/emacs-workspaces/alpha/` is gone.

   - **Broken-home tolerance**:
     - `M-x workspace-new bravo`.
     - From a shell: `rm -rf ~/emacs-workspaces/bravo/`.
     - `M-x workspace-save` then quit Emacs (or just quit; broken-state
       detection happens on next load).
     - Restart Emacs. Check `*Messages*` for the notice naming
       `bravo` and the missing path.
     - `M-x workspace-switch bravo` → assert user-error mentioning
       `workspace-re-anchor` and `workspace-purge`.
     - `M-x workspace-re-anchor bravo` → pick `~/emacs-workspaces/`
       or another fresh dir; verify recovery.

   - **gptel session routing**:
     - On workspace `alpha` (re-created): `M-x gptel-sessions-new`
       (or whatever the actual entry point is named) → assert the
       new file is under `~/emacs-workspaces/alpha/sessions/`.
     - `C-u M-x gptel-sessions-new` → assert global directory.
     - Switch to a non-workspace tab → `M-x gptel-sessions-new` →
       assert global directory.

5. **Persistence-v3 migration check**:
   - Save a workspaces persistence file by hand (or grab one from
     git stash before this branch) at `:version 2`.
   - Restart Emacs.
   - Assert: `*Messages*` notice naming the file and v2 mismatch;
     registry is empty; no auto-recreation.

6. If any of the above reveals a defect, file a fix-up commit on the
   relevant module and re-run the affected manual + automated tests
   before marking this task done.

## Design rationale

Verification is a separate task so it lands after all functional
changes are merged, providing a single coherent end-to-end review
moment rather than partial validation in each PR. The manual smoke
catches integration issues that unit tests miss (key bindings, UI
flow, real `gptel` interaction).

The scenario-coverage audit (step 3) is a structural check that the
spec-driven discipline actually held — every behavioral promise in
the spec has at least one automated test.

## Verification

- All commands above complete without error.
- Snapshot diff is fully expected and committed.
- Manual smoke covers all spec scenarios (see step 4 mapping).
- No regressions in the broader test suite (`./bin/run-tests.sh`).

## Context

design.md § Decisions / D9 — testing approach (full suite + snapshot)
specs/workspaces/spec.md (all scenarios — verify coverage)
proposal.md (verify the proposal's "What Changes" list is fully delivered)


## Cycle 2 updates (cycle-20260525-213500)

### Status

- One of 6 blockers (`gptel-sessions-workspace-consult`) closed at
  merge `8ce82df`. Still blocked by 5 cycle-3+ tasks.

### Cycle-2 register-diff hits relevant to this task

The verify suite will need to exercise:

- The scaffold pipeline (default-path; anchor sub-cases) per
  `register/boundary/workspace-scaffold-pipeline` (reconciled).
  Manual sanity check: invoke `workspace-new` (no prefix) and
  `C-u workspace-new` against existing + non-existing directories.

- The gptel routing per
  `register/boundary/gptel-sessions-workspace-consult` (reconciled).
  Manual sanity check: on a workspace tab, `M-x
  jf/gptel-persistent-session` files under `<HOME>/sessions/`;
  `M-x jf/gptel-persistent-session-global` files in
  `jf/gptel-sessions-directory`.

- Persistence v3 round-trip per
  `register/invariant/home-required-no-floating-workspaces` (re-
  confirmed) and `register/invariant/broken-tag-runtime-only`
  (reconciled). Manual sanity check: save with at least one broken
  workspace; load; assert `:broken` is set freshly (not from disk)
  and `:restore-pending` is set fresh on every loaded entry.

- The new spec files added this cycle that the verify run picks
  up automatically: `persistence-v3-spec.el`, `broken-home-load-spec.el`,
  `scaffold-spec.el`, `gptel-integration-spec.el`,
  `workspace-routing-spec.el`.

### Cycle-2 inline-fix hits

The orchestrator inline-fix commits (`63d60ec` cross-contract
collision; `cd1d721` architect findings) both shipped with green
test runs (195/0 workspaces + 98/0 gptel/sessions). Your full
verify should reproduce these numbers.

### Cycle-2 user-ask resolution dependency

The verify of the gptel UX (prefix-arg vs separate command) cannot
proceed until `ask-cycle-20260525-213500-2` is dispositioned. If
the user picks "revise spec" → verify against the separate-command
UX. If "re-implement to rebind" → verify against prefix-arg
behaviour. If "defer" → verify against the current separate-
command UX with the spec marked tentative.

## Cycle 3 updates (cycle-20260526-171719)

### Status

- Still `blocked`. Two of five cycle-2 blockers cleared this cycle
  (`workspace-new-default-path` and `broken-home-tolerance` both done).
  Three remain: `workspace-new-anchor-existing`,
  `workspace-delete-and-purge`, `update-docs-readme`. Terminal task
  for archiving the change.

### Cycle-3 scope additions to verify

The verification matrix should now include:

- **Default-path scaffold flow**: `M-x workspace-new foo` →
  `~/emacs-workspaces/foo/` exists with `.git/`, `home.org`,
  `sessions/<date>-initial.org`, one git commit. Tab shows `home.org`.
- **Collision detection**: pre-create `~/emacs-workspaces/foo/`; invoke
  `M-x workspace-new foo`; expect hard `user-error`.
- **Default home builder**: after `workspace-new foo`, the active
  buffer is `<home>/home.org`.
- **`workspace-re-anchor` (`C-x w R`)**: create workspace, `rm -rf`
  the home dir, restart Emacs, observe `*Messages*` notice,
  `M-x workspace-switch <name>` → user-error naming `workspace-re-anchor`
  and `workspace-purge`; `M-x workspace-re-anchor` → pick fresh dir;
  registry updates with `:home` and `:name` BOTH equal to the new
  basename; rename survives save/restore.
- **Activation guards**: `workspace-switch` and `workspace-restore`
  on a broken workspace produce the user-error; consistent message.
- **Absolute-path enforcement**: hand-write a persistence file with
  a relative-path `:home`; restart; observe the `*Messages*`
  "skipping persisted entry %S — :home %S is not absolute" notice
  and confirm the entry is NOT in the registry.

### Cycle-3 register-touch coverage (use as a checklist)

Verify the verification covers the cycle's 9 touched register entries:

- `home-org-user-authored-after-creation` — confirm
  `home-org-writer-lint-spec.el` passes (note: see open ask
  `ask-cycle-20260526-171719-1` on heuristic adequacy).
- `workspace-plist-v3` — confirm `workspace--set-name` lockstep
  enforcement is exercised (the re-anchor rename test does this).
- `workspace-scaffold-pipeline` — confirm scaffold runs cleanly +
  leaves partial on failure.
- `scaffold-leave-partial-on-failure` — exercise scaffold failure
  (e.g., target inside a read-only dir) and confirm no auto-cleanup.
- `home-required-no-floating-workspaces` — exercise the absolute-path
  deserializer arm; note open ask `ask-cycle-20260526-171719-2` on
  constructor-side enforcement.
- `home-org-read-pipeline` — confirm home-builder routes through
  `workspace-home-org-path`.
- `broken-tag-runtime-only` — confirm `:broken` is set on load and
  cleared on re-anchor; persistence file does NOT contain `:broken`.
- `registry-name-equals-basename` — confirm the rename-on-different-
  basename branch updates both `:home` and `:name`, and the rename
  survives save/restore.
- `workspace-broken-disposition` — confirm error messages name both
  remediation commands consistently.

### Open asks (verification should NOT preempt user disposition)

If `ask-cycle-20260526-171719-1` (writer-lint heuristic) is
unresolved at verification time, verify the current lint state but
flag the trivially-passing concern.

If `ask-cycle-20260526-171719-2` (*scratch* fallback) is unresolved,
verify the current behaviour (fallback exists; one spec exercises it)
but flag in the verification report.
