---
name: cleanup-workspaces-test-isolation
description: Backport the workspace-new-default-spec.el after-each cleanup pattern (delete tmpdir + restore defcustom) to the 9 pre-existing workspaces test files updated during cycle-3
change: add-workspace-home-directory
status: done
relations:
  - discovered-from:workspace-new-default-path
---

## Resolution (2026-06-06, manual review)

Root cause confirmed: in all 9 files the
`workspaces-default-parent-directory` tmpdir was created inline with
`make-temp-file` and never captured, so it could never be deleted —
even the files with an existing `--cleanup` only deleted a *separate*
state-file tmpdir. Fix (modelled on `wnd-spec--cleanup`): capture the
parent-dir in a per-file `*--parent-dir` defvar in the `before-each`
setup, and delete it in an `after-each`. No defcustom restore needed —
every `before-each` re-sets the value (same shape as the model).

- Extended existing `--cleanup` + defvar: `anti-save-spec.el`,
  `persistence-spec.el`, `revert-spec.el`, `save-restore-spec.el`.
- Added defvar + `--cleanup` fn + `after-each` to each describe block:
  `buffer-membership-spec.el` (4), `home-spec.el` (1),
  `tabs-spec.el` (3), `layouts-spec.el` (helper sites + the inline
  `workspace--capture-frameset` block), `buffer-reincarnation-spec.el`
  (3 inline blocks).

Verified: `./bin/run-tests.sh -d config/workspaces` → 231 passed;
isolated re-runs of the fixed files leave zero net tmpdir
accumulation (before == after).

<!-- Provenance fields (orchestrator schema):
     discovered_from: workspace-new-default-path
     discovered_by: reviewer (workspace-new-default-path, Finding 2,
       severity: advisory)
     discovered_class: sub-par-code
     reconciled_into: n/a — no register entry pressure; pure
       test-hygiene cleanup -->

## Why this task exists

Cycle-3's `workspace-new-default-path` task expanded scope to update
9 pre-existing workspaces test files so they would survive the new
`workspace-new` contract (real filesystem effects via the scaffold
pipeline). Each file's `before-each` block does:

```elisp
(setq workspaces-default-parent-directory (make-temp-file "..." t))
```

with **no restoration and no tmpdir cleanup**. Two consequences:

- Per-test tmpdirs accumulate under `/tmp` across spec invocations.
- The defcustom's value leaks across files within a single run.

The new spec `config/workspaces/test/workspace-new-default-spec.el`
models the correct shape via its `wnd-spec--cleanup` `after-each`
(deletes the tmpdir; lets the next spec re-initialise the defcustom
via its own `before-each`). The 9 pre-existing files want the same
pattern.

The functional impact on today's suite is minimal (OS reaps tmpdirs;
the leaked value happens to be a valid tmpdir for the next spec),
but the pattern sets a precedent the cycle-4+ spec-update work will
copy.

## Files to modify

The 9 spec files identified by the cycle-3 reviewer:

- `config/workspaces/test/anti-save-spec.el`
- `config/workspaces/test/buffer-membership-spec.el`
- `config/workspaces/test/buffer-reincarnation-spec.el` (3 sites)
- `config/workspaces/test/home-spec.el`
- `config/workspaces/test/layouts-spec.el` (2 sites)
- `config/workspaces/test/persistence-spec.el`
- `config/workspaces/test/revert-spec.el`
- `config/workspaces/test/save-restore-spec.el`
- `config/workspaces/test/tabs-spec.el`

Reviewer's line citations are in
`.orchestrator/cycles/cycle-20260526-171719/reviews/workspace-new-default-path.md`
under Finding 2.

## Implementation steps

1. In each file, add an `after-each` hook to the relevant `describe`
   block(s) that:
   - Deletes the tmpdir created in the `before-each`.
   - Restores `workspaces-default-parent-directory` to its prior
     value (or, simpler, captures the original in a let-bound
     `before-each` local and uses `:wrap` semantics).

2. Modelled on `wnd-spec--cleanup` in
   `config/workspaces/test/workspace-new-default-spec.el`.

3. Each file is independent; do them one at a time or batch — no
   inter-file ordering constraint.

## Verification

```bash
./bin/run-tests.sh -d config/workspaces
```

Expected: all 209+ specs pass. After the run, `/tmp/` should not
accumulate per-test directories beyond what was present pre-run.

## Cited register entries

None — this is pure test hygiene; no register entry pressure.

## Notes for the implementor

This is a low-risk mechanical cleanup. It blocks no other task.
Pick it up between cycles if the orchestrator has capacity.
