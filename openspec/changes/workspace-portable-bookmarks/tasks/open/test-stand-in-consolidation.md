---
name: test-stand-in-consolidation
description: Move per-spec stand-ins (fake TRAMP handlers, registered-names stand-ins, the bookmark-jump harness) into the shared fixtures module
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790604160
cites_register_entries:
  - register/invariant/spec-registry-isolation-via-fixtures
relations:
  - discovered-from:eoc-1790453714-03
  - discovered-from:meta-discovery/fixture-regrowth
discovered_from: [eoc-1790453714-03, meta-discovery/fixture-regrowth]
discovered_by: architect
discovered_class: duplication
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714 from `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § eoc-1790453714-03.
> Pressure-tested at plan of cycle-1790604160 against HEAD c65efb86 (finding arch-cycle-1790604160-plan-04). Wave 3, LAST: it edits
> spec files that waves 1-2 also edit (missing-repo, jump, off-workspace), and its counting validator must land on the FINAL spec set.

## Counts at c65efb86 (re-verified)
- Fake-remote file-name handler: FIVE, two dialects. "Name ops pass" (nine name operations pass through, everything else records and
  signals): `broken-home-load-spec.el:151`, `workspace-delete-purge-spec.el:579`, `workspace-new-anchor-spec.el:363`. "Only
  `file-remote-p`" (everything else signals `file-error`): `git-worktrees-spec.el:209`, `portable-bookmarks-cost-spec.el:366`.
- `workspace--registered-names` stand-in `(unless (fboundp ...) (defun ...))`: SIX — `portable-bookmarks-cost-spec.el:44`,
  `git-worktrees-spec.el:53`, `portable-bookmarks-discovery-spec.el:49`, `portable-bookmarks-jump-spec.el:47`,
  `portable-bookmarks-missing-repo-spec.el:64`, `portable-bookmarks-resolver-spec.el:44`. (The fixtures module already has a
  `(defvar workspace--registry ...)` fallback at :47; the stand-in belongs beside it.)
- Bookmark-jump harness (`--hermetic`, `--world`, `--set`, `--jump`, `--decline`, `--visits-p`): THREE —
  `portable-bookmarks-jump-spec.el:69-170`, `portable-bookmarks-missing-repo-spec.el:103-215`,
  `portable-bookmarks-off-workspace-spec.el:103-280`. Divergence: off-workspace's `--visits-p` (:273) lacks the Dired arm that
  jump-spec's (:161-170) and missing-repo's (:204) have. `portable-bookmarks-discovery-spec.el:64` has a fourth `--hermetic` macro
  (no jump helpers) — fold it in if its body is the same shape; otherwise leave it and say why.

## Files
- `config/workspaces/test/portable-bookmarks-fixtures.el` (registered-names stand-in; jump harness).
- NEW core test helper, e.g. `config/workspaces/test/workspaces-test-helpers.el` (the fake-remote handler) — see step 1.
- The five handler files, the six stand-in files, the three harness files (above); `config/workspaces/test/portable-bookmarks-spec.el`
  (the counting validator, beside the existing fixture specs :99-256).
- No production code.

## Steps
1. ONE fake-remote file-name handler. HOME DECIDED: a CORE test-helper file that both core specs and the fixtures module load —
   `register/invariant/spec-registry-isolation-via-fixtures` › known_violations records the accepted rule "a core spec must not depend
   on a droppable consumer's fixtures", and three of the five copies are core specs (broken-home-load, workspace-delete-purge,
   workspace-new-anchor). `portable-bookmarks-fixtures.el` `load`s the helper (the specs already `load` the fixtures by path,
   jump-spec.el:38, so the same pattern works for the helper). Shape: a `with-fake-remote` macro parameterised by PREFIX, recording
   touches, with the STRICT permitted set by default (only `file-remote-p` is free — matching the register's "never contacted"
   wording) and an explicit `:allow` list of name operations for a spec that needs one (the three "name ops pass" specs list the
   nine they use today; the implementor checks each still passes with only the operations it actually needs). Replace the five copies.
2. ONE `workspace--registered-names` stand-in in `portable-bookmarks-fixtures.el` next to the `workspace--registry` fallback (:47);
   replace the six copies.
3. ONE bookmark-jump harness in `portable-bookmarks-fixtures.el` based on jump-spec's Dired-aware `--visits-p` (:161-170), with
   `--hermetic` / `--world` / `--set` / `--jump` / `--decline` parameterised where the three copies differ (missing-repo's `--jump`
   takes an ANSWER; off-workspace's takes an answer and a chooser). Replace the three copies; the off-workspace specs about directory
   bookmarks stop passing vacuously — note any that go red and fix the SPEC, not the harness.
4. Counting validator in `portable-bookmarks-spec.el`: a spec that reads every `config/workspaces/test/*-spec.el` and asserts ZERO
   matches of `(defun workspace--registered-names`, `remote-handler (operation`, `--visits-p (path)` and `defmacro [a-z-]*--hermetic`
   outside the fixtures module and the core helper (register validator 6). It runs in the normal suite, so a regrown copy fails the
   run, not only a review.
5. No inline stand-in of any kind is added by this task; every helper it creates lives in the fixtures module or the core helper.

## Cycle-3 on-touch additions (arch-on-touch-1790604160-08, from the wave-1 gate merge 360ad60c)
- `portable-bookmarks-missing-repo-spec.el` gained an inline `--preferring` answer stand-in (~:164-176) that subsumes the pre-existing
  inline `--answering` (~:154-162): fold both into ONE fixtures helper (the harness's answer parameter, step 3) and delete the inline pair.
- The same file fakes a remote home via `cl-letf` on `file-remote-p` / `file-directory-p` (~:460-477): a SIXTH remote stand-in of a
  different shape than the five `*-remote-handler` handlers. Route it through the single fake-remote helper (step 1) and make the
  counting validator (step 4) also match `cl-letf.*file-remote-p` outside the fixtures module and the core helper.
- Re-verify all counts at your base commit (the wave-1 and wave-2 merges shift lines and may add stand-ins).

## Verification
- Isolated suite exit 0; full suite `compare-baseline.py` → NO-REGRESSION; spec count unchanged except the validator (+1) and any
  off-workspace directory spec that the Dired-aware `--visits-p` turns from vacuous into real (list them).
- `grep -c '(defun workspace--registered-names\|remote-handler (operation\|--visits-p (path)\|defmacro [a-z-]*--hermetic' config/workspaces/test/*-spec.el`
  → 0 for every file (the validator's own grep).
- Sample 3 replaced specs (one per family) and show each still fails with its production change reverted.
- `grep -n "require\|load" config/workspaces/test/broken-home-load-spec.el config/workspaces/test/workspace-delete-purge-spec.el config/workspaces/test/workspace-new-anchor-spec.el`
  → they load the core helper, NOT `portable-bookmarks-fixtures.el`.

## Register
On-touch fires on `register/invariant/spec-registry-isolation-via-fixtures` (validator 6, the counting validator; the helper's home).
Neighbours: `.orchestrator/cycles/cycle-1790604160/briefs/entries-test-stand-in-consolidation.txt`.
