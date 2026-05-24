---
name: cutover-remove-legacy
description: Hard-cutover commit that removes activities, activities-extensions, perspective, and the activities-integration gptel module. Run only after the user judges the workspaces package stable.
change: add-workspaces-package
status: done
relations:
  - "blocked-by:keybindings-and-prefix"
---

## Files to modify

- `config/activities/` (delete entire subtree).
- `config/gptel/sessions/activities-integration.org` (delete).
- `config/gptel/sessions/activities-integration.el` (delete — generated).
- `config/core/window-management.org` (modify) — drop `(use-package activities ...)` and `(use-package perspective ...)` blocks; leave winner-mode and ace-window.
- `config/gptel/gptel.org` (modify) — drop the `jf/load-module` line for `activities-integration.el`.
- `init.org` (modify) — remove `("activities/activities" ...)` entry from `jf/enabled-modules`.
- `openspec/specs/activities-extensions.md` (delete) — superseded by `openspec/specs/workspaces.md` (the latter lands when this change is archived).

## Implementation steps

**Precondition**: the user has explicitly confirmed the workspaces package is stable and ready for cutover. This task is `status: blocked` until then. The blocker is *judgment*, not a mechanical predicate.

1. Confirm with the user that cutover is authorized. (This is the manual gate.)
2. Capture the current keybindings under `C-x C-a` (activities) and `C-c M-p` (perspective) for the changelog — these go away in this commit.
3. Delete `config/activities/` (entire directory). Verify with `git status` — should show all activity files as deleted.
4. Delete `config/gptel/sessions/activities-integration.org` and its `.el`.
5. In `config/gptel/gptel.org`, remove the `jf/load-module` line referencing `activities-integration.el`. Tangle and confirm `config/gptel/gptel.el` no longer references it.
6. In `config/core/window-management.org`:
   - Delete the `* Perspective` heading and its `use-package perspective` block (lines roughly 30-45).
   - Delete the `* Activities` heading and its `use-package activities` block (lines roughly 59-95).
   - Delete the corresponding `** Perspective` and `** Activities` subsections in `* Usage`.
   - Leave `* Winner Mode` and `* Ace Window` untouched.
7. In `init.org`, remove the `("activities/activities" ...)` entry from `jf/enabled-modules`.
8. Tangle all modified `.org` files (see Verification).
9. Delete `openspec/specs/activities-extensions.md`. (The new main spec for `workspaces` lands at `openspec/specs/workspaces.md` during `/opsx-archive` of this change — no manual move needed here.)
10. Launch isolated Emacs and confirm clean startup:
    - No errors loading.
    - `(featurep 'activities)` is nil.
    - `(featurep 'perspective)` is nil.
    - `(featurep 'workspaces)` is t.
    - `activities-ext-create` is unbound.
11. Run the full test suite to confirm no other modules silently depended on activities or perspective: `./bin/run-tests.sh`.
12. Optionally (not part of this commit; user discretion): `rm -rf state/activities/` to clean the legacy persistence files. Workspaces' persistence under `state/workspaces/` is unaffected.

## Design rationale

Cutover is intentionally a single atomic commit (design.md §"Migration Plan" Phase 2) so that:

- The repository is never in a state where two systems both believe they own the tab bar.
- Rollback is a single `git revert`.
- The diff serves as the canonical record of what disappeared.

The pre-cutover side-by-side period (design.md §D8, §"Migration Plan" Phase 1) means by the time this task runs, the user has already been driving workspaces day-to-day and can sign off with confidence. There is no automated "the package is stable" predicate; the gate is the user's judgment.

Removing `openspec/specs/activities-extensions.md` matches the REMOVED delta declared in `specs/activities-extensions/spec.md`. The archive workflow (`/opsx-archive`) will also move `openspec/changes/add-workspaces-package/specs/workspaces/spec.md` into `openspec/specs/workspaces.md`, completing the spec rotation.

## Verification

- `./bin/tangle-org.sh config/core/window-management.org`
- `./bin/tangle-org.sh config/gptel/gptel.org`
- `./bin/tangle-org.sh init.org`
- `./bin/run-tests.sh` — full suite passes.
- `ls config/activities/ 2>&1` reports the directory does not exist.
- `ls config/gptel/sessions/activities-integration.* 2>&1` reports no matches.
- `grep -n "activities-extensions\|perspective\|activities/activities" init.el config/core/window-management.el config/gptel/gptel.el` returns no matches.
- `grep -n "(use-package activities\|(use-package perspective" config/core/window-management.el` returns no matches.
- Isolated Emacs assertions from step 10 all hold.

## Context

- design.md §D8 "Side-by-side development; single hard-cutover commit"
- design.md §"Migration Plan" Phase 2
- proposal.md §What Changes ("BREAKING" entries)
- specs/activities-extensions/spec.md (REMOVED delta)
