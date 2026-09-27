---
name: registration-and-purge-guards
description: Name-collision refusal on every registration path; remote-home refusal at registry load; purge scope strictly below the default parent; remote spec proves the refusal it names
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/invariant/directory-identity-by-device-inode
relations:
  - discovered-from:cycle2-review-cleanups
  - discovered-from:arch-on-touch-1790453714-60
  - discovered-from:arch-on-touch-1790453714-61
  - discovered-from:arch-on-touch-1790453714-62
  - discovered-from:arch-on-touch-1790453714-63
  - discovered-from:arch-on-touch-1790453714-65
  - discovered-from:arch-on-touch-1790453714-66
discovered_from: [review cycle2-review-cleanups findings 1-4, arch-on-touch-1790453714-60/61/62/63/65/66, implementor disc-cycle2-review-cleanups-4/5]
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-27T20:50:00-04:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute from the author-blind review of `cycle2-review-cleanups`
> (merge acc335bc, `.orchestrator/cycles/cycle-1790453714/reviews/cycle2-review-cleanups.md`), its on-touch audit
> (`.orchestrator/cycles/cycle-1790453714/findings/on-touch-cleanups.md`, 60-66) and the implementor's own discoveries.

## Steps
1. **Name collision on every registration path (review 1 / arch 60 BLOCKING / disc-4).** A broken entry of the same name is also
   replaced silently; share ONE helper between the scaffold and anchor branches. `workspace-new NAME` without a prefix (default-path scaffold,
   tabs.el ~:291) still silently REPLACES an existing registry entry with the same name (e.g. one anchored elsewhere), losing its saved
   layouts and membership at the next save. Refuse it with the same message the C-u anchor path now uses. Enumerate every other path
   that `puthash`es into the registry (re-anchor, integrations, persistence load) and say in `## Observations` which are guarded and why
   the rest are safe. Spec the default-path collision.
2. **Remote spec proves remote (review 2).** The spec "second anchor of a registered remote home" passes with the remote refusal
   removed, because the name guard refuses first. Make the remote specs assert the REMOTE message (and use a name that does not collide),
   so each fails with its own guard reverted. Correct the `tabs.org` rationale that overstates what the remote refusal alone prevents.
3. **Purge scope (review 3 / arch 62 — destructive).** The purge safeguard treats the default parent directory ITSELF as in scope: a workspace
   anchored AT the parent can be purged without C-u, recursively deleting every workspace below it. `workspace-purge`'s docstring says
   "descendant". Make scope strict (a proper descendant only; the parent itself requires C-u like any external home). Spec it.
4. **Remote homes at registry load (review 4 / arch 61 / disc-5; USER DECISION: remote homes unsupported; the user never uses them).**
   Loading the state file must not stat a remote `:home` (today `file-directory-p` there opens a TRAMP connection at startup). Load such
   an entry TAGGED BROKEN without any filesystem call on it (so its data survives the next save and it is never restored or offered),
   with one `display-warning` naming it. Do not signal (that would abort the whole restore) and do not drop it. Spec with a stand-in
   remote handler that fails the spec if called; no real connections.
4b. **Re-anchor duplicate-home guard (arch 65).** `workspace-re-anchor` has no duplicate-home guard: re-anchoring onto (a case/spelling
   variant of) ANOTHER workspace's home makes two workspaces share one directory. Refuse it through the core identity predicate. Spec.
4c. **Cross-module spec really isolated (arch 63).** The arch-57 cross-module spec binds `features` with `let`; in Emacs 30 `features` is
   not special, so `featurep` never sees the binding and the spec only passes because the harness preloaded magit/workspaces (fails
   under `-Q` at portable-bookmarks-missing-repo-spec.el:391). Change `features` for real and restore it in `unwind-protect` (or
   restructure so the spec does not depend on load state). Show it passes under an isolated run of that file alone.
4d. **Leftovers (arch 66).** Replace the test-side `file-in-directory-p` at persistence-spec.el:319 with the core containment predicate.
5. Tangle each touched `.org`; commit `.org` + `.el`. README / prose where behaviour is described.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green; full suite NO-REGRESSION via compare-baseline.py.
- Each new spec fails with its production change reverted.
