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

## Observations

- **Every registry `puthash`, and which are guarded (step 1).**
  - `tabs.org` § `workspace--new-anchor-existing`: GUARDED. The remote refusal, then the duplicate-home guard
    (`workspace--registered-for-home-p`), then the shared name guard `workspace--refuse-taken-name`.
  - `tabs.org` § `workspace--new-default-path`: GUARDED (new). The remote refusal, then `workspace--refuse-taken-name`, then the
    existing directory check. No duplicate-home guard is needed: the home is `<default-parent>/NAME`, which must not exist yet, so
    it cannot be anyone's home.
  - `workspaces.org` § `workspace-re-anchor`: GUARDED. If the name is unchanged, it rewrites NAME's own entry, which is not a new
    key. If the name changes, the existing `(gethash new-name)` collision refusal applies, and a remhash/puthash pair moves the
    entry. It now also refuses another workspace's home (step 4b). It keeps its own "Cannot rename to X" message rather than the
    shared helper, because that branch is a RENAME of an existing entry, not a registration. The shared helper serves both
    registering branches.
  - `persistence.org` § `workspace--deserialize-state`: this rebuilds the registry from the file after `clrhash`, so it can
    replace nothing that exists at runtime. The remaining gap is arch 66: a duplicate `:name` in a hand-edited file lets the later
    entry win silently. It is not in this task's steps and is left as is.
  - `layouts.org` (5 sites) and `buffer-membership.org` (3 sites): read-modify-write of an entry fetched by `gethash` under the
    SAME key (the current workspace, or a name already checked present). They never introduce a key.
  - Integrations register no workspaces. `git-worktrees.org:219`'s `puthash` goes into a local dedupe table, and gptel's
    `:on-create` handler writes a session file. portable-bookmarks writes only to its own memo tables.
- **Existing spec retargeted.** `broken-home-runtime-spec.el` "refuses to clobber an existing workspace" re-anchored onto bar's
  EXACT home to exercise the rename-collision message. The new duplicate-home guard now refuses that case first, with "Already a
  registered workspace". The spec now re-anchors onto a DIFFERENT directory named `bar` (`<tmp>/elsewhere/bar/`), so it still pins
  the rename-collision branch, and the new describe pins the duplicate-home branch.
- **Re-anchor excludes NAME's own entry** (`workspace--registered-for-home-p` gained `&optional except-name`). Without the
  exclusion, a broken workspace whose home reappeared (a remounted disk) could not be re-anchored onto that home. A spec pins
  this (mutant M5b fails it).
- **Remote-home specs (step 2).** Every remote spec now asserts the exact remote-refusal message (helper
  `wna-spec--remote-refusal`). The old "second anchor of a registered remote home" became "a remote ALIAS of a registered remote
  home". The registered home is `/…/srv/proj/` and the anchor is `/…/mnt/proj-alias/`, a different basename, so the name guard
  cannot fire and only the remote refusal can. The tabs.org rationale is corrected to match: the name guard already refuses the
  same spelling, and the refusal is needed for an alias, which would register a SECOND workspace on the same directory.
- **Purge strictness (step 3).** A missing `:home` is still judged by its nearest existing ancestor. The strict check is skipped
  on that arm (`previous` non-nil): the ancestor is then a proper ancestor of HOME, so a broken home directly in the parent stays
  in scope. Mutant M3b, strict without the arm, fails the two existing broken-home purge specs. The new strict specs never delete
  anything, even when reverted: `delete-directory` is replaced by a recorder in a scoped `cl-letf`. It is not a spy, because a
  spy would still be active in `after-each` and block the temp-dir cleanup.
- **Remote home at load (step 4).** The arm asks only `file-remote-p` (no CONNECTED argument, so no connection) and uses
  `display-warning` (type `workspaces`, `:warning`), once per remote entry. It never signals. The spec's stand-in handler signals
  on any operation other than `file-remote-p` and pure file-NAME operations, so it fails loudly instead of connecting.
- **Residual, not changed (remote × purge):** `C-u workspace-purge` on a remote-broken entry passes the safeguard and then calls
  `(file-directory-p home)`. Under real TRAMP that connects, and it would go on to a recursive remote `delete-directory`. The load
  warning recommends `workspace-delete`, not purge. This needs the user's explicit C-u, so it is not a regression, but see
  disc-3. Relatedly, `workspace-restore`'s broken message says ":home … no longer exists", which is inaccurate for a remote
  entry. It is cosmetic.
- **Step 4c.** The spec now adds only the ABSENT features among `workspaces`/`magit`, and in `unwind-protect` it removes exactly
  those. A blanket restore of the saved `features` list would also drop anything the load itself `provide`d. It also asserts the
  precondition `(and (featurep 'workspaces) (featurep 'magit))`, so the old lexical-`let` mutant fails at that line.
  - Isolated command: `/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch -L runtime/straight/build/buttercup -l buttercup
    -l config/workspaces/test/portable-bookmarks-missing-repo-spec.el -f buttercup-run </dev/null`. Result: 28 specs, 0 failed.
  - With the `features` change reverted to a `let`: 1 failed.
  - With `workspace--integration-menu-command-p` reverted to always-nil: this spec fails at "runnable-p … t".
  - Most other workspace spec files cannot run under bare `-Q`: `tabs.el` requires `workspace-integrations` /
    `workspace-home-org` from the load path, and `workspaces.el` needs `jf/load-module`. That is expected, and owned by
    `.tasks/test-suite-shared-global-state-isolation`.
- **Step 4d.** persistence-spec's positive containment assertion now uses `workspace--path-within-p`. That predicate needs the
  directory to exist, so the spec creates this process's own `workspaces-batch-state-<pid>` sandbox if it is absent, and removes it
  (non-recursively) only when it created it.
- **Fail-on-revert runs.** Each production change was reverted in the tangled `.el` alone, one at a time, and the owning spec file
  was run in the harness image, one process per file. The `.el` files were restored byte-identical afterwards.
  - M1, the default-path name guard dropped: workspace-new-default-spec, 2 failed.
  - M2, the remote refusal a no-op: workspace-new-anchor-spec, 5 failed. That is all five remote specs, including the alias.
  - M3, strictness dropped: workspace-delete-purge-spec, 3 failed.
  - M3b, strict without the missing-home arm: 3 failed.
  - M4, the load remote arm dropped: broken-home-load-spec, 1 failed ("Remote filesystem touched: file-directory-p").
  - M5, the re-anchor duplicate-home guard dropped: broken-home-runtime-spec, 2 failed (symlink and case variant).
  - M5b, the guard without EXCEPT-NAME: 1 failed (own home).

## Discoveries

- discovery_id: disc-registration-and-purge-guards-1
  class: interface-drift
  description: |
    `workspace--registered-for-home-p` (tabs.org, core) gained `&optional except-name`: the named workspace's own entry is
    skipped. `workspace-re-anchor` needs this so a workspace can re-anchor onto its own reappeared home. The anchor branch of
    `workspace-new` still calls it with one argument. A new core helper, `workspace--refuse-taken-name (name)` (tabs.org, next
    to `workspace--refuse-remote-home`), is the ONE name-collision refusal, and both registering branches of `workspace-new`
    call it.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Record in the entry (and in register/invariant/registry-name-equals-basename): "every command that registers a workspace
    refuses a taken name via `workspace--refuse-taken-name` (broken entries not exempt); `workspace-re-anchor` refuses another
    workspace's home via `workspace--registered-for-home-p NEW-HOME NAME`." Validator:
    `grep -n 'refuse-taken-name' config/workspaces/tabs.el` shows one defun and two calls.

- discovery_id: disc-registration-and-purge-guards-2
  class: invariant-gap
  description: |
    Draft corollary 5 is realised: "a destructive scope check needs STRICT containment". `workspace--purge-in-scope-p` =
    `workspace--path-within-p` on HOME, or on HOME's nearest existing ancestor when HOME is missing, AND, when HOME exists, NOT
    `workspace--same-file-p` HOME parent. `workspace--path-within-p` keeps its reflexive contract for the ownership callers.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Adopt corollary 5 as written in the on-touch draft, citing workspaces.el `workspace--purge-in-scope-p`. Add
    workspace-delete-purge-spec § "workspace-purge scope is STRICT" to test_corpus.

- discovery_id: disc-registration-and-purge-guards-3
  class: spec-signal
  description: |
    User decision implemented: a persisted remote `:home` is loaded tagged broken, with no filesystem call on it (only
    `file-remote-p`, which does not connect) and one `display-warning`. It never signals and is never dropped. One residual
    path still reaches a remote home: `C-u workspace-purge` on such an entry calls `file-directory-p` and then a recursive
    `delete-directory` on the remote directory. It needs an explicit C-u and a yes, so it is not silent, but it is the one
    command left that contacts, and could destroy, an unsupported remote home.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Amend draft corollary 4 to read "… refused by every command that registers a home; a remote home in the state file is
    loaded broken, never contacted (persistence.el `workspace--deserialize-state`)". Route to the user: should
    `workspace-purge` skip the filesystem step, and the teardown dispatch, for a remote `:home` even under C-u? That is a
    one-line guard (`(and (not (file-remote-p home)) (file-directory-p home))`), and it would make "never contacted" hold for
    every command.
