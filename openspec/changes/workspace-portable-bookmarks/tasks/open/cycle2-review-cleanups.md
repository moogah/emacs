---
name: cycle2-review-cleanups
description: Refuse remote workspace homes (user decision); move consumer specs off file-equal-p; make the off-workspace "no workspace" answer honest
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/invariant/directory-identity-by-device-inode
  - register/invariant/no-silent-wrong-file-open
relations:
  - discovered-from:workspaces-file-equal-p-timestamp-race
  - discovered-from:docs-readme
  - discovered-from:arch-on-touch-1790453714-51
  - discovered-from:arch-on-touch-1790453714-52
  - discovered-from:arch-on-touch-1790453714-53
  - discovered-from:arch-on-touch-1790453714-54
  - discovered-from:arch-on-touch-1790453714-55
discovered_from: [review workspaces-file-equal-p-timestamp-race findings 1+2, review docs-readme finding 1]
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-27T23:00:00+00:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute; extended with the on-touch audit of the file-identity task
> (`.orchestrator/cycles/cycle-1790453714/findings/on-touch-file-equal-p.md`, 51-55). Runs beside `discovery-scope-hardening` and
> merges AFTER it (only step 3 touches portable-bookmarks.org, prose/docstring only).

## Steps
1. **Remote workspace homes are unsupported (USER DECISION 2026-09-27: "never; refuse them").** Since the core identity predicate
   `workspace--same-file-p` answers nil for remote paths, the duplicate-home guard (`workspace--registered-for-home-p`, tabs) no longer
   catches a second anchor of the same TRAMP home, and registration then silently overwrites the entry. Refuse a remote home
   (`file-remote-p`) at every entry point that registers one — create (scaffold) and anchor (`C-u`), and any other path that puts a home
   into the registry — with a clear `user-error` naming the path, before any filesystem work. Spec each entry point. State it in the
   `tabs.org` prose, the README (*Creating workspaces* / the double-registration paragraph) and report the register wording for
   `directory-identity-by-device-inode` in `## Discoveries`.
   **Also (arch 51, needed regardless):** `workspace--new-anchor-existing` has no registry-NAME collision check (`workspace-re-anchor`
   has one), so anchoring a different local directory with the same basename REPLACES an existing workspace's registry entry (saved
   layouts, membership, recent-layout pointer lost at the next save). Add the same refusal there; spec the local same-basename case.
1b. **(arch 52) Containment.** The purge safeguard (`workspaces.org`, workspaces.el:174) decides containment with `file-in-directory-p`,
   which ends in `file-equal-p` — the same timestamp race survives in core (fails closed: purge refused). Move portable-bookmarks'
   `workspace-bookmark--path-within-p` logic into core as the ONE containment predicate (next to `workspace--same-file-p` in
   data-model), route the purge safeguard and the bookmarks module to it, delete the module copy. Spec the race for containment.
1c. **(arch 53) Prose.** `data-model.org` now says "pure except one read", contradicting `home-org.org:6-8`'s stated reason for being a
   separate module. Keep the placement (load order); align the prose in both files.
1d. **(arch 54) Spec leak.** `test/buffer-reincarnation-spec.el`: three `workspace-new` calls leave a home.org buffer current and
   `br-spec--cleanup` then deletes its directory, so later specs' `make-process`/`process-file` fail with `file-missing`. Fix in that
   spec (restore/bind `default-directory` and kill the buffers it creates).
2. **Specs off `file-equal-p` (review 2 / arch 55).** ~20 assertions in `test/portable-bookmarks-jump-spec.el` and `test/portable-bookmarks-off-workspace-spec.el`
   use `file-equal-p` for path equality (jump-spec :168 and :261 compare directories — the exact race case). Switch them to
   `workspace--same-file-p`. Three are NEGATIVE assertions that would pass for the wrong reason under the race — check each still
   asserts what it means.
3. **The "no workspace" answer at the off-workspace prompt.** The README (*Off-workspace jumps*) and the prompt function's docstring in
   `portable-bookmarks.org` say an empty answer means "no workspace". With a default set (the first holder) stock `completing-read`
   returns the default on empty input, and vertico's RET picks the preselected candidate, so "no workspace" is reachable only when no
   workspace holds the repo, and then only via M-RET. Make docs and behaviour agree: the smallest honest fix is to correct the docstring
   and README; if a real "no workspace" choice is wanted, add it as an explicit candidate. Pick the former unless the code already
   intends the latter, and add a spec that exercises `completing-read`'s real default-on-empty semantics (not a replaced `completing-read`).
4. Tangle each touched `.org`; commit `.org` + `.el`.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green; full suite NO-REGRESSION via compare-baseline.py.
- `grep -n file-equal-p config/workspaces/test/portable-bookmarks-*-spec.el` empty.

## Observations

- **Steps added mid-task by the orchestrator** (from the on-touch audit of the discovery-scope-hardening merge; merged
  76f581df before the commit):
  - **5. (arch-on-touch-1790453714-57) Core accessor for the add-worktree gate.** New core predicate
    `workspace--integration-menu-command-p` (integrations.org § Menu-command query). It walks `workspace--integrations`
    the way the workspace menu does. `workspace-bookmark--add-worktree-runnable-p` now calls it, `fboundp`-guarded, and
    the module's forward `(defvar workspace--integrations)` is deleted. The module no longer reads core's registry shape.
    New cross-module spec in portable-bookmarks-missing-repo-spec.el ("the add-worktree offer follows git-worktrees'
    real registration"). It loads git-worktrees.el's OWN double-gated registration into an isolated registry, with
    `features` let-bound so that nothing is provided globally, and shows the option withheld before and offered after.
    Accessor unit specs are in integration-registry-spec.el.
  - **6. (arch-on-touch-1790453714-58) Spec leaks of the integration registry.** integration-registry-spec,
    integration-dispatch-spec (in `idsp--reset` / `idsp--cleanup`) and git-worktrees-spec's registration-gating
    describe now save and restore `workspace--integrations` instead of clobbering it. Probe: the harness starts with
    `(git-worktree gptel-session)`, and after those three files run it is unchanged.
  - **7. (review discovery-scope-hardening finding 3) No "magit" in portable-bookmarks.el.** The
    `workspace-bookmark--add-worktree-runnable-p` docstring (added by the hardening merge) named magit twice. It is
    reworded to "can run only once the integration that provides it has registered it". Now
    `grep -n 'magit\|projectile' config/workspaces/portable-bookmarks.el` is empty. The .org prose outside the
    docstrings still names magit, which the register validator does not grep.
- **Step 1b departure (purge, missing home).** A literal swap to `(workspace--path-within-p home parent)` would change
  behaviour. The predicate answers nil for a path that does not exist, while `file-in-directory-p` answers t for a
  vanished home under the parent. So the purge of a broken workspace would start asking for `C-u` (two existing
  broken-home purge specs failed on that swap). I added `workspace--purge-in-scope-p` (workspaces.org). It judges a
  missing `:home` by its nearest existing ancestor, through the core predicate, which keeps the old answers:
  - a vanished home under the parent is in scope;
  - a vanished home under an external tree is refused, as before (a new spec pins this; it passes against the old code
    too, by design).
  A remote `:home` is never in scope, which is also as before.
- **Step 1: entry points refused.** `workspace--refuse-remote-home` (tabs.el:133) is called before any filesystem work
  in three places:
  - `workspace--new-anchor-existing` (C-u), before the duplicate-home guard;
  - `workspace--new-default-path` (a remote `workspaces-default-parent-directory`, or an absolute remote NAME), before
    `file-exists-p`;
  - `workspace-re-anchor` (workspaces.el:87), before `file-directory-p`.
  No other code path registers a NEW home. Persistence's deserialiser re-hydrates homes from the state file and was
  deliberately NOT changed; see the scope-question discovery.
- **Step 1: the name guard is anchor-only, as specified.** The default-path branch (`workspace--new-default-path`,
  tabs.el:291) has the same clobber and was left alone. With "proj" anchored at `~/code/proj`, `M-x workspace-new proj`
  scaffolds `<parent>/proj` (which does not exist yet) and `puthash`es over the existing "proj" entry, losing its
  layouts. That is outside this task's steps and recorded as a discovery.
- **Step 1: how the remote specs avoid connecting.** They use a stand-in file-name handler, `/wsna-remote:`.
  `file-remote-p` answers the prefix, pure name operations pass through, and any other operation is recorded and
  signals. Reverted, the specs therefore fail loudly ("Remote filesystem touched") instead of trying a connection. The
  helper itself is also specced with a real TRAMP name (`/ssh:…invalid:`), where `file-remote-p` makes no connection.
  No `mock`-method probe was run.
- **Step 1c.** data-model.org's header now says "never writes, holds no state beyond the registry shapes, reads the
  filesystem only in the identity and containment predicates (stat-level, never file contents)". home-org.org gives
  as its reason for being separate that it reads file contents and must never cache them. The two files agree.
- **Step 1d.** I fixed the source (`br-spec--cleanup` kills every buffer whose `default-directory` lies in the per-test
  directory, via the core containment predicate, then leaves `*scratch*` current). I did not bind
  `workspace-home-builder` to nil, so those specs still exercise the real builder. A new trailing describe in the same
  file pins the invariant: the current directory exists, `process-file` works, and no buffer is left in a deleted
  `ws-br-spec-` directory. It fails with the old cleanup.
- **Step 2.** All 20 `file-equal-p` oracles in jump-spec (14) and off-workspace-spec (6) now use
  `workspace--same-file-p`. The three negatives (jump-spec :206, :266, :380) are each preceded by an existence
  precondition (`file-exists-p` / `file-directory-p` of A's copy), so nil can only mean "a different file". The
  discovery spec's two `workspace-bookmark--path-within-p` calls now name the core symbol.
- **Step 3.** I took the smaller fix: the docstring, the § "One prompt" and § fallback prose, and the README
  *Off-workspace jumps* now say what the code does:
  - with a holder offered, an empty answer restores the default;
  - "no workspace" is an empty answer only when no workspace holds the repository (M-RET under vertico).
  No code intends an explicit "no workspace" candidate. The new spec drives the REAL `completing-read` /
  `completing-read-default`, answering only `read-from-minibuffer` with "". It shows the DEF substitution (holder
  returned) and the nil answer when there is no default. Step 3 changes no code, so this spec has no production change
  to fail against. It is a characterization pin.
- **Revert check (how).** In a `mktemp -d` copy of config/workspaces, data-model/tabs/workspaces/portable-bookmarks.el
  were restored from 76f581df and buffer-reincarnation-spec's old `br-spec--cleanup` was put back. `jf/emacs-dir` was
  pointed at the copy so that workspaces.el's loader cascade loaded the old modules, the three new core symbols were
  `fmakunbound`, and the six changed spec files were run. 21 failures, covering every new spec except the two noted
  above (the vanished-external purge pin and the step-3 characterization):
  - the 9 containment specs;
  - the 2 discovery specs, now naming the core symbol;
  - the name-guard spec;
  - the 5 remote specs;
  - the 2 purge race/case specs;
  - the 2 cleanup-guard specs.
  The accessor unit specs added for step 5 fail by construction without the accessor (void-function). The step-5
  cross-module spec passes against the old raw-shape reader too: step 5 is a surface refactor, not a behaviour change.
  The spec pins the cross-module contract.
- **Not done (outside the steps).** arch-on-touch-1790453714-59 suggests "at the next touch of integrations.org" to
  reword `workspace--integration-payload`'s docstring, which names portable-bookmarks in prose. I touched
  integrations.org only for step 5 and left that docstring alone.
- **A trap worth knowing.** An unescaped `"` inside an edited docstring tangles cleanly and passes `check-parens`
  (balanced), then fails at runtime as `void-variable`. Specs caught it here (tabs.el). tangle-org.sh's validation
  cannot.

## Discoveries

- discovery_id: disc-cycle2-review-cleanups-1
  class: interface-drift
  description: |
    Remote workspace homes are refused (user decision 2026-09-27) at every entry point that registers a NEW home,
    by `workspace--refuse-remote-home` (tabs.el:133): C-u anchor (tabs.el:218, before the duplicate-home guard),
    the default path (tabs.el:282), and `workspace-re-anchor` (workspaces.el:87). The identity predicate's nil for a
    remote name is therefore never read as "different" by the uniqueness guard. The purge safeguard's
    `file-in-directory-p` known violation is cleared: core's ONE containment predicate is `workspace--path-within-p`
    (data-model.el:288), built on `workspace--same-file-p`; the module copy is deleted.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Amend: new corollary 4 — "nil means NOT PROVABLY the same; remote names are never compared and no connection is
    opened. Remote workspace homes are unsupported and refused at every registration entry point
    (`workspace--refuse-remote-home`, tabs.el), so no uniqueness guard reads a remote nil as 'different'." Corollary
    1 gains `file-in-directory-p` (rejected for containment; it ends in `file-equal-p` and compares spellings
    case-sensitively). Validator 1: `grep -n 'file-equal-p\|file-in-directory-p' config/workspaces/*.el` -> docstrings
    only (data-model.el:268, :297-298); test-side `grep -n file-equal-p config/workspaces/test/portable-bookmarks-*-spec.el`
    -> comment only (discovery-spec :630). Validator 2 adds `defun .*path-within-p` -> exactly one body
    (data-model.el:288). Validator 3's permitted list: the loop guards in `workspace--path-within-p` (data-model.el)
    and `workspace--purge-in-scope-p` (workspaces.el:124). known_violations: workspaces.el:174 CLEARED (now
    workspaces.el:195 via `workspace--purge-in-scope-p`). test_corpus: file-identity-spec § workspace--path-within-p,
    workspace-delete-purge-spec § "decides containment by file identity", workspace-new-anchor-spec § remote homes.
- discovery_id: disc-cycle2-review-cleanups-2
  class: interface-drift
  description: |
    portable-bookmarks now names two more core symbols: `workspace--path-within-p` (data-model; declare-function at
    portable-bookmarks.el:98, called unguarded like `workspace--same-file-p`, because data-model loads first and is
    not droppable) and `workspace--integration-menu-command-p` (integrations; declare-function at :603, called
    `fboundp`-guarded like its other soft core calls). It no longer names `workspace--integrations` at all. Core still
    names no `workspace-bookmark-*` symbol.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    directionality.permitted: add `workspace--path-within-p` (unguarded, the ONE containment predicate) and
    `workspace--integration-menu-command-p` (fboundp-guarded, read-only); remove any mention of reading
    `workspace--integrations`.
- discovery_id: disc-cycle2-review-cleanups-3
  class: interface-drift
  description: |
    Core's integration registry publishes a read accessor, `workspace--integration-menu-command-p` (integrations.el:41):
    non-nil when COMMAND is the `:menu` command of a registered integration, walking the registry the way
    `workspace--menu-integration-children` does. Consumers ask it instead of reading the (ID . PLIST) / :menu shape.
  affected_register_entry: register/boundary/workspace-integration-registry
  recommendation: |
    Record the accessor as the published read surface; readers: workspaces-transient (shape, core-internal) and
    portable-bookmarks (via the accessor only). Pinned cross-module by portable-bookmarks-missing-repo-spec § "follows
    git-worktrees' real registration".
- discovery_id: disc-cycle2-review-cleanups-4
  class: invariant-gap
  description: |
    The anchor branch now refuses a registry-NAME collision (tabs.el:223), closing the local same-basename clobber.
    The default-path branch has the same clobber and no check: with "proj" registered at ~/code/proj,
    `workspace-new proj` scaffolds <parent>/proj and replaces the "proj" entry (tabs.el:291 puthash), losing its
    layouts, membership and recent-layout pointer at the next save. Not fixed: outside this task's steps.
  affected_register_entry: register/invariant/registry-name-equals-basename
  recommendation: |
    Follow-up: give `workspace--new-default-path` the same `(gethash name workspace--registry)` refusal before
    scaffolding, with a spec; state in the register that every registration path refuses a taken name.
- discovery_id: disc-cycle2-review-cleanups-5
  class: scope-question
  description: |
    Persistence's deserialiser (persistence.el:128) re-hydrates every persisted home, calling `file-directory-p` on
    it, so a remote home saved before this change would open a TRAMP connection at startup and be registered. It is
    not refused here: refusing at load would drop the entry at the next save (data loss), and it is not an entry point
    that registers a new home.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    User decision if wanted: at load, keep a remote entry but tag it broken without touching the filesystem
    (`file-remote-p` first), so it is never connected to and can be purged; or leave as is (no remote home can be
    registered from now on).
- discovery_id: disc-cycle2-review-cleanups-6
  class: invariant-gap
  description: |
    Two shared globals leaked across spec files and are now restored at their source: the current buffer /
    `default-directory` (buffer-reincarnation-spec's cleanup deleted the directory of the home.org buffer it left
    current) and `workspace--integrations` (three specs `setq` it without restoring). buffer-reincarnation-spec ends
    with a guard describe for the first.
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    known_violations for the repo-wide generalisation (.tasks/test-suite-shared-global-state-isolation): record both
    classes as closed at their current carriers; the runner-level per-file-process fix remains the general cure.
