---
name: worktree-candidates-git-first
description: Reorder the git-worktrees repo-candidate seam to put git-derived discovery first and demote projectile to optional legacy
change: workspace-portable-bookmarks
status: ready
task_class: contract
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
  - register/boundary/workspace-git-worktrees-consumer
relations:
  - blocked-by:checkout-discovery
---

## Files to modify
- `config/workspaces/git-worktrees.org` (modify)
- `config/workspaces/git-worktrees.el` (tangled)
- `config/workspaces/test/git-worktrees-spec.el` (modify — existing suite)

## Implementation steps

1. `jf/workspace--worktree-repo-candidates` currently appends, in order:
   `projectile-known-projects` (when projectile is loaded), `(magit-list-repos)`
   (when magit is loaded), `project-known-project-roots` (when project.el is
   available), then de-dupes on expanded absolute paths and keeps only real git
   repositories. Each source is guarded by `boundp`/`fboundp`/`featurep`.

2. Prepend a git-derived source ahead of all three: the repositories of the
   checkouts already in view — the current workspace's children, other registered
   workspaces' children, and `jf/emacs-dir` — expanded to their sibling checkouts
   via git. Reuse the `portable-bookmarks` discovery helpers when that module is
   loaded (`fboundp` guard), and contribute nothing when it is not; this module
   must not gain a hard dependency on it.

   Note: `magit-repository-directories` is unset in this config, so
   `(magit-list-repos)` contributes nothing today — in practice this seam has been
   projectile-only. Git-derived discovery is what makes it work with no
   registration at all.

3. Keep every existing guarantee intact: merge, de-dupe on expanded absolute
   paths, filter to real git repositories, no hard dependency on any source, and
   the `read-directory-name` floor in `jf/workspace--worktree-read-repo`
   unchanged.

4. Prefer canonical checkouts as candidates: a worktree source for a *new*
   worktree should be a repository's main worktree, not another workspace's
   worktree of it. Filter git-derived candidates through the classification helper
   so workspace-owned checkouts are not offered as sources.

5. Extend `config/workspaces/test/git-worktrees-spec.el` (do not create a new
   file — this is an existing suite):
   - candidates include git-derived repositories when no project subsystem is
     loaded (stub all three sources to unbound/empty);
   - ordering places git-derived candidates before projectile's;
   - a repository known to both git-derived discovery and projectile appears once;
   - non-git directories are still excluded;
   - with projectile unloaded, discovery is not empty;
   - a workspace-owned checkout is not offered as a source.

6. Update the module's Overview/"Repo-source seam" prose in
   `git-worktrees.org` to state the new ordering and why projectile is no longer
   the baseline.

7. Tangle and validate.

## Design rationale

Projectile only reports repositories that have been registered with it, so
candidate sets differ between machines for reasons unrelated to what is actually
cloned — a per-machine chore and a source of drift. Git-derived discovery answers
the same question from the filesystem, with no registration and no configuration,
which turns a hope into a contract.

This is the delta the change's modified spec describes: the
`workspace-git-worktrees` "Soft repo-source selection" requirement previously
named `projectile-known-projects` as the *minimum* source. It becomes one optional
legacy source among others, with git-derived discovery primary and the
`read-directory-name` floor unchanged. No worktree creation, teardown, or
derived-not-stored behaviour changes.

Filtering out workspace-owned checkouts as *sources* is the same safety rule the
resolver applies: creating a worktree from another workspace's worktree would
chain checkouts in a way that makes the ownership of a branch ambiguous.

The `fboundp` guard on the portable-bookmarks helpers keeps the dependency
direction sane: git-worktrees does not require the bookmarks module, it just uses
its discovery when present.

## Verification
- `./bin/tangle-org.sh config/workspaces/git-worktrees.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — the extended
  `git-worktrees-spec.el` passes and no other spec regresses.
- `grep -n "projectile-known-projects" config/workspaces/git-worktrees.el` shows
  it still guarded and still present, but no longer first in the append order.
- `grep -n "read-directory-name" config/workspaces/git-worktrees.el` shows the
  floor prompt unchanged.
- Manual: with projectile's known-projects list emptied, the add-worktree command
  still offers the repositories of checkouts in view.

## Context
- specs/workspace-git-worktrees/spec.md § MODIFIED 'Soft repo-source selection'
- proposal.md § 'What Changes' (BREAKING spec-level note)
- design.md § D5 'Warm-path (git-derived) discovery only'

## Amendments (2026-09-24, after wave 3)

A. **Reuse, don't re-derive, the in-view set.** Wave 3 shipped
   `workspace-bookmark--checkouts-in-view` (current workspace's checkouts, then
   other workspaces', then `jf/emacs-dir`) and `workspace-bookmark--classify`.
   Step 2 MUST call those (behind an `fboundp` guard) rather than walking
   workspace homes itself: a second enumeration here is duplication, which the
   project overlay rates blocking. Step 4's filter MUST use `--classify`; offer
   only `canonical`.
B. A parallel task (`discovery-refinements`) is changing what "in view"
   includes: anchored homes, and directory-only symlink ownership. Your specs
   should assert behaviour THROUGH those helpers, not pin their internals, so
   they stay green when that task merges.

## Observations

- **Shape of the git-derived source.** A new function,
  `jf/workspace--worktree-git-derived-candidates`, composes
  `workspace-bookmark--checkouts-in-view` → `--checkouts-of` (the "sibling
  checkouts via git" expansion) → `--classify`, keeping only `canonical`. It
  does no enumeration of its own (Amendment A). It sits first in
  `jf/workspace--worktree-repo-candidates`, and the existing projectile →
  magit → project.el order after it is unchanged.
- **Soft dependency, checked when called.** All three helpers are checked with
  `fboundp` when the function is called, not when the module loads, because
  `workspaces.org` loads git-worktrees.el *before* portable-bookmarks.el. There
  is no `require` and no `declare-function`, and the calls use
  `(funcall 'sym …)` so the byte-compiler raises no unknown-function warning.
  I did not add a `condition-case`: the helpers are documented as never
  signalling, and wrapping them would hide bugs in them.
- **Small change to de-duplication: paths are now normalised as directories.**
  Before de-duping, candidates now go through `file-name-as-directory` as well
  as `expand-file-name`. Git-derived candidates always end in a slash, so a
  legacy source that spells the same root without one would otherwise show up
  twice. This is still de-duplication on expanded absolute paths, only
  stricter; see disc-2. De-duplication still does NOT resolve symlinks (for
  example `/var` vs `/private/var` on macOS). A projectile entry reached
  through a symlinked prefix could still appear twice next to git's truename
  spelling. Out of scope, and noted here only.
- **Where projectile sits.** The task said to *prepend* the git-derived
  source, so projectile stays ahead of magit and project.el. The spec delta
  lists the legacy sources as magit, project.el, projectile. No scenario
  depends on the order among the legacy sources, so I left it alone.
- **Existing unit specs made hermetic.** The existing "repo-candidate seam"
  unit block now spies `jf/workspace--worktree-git-derived-candidates` to nil
  in its `before-each`. Without that spy, the "returns nil when no source is
  present" spec (which stubs `fboundp` to t) would pick up whatever checkouts
  the running Emacs has in view, such as `jf/emacs-dir`'s main worktree.
- **New specs use real git and go through the helpers.** They use
  `portable-bookmarks-fixtures.el` repositories, a fresh `workspace--registry`
  and `jf/emacs-dir` bound to nil. They assert only what ends up in the
  candidate list. Ownership is modelled only by checkouts that physically sit
  inside a home: a linked worktree, and a full `git clone` in a second home.
  There are no symlinks, which matches the coordinator's later correction that
  discovery ignores symlinks entirely. Anchored homes are not checkouts in the
  fixtures, so the discovery-refinements merge should not affect these specs.
- **Each new spec fails with its change reverted.** For each case below I
  changed the .org, re-tangled, ran `./bin/run-tests.sh -d config/workspaces`,
  and then restored the file (byte-identical .el confirmed with `cmp`):
  - git-derived source removed (original code): 6 behavioural specs fail
    (no-source, projectile-unloaded, ordering, dedupe, non-git, workspace-owned).
  - `file-name-as-directory` normalisation removed: the dedupe spec fails.
  - `--classify` filter replaced by `t`: all 6 behavioural specs fail,
    including workspace-owned (the linked worktree and the clone leak in).
  - `fboundp` guard removed: the "portable-bookmarks not loaded" spec fails
    (void-function).
  - Git-derived source appended last instead of first: the ordering spec fails.
- **Verification.** `./bin/run-tests.sh -d config/workspaces`: 521 ran, 521
  passed (base 514 plus 7 new). Full suite: 3257 ran, 3243 passed.
  `compare-baseline.py` → NO-REGRESSION (no new failing names).
  `projectile-known-projects` is still guarded but no longer first, and the
  `read-directory-name` floor is unchanged.
- **Latent issue in the existing spec file, not fixed.** Written as
  `(expect (lambda () …) :not :to-throw)`, the "no signal" check never ran the
  body (a `setq` inside the lambda did not happen). I used an explicit
  `condition-case` instead.

## Discoveries

- discovery_id: disc-worktree-candidates-git-first-1
  class: deviation
  description: |
    The sharing question for the git-derived discovery helpers was left open
    by dependency_direction, and is now RESOLVED as a SOFT call, checked when
    called. git-worktrees calls workspace-bookmark--checkouts-in-view,
    --checkouts-of and --classify only when all three are fboundp at call
    time. It has no `require` and no `declare-function`, and it does not
    duplicate the enumerator. When portable-bookmarks is not loaded, the
    git-derived source contributes nothing and the other sources behave
    exactly as before. No third-place helper module was needed, and nothing
    was deliberately duplicated. A spec with the helpers unbound shows that
    the candidates come from projectile alone and nothing signals.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Record in dependency_direction: "RESOLVED (cycle-1789935943,
    worktree-candidates-git-first): soft, fboundp-guarded call from
    jf/workspace--worktree-git-derived-candidates into
    workspace-bookmark--checkouts-in-view / --checkouts-of / --classify,
    checked when called (git-worktrees loads before portable-bookmarks). No
    require, no declare-function, no duplicate enumerator. The absent module
    contributes nothing." Add jf/workspace--worktree-git-derived-candidates to
    second_consumer.

- discovery_id: disc-worktree-candidates-git-first-2
  class: interface-drift
  description: |
    Pin 3 (SOFT REPO-SOURCE SELECTION) of the CONFIRMED consumer entry names
    only projectile / magit / project.el as candidate lists. It now has a
    PRIMARY git-derived source ahead of them, as the git-derived entry's
    second_consumer anticipated. Every other guarantee in pin 3 holds:
    the sources are guarded, none is required, candidates are filtered to real
    git repos, and the read-directory-name floor is unchanged. One tightening:
    de-duplication now normalises each expanded path to directory form
    (trailing slash), so the same root spelled with and without a slash counts
    as one candidate. Git-derived candidates are also restricted to
    `canonical` checkouts, so no workspace-owned checkout is offered as a
    worktree source.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Amend pin 3 to read: "consumes git-derived discovery FIRST (canonical
    checkouts of the repos in view, via the portable-bookmarks helpers under
    an fboundp guard), then candidate LISTS from projectile-known-projects,
    (magit-list-repos) and project-known-project-roots, each guarded;
    de-duped on expanded absolute directory names; filtered to real git
    repos; read-directory-name floor unchanged." Add
    jf/workspace--worktree-git-derived-candidates to producer_side.functions.
