---
name: worktree-candidates-git-first
description: Reorder the git-worktrees repo-candidate seam to put git-derived discovery first and demote projectile to optional legacy
change: workspace-portable-bookmarks
status: blocked
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
