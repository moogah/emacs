## Why

A workspace is, in practice, a *containing directory for git worktrees*: the
common workflow is to select an already-cloned local repo and create a fresh
worktree off its main branch inside the workspace, often for several repos
working the same feature. Today nothing in the workspaces system knows about
git, so the user creates worktrees by hand (`git worktree add ...`), names
branches by hand, and — when purging a workspace — leaves each source repo
holding stale worktree registrations and orphan branches. We want this to be a
first-class, mostly-automatic part of the workspace lifecycle while keeping git
*non-mandatory*: a workspace with no worktrees must remain a valid workspace.

## What Changes

- **New first-party, droppable module** `config/workspaces/git-worktrees.el`
  that creates and manages git worktrees *inside* a workspace's `:home`
  directory. Shipped enabled-by-default in `jf/enabled-modules`; removing it
  leaves workspaces fully functional (worktrees are the 99% case, not a
  requirement).
- **Soft repo-source selection.** "Select a repo already cloned locally" reads
  from whatever is loaded — `projectile-known-projects`, `magit-list-repos`,
  `project-known-project-roots` — merged, filtered to real git repos, with a
  `read-directory-name` floor. No hard dependency on projectile (consumed only
  as one candidate list, never its workflow); magit is used opportunistically
  where present.
- **Worktree-off-main creation.** Pick a repo, detect its main branch
  (`magit-main-branch` if present, else `git symbolic-ref refs/remotes/origin/HEAD`,
  falling back to `main`/`master`), create a worktree on a new branch (default:
  the workspace name) inside the workspace home, with the child directory named
  for the source repo's basename. Registered as the integration's interactive
  `:menu` command ("add worktree").
- **First worktree offered at birth.** `workspace-new` optionally invokes the
  registered add-worktree `:menu` command(s) after the container directory
  exists — reusing the existing interactive surface rather than adding a new
  one. Workspaces core names no integration: it offers to run registered
  `:menu` items generically.
- **Clean teardown.** A new `:on-purge` registry surface fires *before*
  `workspace-purge` deletes the home directory, so the git integration can
  remove each child worktree from its source repo and offer guarded deletion of
  its branch (only when git considers the branch safely removable; unmerged
  branches are kept), leaving source repos clean. Fires on `workspace-purge`
  only, never on `workspace-delete` (which keeps `:home` and its worktrees on
  disk).
- **No new persistent workspace slots.** Repo, branch, and the set of
  worktrees are *derived* on demand from disk + git (`git worktree list`,
  `git symbolic-ref HEAD`), consistent with how the codebase already treats
  `:home`/broken-state as disk-derived rather than trusted from the save file.

## Capabilities

### New Capabilities
- `workspace-git-worktrees`: Creating and removing git worktrees inside a
  workspace home — soft repo-source selection, main-branch detection,
  worktree-off-main creation on a workspace-named branch, and `git worktree
  remove` cleanup. Registers against the workspace integration registry as a
  droppable consumer.

### Modified Capabilities
- `workspace-integrations`: Add an `:on-purge` teardown surface to the
  registry contract and a purge-time dispatch that runs each registered
  `:on-purge` handler before the home directory is deleted. The registration
  rule extends to allow `:on-purge` as a declared surface.
- `workspaces`: `workspace-new` gains an optional birth-time step that offers
  to run registered add-worktree `:menu` command(s) once the container exists;
  `workspace-purge` dispatches `:on-purge` handlers before `delete-directory`.

## Impact

- **New code**: `config/workspaces/git-worktrees.org`/`.el` and its tests under
  `config/workspaces/test/`.
- **Modified code**: `config/workspaces/integrations.org` (new `:on-purge`
  surface + purge dispatch), `config/workspaces/workspaces.org`
  (`workspace-new` birth offer, `workspace-purge` teardown dispatch),
  `init.org` (`jf/enabled-modules` entry).
- **Specs**: new `workspace-git-worktrees`; delta specs for
  `workspace-integrations` and `workspaces`.
- **Dependencies**: the git-worktrees module depends on `magit` (worktree
  operations + main-branch detection) and `git`; it registers only when magit
  is present, so its absence simply means no git integration rather than an
  error. Repo-source selection remains soft and presence-detected over
  projectile / project.el / magit. Workspaces core gains no new dependency.
- **Directionality preserved**: workspaces core continues to name no consumer;
  the git module attaches via `workspace-register-integration`, the same
  publish/attach contract gptel uses.
