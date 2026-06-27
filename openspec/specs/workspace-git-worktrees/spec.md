# workspace-git-worktrees Specification

## Purpose

The `workspace-git-worktrees` capability makes git worktrees a first-class,
mostly-automatic part of the workspace lifecycle while keeping git
*non-mandatory*. A workspace is, in practice, a containing directory for git
worktrees: the user selects an already-cloned local repo and creates a fresh
worktree off its main branch inside the workspace `:home`, often for several
repos sharing one feature branch. The capability ships as a droppable,
enabled-by-default first-party module that attaches to the workspaces system
solely through `workspace-register-integration` — it declares a `:menu`
"add worktree" command and an `:on-purge` teardown handler, and never modifies
the workspace data model or workspaces core. It depends on `magit` (for
worktree operations and main-branch detection) and `git`, registering only
when `magit` is present, so the directionality boundary that workspaces core
maintains with its consumers is preserved.

## Requirements

### Requirement: Git non-mandatory and module droppable

The `workspace-git-worktrees` capability SHALL be a first-party workspace
integration shipped enabled by default, attaching to the workspaces system
solely through `workspace-register-integration` (it SHALL NOT modify the
workspace data model or require changes to workspaces core to function).
Removing the module from the active module set SHALL leave workspaces fully
functional. A workspace containing zero worktrees SHALL be a valid, usable
workspace. The capability SHALL depend on `magit` (for worktree operations and
main-branch detection) and on `git`; it SHALL register its integration only
when `magit` is available, so the absence of `magit` SHALL mean simply that no
git integration is present rather than an error. Any per-operation git failure
SHALL degrade to a `failed` integration outcome and SHALL NOT break workspace
creation or purge.

#### Scenario: A workspace with no worktrees is valid
- **WHEN** a workspace is created and the user declines to add any worktree
- **THEN** the workspace is registered, has a live tab, and is usable
- **AND** no git worktree is created

#### Scenario: Removing the module leaves workspaces working
- **WHEN** the `workspace-git-worktrees` module is not loaded
- **THEN** `workspace-new`, `workspace-delete`, and `workspace-purge` all
  operate normally
- **AND** the workspace integration registry exposes no git integration

#### Scenario: Absent magit means no git integration, not an error
- **WHEN** `magit` is not available
- **THEN** the git integration is not registered
- **AND** workspace creation, deletion, and purge are unaffected

#### Scenario: A git operation failure is reported, not fatal
- **WHEN** the add-worktree command runs and the underlying git operation fails
- **THEN** the command reports a `failed` outcome with a human-readable reason
- **AND** the workspace is unaffected

---

### Requirement: Soft repo-source selection

When prompting the user to "select a repo already cloned locally", the capability SHALL build the candidate list from whichever project subsystems
are loaded — at minimum `projectile-known-projects` (when projectile is
loaded), `magit-list-repos` (when magit is loaded), and
`project-known-project-roots` (when project.el is available) — merged and
de-duplicated, filtered to directories that are actual git repositories, and
SHALL always fall back to a free-form `read-directory-name` prompt so a repo
absent from every list can still be chosen. The capability SHALL NOT hard-
require projectile, magit, or project.el; it consumes only their candidate
lists, never their workflows, and SHALL function with none of them loaded.

#### Scenario: Candidates merge from loaded sources
- **WHEN** projectile and magit are both loaded and each knows a distinct repo
- **THEN** the repo selection candidates include repos from both sources
- **AND** duplicates appearing in more than one source are listed once

#### Scenario: Non-git candidates are excluded
- **WHEN** a known project directory is not a git repository
- **THEN** it is not offered as a worktree source

#### Scenario: Floor prompt always available
- **WHEN** no project subsystem is loaded, or the desired repo is in no list
- **THEN** the user can still specify a repo via `read-directory-name`

---

### Requirement: Worktree creation off main

The capability SHALL provide an interactive command, registered as the
integration's `:menu` entry, that creates a git worktree inside the target
workspace's `:home`. Operating on the anchor payload it is given (never by
consulting the current tab), the command SHALL:

1. Prompt for a source repo via soft repo-source selection.
2. Determine the repo's main branch via magit (`magit-main-branch`).
3. Prompt for the new branch name, defaulting to the workspace `:name`.
4. Create the worktree on a new branch off the detected main branch, via
   magit's worktree plumbing, at a child directory of `:home` named for the
   source repo's basename.
5. Report `ok` on success, or `failed` with a reason on any git error.

A second invocation against the same workspace SHALL add a further worktree,
so one workspace MAY contain worktrees of several repos. When a child
directory named for the chosen repo's basename already exists in `:home`, the
command SHALL report `failed` with a reason rather than overwrite it.

#### Scenario: Add a worktree off main on a workspace-named branch
- **WHEN** the add-worktree command runs against workspace `add-auth` with
  `:home ~/emacs-workspaces/add-auth/`
- **AND** the user selects source repo `~/code/frontend` whose main branch is
  `main`
- **AND** accepts the default branch name `add-auth`
- **THEN** `~/emacs-workspaces/add-auth/frontend/` is a git worktree of
  `~/code/frontend` checked out on a new branch `add-auth` created off `main`
- **AND** the command reports `ok`

#### Scenario: Multiple repos in one workspace
- **WHEN** the add-worktree command is invoked twice against workspace
  `add-auth`, selecting `~/code/frontend` then `~/code/backend`
- **THEN** `~/emacs-workspaces/add-auth/` contains both a `frontend/` and a
  `backend/` worktree

#### Scenario: Child collision is refused
- **WHEN** the add-worktree command selects repo `frontend` but
  `<home>/frontend/` already exists
- **THEN** the command reports `failed` with a reason
- **AND** no existing directory is modified

---

### Requirement: Worktree teardown on purge

The capability SHALL register an `:on-purge` handler that runs before
`workspace-purge` deletes the workspace home. The handler SHALL enumerate the
git worktrees living under `:home` and remove each via magit's worktree
plumbing, so each source repo's worktree administration is left clean rather
than holding stale/prunable entries pointing at a deleted directory. For each
removed worktree the handler SHALL offer to delete its branch, but SHALL only
delete a branch that git considers safe to remove (fully merged / carries no
unique commits); an unmerged branch SHALL be kept and SHALL NOT be force-
deleted, so possibly-unfinished work is never destroyed by teardown. A
worktree that magit refuses to remove (e.g. a dirty tree) SHALL be reported in
the `failed` reason; teardown SHALL proceed for the remaining worktrees and
SHALL NOT abort the purge.

#### Scenario: Purge removes worktrees from their source repos
- **WHEN** workspace `add-auth` contains worktrees of `~/code/frontend` and
  `~/code/backend`
- **AND** the user purges `add-auth`
- **THEN** before the home directory is deleted, each worktree is removed from
  its source repo
- **AND** neither `~/code/frontend` nor `~/code/backend` retains a worktree
  entry for the deleted paths

#### Scenario: A merged branch may be cleaned up, an unmerged one is kept
- **WHEN** a removed worktree's branch is fully merged and the user accepts the
  branch-deletion offer
- **THEN** that branch is deleted from its source repo
- **WHEN** a removed worktree's branch carries unique unmerged commits
- **THEN** that branch is kept and is never force-deleted

#### Scenario: A dirty worktree does not abort purge
- **WHEN** one worktree under `:home` has uncommitted changes and magit refuses
  to remove it
- **THEN** the `:on-purge` handler reports `failed` naming the unremoved
  worktree
- **AND** the remaining worktrees are still removed
- **AND** `workspace-purge` still deletes the home directory

---

### Requirement: Worktree state is derived, not stored

The capability SHALL NOT add persistent slots (such as `:repo` or `:branch`)
to the workspace plist. The set of worktrees, each worktree's source repo, and
each worktree's branch SHALL be derived on demand from the filesystem and git
(`git worktree list`, `git symbolic-ref HEAD`), so the directory and git
remain the single source of truth and saved registry state cannot drift from
reality.

#### Scenario: No git data persisted in the registry
- **WHEN** a workspace with worktrees is saved and reloaded
- **THEN** the persisted workspace entry carries no repo or branch slots
- **AND** the worktree set is recomputed from `:home` and git on demand
