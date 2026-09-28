## MODIFIED Requirements

### Requirement: Soft repo-source selection

When prompting the user to "select a repo already cloned locally", the capability SHALL build the candidate list from git-derived discovery first — the checkouts
already in view (the current workspace's worktrees, the worktrees of other
registered workspaces, and the configuration repository the running Emacs was
loaded from), each contributing its repository's main worktree — and SHALL then
add candidates from `magit-list-repos` when magit is loaded. No
other project subsystem is consulted: projectile and project.el are NOT sources.
Every candidate from every source SHALL pass one admission predicate — it is
offered only when it is a canonical checkout (a repository's main worktree) that
no registered workspace owns; a magit-listed linked worktree contributes its
repository's main worktree in its place. Candidates SHALL be de-duplicated, and
the capability SHALL always fall back to a free-form `read-directory-name`
prompt so a repo absent from every source can still be chosen.

No project subsystem is a minimum requirement. The capability SHALL NOT
hard-require magit; it consumes only its candidate list, never its workflows,
and SHALL function without it — in which case git-derived discovery plus the
`read-directory-name` floor are the whole candidate set. Projectile was dropped
as a source because it only reports repositories that have been registered with
it, which makes candidate sets differ between machines for reasons unrelated to
what is actually cloned.

#### Scenario: Git-derived candidates need no configuration
- **WHEN** no project subsystem is loaded and no scanning configuration exists
- **THEN** the candidate list still includes the repositories of the checkouts in
  view, discovered via git

#### Scenario: A magit-listed linked worktree contributes its main worktree
- **WHEN** magit lists a linked worktree of a repository
- **THEN** the candidate offered is that repository's main worktree, listed once

#### Scenario: A workspace-owned checkout is never offered
- **WHEN** a checkout lies inside a registered workspace's home
- **THEN** it is not offered as a worktree source, whichever source reported it

#### Scenario: Remote magit entries are dropped without a connection
- **WHEN** magit lists a repository on a remote (TRAMP) path
- **THEN** it is dropped from the candidates and no connection is attempted

#### Scenario: Non-git candidates are excluded
- **WHEN** a known project directory is not a git repository
- **THEN** it is not offered as a worktree source

#### Scenario: Floor prompt always available
- **WHEN** no project subsystem is loaded, or the desired repo is in no list
- **THEN** the user can still specify a repo via `read-directory-name`

#### Scenario: Magit absent does not shrink discovery
- **WHEN** magit is not loaded
- **THEN** the candidate list is still populated from git-derived discovery

---

### Requirement: Worktree creation off main

The capability SHALL provide an interactive command, registered as the
integration's `:menu` entry, that creates a git worktree inside the target
workspace's `:home`. Operating on the anchor payload it is given (never by
consulting the current tab), the command SHALL:

1. Take the source repo from the payload's `:source-repo` when present, with
   no prompt and no discovery. A `:source-repo` that is not an existing local
   directory reports `failed` with a reason and prompts nothing. Otherwise
   prompt via soft repo-source selection.
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

#### Scenario: A payload naming the source repo skips the source prompt
- **WHEN** the add-worktree command runs against workspace `add-auth` with a
  payload whose `:source-repo` is `~/code/frontend`
- **THEN** no source-repo prompt is shown and no candidate discovery runs
- **AND** the worktree is created from `~/code/frontend` exactly as if the user
  had selected it

#### Scenario: A source repo that is not a local directory is refused
- **WHEN** the payload's `:source-repo` is not an existing local directory
- **THEN** the command reports `failed` with a reason
- **AND** no prompt is shown and no worktree is created

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
