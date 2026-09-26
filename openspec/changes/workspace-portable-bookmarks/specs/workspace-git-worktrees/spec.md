## MODIFIED Requirements

### Requirement: Soft repo-source selection

When prompting the user to "select a repo already cloned locally", the capability SHALL build the candidate list from git-derived discovery first — the checkouts
already in view (the current workspace's worktrees, the worktrees of other
registered workspaces, and the configuration repository the running Emacs was
loaded from), together with the sibling checkouts git reports for each of them —
and SHALL then add candidates from `magit-list-repos` when magit is loaded. No
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
