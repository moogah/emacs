## MODIFIED Requirements

### Requirement: Soft repo-source selection

When prompting the user to "select a repo already cloned locally", the capability SHALL build the candidate list from git-derived discovery first — the checkouts
already in view (the current workspace's worktrees, the worktrees of other
registered workspaces, and the configuration repository the running Emacs was
loaded from), together with the sibling checkouts git reports for each of them —
and SHALL then merge in candidates from whichever project subsystems happen to be
loaded: `magit-list-repos` (when magit is loaded),
`project-known-project-roots` (when project.el is available), and
`projectile-known-projects` (when projectile is loaded). Candidates SHALL be
merged and de-duplicated, filtered to directories that are actual git
repositories, and the capability SHALL always fall back to a free-form
`read-directory-name` prompt so a repo absent from every source can still be
chosen.

No project subsystem is a minimum requirement. The capability SHALL NOT
hard-require projectile, magit, or project.el; it consumes only their candidate
lists, never their workflows, and SHALL function with none of them loaded — in
which case git-derived discovery plus the `read-directory-name` floor are the
whole candidate set. Projectile in particular SHALL be treated as one optional
legacy source among others rather than as the baseline, because it only reports
repositories that have been registered with it, which makes candidate sets differ
between machines for reasons unrelated to what is actually cloned.

#### Scenario: Git-derived candidates need no configuration
- **WHEN** no project subsystem is loaded and no scanning configuration exists
- **THEN** the candidate list still includes the repositories of the checkouts in
  view, discovered via git

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

#### Scenario: Projectile absent does not shrink discovery
- **WHEN** projectile is not loaded
- **THEN** the candidate list is still populated from git-derived discovery and
  any other loaded sources
