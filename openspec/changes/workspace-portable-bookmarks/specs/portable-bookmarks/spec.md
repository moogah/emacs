## ADDED Requirements

### Requirement: Portable coordinates stamped at bookmark creation

When a bookmark record is created for a location inside a git repository, the capability SHALL stamp three additional properties onto that record: the
repository's *identity* (its remote URL, normalized so that the SSH and HTTPS
forms of one repository yield the same identity), the location's path
relative to the repository root as git spells it, and a stamp-format version
(currently `1`). The three SHALL be stamped all together or not at all; a
record carrying coordinates with a missing or unknown format version SHALL be
treated as non-portable. A location git does not track (untracked or ignored)
SHALL be left unstamped. The record's existing absolute `filename`
property SHALL be left unchanged, so the record remains valid for a stock Emacs
and for sessions where this capability is not loaded.

The capability SHALL stamp only records whose location is a single file or
directory (one location per record). A record whose location cannot be
determined that way SHALL be left unstamped rather than guessed at.

A repository with no remote SHALL produce no identity, and its records SHALL be
left unstamped: such a repository cannot be named portably, and a path-based
identity would be meaningless on another machine.

Stamping SHALL be silent — it SHALL NOT prompt, and SHALL NOT alter the
bookmark name the user chose.

#### Scenario: A file inside a repo is stamped
- **WHEN** the user sets a bookmark on a file inside a git worktree whose
  repository's remote is `git@github.com:moogah/emacs.git`
- **THEN** the stored record carries the identity `github.com/moogah/emacs` and
  the file's repo-relative path
- **AND** the record's absolute `filename` is unchanged

#### Scenario: SSH and HTTPS remotes yield one identity
- **WHEN** two checkouts of one repository name it as `git@github.com:moogah/emacs.git`
  and as `https://github.com/moogah/emacs` respectively
- **THEN** records stamped from either checkout carry the same identity

#### Scenario: A file outside any repo is left alone
- **WHEN** the user sets a bookmark on a file that is in no git repository
- **THEN** the record carries no portable coordinates
- **AND** the bookmark behaves exactly as it did before this capability existed

#### Scenario: A repo with no remote is left alone
- **WHEN** the user sets a bookmark inside a git repository that has no remote
- **THEN** the record carries no portable coordinates

#### Scenario: A record with no single location is left alone
- **WHEN** a bookmark record's location is not a single file or directory (for
  example a record produced by a third-party handler that stores a label in
  place of a path)
- **THEN** no portable coordinates are stamped
- **AND** the record is not otherwise modified

---

### Requirement: Jump-time resolution on a copy

When a bookmark carrying portable coordinates is jumped to, the capability SHALL
compute the location's path in the *current workspace's* checkout of the
identified repository, substitute it into a **copy** of the record, and hand that
copy to the record's own native handler.

The stored record SHALL NOT be modified by a jump. Because the bookmark file is
written whenever the bookmark list changes, modifying the stored record would
persist the resolved path and destroy the absolute fallback that makes
degradation safe.

The native handler SHALL be preserved: the capability SHALL NOT replace the
handler of file, directory, or third-party records, so handler-specific
restoration (point, narrowing, mode state) continues to work.

Resolution SHALL cover every jump entry point that dispatches through Emacs's
bookmark handler dispatch, including jumping in another window and
completion-based bookmark commands.

#### Scenario: A jump resolves into the current workspace
- **WHEN** the current workspace holds a checkout of the bookmark's repository
  and the file exists there
- **THEN** the file is opened from that checkout at the recorded position
- **AND** the stored record's `filename` still holds its original absolute path

#### Scenario: A stamped bookmark jumped from a different workspace
- **WHEN** a bookmark was stamped in workspace A and is jumped to while
  workspace B is current, and both hold a checkout of the repository
- **THEN** the file opens from workspace B's checkout, not workspace A's

#### Scenario: Native handler still runs
- **WHEN** a stamped bookmark's record is a directory (dired) record
- **THEN** the directory is opened by the dired bookmark handler, in the current
  workspace's checkout

#### Scenario: Unstamped bookmarks are unaffected
- **WHEN** a record carrying no portable coordinates is jumped to
- **THEN** it is handed to its handler unchanged, opening its stored absolute
  location

---

### Requirement: Closed set of resolution outcomes

Resolution SHALL produce exactly one of the following outcomes, and SHALL NOT
silently open a location the user did not ask for:

1. `resolved` — the current workspace holds a checkout of the repository and the
   location exists there; the location is opened.
2. `not-on-branch` — the checkout exists but the location does not exist within
   it; the capability SHALL report this and SHALL NOT visit the missing path as
   a new empty buffer.
3. `missing-repo` — the current workspace holds no checkout of the repository
   but exactly one canonical checkout is in view; the capability SHALL offer the
   user a bounded choice: add a worktree of that repository to the current
   workspace, open the repository's canonical checkout read-only, or cancel.
   When the canonical checkout does not contain the location either, the
   outcome carries no path and the read-only choice SHALL NOT be offered.
4. `ambiguous` — the current workspace holds two or more checkouts of the
   repository, or it holds none and two or more canonical checkouts are in view;
   the capability SHALL report the candidates and SHALL NOT choose among them or
   open anything.
5. `no-clone` — no checkout of the repository is in view on this machine; the
   capability SHALL report this and SHALL NOT open anything.
6. `non-portable` — the record carries no portable coordinates (or coordinates
   this version does not understand); the record's stored absolute location is
   used.

The resolution result SHALL carry the keys `:outcome`, `:path`, `:identity`,
`:relpath`, `:candidates` and `:checkout`. Confinement of the repo-relative path
SHALL be lexical: a path that is absolute, contains a `..` segment, or contains
a NUL SHALL be refused as `non-portable`; any other path resolves to
`<checkout>/<relpath>` by its spelling, whatever symbolic links along it point
at.

Opening a canonical checkout under `missing-repo` SHALL be read-only and SHALL
be visibly marked as such, so a lookup cannot become an unnoticed edit of the
wrong checkout. Cancelling SHALL open nothing.

#### Scenario: File absent on this branch is reported, not created
- **WHEN** the current workspace's checkout exists but the bookmark's
  repo-relative path does not exist within it
- **THEN** the user is told the file is not present in this checkout
- **AND** no new empty buffer is created for that path

#### Scenario: Missing repo offers a bounded choice
- **WHEN** the current workspace holds no checkout of the bookmark's repository
- **THEN** the user is offered to add a worktree of it to this workspace, to
  view the canonical checkout read-only, or to cancel

#### Scenario: Read-only view is marked
- **WHEN** the user chooses to view the canonical checkout
- **THEN** the buffer is read-only and visibly indicates that it is the
  canonical checkout rather than a workspace checkout

#### Scenario: Two checkouts in one workspace are reported, not chosen
- **WHEN** the current workspace holds two worktrees of the bookmark's repository
- **THEN** the user is told which candidates were found
- **AND** nothing is opened

#### Scenario: Cancel opens nothing
- **WHEN** the user cancels at the `missing-repo` choice
- **THEN** no buffer is displayed and no error is signalled

#### Scenario: No clone in view is reported
- **WHEN** no checkout of the bookmark's repository is in view on this machine
- **THEN** the user is told so
- **AND** nothing is opened

#### Scenario: The main checkout is never a silent fallback
- **WHEN** the current workspace holds no checkout of the bookmark's repository
- **THEN** the canonical checkout is NOT opened without the user choosing it

---

### Requirement: Workspace context is established before resolution

Resolution SHALL always run with a workspace in hand. When a stamped bookmark is
jumped to while the current tab is not a workspace tab, the capability SHALL
prompt the user to choose a workspace from the *registry* — including workspaces
that have no live tab in this session — and SHALL restore the chosen workspace
through the workspaces package's own restore path before resolving.

Broken workspaces (those whose home directory no longer exists) SHALL NOT be
offered, consistent with the workspaces package refusing to switch to or restore
them.

When no workspace can serve the jump (the registry is empty, or the user
declines to choose one), the capability SHALL fall back to offering the
read-only canonical checkout, or cancelling. It SHALL NOT open the stored
absolute path as if it were the current workspace's copy.

#### Scenario: Off-workspace jump restores a workspace
- **WHEN** the current tab is not a workspace and a stamped bookmark is jumped to
- **THEN** the user is prompted to choose a workspace from the registry
- **AND** the chosen workspace is restored before the location is opened

#### Scenario: A workspace with no live tab is offerable
- **WHEN** a saved workspace has no tab in the current session
- **THEN** it still appears among the choices, and choosing it materializes it

#### Scenario: Broken workspaces are not offered
- **WHEN** a registered workspace's home directory no longer exists
- **THEN** it is not among the choices

#### Scenario: Empty registry falls back, not through
- **WHEN** no workspaces are registered and a stamped bookmark is jumped to
- **THEN** the user is offered the read-only canonical checkout or cancellation
- **AND** the stored absolute path is not opened as a workspace copy

---

### Requirement: Checkout classification and canonical-checkout safety

The capability SHALL classify each checkout of a repository it discovers as one
of: the repository's *canonical* checkout (its main worktree), a
*workspace-owned* worktree (one whose root lies within a registered workspace's
home directory), or *another* linked worktree.

The canonical checkout SHALL never be taken to be a workspace-owned worktree,
and SHALL never be taken to be a linked worktree that belongs to no workspace.
Offering a workspace-owned worktree as the canonical checkout would open one
workspace's files while the user is in another — the defect this capability
exists to remove.

#### Scenario: Main worktree is the canonical checkout
- **WHEN** a repository has a main worktree and several linked worktrees
- **THEN** the main worktree is classified canonical and the others are not

#### Scenario: A workspace worktree is never canonical
- **WHEN** a repository's checkouts include worktrees inside registered
  workspace home directories
- **THEN** none of them is offered as the canonical checkout

#### Scenario: A stale sibling worktree is not canonical
- **WHEN** a repository has a linked worktree that lies in no registered
  workspace home
- **THEN** it is not offered as the canonical checkout

---

### Requirement: Discovery without configuration

Identity and checkout discovery SHALL require no user configuration and no
registry of known projects. The capability SHALL derive what it needs by
querying git about checkouts already in view — the current workspace's
worktrees, the worktrees of other registered workspaces, and the configuration
repository the running Emacs was loaded from.

The capability SHALL NOT depend on `magit`, on `projectile`, or on any
directory-scanning configuration. Its absence of configuration is the point:
resolution behaviour SHALL NOT differ between machines because of registration
state.

Where git cannot answer (the repository's metadata is unavailable or the git
invocation fails), the capability SHALL degrade to `no-clone` or
`non-portable` rather than signalling an error into the jump.

#### Scenario: Resolution works with no configuration set
- **WHEN** no project-listing package is configured or loaded, and no scanning
  roots are set
- **THEN** a stamped bookmark still resolves in a workspace that holds a
  checkout of its repository

#### Scenario: Magit absent is not an error
- **WHEN** `magit` is not loaded
- **THEN** stamping and resolution operate normally

#### Scenario: Git failure degrades rather than signalling
- **WHEN** a git invocation needed for discovery fails
- **THEN** the jump reports a degraded outcome and no error escapes into the
  bookmark machinery

---

### Requirement: Degradation and isolation

The capability SHALL be additive: a bookmark file written with portable
coordinates SHALL remain readable by a stock Emacs with this capability absent,
and bookmarks created before this capability existed SHALL continue to work
through the `non-portable` outcome.

A failure affecting one bookmark SHALL NOT affect other bookmarks or the
bookmark file as a whole.

#### Scenario: Stock Emacs can still read the file
- **WHEN** a bookmark file containing stamped records is loaded by an Emacs
  without this capability
- **THEN** the file loads and the bookmarks jump to their stored absolute
  locations

#### Scenario: Pre-existing bookmarks keep working
- **WHEN** a bookmark created before this capability is jumped to
- **THEN** it opens its stored absolute location

#### Scenario: One bad record does not break the rest
- **WHEN** one record's coordinates cannot be resolved or are malformed
- **THEN** that jump reports a degraded outcome and other bookmarks are
  unaffected
