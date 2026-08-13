# org-graph capability — delta for vulpea-human-commands

Delta against the org-graph-spike change's spec
(`openspec/changes/org-graph-spike/specs/org-graph/spec.md`, not yet
archived to `openspec/specs/`).

## MODIFIED Requirements

### Requirement: Workspace-Substrate Discovery

The system SHALL index org notes using `vulpea` as the single index, fed an
*explicit, bounded* set of root directories: the durable concept vault
(default `~/org/`) plus the `:home` directory of each active workspace
(and its `sessions/`), obtained from the `workspaces` registry. The system
SHALL NOT walk `~/work/` (or any directory tree) wholesale to discover notes.

The vault root covers the whole vault directory, not only its `roam/`
subdirectory: ID-bearing notes anywhere under the vault root SHALL be
indexed, so notes created at the vault top level share one index and one
re-scan boundary with the pre-existing `roam/` corpus.

The system SHALL ensure `id:` links resolve in arbitrary org buffers —
including buffers outside the indexed roots — by keeping Emacs's global
`org-id-locations` populated for every indexed note. Because `vulpea`
registers IDs lazily (as files are touched), the system SHALL additionally
seed `org-id-locations` from the `vulpea` database at module load so that a
note indexed in a prior session is link-resolvable without first re-visiting
its file.

`org-node` is NOT used; running a second discovery index alongside `vulpea`
is explicitly out of scope (it duplicates scanning and caching with no added
capability).

#### Scenario: Workspace home is indexed without configuration

- **WHEN** a workspace anchored at `~/work/PROJ-1234/` exists and the user
  triggers discovery configuration
- **THEN** `~/work/PROJ-1234/` and its `sessions/` are present in the
  `vulpea` sync roots and their ID-bearing notes are indexed, without the
  user editing any per-directory configuration

#### Scenario: id-link resolves across sessions via the startup seed

- **WHEN** a note with an `:ID:` was indexed in a previous Emacs session and
  the user clicks an `id:` link to it from an unrelated buffer in a fresh
  session, before that note's file has been visited
- **THEN** the link resolves via `org-id-find` because `org-id-locations` was
  seeded from the `vulpea` database at load

#### Scenario: Note added during a session is picked up automatically

- **WHEN** a new ID-bearing org file is created under an indexed root while
  Emacs is running
- **THEN** the file is indexed within a few seconds of being saved without
  the user invoking a manual sync command

#### Scenario: Externally modified file is reindexed

- **WHEN** a file under an indexed root is modified outside of Emacs
  (e.g. `git pull`, an AI agent's tool-call write, Syncthing)
- **THEN** the system detects the change via `vulpea`'s external-change
  detection (fswatch, or fd/find polling fallback) and reindexes the file
  without requiring a manual sync

#### Scenario: The wider work tree is not walked

- **WHEN** the user has many checked-out repositories under `~/work/` but only
  a few are anchored as workspaces
- **THEN** only the anchored workspace homes (plus the concept vault) are in
  the sync roots; un-anchored repositories are not scanned or watched

#### Scenario: Vault note outside roam/ is indexed

- **WHEN** an ID-bearing note exists directly under the vault root
  (e.g. `~/org/inbox.org`), outside the `roam/` subdirectory
- **THEN** the note is indexed by `vulpea` and returned by graph queries and
  finders, the same as a note under `roam/`

## ADDED Requirements

### Requirement: Boot-Order-Independent Default Note Placement

The system SHALL pin `vulpea`'s default new-note directory
(`vulpea-default-notes-directory`) to the vault root at module load, so
that where a new note lands does not depend on whether sync configuration
(`org-graph/configure-sync`) has run in the current session. The pinned
directory SHALL lie within the index roots, so every note created through
the default placement survives a full re-scan or database rebuild.

#### Scenario: Note created before sync is configured lands in the vault

- **WHEN** on a fresh boot, before any sync configuration has run, the user
  creates a note through a vulpea-backed creation path with no explicit
  directory
- **THEN** the note's file is created under the vault root (`~/org/`), not
  under `org-directory` or any sync-order-dependent fallback

#### Scenario: Default-placed note survives a full re-scan

- **WHEN** a note was created via the default placement and a full `vulpea`
  re-scan or database rebuild subsequently runs
- **THEN** the note is still present in the database afterwards, because its
  file lies inside the configured index roots
