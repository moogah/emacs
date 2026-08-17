# org-graph-note-commands capability

Human-side interactive note commands for the vulpea-backed graph — the
org-roam-equivalent surface for finding, creating, and linking notes.
vulpea is the replacement for org-roam, so these commands stand alone:
they neither read from nor write to org-roam's database.

## ADDED Requirements

### Requirement: Find-or-Create Note Command

The system SHALL provide an interactive find-or-create command
(`org-roam-node-find` equivalent) that completes over every indexed note
regardless of note type. Selecting an existing note SHALL visit it.
Submitting a title that matches no indexed note SHALL create a new note
with that title and visit it.

Creation SHALL be synchronous and complete before the command returns:
the file is written under the default note placement
(`vulpea-default-notes-directory`), an `:ID:` property is assigned, the
ID is registered in `org-id-locations`, and the note is inserted into
the `vulpea` database — no re-scan, save hook, or manual sync step is
required for the note to be fully live.

#### Scenario: Existing note is found and visited

- **WHEN** the user invokes the find-or-create command and selects an
  indexed note's title
- **THEN** the note's file is visited, with no note created

#### Scenario: Unmatched title creates and visits a new note

- **WHEN** the user invokes the find-or-create command and submits a title
  matching no indexed note
- **THEN** a new org file is created under the default notes directory with
  that `#+title:` and a file-level `:ID:`, and the buffer visiting it is
  selected

#### Scenario: Created note is immediately findable

- **WHEN** a note has just been created via the find-or-create command, with
  no manual re-index in between
- **THEN** immediately re-invoking the command (or any graph query) lists
  the new note among its candidates

#### Scenario: Created note is immediately id-resolvable

- **WHEN** a note has just been created via the find-or-create command
- **THEN** `org-id-find` on its ID resolves to the new file in the same
  session, without the file having been re-visited or re-indexed

### Requirement: Insert-Link Command

The system SHALL provide an interactive insert-link command
(`org-roam-node-insert` equivalent) that selects a note — creating it
first via the same synchronous creation path as the find-or-create
command when the submitted title matches no indexed note — and inserts
an `[[id:<uuid>][<description>]]` link to it at point. The description
SHALL default to the note's title. When the region is active, the region
text SHALL become the link description and the region SHALL be replaced
by the link.

#### Scenario: Link to an existing note is inserted at point

- **WHEN** the user invokes the insert-link command in an org buffer and
  selects an existing note
- **THEN** an `id:` link to that note, described by its title, is inserted
  at point

#### Scenario: Linking to a nonexistent title creates the note first

- **WHEN** the user invokes the insert-link command and submits a title
  matching no indexed note
- **THEN** the note is created (same guarantees as find-or-create: file,
  `:ID:`, org-id registration, database insertion) and a link to the new
  note is inserted at point

#### Scenario: Active region becomes the link description

- **WHEN** the user selects a region of text and invokes the insert-link
  command
- **THEN** the inserted link replaces the region, and the region's text is
  used as the link description
