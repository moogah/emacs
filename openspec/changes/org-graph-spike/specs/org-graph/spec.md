# org-graph capability

## ADDED Requirements

### Requirement: Distributed Note Discovery

The system SHALL discover and index any org file that has a top-level
`ID` property, regardless of its filesystem location, by integrating with
`org-id-locations`. Notes co-located with their work (e.g. inside a
project directory under `~/work/<ticket>/`) SHALL be reachable from the
graph without per-directory configuration.

The system SHALL provide an eager-discovery entry point that walks a
configured set of root directories (default: `~/org/roam/` and `~/work/`)
and registers every ID-bearing org file with `org-id-locations` so first
visit isn't required.

#### Scenario: Project-co-located note is discovered after eager scan

- **WHEN** an org file with an `:ID:` property exists at
  `~/work/PROJ-1234/notes.org` and the user runs the eager-discovery
  entry point
- **THEN** the file's ID resolves via `org-id-find` and the note appears
  in the org-graph navigator without the user having visited the file

#### Scenario: Note added during a session is picked up automatically

- **WHEN** a new ID-bearing org file is created under a watched root
  while Emacs is running
- **THEN** the file is indexed within 5 seconds of being saved without
  the user invoking a manual sync command

#### Scenario: Externally modified file is reindexed

- **WHEN** a file under a watched root is modified outside of Emacs
  (e.g. `git pull`, an AI agent's tool-call write, Syncthing)
- **THEN** the system detects the change via filesystem watching and
  reindexes the file without requiring a manual sync

### Requirement: Typed Semantic Edges

The system SHALL extract typed relations declared as PROPERTIES-drawer
entries on a note and store them in a queryable `typed_edges` index.
The supported relation types SHALL be configurable; the initial set is
`IMPLEMENTS`, `CONTRADICTS`, `SUPERSEDES`, `RELATES_TO`.

A typed-edge property MAY appear multiple times on the same note and MAY
contain one or more `id:` link references. The extractor SHALL parse
each into a separate `(from-id, rel-type, to-id)` row.

The system SHALL expose a query API that returns:
- All outgoing typed edges for a given note ID and relation type.
- All incoming typed edges (typed backlinks) for a given note ID and
  relation type.
- All notes connected to a given note by any typed relation.

Typed-edge extraction SHALL only run on notes located under the
typed-graph-scoped roots (default: `~/org/roam/`), not on project-local
notes discovered via the broader org-id mechanism.

#### Scenario: Single typed property creates one edge row

- **WHEN** a note has `:IMPLEMENTS: [[id:abc]]` in its PROPERTIES drawer
  and the extractor runs
- **THEN** the `typed_edges` index contains exactly one row with
  `from-id = <note-id>`, `rel-type = implements`, `to-id = abc`

#### Scenario: Multi-valued typed property creates multiple edge rows

- **WHEN** a note has `:RELATES_TO: [[id:abc]] [[id:def]]` in its
  PROPERTIES drawer
- **THEN** the `typed_edges` index contains two rows, one for each
  destination, sharing `from-id` and `rel-type = relates-to`

#### Scenario: Typed-edge query returns incoming edges

- **WHEN** notes A and B both declare `:IMPLEMENTS: [[id:C]]` and the
  user queries incoming edges of type `implements` for note C
- **THEN** the query returns rows for both A and B

#### Scenario: Project-local note is excluded from typed-edge index

- **WHEN** a note at `~/work/PROJ-1234/notes.org` declares an
  `:IMPLEMENTS:` property
- **THEN** that note is reachable via the navigator but no row for it
  appears in the `typed_edges` index

### Requirement: Note-Type Taxonomy and Finders

The system SHALL recognize a fixed taxonomy of note types via filetags:
`log` (daily/work logs), `debug` (troubleshooting sessions), `topic`
(durable concept notes), `reference` (annotated external content), and
`project` (project-local notes co-located with work).

For each type the system SHALL expose a dedicated finder command that
restricts completion to notes carrying the corresponding filetag. The
existing org-roam-find / org-node-find namespaces remain available and
unfiltered.

#### Scenario: Topic finder excludes other note types

- **WHEN** the vault contains notes tagged `:topic:`, `:log:`, and
  `:debug:` and the user invokes the topic finder
- **THEN** completion candidates are limited to notes carrying the
  `:topic:` filetag

#### Scenario: Untagged note is invisible to type-specific finders but visible to the catch-all

- **WHEN** a note has an `:ID:` but no taxonomy filetag
- **THEN** it does not appear in any type-specific finder, but it does
  appear in the unfiltered navigator

### Requirement: Agent-Facing Graph Tools

The system SHALL register gptel tools that expose the graph to AI
agents:
- A read tool returning notes matching a structured query (filetags,
  typed-edge predicates, full-text title match).
- A read tool returning typed edges incident to a given note ID.
- A write tool creating a new note with deterministic filename, ID,
  taxonomy filetag, and optional initial typed-edge properties.

All write operations from agent tools SHALL be serialized through a
single coordinator so that two concurrent tool calls writing to the
same file cannot interleave or corrupt each other. Writes to distinct
files MAY proceed in parallel.

Agent-authored notes SHALL receive an `:agent-draft:` filetag by
default. Type-specific finders MAY exclude `:agent-draft:` notes by
default; a dedicated review finder SHALL include them.

#### Scenario: Concurrent agent writes to same file are serialized

- **WHEN** two agent tool calls write to the same target file path
  within the same instant
- **THEN** the writes apply sequentially under the coordinator's lock
  and the final file content reflects both writes (no truncation, no
  interleaved bytes)

#### Scenario: Agent-authored note carries draft tag

- **WHEN** an AI agent creates a new note via the write tool without
  explicitly clearing the draft tag
- **THEN** the resulting file carries `:agent-draft:` in its filetags
  and is excluded from the default topic finder

#### Scenario: Graph query returns typed edges

- **WHEN** an agent calls the typed-edges read tool with a note ID
- **THEN** the response is a structured list of `{from, rel-type, to,
  to-title}` objects, ready for the agent to reason over without
  re-parsing files

### Requirement: Coexistence with org-roam

The system SHALL operate without modifying org-roam state, schema,
keybindings, or capture templates. org-roam SHALL remain fully usable
during the spike with the same UX as before.

The org-graph indices SHALL be stored in locations distinct from the
org-roam database (under `runtime/state/`) so that one tool's resync,
rebuild, or wipe never affects the other.

#### Scenario: org-roam continues to work after org-graph load

- **WHEN** the org-graph module loads and finishes its initial sync
- **THEN** `org-roam-node-find`, `org-roam-buffer-toggle`, and
  `org-roam-capture` continue to function with no observable behavior
  change

#### Scenario: Wiping the org-graph database does not affect org-roam

- **WHEN** the user deletes the org-graph database file under
  `runtime/state/` and triggers a full resync
- **THEN** org-roam's database, backlinks, and node list are unaffected

### Requirement: Non-Blocking Synchronization

Database synchronization SHALL run asynchronously. Saving a file under
a watched root SHALL NOT block the user's editing for a perceptible
duration (target: under 50ms of foreground latency for the save itself,
with reindexing completing in the background).

#### Scenario: Save in a large file does not stutter

- **WHEN** the user saves an org file containing 1000+ headings under a
  watched root
- **THEN** the save returns control to the editor in under 50ms, and
  the index is updated within 2 seconds of the save without further
  user interaction
