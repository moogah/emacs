# org-graph capability

## ADDED Requirements

### Requirement: Workspace-Substrate Discovery

The system SHALL index org notes using `vulpea` as the single index, fed an
*explicit, bounded* set of root directories: the durable concept vault
(default `~/org/roam/`) plus the `:home` directory of each active workspace
(and its `sessions/`), obtained from the `workspaces` registry. The system
SHALL NOT walk `~/work/` (or any directory tree) wholesale to discover notes.

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

### Requirement: Workspace Integration

The system SHALL attach to the `workspaces` package exclusively through the
published integration registry (`workspace-register-integration`), never by
modifying `workspaces` core. The integration SHALL be strictly additive: a
workspace with org-graph absent or failing behaves exactly as before, and a
failing handler is surfaced but never rolls back the workspace.

The integration SHALL register an `:on-create` handler that, using only the
pushed anchor payload (`:name`, `:home`, `:sessions-dir`, `:context`), adds
the new workspace `:home` to the `vulpea` sync roots and triggers a targeted
index update for it — because filesystem watchers are not auto-installed for
directories added after autosync has started.

The system SHALL populate the `workspace-assistant` preset's tool slot with
the org-graph agent tools (see Agent-Facing Graph Tools), so the
per-workspace assistant — already directory-scoped to its `:home` — gains the
graph read/write surface.

#### Scenario: Creating a workspace registers it for indexing

- **WHEN** the user creates a new workspace and the org-graph `:on-create`
  handler runs with the workspace's anchor payload
- **THEN** the workspace `:home` is added to the `vulpea` sync roots and its
  ID-bearing notes are indexed, with no manual sync

#### Scenario: Workspace assistant exposes graph tools

- **WHEN** a workspace's assistant session starts from the
  `workspace-assistant` preset
- **THEN** the org-graph query, typed-edge, and write tools are available to
  that assistant

#### Scenario: org-graph absent leaves workspaces unaffected

- **WHEN** the `workspaces` package is loaded but `org-graph` is not
- **THEN** workspace creation, the assistant preset, and the integration
  menu behave exactly as they do without org-graph

### Requirement: Indexable Workspace Notes

Because `vulpea` only indexes notes that carry an `:ID:`, the system SHALL
ensure that workspace `home.org` files and gptel session files receive a
stable org ID at creation, so they participate in the index and can serve as
`id:` link targets. ID assignment SHALL be additive and idempotent: a file
that already has an `:ID:` is left unchanged.

#### Scenario: Scaffolded home.org is indexable

- **WHEN** a new workspace is scaffolded
- **THEN** its `home.org` carries an `:ID:` (captured in the workspace's
  initial commit) and appears in the `vulpea` index after a sync

#### Scenario: New session file is indexable

- **WHEN** a new gptel session is created under a workspace's `sessions/`
- **THEN** the session file carries an `:ID:` and appears in the index

### Requirement: Typed Semantic Edges

The system SHALL extract typed relations from notes and store them in a
queryable `typed_edges` index, implemented as a custom `vulpea` extractor
table holding `(from-id, rel-type, to-id)` rows. Edges are directional and
explicitly authored; the system SHALL NOT materialize inverse rows.

**Open vocabulary.** The set of relation types SHALL be open: any
author-coined relation is extracted the moment it appears, with no
allowlist and no registration step. The relation type is stored verbatim as
a symbol; the system SHALL NOT restrict extraction to a fixed set of type
names. A configurable list MAY seed completion suggestions but SHALL NOT
gate extraction.

**Two authoring surfaces.** The system SHALL extract edges from both:

1. **Edge-drawer items.** A dedicated drawer (its name a single
   configurable value, default `EDGES`) holds description-list items of the
   form `- <type> :: [[id:target][description]]`. The item tag is the
   relation type — trimmed, lowercased, with spaces and underscores mapped
   to hyphens, interned as a symbol (`- follows up ::` → `follows-up`). The
   drawer name is the sole discriminator: ordinary PROPERTIES entries and
   links outside the drawer SHALL NOT be treated as edges, even when a
   property value contains an `id:` link. An item MAY hold one or more
   `id:` references and a type MAY repeat across items; each reference
   SHALL become a separate row. Edge-drawer rows SHALL attribute to the
   nearest ancestor node carrying an `:ID:` (the enclosing heading, else
   the file-level node) — the same rule as inline links; a drawer with no
   ID-bearing ancestor SHALL contribute nothing. Non-item drawer content
   and malformed items SHALL be ignored without error, and the drawer
   SHALL be excluded from export by default.
2. **Typed inline `rel:` links.** A link of the form
   `[[rel:<type>:<target-id>][description]]` (the `rel` link-type name is
   likewise a single configurable value, default `rel`) in a note's body
   SHALL become a typed edge whose `to-id` is `<target-id>` and whose
   `from-id` is the
   nearest ancestor node carrying an `:ID:` (the enclosing heading, else the
   file-level node). A `rel:` link with no ID-bearing ancestor SHALL be
   dropped, not attributed to an unrelated note.

Both surfaces SHALL feed the same `typed_edges` table and the same query
API. `vulpea`'s native link `:type` (link-kind: id/file/https) is NOT a
semantic relation type; semantic relations exist only in the `typed_edges`
index this requirement defines.

**Optional edge-type registry.** A relation type SHALL function fully
without registration and, when unregistered, SHALL render as its raw
symbol. A note tagged `:edge-type:` MAY declare metadata for a type — a
human label, an `:INVERSE:` symbol, a `:SYMMETRIC:` boolean, and a
description — held as ordinary vault data (git-versioned, indexed,
discoverable via a dedicated finder). Registry metadata SHALL only enrich
reads and completion; it SHALL NOT be required for extraction, and its
absence SHALL NOT drop any edge.

The system SHALL expose a query API that returns:
- All outgoing typed edges for a given note ID and relation type.
- All incoming typed edges (typed backlinks) for a given note ID and
  relation type.
- All notes connected to a given note by any typed relation.

Inverse and symmetry SHALL be derived at query/display time from registry
metadata, not stored: a registered `:INVERSE:` MAY be used to render a
stored edge from the target's perspective, and a `:SYMMETRIC: t` type MAY
be surfaced in both directions by reads, without any additional stored row.

Typed-edge extraction SHALL only run on notes located under the
typed-graph-scoped root (default: `~/org/roam/`), not on workspace-local or
session notes that are indexed for discovery and navigation.

#### Scenario: Edge-drawer item creates one edge row

- **WHEN** a note's `EDGES` drawer contains `- implements :: [[id:abc]]`
  and the extractor runs
- **THEN** the `typed_edges` index contains exactly one row with
  `from-id = <note-id>`, `rel-type = implements`, `to-id = abc`

#### Scenario: Novel unregistered relation type extracts with no configuration

- **WHEN** a note's `EDGES` drawer contains `- falsifies :: [[id:abc]]` and
  no `falsifies` edge-type registry note or configuration entry exists
- **THEN** the `typed_edges` index contains a row with
  `rel-type = falsifies`, extracted purely from its presence in the edge
  drawer

#### Scenario: Ordinary property or body link is not an edge

- **WHEN** a note has `:SOURCE: [[id:abc]]` in its PROPERTIES drawer and a
  bare `[[id:def]]` link in its body, neither inside the `EDGES` drawer
- **THEN** no row for either appears in the `typed_edges` index

#### Scenario: Multi-link edge-drawer item creates multiple edge rows

- **WHEN** a note's `EDGES` drawer contains
  `- relates-to :: [[id:abc]] [[id:def]]`
- **THEN** the `typed_edges` index contains two rows, one for each
  destination, sharing `from-id` and `rel-type = relates-to`

#### Scenario: Edge drawer under an ID-bearing heading attributes to that heading

- **WHEN** a heading carrying `:ID: heading-id` has an `EDGES` drawer
  containing `- implements :: [[id:abc]]`
- **THEN** the row's `from-id` is `heading-id`, not the file-level node's
  ID

#### Scenario: Inline rel link is attributed to its enclosing node

- **WHEN** a note's body contains
  `[[rel:falsifies:xyz][the earlier claim]]` under a heading that carries
  its own `:ID: heading-id`
- **THEN** the `typed_edges` index contains a row with
  `from-id = heading-id`, `rel-type = falsifies`, `to-id = xyz`, and a
  bare `id:` link in the same prose produces no edge

#### Scenario: Typed-edge query returns incoming edges

- **WHEN** notes A and B each contain `- implements :: [[id:C]]` in their
  `EDGES` drawers and the user queries incoming edges of type `implements`
  for note C
- **THEN** the query returns rows for both A and B

#### Scenario: Registered inverse renders from the target's perspective

- **WHEN** an `implements` edge-type registry note declares
  `:INVERSE: implemented-by`, note A's `EDGES` drawer contains
  `- implements :: [[id:B]]`, and the display layer shows note B's
  relations
- **THEN** the relation to A is presented as `implemented-by` without any
  `implemented-by` row existing in `typed_edges`

#### Scenario: Workspace-local note is excluded from typed-edge index

- **WHEN** a note under a workspace `:home` (outside `~/org/roam/`)
  contains an `- implements ::` item in its `EDGES` drawer
- **THEN** that note is reachable via the navigator but no row for it
  appears in the `typed_edges` index

### Requirement: Schema-Backed Note-Type Taxonomy and Finders

The system SHALL define a fixed taxonomy of note types — `log`
(daily/work logs), `debug` (troubleshooting sessions), `topic` (durable
concept notes), `reference` (annotated external content), and `project`
(project-local notes) — as `vulpea-schema` definitions. Each schema SHALL
declare a predicate selecting the notes of that type (typically by filetag)
and field expectations validated by `vulpea-schema-validate`. The filetag is
the type selector, not the entire taxonomy.

For each type the system SHALL expose a dedicated finder command that
restricts completion to notes of that type using the schema predicate as the
single source of truth, so finder membership and validation cannot drift.
Existing `org-roam` / `vulpea` selection commands remain available and
unfiltered.

The system SHALL provide a way to validate notes of a given type against
their schema and report violations.

#### Scenario: Topic finder excludes other note types

- **WHEN** the vault contains notes of type `topic`, `log`, and `debug` and
  the user invokes the topic finder
- **THEN** completion candidates are limited to notes the `topic` schema
  predicate selects

#### Scenario: Untagged note is invisible to type-specific finders but visible to the catch-all

- **WHEN** a note has an `:ID:` but matches no taxonomy schema predicate
- **THEN** it does not appear in any type-specific finder, but it does
  appear in the unfiltered navigator / catch-all finder

#### Scenario: Schema validation reports a missing required field

- **WHEN** a note selected by the `reference` schema predicate is missing a
  field the schema marks required, and the user validates references
- **THEN** a `missing-required` violation is reported for that note

### Requirement: Agent-Facing Graph Tools

The system SHALL register gptel tools that expose the graph to AI agents:
- A read tool returning notes matching a structured query (note type,
  typed-edge predicates, title match).
- A read tool returning typed edges incident to a given note ID, with target
  titles resolved.
- A write tool creating a note with a deterministic ID, taxonomy
  membership, and optional initial typed-edge properties.

The tools SHALL be exposed both in the global gptel tool registry and, via
the Workspace Integration requirement, to the per-workspace assistant.

All write operations from agent tools SHALL be serialized through a single
coordinator so that two concurrent tool calls writing to the same file
cannot interleave or corrupt each other. Writes to distinct files MAY
proceed in parallel.

Agent-authored notes SHALL receive an `:agent-draft:` filetag by default.
Type-specific finders MAY exclude `:agent-draft:` notes by default; a
dedicated review finder SHALL include them.

Tool descriptions SHALL state that typed-edge extraction runs only on
`~/org/roam/` notes, so a write to a workspace-local path will not produce
typed edges.

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

The org-graph / `vulpea` indices SHALL be stored in locations distinct from
the org-roam database (under `runtime/state/`) so that one tool's resync,
rebuild, or wipe never affects the other.

#### Scenario: org-roam continues to work after org-graph load

- **WHEN** the org-graph module loads and finishes its initial sync
- **THEN** `org-roam-node-find`, `org-roam-buffer-toggle`, and
  `org-roam-capture` continue to function with no observable behavior
  change

#### Scenario: Wiping the org-graph database does not affect org-roam

- **WHEN** the user deletes the `vulpea` database file under
  `runtime/state/` and triggers a full resync
- **THEN** org-roam's database, backlinks, and node list are unaffected

### Requirement: Non-Blocking Synchronization

Database synchronization SHALL run asynchronously. Saving a file under
an indexed root SHALL NOT block the user's editing for a perceptible
duration (target: under 50ms of foreground latency for the save itself,
with reindexing completing in the background).

#### Scenario: Save in a large file does not stutter

- **WHEN** the user saves an org file containing 1000+ headings under an
  indexed root
- **THEN** the save returns control to the editor in under 50ms, and
  the index is updated within a couple of seconds of the save without
  further user interaction
</content>
