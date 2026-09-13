# org-graph capability — delta for workspace-notes-in-vault

org-graph becomes a generic graph package with no workspace or session
vocabulary: it indexes the vault only, extracts edges from the whole
vault, and publishes an edge-writer API, a project-note constructor, and
a connected-notes dynamic block for other packages (via the integration
layer) to build on.

## ADDED Requirements

### Requirement: Vault Discovery

The system SHALL index org notes using `vulpea` as the single index, fed
exactly one root: the vault root (`org-graph-vault-root`, default
`~/org/`). ID-bearing notes anywhere under the vault root SHALL be
indexed. The system SHALL NOT add any other directory to the sync roots,
SHALL NOT consult any other package to discover roots, and SHALL NOT walk
any tree outside the vault. Hidden directories under the vault are
excluded by `vulpea`'s own rule.

The system SHALL keep Emacs's global `org-id-locations` populated for
every indexed note, seeding it from the `vulpea` database at load so a
note indexed in a prior session is link-resolvable before its file is
visited. `org-node` is NOT used.

#### Scenario: Only the vault root is a sync root
- **WHEN** discovery is configured
- **THEN** `vulpea-db-sync-directories` equals the list containing only the
  expanded vault root

#### Scenario: Files in a hidden subdirectory of the vault are not indexed
- **WHEN** `~/org/sessions/S/branches/main/.agents/x/session.org` carries
  an `:ID:`
- **THEN** it is absent from the index after a full scan

#### Scenario: id-link resolves across sessions via the startup seed
- **WHEN** a note with an `:ID:` was indexed in a previous Emacs session
  and the user follows an `id:` link to it from an unrelated buffer in a
  fresh session, before that note's file has been visited
- **THEN** the link resolves because `org-id-locations` was seeded from
  the `vulpea` database at load

#### Scenario: Note added during a session is picked up automatically
- **WHEN** a new ID-bearing org file is created under the vault root while
  Emacs is running
- **THEN** the file is indexed within a few seconds of being saved without
  a manual sync

#### Scenario: Externally modified file is reindexed
- **WHEN** a file under the vault root is modified outside of Emacs
- **THEN** the change is detected via `vulpea`'s external-change detection
  and the file is reindexed

### Requirement: Index parser hardening

During indexing the system SHALL prevent evaluation of file-local
variables: `vulpea`'s parse of any file SHALL run with
`enable-local-variables` bound to `nil` (or an equivalent that neither
evaluates `eval:` entries nor prompts), and a parse error in one file
SHALL be logged and skipped rather than entering the debugger from the
sync timer.

#### Scenario: eval local variable is not evaluated during indexing
- **WHEN** a file under the vault root ends with a Local Variables block
  containing `eval: (some-undefined-function)`
- **THEN** a full scan completes without error or prompt and the file is
  indexed (or skipped with a logged message), and the function is never
  called

### Requirement: Edge-writer API

The system SHALL provide a function that appends a typed-edge item
`- <type> :: [[id:<target>]]` to the `EDGES` drawer attached to a given
node (file-level or heading), creating the drawer directly after that
node's `:PROPERTIES:` drawer when absent, under the coordinator's file
lock, without disturbing other content. The call SHALL be idempotent for
an identical `(node, type, target)` triple and SHALL accept a file path
plus optional node ID (defaulting to the file-level node).

#### Scenario: Edge appended to a file without an EDGES drawer
- **WHEN** the writer is called for file `F` (file-level node), type
  `part-of`, target `P`, and `F` has a property drawer but no `EDGES`
  drawer
- **THEN** `F` gains an `EDGES` drawer immediately after its property
  drawer containing `- part-of :: [[id:P]]`
- **AND** the rest of `F` is byte-identical

#### Scenario: Duplicate edge is not appended twice
- **WHEN** the writer is called twice with the same triple
- **THEN** the drawer contains the item once

### Requirement: Project note creation API

The system SHALL provide a function that creates a vault note of type
`project` from a title (and optional body), stamping the `project`
filetag, `status: active`, a file-level `:ID:`, and an empty
connected-notes dynamic block, and returns the note's ID. It SHALL place
the note through the default placement rules and index it immediately.

#### Scenario: Project note is created and indexed
- **WHEN** the function is called with title `myproj`
- **THEN** a file under the vault root exists with `#+title: myproj`,
  `#+filetags: :project:`, `status` `active`, an `:ID:`, and a
  connected-notes block
- **AND** the `project` finder offers `myproj` without a manual sync

### Requirement: Connected-notes dynamic block

The system SHALL provide an org dynamic block (`#+BEGIN: org-graph-connected`)
whose parameters name a note ID (defaulting to the enclosing file-level
node) and optional relation types. Updating the block SHALL render the
notes with typed edges into the node, grouped by the target note's
note-type (schema predicate), each as an `id:` link with its title, and
for any note that itself has `branched-from` edges SHALL render those as
an indented lineage under it. The block content is regenerated on
`org-dblock-update` and SHALL be safe to regenerate repeatedly.

#### Scenario: Block lists incoming notes grouped by type
- **WHEN** notes T (type `topic`) and S (type `session`) have `part-of`
  edges to P and the block in P is updated
- **THEN** P's block contains a `topic` group with a link to T and a
  `session` group with a link to S

#### Scenario: Branch lineage is rendered under its session
- **WHEN** session S2 has `branched-from` S1 and both have `part-of` P
- **THEN** P's block shows S2 indented under S1 within the `session` group

### Requirement: Package independence

Code under `config/org-graph/` SHALL NOT name any `workspace-` or
`jf/gptel-` symbol. Registration of gptel *tools* (upstream `gptel`
tool API) SHALL be guarded so that org-graph loads fully when `gptel` is
absent. Anything that binds graph behaviour to a workspace or a session
belongs to the integration layer (`workspace-graph-integration`).

#### Scenario: org-graph loads without workspaces or gptel
- **WHEN** Emacs starts with org-graph enabled and neither `workspaces`
  nor `gptel` loaded
- **THEN** org-graph loads, indexes the vault, and its finders work

## MODIFIED Requirements

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
   `from-id` is the nearest ancestor node carrying an `:ID:` (the
   enclosing heading, else the file-level node). A `rel:` link with no
   ID-bearing ancestor SHALL be dropped, not attributed to an unrelated
   note.

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

Typed-edge extraction SHALL run on every indexed note, i.e. every
ID-bearing node under the vault root; there is no narrower extraction
root. Session files and ID-bearing headings inside them therefore
contribute edges like any other note.

#### Scenario: Edge-drawer item creates one edge row
- **WHEN** a note's `EDGES` drawer contains `- implements :: [[id:abc]]`
  and the extractor runs
- **THEN** the `typed_edges` index contains exactly one row with
  `from-id = <note-id>`, `rel-type = implements`, `to-id = abc`

#### Scenario: Novel unregistered relation type extracts with no configuration
- **WHEN** a note's `EDGES` drawer contains `- falsifies :: [[id:abc]]` and
  no `falsifies` edge-type registry note or configuration entry exists
- **THEN** the `typed_edges` index contains a row with
  `rel-type = falsifies`

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

#### Scenario: Session heading contributes an edge
- **WHEN** a session file under `~/org/sessions/` contains a heading with
  `:ID: H` whose body holds `[[rel:decides:T][we chose T]]`
- **THEN** the `typed_edges` index contains `(H, decides, T)`

### Requirement: Agent-Facing Graph Tools

The system SHALL register gptel tools that expose the graph to AI agents:
- A read tool returning notes matching a structured query (note type,
  typed-edge predicates, title match).
- A read tool returning typed edges incident to a given note ID, with
  target titles resolved.
- A write tool creating a note with a deterministic ID, taxonomy
  membership, and optional initial typed-edge properties.

The tools SHALL be exposed in the global gptel tool registry. Attaching
them to any preset (for example a per-workspace assistant) is the
responsibility of whoever owns that preset, not of org-graph.

All write operations from agent tools SHALL be serialized through a single
coordinator so that two concurrent tool calls writing to the same file
cannot interleave or corrupt each other. Writes to distinct files MAY
proceed in parallel.

Agent-authored notes SHALL receive an `:agent-draft:` filetag by default
and SHALL be placed under a configurable agent-draft directory within the
vault root (`org-graph-agent-notes-directory`, default `~/org/roam/`).
Type-specific finders MAY exclude `:agent-draft:` notes by default; a
dedicated review finder SHALL include them.

Tool descriptions SHALL state that notes anywhere under the vault root
participate in typed-edge extraction and that writes outside the vault
root are refused.

#### Scenario: Concurrent agent writes to same file are serialized
- **WHEN** two agent tool calls write to the same target file path within
  the same instant
- **THEN** the writes apply sequentially under the coordinator's lock and
  the final file content reflects both writes

#### Scenario: Agent-authored note carries draft tag
- **WHEN** an AI agent creates a new note via the write tool without
  explicitly clearing the draft tag
- **THEN** the resulting file carries `:agent-draft:` in its filetags and
  is excluded from the default topic finder

#### Scenario: Graph query returns typed edges
- **WHEN** an agent calls the typed-edges read tool with a note ID
- **THEN** the response is a structured list of `{from, rel-type, to,
  to-title}` objects

#### Scenario: Write outside the vault is refused
- **WHEN** an agent calls the write tool with a directory outside the
  vault root
- **THEN** the tool returns an error and writes nothing

## REMOVED Requirements

### Requirement: Workspace-Substrate Discovery

**Reason**: Indexing workspace homes recursively swept up third-party org
files (and evaluated their local variables); notes no longer live in
homes. Replaced by Vault Discovery.

**Migration**: `org-graph-watch-workspace-homes`, the workspace-home root
enumeration, and the home-index `:on-create` handler are deleted. The
integration layer's migration command relocates any notes that lived in
homes.

### Requirement: Workspace Integration

**Reason**: Violates package independence. Project-note creation, tool
injection into the workspace-assistant preset, and workspace menu
entries move to `workspace-graph-integration`.

**Migration**: `config/org-graph/workspace-integration.org` is deleted; its
behaviours reappear in the integration layer against org-graph's public
APIs.

### Requirement: Indexable Workspace Notes

**Reason**: `home.org` no longer exists; session-file IDs are the sessions
package's own requirement (Session creation) and need no org-graph
involvement.

**Migration**: None.
