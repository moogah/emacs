# workspace-graph-integration capability

The integration layer between three independent packages: `workspaces`,
gptel sessions, and `org-graph`. It is the only code permitted to name
symbols from two of them. Each package publishes an extension point
(workspaces: the integration registry and two inversion variables; gptel
sessions: two lifecycle hooks; org-graph: the query, edge-writer, and
project-note APIs) and this layer subscribes. Remove the layer and each
package still loads and works alone, with only the cross-package
behaviours described here absent.

## ADDED Requirements

### Requirement: Package independence boundary

The integration layer SHALL be the sole module that references symbols
from more than one of `workspaces`, gptel sessions
(`config/gptel/sessions/`), and `org-graph`. Code under
`config/workspaces/` SHALL NOT name any gptel or org-graph symbol; code
under `config/gptel/sessions/` SHALL NOT name any workspaces or org-graph
symbol; code under `config/org-graph/` SHALL NOT name any workspaces or
gptel-sessions symbol. The integration layer SHALL load after all three
and SHALL degrade to a no-op for any package that is absent, guarded by
feature checks rather than hard requires.

A lint test SHALL enforce the boundary by grepping each package's tangled
`.el` files for the other packages' symbol prefixes.

#### Scenario: Packages load without the integration layer
- **WHEN** Emacs starts with `workspaces`, gptel sessions, and `org-graph`
  enabled but the integration layer disabled
- **THEN** all three load without error, workspace creation, session
  creation, and graph queries each work in isolation
- **AND** no project note is created, no edge is written, and the tab
  label is the registry name

#### Scenario: Cross-package symbol reference fails the lint
- **WHEN** a function under `config/workspaces/` references a symbol whose
  prefix is `jf/gptel-` or `org-graph`
- **THEN** the boundary lint test fails naming the file and symbol

### Requirement: Project note as workspace identity

On workspace creation dispatch the integration layer SHALL create a
**project note** in the vault via org-graph's project-note API: a
file-level note tagged `project`, titled with the workspace's registry
name, with `status` `active`. It SHALL record the note's org ID on the
workspace as its `:node-id` through the workspaces API and SHALL run
non-interactively. If the payload already carries a `:node-id` that
resolves to an existing note, the handler SHALL skip creation.

The project note is the workspace's canonical, machine-independent
identity: it lives in the synced vault, while the per-machine registry
maps the node ID to a local `:home`.

#### Scenario: Fresh workspace gets a project note
- **WHEN** the user creates workspace `myproj` and creation dispatch runs
- **THEN** a note titled `myproj` tagged `project` with `status: active`
  exists under the vault root
- **AND** the `myproj` registry entry's `:node-id` equals that note's ID
- **AND** nothing is written under the workspace `:home`

#### Scenario: Re-anchoring an existing project note does not duplicate it
- **WHEN** a workspace is created on a second machine and the user
  supplies the existing project note's ID (or the payload carries it)
- **THEN** no new project note is created and `:node-id` points at the
  existing note

### Requirement: Display name and home layout come from the project note

The integration layer SHALL install a display-name function into
workspaces that returns the project note's title (read from the graph
index) when `:node-id` resolves, else the registry name. It SHALL install
a home builder that opens the project note's file in a single window,
falling back to the workspaces default when the note cannot be resolved.

#### Scenario: Tab label follows the note title
- **WHEN** the project note for `myproj` is retitled `My Cool Project`
  and re-indexed
- **THEN** the `myproj` tab-bar label reads `My Cool Project`
- **AND** the registry name is still `myproj`

#### Scenario: Home layout opens the project note
- **WHEN** a new workspace's home layout is built with the integration
  layer loaded
- **THEN** the single window shows the project note's buffer
- **AND** that buffer is a member of the workspace

### Requirement: Session association via typed edges

The integration layer SHALL subscribe to the gptel session-created hook
and, when a workspace owns the current tab (or the hook payload names a
`:node-id`), SHALL write a `part-of` edge from the new session file's
file-level node to the workspace's project note using org-graph's
edge-writer API. It SHALL subscribe to the gptel branch-created hook and
write a `branched-from` edge from the new branch's file-level node to the
parent branch's file-level node. Sessions created with no active
workspace SHALL receive no `part-of` edge. Edge writes SHALL be placed so
that a branch's verbatim drawer copy carries the `part-of` edge forward.

The layer SHALL register a `session` note-type schema with org-graph
whose predicate is the `gptel_session` filetag gptel stamps, so sessions
are a first-class note type in finders and the connected-notes block.

#### Scenario: Session created on a workspace tab is linked to the project
- **WHEN** workspace `myproj` owns the current tab and the user creates a
  gptel session
- **THEN** after the next index update the `typed_edges` index contains a
  row `(<session-file-id>, part-of, <myproj-node-id>)`

#### Scenario: Branch inherits project association and records lineage
- **WHEN** the user branches a session that carries a `part-of` edge
- **THEN** the new branch's file-level node has its own `part-of` row to
  the same project note
- **AND** a `branched-from` row from the new branch's node to the parent
  branch's node

#### Scenario: Off-workspace session has no project edge
- **WHEN** the current tab is not a workspace and the user creates a session
- **THEN** no `part-of` row is written for that session

### Requirement: Birth-time session and workspace menu entries

The integration layer SHALL register a workspace integration providing
the `:on-create` handler for the birth session (a session created with
the workspace's `:home` as `project-root` for scope expansion, using the
configured workspace-initial preset) and `:menu` entries for adding a
session, finding notes in the workspace, and creating a note in the
workspace. The birth session SHALL be created through gptel's public
session-creation API and SHALL therefore trigger the session-created hook,
so it receives its `part-of` edge like any other session.

#### Scenario: Birth session lands in the sessions root with a project edge
- **WHEN** a fresh workspace is created with the integration layer loaded
- **THEN** a session exists under the gptel sessions root whose drawer
  carries `GPTEL_WORK_ROOT` equal to the workspace `:home`
- **AND** the session's file-level node has a `part-of` edge to the
  project note
- **AND** no `sessions/` directory exists under `:home`

### Requirement: Workspace-scoped note commands

The integration layer SHALL provide `find-in-workspace`, which offers
completion over exactly the notes with a `part-of` edge (direct or via
heading-level inheritance of the file's node) to the active workspace's
project note, including session notes and ID-bearing topic headings
inside sessions; and `new-in-workspace`, which creates a vault note via
org-graph's find-or-create path and writes a `part-of` edge to the active
workspace's project note. Both SHALL signal a `user-error` when no
workspace owns the current tab. Association SHALL be derived from the
edge index only; no per-workspace filetag is required.

#### Scenario: Finder shows only this workspace's notes
- **WHEN** notes A and B have `part-of` edges to `myproj`, note C has a
  `part-of` edge to `other`, and the user invokes `find-in-workspace` on
  the `myproj` tab
- **THEN** completion offers A and B and not C

#### Scenario: ID'd topic heading inside a session is findable
- **WHEN** a session with a `part-of` edge to `myproj` contains a heading
  carrying its own `:ID:` and title `Retry strategy`
- **THEN** `find-in-workspace` on the `myproj` tab offers `Retry strategy`
  and selecting it visits that heading

#### Scenario: New note is linked on creation
- **WHEN** the user invokes `new-in-workspace` on the `myproj` tab with
  title `Design notes`
- **THEN** a vault note titled `Design notes` is created
- **AND** its `EDGES` drawer contains `- part-of :: [[id:<myproj-node-id>]]`

### Requirement: Virtual home via the connected-notes block

The project note created by the integration layer SHALL contain an
org-graph connected-notes dynamic block so the note itself serves as the
workspace's home view: regenerating the block lists the notes and
sessions with edges to the project note, grouped by note type, and the
branch lineage of sessions. The integration layer SHALL regenerate the
block when the home layout is built.

#### Scenario: Home view lists sessions and notes
- **WHEN** workspace `myproj` has two sessions and one topic note with
  `part-of` edges and the user builds its home layout
- **THEN** the project note's dynamic block lists the topic note under
  its type and both sessions under `session`, each as an `id:` link

### Requirement: Migration of co-located workspaces

The integration layer SHALL provide a one-shot interactive migration
command that, for each registered workspace lacking `:node-id`: creates
the project note (carrying over the `#+TITLE:` and body of
`<home>/home.org` when present), records `:node-id`, moves every session
under `<home>/sessions/` into the gptel sessions root, writes `part-of`
edges for the moved sessions, and removes `home.org` and the empty
`sessions/` directory from `:home`. It SHALL also relocate sessions from
any previous sessions root into the configured one and rename each
`agents/` directory to `.agents/`. The command SHALL report every file
moved and SHALL be idempotent.

#### Scenario: Legacy workspace is migrated
- **WHEN** workspace `emacs-mcp` has `home.org`, one session under
  `sessions/`, and no `:node-id`, and the user runs the migration command
- **THEN** a project note titled from `home.org` exists, `:node-id` is
  set, the session now lives under the sessions root with a `part-of`
  edge, and `:home` contains neither `home.org` nor `sessions/`

### Requirement: Sessions root within the vault

The configuration SHALL point gptel's sessions root at a non-hidden
directory under the org-graph vault root (default `~/org/sessions/`) so
that session files are indexed by the vault-only discovery. The
integration layer SHALL verify this at load and warn when the sessions
root lies outside the vault root, since edges from sessions would then
never be extracted.

#### Scenario: Session file appears in the index
- **WHEN** a session is created and saved under the configured root
- **THEN** within a few seconds it is present in the vulpea index with
  its stamped title

#### Scenario: Misconfigured root warns at load
- **WHEN** the sessions root is `~/.gptel/sessions/` and the integration
  layer loads
- **THEN** a warning names both paths and states that sessions will not
  be indexed
