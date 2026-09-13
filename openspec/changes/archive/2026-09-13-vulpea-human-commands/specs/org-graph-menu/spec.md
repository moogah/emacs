# org-graph-menu capability

A single Transient menu, bound to `SPC v` in evil normal state, that
aggregates the full human-side interaction surface for the graph. The
menu is the discoverable front door: everything a human does with the
graph — find, create, link, inspect edges, maintain the index — is
reachable from it without memorizing `M-x` names.

## ADDED Requirements

### Requirement: Graph Menu Prefix

The system SHALL provide a Transient prefix command for the graph whose
entries cover, at minimum, four groups:

- **Find** — the per-type finders (`topic`, `debug`, `log`, `reference`,
  `project`), the catch-all any-note finder, and the agent-drafts finder.
- **Author** — the find-or-create command and the insert-link command
  (see `org-graph-note-commands`).
- **Edges** — typed-edge queries for the note at point: outgoing,
  incoming, and connected.
- **Maintain** — re-index (`org-graph/configure-sync`), note-type
  validation for the note at hand, and `vulpea-doctor`.

Each menu entry SHALL dispatch to the same command that is available via
`M-x`; the menu adds discoverability, not divergent behavior.

#### Scenario: Menu exposes the full interaction surface

- **WHEN** the user invokes the graph menu
- **THEN** the transient displays the Find, Author, Edges, and Maintain
  groups with entries for each command listed above

#### Scenario: Menu entry behaves identically to the command

- **WHEN** the user invokes a command via its menu entry
- **THEN** the behavior is identical to invoking the same command via
  `M-x` (same prompts, same results)

### Requirement: Leader Binding

The system SHALL bind the graph menu to `SPC v` in evil normal state,
globally. The binding SHALL be installed only when `evil` is available;
loading org-graph without evil SHALL NOT error and SHALL leave the menu
reachable via `M-x`.

#### Scenario: SPC v opens the menu

- **WHEN** the user presses `SPC v` in a buffer in evil normal state
- **THEN** the graph menu transient opens

#### Scenario: Absent evil degrades gracefully

- **WHEN** org-graph loads in a session where `evil` is not loaded
- **THEN** module load completes without error and the menu prefix command
  remains invocable via `M-x`

### Requirement: Edge Queries Resolve the Note at Point

Menu edge-query entries SHALL resolve their subject note from point: the
ID of the enclosing ID-bearing heading, else the file-level ID. When no
ID-bearing node encloses point, the entry SHALL signal a clear
`user-error` naming the problem, not fail silently or query an empty ID.
Query results SHALL be presented to the user in a readable form
(presentation format is a design decision).

#### Scenario: Edge query on a note at point returns its edges

- **WHEN** the user invokes an edge query from the menu with point inside
  an ID-bearing note
- **THEN** the typed edges for that note (outgoing, incoming, or connected,
  per the chosen entry) are displayed

#### Scenario: Edge query outside any node reports a clear error

- **WHEN** the user invokes an edge query from the menu with point in a
  buffer or location with no enclosing `:ID:`
- **THEN** a `user-error` clearly states that no note was found at point
