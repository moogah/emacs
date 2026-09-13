# org-graph-menu capability — delta for workspace-notes-in-vault

The menu gains a Workspace group whose entries are provided by the
integration layer and are hidden when it is not loaded.

## MODIFIED Requirements

### Requirement: Graph Menu Prefix

The system SHALL provide a Transient prefix command for the graph whose
entries cover, at minimum, four groups:

- **Find** — the per-type finders (`topic`, `debug`, `log`, `reference`,
  `project`, `session`), the catch-all any-note finder, and the
  agent-drafts finder.
- **Author** — the find-or-create command and the insert-link command
  (see `org-graph-note-commands`).
- **Edges** — typed-edge queries for the note at point: outgoing,
  incoming, and connected.
- **Maintain** — re-index (`org-graph/configure-sync`), note-type
  validation for the note at hand, `vulpea-doctor`, and update of the
  connected-notes dynamic block at point.

The menu SHALL additionally expose a **Workspace** group whose entries
(`find-in-workspace`, `new-in-workspace`) are contributed by the
integration layer through a published extension variable rather than
named by org-graph; the group SHALL be absent when no contributions are
registered.

Each menu entry SHALL dispatch to the same command that is available via
`M-x`; the menu adds discoverability, not divergent behavior.

#### Scenario: Menu exposes the full interaction surface
- **WHEN** the user invokes the graph menu with the integration layer
  loaded
- **THEN** the transient displays the Find, Author, Edges, Maintain, and
  Workspace groups with entries for each command listed above

#### Scenario: Workspace group is absent without the integration layer
- **WHEN** the user invokes the graph menu and no workspace entries have
  been contributed
- **THEN** the transient displays Find, Author, Edges, and Maintain only

#### Scenario: Menu entry behaves identically to the command
- **WHEN** the user invokes a command via its menu entry
- **THEN** the behavior is identical to invoking the same command via
  `M-x` (same prompts, same results)
