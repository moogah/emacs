## ADDED Requirements

### Requirement: gptel Tool Registry as the Agent API Source

The system SHALL use gptel's tool registry as the single source of agent-callable
operations. A module that offers an operation to agents SHALL define it once, as a
gptel tool (`gptel-make-tool`), and that definition SHALL serve both in-Emacs gptel
sessions and external agents. The system SHALL NOT maintain a separate registry of
agent operations.

Access to gptel's internal registry SHALL be confined to a single function, so a
change in gptel's internals affects one place.

#### Scenario: A module's gptel tool is callable by an external agent

- **WHEN** a module has defined a gptel tool and that tool is allowlisted by an
  agent preset
- **THEN** an external agent can invoke it through the dispatcher without the module
  registering it anywhere else

### Requirement: Presets as Allowlists

The set of tools exposed to external agents SHALL be defined by two gptel presets:

- `emacs-agent-read` — tools that do not modify user data or Emacs state;
- `emacs-agent-write` — tools that modify user data through the owning module's API.

Only tools named in one of these presets SHALL be callable through the `call` /
`call-write` tiers (see `agent-target-policy`). The initial presets SHALL be:

- `emacs-agent-read`: `org_graph_query`, `org_graph_typed_edges`,
  `search_roam_nodes`, `list_roam_nodes`, `read_roam_node`,
  `get_roam_node_metadata`, `query_roam_backlinks`, and the introspection tools
  (see `agent-introspection-tools`);
- `emacs-agent-write`: `org_graph_write_node`, `workspace_add_layout`,
  `workspace_remove_layout`.

Tools that require a gptel session buffer (scope-wrapped tools) and tools that open
interactive UI SHALL NOT be added to either preset.

#### Scenario: Preset membership decides exposure

- **WHEN** a tool is registered with gptel but named in neither preset
- **THEN** it does not appear in the manifest and cannot be invoked through `call`
  or `call-write`

#### Scenario: A preset tool missing from the registry

- **WHEN** a preset names a tool that is not currently registered (for example
  because its module is not loaded)
- **THEN** the manifest omits it and invoking it returns an error naming the missing
  tool

### Requirement: Dispatcher Invokes Tools as gptel Does

The system SHALL provide an in-Emacs dispatcher that invokes a tool by name with
arguments supplied as a JSON object, mapping them to the tool function exactly as
gptel does: values are taken by argument name and passed positionally in the order
of the tool's declared arguments, missing optional arguments are passed as nil, JSON
arrays arrive as vectors, and JSON `false` arrives as `:json-false`. The dispatcher
SHALL return a JSON envelope indicating success with the tool's result as a string,
or failure with an error message. An error inside the tool SHALL be reported in the
envelope and SHALL NOT leave the target unusable.

#### Scenario: Arguments are mapped positionally by name

- **WHEN** an agent invokes a tool declaring arguments `(a b c)` with the JSON object
  `{"c": 3, "a": 1}`
- **THEN** the tool function is called with `1`, `nil`, `3`

#### Scenario: Tool error is contained

- **WHEN** a tool function signals an error during a dispatched call
- **THEN** the dispatcher returns a failure envelope carrying the error message, and
  subsequent calls to the same target succeed

### Requirement: Self-Describing Manifest

The system SHALL provide a manifest, generated at call time, listing every tool
callable through the presets on that target. Each entry SHALL include the tool's
name, category, tier (`read` or `write`), description, and typed argument schema
(name, type, optional flag, description). Because it is generated from the presets
and the live registry, the manifest SHALL NOT be maintained as a separate list.

#### Scenario: Agent discovers the callable tools

- **WHEN** an agent requests the manifest from a full-configuration target
- **THEN** it receives a JSON list of the preset-allowlisted tools with their tiers
  and argument schemas

#### Scenario: Manifest follows the presets

- **WHEN** a tool is added to the `emacs-agent-read` preset and the preset is
  reloaded
- **THEN** the next manifest includes that tool with tier `read`, without any other
  list being edited

#### Scenario: Bare target has no manifest

- **WHEN** an agent requests the manifest from a bare (`-Q`) target, which has no
  gptel loaded
- **THEN** it receives an empty list or an explanatory error, and raw `eval` remains
  available on that target

### Requirement: Minimal Static Bootstrap Seed

The system SHALL provide a minimal static entry point — a Claude Code skill and a
`CLAUDE.md` pointer — that tells an agent how to begin: the `bin/emacs-agent`
wrapper, the target-selection guidance and permission tiers (see
`agent-target-policy`), and how to request the manifest. The seed SHALL NOT
enumerate individual tools; enumeration is the manifest's responsibility.

#### Scenario: Agent learns the entry point from static context

- **WHEN** an agent starts a task that involves driving Emacs and consults its
  loaded context
- **THEN** it learns to use `bin/emacs-agent`, how to choose a target, and how to
  list tools at runtime

#### Scenario: Seed does not change when tools change

- **WHEN** a tool is added to or removed from a preset
- **THEN** no change to the static seed is needed for agents to discover it
