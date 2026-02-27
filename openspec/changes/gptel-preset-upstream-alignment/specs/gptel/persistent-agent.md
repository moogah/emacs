## MODIFIED Requirements

### Requirement: Agent session creation

The system SHALL create agent sessions under the parent's branch directory with their own metadata, scope configuration, and conversation history.

Agent directory structure SHALL follow:
```
<parent-session-dir>/branches/<parent-branch-name>/agents/<preset>-<timestamp>-<slug>/
├── session.md           (agent conversation history)
├── scope.yml            (agent scope configuration)
├── metadata.yml         (agent metadata including type: "agent" and parent_session_id)
└── tools.org            (agent tool execution log)
```

Agent sessions SHALL NOT have:
- `branches/` subdirectory (agents don't support branching)
- `current` symlink (no branching means no active branch tracking)
- `preset.md` (configuration comes from `gptel--known-presets` by name)

#### Scenario: Agent directory created under parent branch
- **WHEN** creating a new agent with preset "researcher" and description "analyze code"
- **THEN** the system creates directory: `<parent-branch-dir>/agents/researcher-<timestamp>-analyze-code/`
- **AND** the directory is under the parent's **current branch** directory, not session root

#### Scenario: scope.yml created with agent scope
- **WHEN** creating an agent with allowed_paths ["/path/to/project/**"]
- **THEN** the system creates `scope.yml` in the agent directory with:
```yaml
paths:
  read:
    - "/path/to/project/**"
  write:
    - "/tmp/**"
  deny:
    - "**/.git/**"
    - "**/runtime/**"
    - "**/.env"
    - "**/node_modules/**"
```

#### Scenario: Empty scope when paths not specified
- **WHEN** creating an agent without specifying allowed_paths (or with empty array)
- **THEN** the system creates `scope.yml` with empty paths.read section
- **AND** agent has no read permissions (cannot read any files)
- **AND** paths are never inherited from parent session

#### Scenario: metadata.yml created with agent metadata
- **WHEN** creating an agent session
- **THEN** the system writes `metadata.yml` with:
  - version: "3.0"
  - session_id: "<agent-session-id>"
  - created: "<ISO8601 timestamp>"
  - updated: "<ISO8601 timestamp>"
  - type: "agent"
  - parent_session_id: "<parent-session-id>"
  - preset: "<preset-name>"

#### Scenario: No preset.md in agent directory
- **WHEN** creating an agent session
- **THEN** the system SHALL NOT copy a preset.md file to the agent directory
- **AND** agent configuration comes entirely from the registered preset in `gptel--known-presets`

### Requirement: Configuration isolation

The system SHALL enforce zero inheritance - agent configuration comes ONLY from the named preset registered in `gptel--known-presets`, never from parent session state.

Configuration loading SHALL:
1. Create agent directory with scope.yml and metadata.yml
2. Set buffer-local session variables BEFORE applying preset
3. Apply preset by name via `gptel--apply-preset` with buffer-local setter
4. Capture tools from agent buffer context AFTER preset application
5. Use captured tools for gptel-request (not parent's tools)

#### Scenario: Preset applied by name from registry
- **WHEN** initializing an agent buffer with preset "executor"
- **THEN** the system calls `(gptel--apply-preset 'executor setter)` with a buffer-local setter
- **AND** the preset is looked up from `gptel--known-presets`
- **AND** does NOT read from any file on disk

#### Scenario: Buffer-local session variables set before preset
- **WHEN** initializing an agent buffer
- **THEN** the system sets buffer-local variables in this order:
  1. jf/gptel--session-id (agent's session ID)
  2. jf/gptel--session-dir (agent's directory path)
  3. jf/gptel--branch-name ("main")
  4. jf/gptel--branch-dir (same as session-dir, agents don't branch)
  5. Apply preset by name via gptel--apply-preset
- **AND** session variables persist outside preset's dynamic scope

#### Scenario: Tools captured from agent buffer context
- **WHEN** setting up gptel-request for the agent
- **THEN** the system captures gptel-tools INSIDE the agent buffer's with-current-buffer
- **AND** uses the captured tools list for gptel-request

#### Scenario: Backend, model, system message from registered preset
- **WHEN** agent preset "executor" is registered with:
  - :backend "Claude"
  - :model 'claude-sonnet-4-5
  - :temperature 0.3
- **THEN** the agent buffer uses those exact settings via `gptel--apply-preset`
- **AND** does NOT inherit parent's backend/model/temperature

#### Scenario: Parent configuration never leaked
- **WHEN** parent session uses different backend, model, tools, or system message
- **THEN** the agent SHALL NOT inherit any of those settings
- **AND** agent configuration comes exclusively from the named preset

#### Scenario: Request uses buffer-local settings (not gptel-with-preset)
- **WHEN** executing the agent request via `gptel-request`
- **THEN** the system sends the request from within the agent buffer (via `with-current-buffer`)
- **AND** `gptel-request` reads buffer-local `gptel-backend`, `gptel-model`, `gptel-tools`, etc. set by `gptel--apply-preset` during initialization
- **AND** does NOT use `gptel-with-preset` (which creates temporary dynamic scope — unnecessary since buffer-local settings are already correct)
- **AND** passes `:use-tools t` and `:include-tool-results t` to `gptel-request`

**Rationale:** `gptel--apply-preset` with buffer-local setter sets all configuration as buffer-local variables. Since `gptel-request` is called from within the agent buffer, it naturally picks up these settings. `gptel-with-preset` would be redundant and could mask issues where buffer-local state drifts from expectations.

### Requirement: Integration with sessions subsystem

The PersistentAgent tool SHALL use the sessions filesystem, metadata, and registry modules for directory management and session tracking.

#### Scenario: Agent directory created via filesystem module
- **WHEN** creating an agent session
- **THEN** the system calls jf/gptel--create-agent-directory from filesystem module
- **AND** passes parent-branch-dir, preset, and description
- **AND** returns agent directory path

#### Scenario: scope.yml written directly
- **WHEN** initializing agent scope
- **THEN** the system writes `scope.yml` with path controls
- **AND** does NOT copy a preset template file

#### Scenario: Session registered via registry module
- **WHEN** agent buffer is initialized
- **THEN** the system calls jf/gptel--register-session
- **AND** creates hash table entry with key `"<session-id>/main"`

#### Scenario: Path resolution via constants
- **WHEN** constructing file paths for agent session
- **THEN** the system uses constants from gptel-session-constants module
- **AND** calls helpers like jf/gptel--context-file-path, jf/gptel--scope-file-path, jf/gptel--metadata-file-path

### Requirement: Integration with scope subsystem

The PersistentAgent tool SHALL integrate with the scope system through scope.yml-based configuration.

#### Scenario: Agent scope loaded from scope.yml
- **WHEN** agent buffer is initialized and tools execute
- **THEN** the scope system reads `scope.yml` from the agent's directory
- **AND** validates agent tool calls against those patterns

#### Scenario: scope.yml created with path controls
- **WHEN** creating agent with allowed_paths ["/project/**"]
- **THEN** the system writes `scope.yml` with the paths section
- **AND** includes allowed (read), write defaults, and deny defaults

#### Scenario: Agent tools validated against agent scope
- **WHEN** agent executes a tool like Read or Edit
- **THEN** the scope system checks buffer-local jf/gptel--branch-dir
- **AND** loads scope config from agent's `scope.yml` (not parent's)

### Requirement: Persistence and resumption

The system SHALL auto-save the agent buffer after every API response, preserving full conversation history including tool results.

#### Scenario: Session resumable via find-file
- **WHEN** user opens `<agent-dir>/session.md` via find-file
- **THEN** the auto-initialization hook fires (see `sessions-persistence.md` Open lifecycle — agent pattern)
- **AND** matches the `*/agents/*/session.md` path pattern
- **AND** sets `jf/gptel--session-dir` and `jf/gptel--branch-dir` to the agent directory (flat structure, no branches)
- **AND** sets `jf/gptel--branch-name` to `"main"`
- **AND** detects session state via `(local-variable-p 'gptel--preset)`
- **AND** upstream's `gptel--restore-state` applies preset from Local Variables
- **AND** scope system loads `scope.yml` from agent directory

### Requirement: Error handling and recovery

#### Scenario: Preset not found in registry
- **WHEN** agent creation specifies a preset name that is not in `gptel--known-presets`
- **THEN** the system raises error: "Preset '%s' not found in gptel--known-presets"
- **AND** does NOT silently fall back to defaults
- **AND** does NOT create partial agent directory

## REMOVED Requirements

### Requirement: Preset template copied to agent directory
**Reason**: Agent configuration now comes from `gptel--known-presets` (applied by name via `gptel--apply-preset`), not from a copied `preset.md` file. Upstream's save/restore handles persistence via Local Variables.
**Migration**: Remove `jf/gptel--copy-preset-template` call from agent creation. Replace `jf/gptel--load-preset-from-file` + `jf/gptel--apply-session-preset` with `(gptel--apply-preset preset-name setter)`.

### Requirement: Agent inherits parent allowed paths via preset.md
**Reason**: Path inheritance via preset.md manipulation is replaced by direct `scope.yml` creation with explicit paths. Zero-inheritance principle is preserved — paths come from the PersistentAgent tool's `allowed_paths` parameter, not from parent.
**Migration**: Replace `jf/gptel-scope--update-preset-paths` calls with direct `scope.yml` writing.
