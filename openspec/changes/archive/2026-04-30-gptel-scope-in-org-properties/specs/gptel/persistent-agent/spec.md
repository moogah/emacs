# PersistentAgent Tool (Delta Spec)

This delta moves agent scope configuration from a `scope.yml` sidecar to the agent's `session.org` `:PROPERTIES:` drawer. Configuration isolation (zero inheritance) and the explicit-paths-only contract are preserved verbatim — only the storage medium changes.

## MODIFIED Requirements

### Requirement: Agent session creation

The system SHALL create agent sessions under parent's branch directory with isolated configuration. The agent's `session.org` is written with a `:PROPERTIES:` drawer at `point-min` containing `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, and the agent's scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`). No `scope.yml` is written.

**Implementation**: `config/gptel/tools/persistent-agent.org`

#### Scenario: Agent directory created
- **WHEN** creating agent with preset "researcher" and description "analyze code"
- **THEN** creates `<parent-branch-dir>/agents/researcher-<timestamp>-analyze-code/`
- **AND** directory under parent's current branch, not session root

#### Scenario: Drawer with explicit paths only
- **WHEN** creating agent with `allowed_paths ["/path/to/project/**"]`
- **THEN** writes `session.org` with drawer:
```org
:PROPERTIES:
:GPTEL_PRESET: researcher
:GPTEL_PARENT_SESSION_ID: <parent-id>
:GPTEL_SCOPE_READ: /path/to/project/**
:GPTEL_SCOPE_WRITE: /tmp/**
:GPTEL_SCOPE_DENY: **/.git/**
:GPTEL_SCOPE_DENY+: **/runtime/**
:GPTEL_SCOPE_DENY+: **/.env
:GPTEL_SCOPE_DENY+: **/node_modules/**
:END:
#+begin_user

#+end_user
```

#### Scenario: Empty paths when not specified (zero inheritance)
- **WHEN** creating agent without `allowed_paths`
- **THEN** the drawer omits `:GPTEL_SCOPE_READ:` entirely (the validator will load `:read nil`, i.e. no read permissions)
- **AND** writes the standard write/deny defaults
- **AND** paths are never inherited from parent (zero inheritance)

#### Scenario: metadata.yml created
- **WHEN** creating agent
- **THEN** writes metadata.yml with `type: "agent"`, `parent_session_id`
- **AND** includes `session_id`, `created`, `updated`, `preset`

#### Scenario: No branching support
- **WHEN** creating agent
- **THEN** no `branches/` subdirectory created
- **AND** no `current` symlink
- **AND** agents are single-timeline only

#### Scenario: No scope.yml written
- **WHEN** creating any agent
- **THEN** no `scope.yml` exists in the agent directory at any point in the lifecycle
- **AND** the agent directory contains only `session.org`, `metadata.yml`, and (later) `tools.org`

### Requirement: Configuration isolation (zero inheritance)

The system SHALL enforce zero inheritance — agent configuration ONLY from the agent's own `session.org` drawer and preset definition.

**Buffer-local vars setup**:
1. Set `jf/gptel--session-id` (agent's ID)
2. Set `jf/gptel--session-dir` (agent's directory)
3. Set `jf/gptel--branch-name` to "main"
4. Set `jf/gptel--branch-dir` (same as session-dir)
5. Apply preset configuration (dynamically scoped)

Variables persist OUTSIDE preset's dynamic scope.

#### Scenario: Path configuration never inherited
- **WHEN** parent has scope keys in its `session.org` drawer
- **AND** agent created without `allowed_paths`
- **THEN** the agent's drawer omits `:GPTEL_SCOPE_READ:` (empty read paths)
- **AND** does NOT inherit parent's drawer scope keys
- **AND** agent only accesses files explicitly in `allowed_paths` parameter (i.e. only those that the agent's drawer carries)

#### Scenario: Buffer-local vars set before preset
- **WHEN** initializing agent buffer
- **THEN** sets session vars first (session-id, session-dir, branch-name, branch-dir)
- **AND** then applies preset configuration
- **AND** session vars persist outside preset's dynamic scope

#### Scenario: Agent uses own drawer
- **WHEN** agent buffer configured
- **THEN** scope system reads from the agent buffer's `:PROPERTIES:` drawer (or, when called outside the buffer, from `<agent-dir>/session.org`'s drawer)
- **AND** NOT from the parent's drawer or any `scope.yml`
- **AND** validates agent tools against the agent's drawer only

#### Scenario: Backend/model from preset only
- **WHEN** agent configured
- **THEN** uses settings from preset in `gptel--known-presets`
- **AND** does NOT inherit parent's backend/model/temperature/system-message

## REMOVED Requirements

### Requirement: scope.yml in agent directory

**Reason**: Replaced by drawer-resident scope in the agent's `session.org`. Removing the sidecar collapses the agent file inventory and matches the new whole-system contract that scope lives in the org property drawer.

**Migration**: Replace the call to `jf/gptel-scope-profile--write-scope-yml` in `persistent-agent.el` with a drawer-text helper that emits the agent's `:GPTEL_SCOPE_*` keys, and prepend that drawer text to the agent's initial `session.org` content before the chat-mode `#+begin_user`/`#+end_user` block. Delete agent-creation tests that fixture `scope.yml`; rewrite them to assert against the drawer of the produced `session.org`.
