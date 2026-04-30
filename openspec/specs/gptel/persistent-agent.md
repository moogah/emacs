# PersistentAgent Tool

## Purpose

Enables autonomous sub-agents with isolated configuration, full conversation history, and parent-child relationships. Agents execute asynchronously in their own buffers while providing non-intrusive progress feedback via overlays in the parent buffer.

## Key Concepts

### Configuration Isolation (Zero Inheritance)

**CRITICAL PRINCIPLE**: Agents have ZERO inheritance from parent sessions.

Agent configuration comes ONLY from:
1. **Preset definition** (backend, model, tools, system message)
2. **Explicit allowed_paths parameter** (scope permissions)

Agents do NOT inherit:
- Parent's drawer scope keys
- Parent's backend/model/temperature
- Parent's tools or system message

**Path configuration**:
- `allowed_paths` provided → agent uses exactly those patterns
- `allowed_paths` nil/empty → agent's drawer omits `:GPTEL_SCOPE_READ:` (no read permissions)
- **Never** inherits parent's allowed paths

### Agent Session Structure

```
<parent-branch-dir>/agents/<preset>-<timestamp>-<slug>/
├── session.org          # Agent conversation + :PROPERTIES: drawer (preset, parent id, scope keys)
├── metadata.yml         # type="agent", parent_session_id
└── tools.org            # Tool execution log
```

Agents do NOT have:
- `branches/` subdirectory (no branching support)
- `current` symlink (no branch tracking)

### Execution Lifecycle

1. **Creation**: Validate parent, create directory, write files
2. **Initialization**: Set buffer-local vars, apply preset
3. **Execution**: Insert prompt, initiate gptel-request with FSM
4. **FSM States**: WAIT (overlay: "Waiting..."), TOOL (overlay: "Tools (+N)")
5. **Accumulation**: Dual-duty callback (insert to buffer + accumulate for parent)
6. **Auto-save**: After each response (incremental persistence)
7. **Completion**: Delete overlay, invoke parent callback
8. **Resumption**: Open session.md, auto-init, continue conversation

### Parent-Child Communication

**Overlay system**:
- Created at marker position in parent buffer
- Shows task description, preset name, progress status
- Updates during WAIT and TOOL states
- Displays cumulative tool count with formatted calls
- Deleted on completion or error

**Result accumulation**:
- String responses accumulated for parent callback
- Tool calls/results inserted to agent buffer (persistence)
- Parent receives concatenated string result

## Requirements

### Requirement: Tool invocation and validation

PersistentAgent SHALL only operate within persistent session buffers with preset, description, and prompt parameters.

**Tool registration**:
- `:async t` (non-blocking parent)
- `:confirm t` (requires user confirmation)
- `:include t` (results appear in parent)
- `:category "gptel-persistent"`
- Optional: `allowed_paths`, `denied_paths` arrays

**Categorized as "meta"**: Bypasses scope validation for tool invocation itself (agent's tools still respect agent's scope).

#### Scenario: Parent session requirement
- **WHEN** PersistentAgent invoked without `jf/gptel--session-dir`
- **THEN** raises user-error: "PersistentAgent requires parent persistent session"

#### Scenario: Explicit path configuration (zero inheritance)
- **WHEN** `allowed_paths` not provided (nil)
- **THEN** agent has no read permissions (the drawer omits `:GPTEL_SCOPE_READ:`)
- **AND** paths NOT inherited from parent
- **WHEN** `allowed_paths` is empty array `[]`
- **THEN** agent has no read permissions
- **WHEN** `allowed_paths` provided with patterns
- **THEN** agent uses exactly those patterns
- **AND** paths come exclusively from parameter, never from parent

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

### Requirement: Execution lifecycle

The system SHALL execute agents asynchronously with FSM state tracking and dual-duty response accumulation.

**Execution flow**:
1. Insert prompt into agent buffer
2. Associate buffer with session.md file
3. Set buffer as modified
4. Initiate gptel-request with custom FSM
5. WAIT state → overlay shows "Waiting..."
6. TOOL state → overlay shows "Calling Tools (+N)"
7. Accumulate string responses for parent
8. Insert responses to buffer (persistence)
9. Auto-save after each response
10. Delete overlay and invoke parent callback on completion

#### Scenario: Prompt inserted before request
- **WHEN** launching agent with prompt
- **THEN** inserts prompt text into agent buffer
- **AND** appends "\n\n"
- **AND** sets buffer modified
- **AND** gptel-request reads from buffer (not ephemeral prompt)

#### Scenario: gptel-request initiated
- **WHEN** starting execution
- **THEN** calls gptel-request with:
  - `:buffer` agent-buffer
  - `:position` (point-max)
  - `:context` overlay
  - `:fsm` custom handlers
  - `:callback` accumulation handler
  - No `:prompt` (reads from buffer)

#### Scenario: WAIT state updates overlay
- **WHEN** agent enters WAIT state
- **THEN** FSM handler uses run-at-time with 1.5 second delay
- **AND** updates overlay with "Waiting..." message
- **AND** checks overlay validity before update

#### Scenario: TOOL state updates overlay
- **WHEN** agent enters TOOL state
- **THEN** FSM handler increments overlay count
- **AND** displays "Calling Tools... (+N)" with cumulative count
- **AND** shows formatted tool calls

#### Scenario: String responses dual-duty
- **WHEN** agent receives string response chunk
- **THEN** callback:
  1. Inserts chunk into agent buffer (persistence)
  2. Concatenates chunk to accumulator (for parent callback)
- **AND** performs BOTH for every non-raw string response

#### Scenario: Completion deletes overlay
- **WHEN** agent completes
- **THEN** deletes overlay from parent buffer
- **AND** applies optional transformer to result
- **AND** invokes parent callback with final accumulated string

#### Scenario: Auto-save after each response
- **WHEN** agent receives any response
- **THEN** `gptel-post-response-functions` hook fires
- **AND** `jf/gptel--auto-save-session-buffer` saves buffer
- **AND** session.md persisted incrementally

### Requirement: Parent-child communication

The system SHALL provide non-intrusive feedback via overlay and return accumulated results via callback.

**Overlay system**:
- Create at marker position in parent
- Display task description and preset
- Update during WAIT and TOOL states
- Show cumulative tool count
- Delete on completion/error
- Check validity before updates

#### Scenario: Overlay created at marker
- **WHEN** launching agent
- **THEN** captures (point-marker) in parent buffer
- **AND** creates overlay spanning current line
- **AND** sets properties: gptel-persistent-agent t, count 0, msg (header)

#### Scenario: Overlay shows task info
- **WHEN** overlay created for preset "researcher", description "analyze patterns"
- **THEN** after-string contains:
  - Horizontal rule
  - "Researcher Task: analyze patterns"
  - Progress status ("Waiting..." or "Tools...")
  - Horizontal rule

#### Scenario: Overlay synchronized with FSM
- **WHEN** agent transitions WAIT → TOOL
- **THEN** overlay updates from "Waiting..." to "Calling Tools... (+N)"
- **AND** shows formatted tool calls

#### Scenario: Tool count accumulation
- **WHEN** agent makes multiple tool calls
- **THEN** overlay shows cumulative count: "Tools (+1)", "Tools (+2)", etc.

#### Scenario: Parent callback receives result
- **WHEN** agent completes
- **THEN** invokes parent callback with concatenated string
- **AND** result appears in parent buffer via gptel's tool system

### Requirement: Persistence and resumption

The system SHALL auto-save agent buffer after every API response, preserving full conversation.

**Persistence**:
- Register auto-save hook during init
- Trigger save after each response
- Include tool calls and results
- Register in global registry
- Enable resumption via find-file

#### Scenario: Auto-save hook registered
- **WHEN** initializing agent buffer
- **THEN** adds `jf/gptel--auto-save-session-buffer` to `gptel-post-response-functions`
- **AND** makes hook buffer-local

#### Scenario: Every response triggers save
- **WHEN** agent receives response chunk or tool result
- **THEN** hook fires automatically
- **AND** saves buffer
- **AND** session.md updated incrementally

#### Scenario: Tool calls included
- **WHEN** agent executes tools
- **THEN** gptel inserts tool calls and results to buffer
- **AND** written to session.md during auto-save

#### Scenario: Session registered
- **WHEN** creating agent
- **THEN** calls `jf/gptel--register-session` with session-dir, buffer, etc.
- **AND** registry stores plist with paths and buffer reference

#### Scenario: Session resumable
- **WHEN** user opens `<agent-dir>/session.md`
- **THEN** auto-init detects pattern `*/agents/*/session.md`
- **AND** loads preset, sets buffer-local vars, enables gptel-mode
- **AND** buffer ready for continued conversation

### Requirement: Error handling

The system SHALL gracefully handle errors with consistent overlay cleanup.

#### Scenario: Network error cleanup
- **WHEN** gptel-request callback receives nil (network failure)
- **THEN** deletes overlay from parent
- **AND** invokes parent callback with error message

#### Scenario: User abort
- **WHEN** user denies tool confirmation
- **THEN** deletes overlay
- **AND** invokes parent callback: "Error: User aborted agent"

#### Scenario: Preset validation
- **WHEN** invoked with preset not in `gptel--known-presets`
- **THEN** raises user-error before directory creation
- **AND** prevents partial state

#### Scenario: Overlay always cleaned up
- **WHEN** ANY terminal state reached (success, error, abort)
- **THEN** callback MUST call `(delete-overlay ov)`
- **AND** no dangling overlays persist

## Integration Points

### With Sessions Subsystem
- Uses `jf/gptel--create-agent-directory` for directory creation
- Writes metadata.yml directly (type="agent")
- Registers via `jf/gptel--register-session`
- Follows same file structure (session.org, metadata.yml); scope keys live inside `session.org`'s `:PROPERTIES:` drawer

### With Scope Subsystem
- Writes the agent's scope keys into `session.org`'s `:PROPERTIES:` drawer (from `allowed_paths` parameter)
- NEVER reads parent's drawer
- Agent tools validated against the agent's own drawer only
- Meta tool categorization bypasses parent scope checks

### With gptel Package
- Extends FSM with custom WAIT/TOOL handlers
- Uses `gptel-request` for async execution
- Hooks into `gptel-post-response-functions` for auto-save
- Uses `gptel-make-tool` for registration

## Critical Invariants

1. **Parent session required**: `jf/gptel--session-dir` must be non-nil
2. **Buffer-local vars before preset**: Set session vars BEFORE applying preset
3. **Zero inheritance**: Paths ONLY from `allowed_paths` parameter, never parent
4. **Auto-save on every response**: Incremental persistence enables resumption
5. **Prompt inserted to buffer**: Request reads from buffer, not ephemeral parameter
6. **Overlay cleanup on all paths**: Delete overlay on success, error, abort
7. **No branching**: Agents are single-timeline, no branches/ subdirectory

## Summary

PersistentAgent provides autonomous sub-agents with:
- **Zero inheritance** (isolated configuration)
- **Explicit scope** (only allowed_paths parameter)
- **Async execution** (non-blocking parent)
- **Progress feedback** (overlays in parent)
- **Full persistence** (resumable conversations)
- **Parent-child tracking** (metadata.yml links)
