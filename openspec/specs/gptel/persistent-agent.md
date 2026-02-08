# GPTEL PersistentAgent Tool

Behavioral specification for the PersistentAgent tool - a spawning and persistence mechanism that enables autonomous sub-agents with isolated configuration, full conversation history, and parent-child session relationships.

**Scope:** This spec covers agent creation, execution lifecycle, configuration isolation, persistence behavior, parent-child communication, and error handling. It does NOT cover branching (see sessions-branching.md) or preset definitions (see implementation .org files).

## Requirements

### Requirement: Tool invocation and validation

The PersistentAgent tool SHALL only operate within persistent session buffers and requires preset, description, and prompt parameters.

Tool registration SHALL:
- Declare :async t (non-blocking parent execution)
- Declare :confirm t (requires user confirmation)
- Declare :include t (results appear in parent buffer)
- Set :category "gptel-persistent" (tool registered as persistent type)
- Accept optional allowed_paths and denied_paths arrays for scope control

The tool SHALL be categorized as "meta" in the scope system, bypassing scope validation for the tool invocation itself (but agent tools still respect agent's scope).

#### Scenario: Parent session requirement enforced
- **WHEN** PersistentAgent is invoked in a non-persistent gptel buffer (no jf/gptel--session-dir)
- **THEN** the system SHALL raise a user-error: "PersistentAgent requires parent persistent session"

#### Scenario: Tool parameters validated
- **WHEN** invoking PersistentAgent
- **THEN** the system requires preset (string from enum), description (string), and prompt (string) parameters
- **AND** accepts optional allowed_paths (array of glob patterns) and denied_paths (array of glob patterns)

#### Scenario: Optional path inheritance from parent
- **WHEN** allowed_paths is not provided (nil)
- **THEN** the agent inherits allowed read paths from parent's preset.md
- **WHEN** allowed_paths is provided as empty array []
- **THEN** the agent has no path restrictions (reads from anywhere)
- **WHEN** allowed_paths is provided with patterns
- **THEN** the agent uses exactly those patterns (no inheritance)

#### Scenario: Meta tool bypasses scope validation
- **WHEN** PersistentAgent is categorized as meta in the scope system
- **THEN** the parent session can invoke PersistentAgent without scope checks on the tool itself
- **AND** the spawned agent's tools still respect the agent's own scope configuration

#### Scenario: Async and confirm flags respected
- **WHEN** PersistentAgent is invoked
- **THEN** the tool runs asynchronously (non-blocking parent)
- **AND** prompts user for confirmation before executing
- **AND** returns results to parent buffer when complete

### Requirement: Agent session creation

The system SHALL create agent sessions under the parent's branch directory with their own metadata, preset, and scope configuration.

Agent directory structure SHALL follow:
```
<parent-session-dir>/branches/<parent-branch-name>/agents/<preset>-<timestamp>-<slug>/
├── session.md           (agent conversation history)
├── preset.md            (agent configuration with YAML frontmatter and system message)
├── scope-plan.yml       (agent metadata including type: "agent" and parent_session_id)
└── tools.org            (agent tool execution log)
```

Agent sessions SHALL NOT have:
- `branches/` subdirectory (agents don't support branching)
- `current` symlink (no branching means no active branch tracking)

#### Scenario: Agent directory created under parent branch
- **WHEN** creating a new agent with preset "researcher" and description "analyze code"
- **THEN** the system creates directory: `<parent-branch-dir>/agents/researcher-<timestamp>-analyze-code/`
- **AND** the directory is under the parent's **current branch** directory, not session root

#### Scenario: Preset template copied to agent directory
- **WHEN** creating an agent with preset "executor"
- **THEN** the system copies `config/gptel/presets/executor.md` to `<agent-dir>/preset.md`
- **AND** the copied file contains YAML frontmatter (backend, model, tools, etc.) and system message

#### Scenario: Paths written to agent preset
- **WHEN** creating an agent with allowed_paths ["/path/to/project/**"]
- **THEN** the system updates `<agent-dir>/preset.md` with a paths section:
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

#### Scenario: Inherited paths from parent
- **WHEN** creating an agent without specifying allowed_paths
- **THEN** the system reads parent's `preset.md` YAML frontmatter
- **AND** extracts paths.read patterns from parent
- **AND** writes those patterns to agent's preset.md paths.read section

#### Scenario: scope-plan.yml created with agent metadata
- **WHEN** creating an agent session
- **THEN** the system writes `scope-plan.yml` with:
  - version: "3.0"
  - session_id: "<agent-session-id>"
  - created: "<ISO8601 timestamp>"
  - updated: "<ISO8601 timestamp>"
  - type: "agent"
  - parent_session_id: "<parent-session-id>"
  - preset: "<preset-name>"

#### Scenario: Empty session.md created
- **WHEN** creating an agent session
- **THEN** the system creates an empty `session.md` file
- **AND** associates the file with the agent buffer via set-visited-file-name

#### Scenario: No current symlink for agents
- **WHEN** creating an agent session
- **THEN** the system SHALL NOT create a `current` symlink
- **AND** agents do not support branching (single timeline only)

#### Scenario: Agent session ID format
- **WHEN** creating an agent with preset "explorer" and description "find module"
- **THEN** the session ID follows format: `explorer-<timestamp>-find-module`
- **AND** the ID is derived from the directory path (not stored separately)

### Requirement: Configuration isolation

The system SHALL enforce zero inheritance - agent configuration comes ONLY from the agent's preset.md file, never from parent session state.

Configuration loading SHALL:
1. Copy preset template to agent directory (file-first approach)
2. Load preset YAML frontmatter from agent's preset.md (not from in-memory alist)
3. Set buffer-local session variables BEFORE applying preset
4. Apply preset configuration (backend, model, tools, system message)
5. Capture tools from agent buffer context AFTER preset application
6. Use captured tools for gptel-request (not parent's tools)

#### Scenario: Preset loaded from file, not memory
- **WHEN** initializing an agent buffer
- **THEN** the system calls jf/gptel--load-preset-from-file on the agent's directory
- **AND** loads configuration from agent's preset.md YAML frontmatter
- **AND** does NOT use gptel-agent's in-memory agents alist

#### Scenario: Buffer-local session variables set before preset
- **WHEN** initializing an agent buffer
- **THEN** the system sets buffer-local variables in this order:
  1. jf/gptel--session-id (agent's session ID)
  2. jf/gptel--session-dir (agent's directory path)
  3. jf/gptel--branch-name ("main")
  4. jf/gptel--branch-dir (same as session-dir, agents don't branch)
  5. Apply preset configuration (which is dynamically scoped)
- **AND** session variables persist outside preset's dynamic scope

#### Scenario: Tools captured from agent buffer context
- **WHEN** setting up gptel-request for the agent
- **THEN** the system captures gptel-tools INSIDE the agent buffer's with-current-buffer
- **AND** uses the captured tools list for gptel-request
- **AND** does NOT use parent buffer's tools (context matters for buffer-local vars)

#### Scenario: Backend, model, system message from preset YAML
- **WHEN** agent preset.md contains:
```yaml
backend: Claude
model: claude-3-7-sonnet-20250219
temperature: 0.3
```
- **THEN** the agent buffer uses those exact settings
- **AND** does NOT inherit parent's backend/model/temperature

#### Scenario: Parent configuration never leaked
- **WHEN** parent session uses different backend, model, tools, or system message
- **THEN** the agent SHALL NOT inherit any of those settings
- **AND** agent configuration comes exclusively from agent's preset.md

#### Scenario: gptel-with-preset isolation
- **WHEN** executing the agent request
- **THEN** the system uses gptel-with-preset to create a clean dynamic scope
- **AND** passes :tools (captured from agent buffer), :use-tools t, :include-tool-results t
- **AND** parent's preset scope does NOT affect agent execution

### Requirement: Execution lifecycle

The system SHALL execute agents asynchronously with FSM state tracking and dual-duty response accumulation.

Execution flow SHALL:
1. Insert prompt into agent buffer (becomes part of conversation history)
2. Initiate gptel-request with buffer, position, overlay context, and FSM handlers
3. Transition through WAIT state (overlay shows "Waiting...")
4. Transition through TOOL state on tool calls (overlay shows "Tools (+N)")
5. Accumulate string responses for parent callback
6. Insert responses into buffer for persistence
7. Trigger auto-save after each response
8. Delete overlay and invoke parent callback on completion

#### Scenario: Prompt inserted to buffer before request
- **WHEN** launching an agent with a prompt
- **THEN** the system inserts the prompt text into the agent buffer
- **AND** appends "\n\n" after the prompt
- **AND** sets buffer as modified
- **AND** gptel-request reads from buffer (prompt=nil) rather than passing ephemeral prompt

#### Scenario: gptel-request initiated with context
- **WHEN** starting agent execution
- **THEN** the system calls gptel-request with:
  - :buffer agent-buffer (target buffer for responses)
  - :position (point-max) (insert at end)
  - :context overlay (for parent feedback)
  - :fsm (custom FSM with WAIT and TOOL handlers)
  - :callback (handles responses, accumulation, and parent return)

#### Scenario: WAIT state updates overlay
- **WHEN** agent enters WAIT state (waiting for API response)
- **THEN** the custom FSM handler jf/gptel-persistent-agent--indicate-wait runs
- **AND** updates overlay after 1.5 seconds with "Waiting..." message
- **AND** checks overlay validity before updating (buffer may be killed)

#### Scenario: TOOL state updates overlay with count
- **WHEN** agent enters TOOL state (tool calls in progress)
- **THEN** the custom FSM handler jf/gptel-persistent-agent--indicate-tool-call runs
- **AND** increments overlay count by number of tool calls
- **AND** displays "Calling Tools... (+N)" with cumulative tool count
- **AND** shows formatted tool calls in overlay

#### Scenario: String responses dual-duty behavior
- **WHEN** agent receives a string response chunk
- **THEN** the callback:
  1. Inserts chunk into agent buffer with gptel 'response property (persistence)
  2. Concatenates chunk to partial accumulator string (for parent callback)
- **AND** performs BOTH operations for every string response (not tool-result)

#### Scenario: Completion deletes overlay and invokes callback
- **WHEN** agent completes (no more tool-use in info)
- **THEN** the system deletes the overlay from parent buffer
- **AND** applies optional transformer to accumulated result
- **AND** invokes parent callback (main-cb) with final accumulated string

#### Scenario: Auto-save triggers after each response
- **WHEN** agent receives any API response
- **THEN** gptel's gptel-post-response-functions hook fires
- **AND** jf/gptel--auto-save-session-buffer saves the buffer
- **AND** session.md is persisted to disk (incremental saves, not just final)

#### Scenario: FSM handlers chained with core handlers
- **WHEN** agent FSM processes a state transition
- **THEN** the custom handler runs FIRST (overlay update)
- **AND** the core gptel handler runs SECOND (e.g., gptel--handle-wait, gptel--handle-tool-use)

### Requirement: Parent-child communication

The system SHALL provide non-intrusive progress feedback via overlay in the parent buffer and return accumulated results via callback.

Overlay system SHALL:
- Create overlay at marker position in parent buffer
- Display task description and preset name
- Update overlay during WAIT and TOOL states
- Show cumulative tool call count
- Delete overlay on completion or error
- Check overlay validity before updates (parent buffer may be killed)

#### Scenario: Overlay created at marker position
- **WHEN** launching an agent
- **THEN** the system captures (point-marker) in parent buffer
- **AND** creates overlay spanning the current line (or inserts newline if at beginning)
- **AND** sets overlay properties: gptel-persistent-agent t, count 0, msg (task header)

#### Scenario: Overlay displays task description
- **WHEN** overlay is created for preset "researcher" with description "analyze patterns"
- **THEN** the overlay's after-string contains:
  - Horizontal rule
  - "Researcher Task: analyze patterns" (with faces)
  - Progress status ("Waiting..." or "Calling Tools...")
  - Horizontal rule

#### Scenario: Overlay synchronized with FSM states
- **WHEN** agent transitions from WAIT to TOOL state
- **THEN** overlay changes from "Waiting..." to "Calling Tools... (+N)"
- **AND** shows formatted tool calls

#### Scenario: Final result accumulated from responses
- **WHEN** agent receives multiple response chunks
- **THEN** the callback concatenates chunks into partial variable (let-bound)
- **AND** only NON-RAW string responses are accumulated (tool-results are raw)
- **AND** parent receives complete concatenated string, not tool internals

#### Scenario: Parent callback receives string result
- **WHEN** agent completes successfully
- **THEN** the system invokes (funcall main-cb partial)
- **AND** main-cb is the callback provided by gptel-agent package
- **AND** result appears in parent buffer via gptel's tool result display

#### Scenario: Overlay validity checked before updates
- **WHEN** FSM handler tries to update overlay
- **THEN** the system checks (overlay-buffer overlay) returns non-nil
- **AND** skips update if parent buffer was killed
- **AND** prevents crashes from dangling overlay references

#### Scenario: Overlay deleted on all completion paths
- **WHEN** agent completes with success, error, or abort
- **THEN** the system calls (delete-overlay ov) in every terminal callback branch
- **AND** ensures overlay is always cleaned up

### Requirement: Persistence and resumption

The system SHALL auto-save the agent buffer after every API response, preserving full conversation history including tool results.

Persistence SHALL:
- Register auto-save hook during agent buffer initialization
- Trigger save after each response (not just final)
- Include tool results in saved conversation via gptel-include-tool-results
- Register session in global registry with branch info
- Enable session resumption via find-file on session.md

#### Scenario: Auto-save hook registered at creation
- **WHEN** initializing agent buffer
- **THEN** the system adds jf/gptel--auto-save-session-buffer to gptel-post-response-functions
- **AND** makes the hook buffer-local (fifth argument t)

#### Scenario: Every API response triggers buffer save
- **WHEN** agent receives a response chunk, tool result, or final response
- **THEN** gptel-post-response-functions hook fires
- **AND** jf/gptel--auto-save-session-buffer checks jf/gptel--session-dir and buffer-file-name
- **AND** calls (save-buffer) if both are non-nil

#### Scenario: Tool results included in conversation
- **WHEN** applying preset configuration to agent buffer
- **THEN** the preset includes :include-tool-results t
- **AND** gptel inserts tool calls and results into buffer
- **AND** full conversation with tools is saved to session.md

#### Scenario: Session registered in global registry
- **WHEN** creating an agent session
- **THEN** the system calls jf/gptel--register-session with:
  - session-dir (agent directory)
  - agent-buffer (buffer reference)
  - session-id (extracted from directory name)
  - branch-name ("main")
  - branch-dir (same as session-dir, agents don't branch)
- **AND** registry stores plist with `:session-id`, `:session-dir`, `:branch-name`, `:branch-dir`, `:buffer`

#### Scenario: Conversation history fully preserved
- **WHEN** agent executes multiple tool calls and receives multiple responses
- **THEN** session.md contains:
  - Original prompt (inserted before request)
  - All tool calls (via gptel tool display)
  - All tool results (via gptel-include-tool-results)
  - All LLM responses (inserted by callback)
- **AND** conversation is complete and resumable

#### Scenario: Session resumable via find-file
- **WHEN** user opens `<agent-dir>/session.md` via find-file
- **THEN** the auto-initialization hook (jf/gptel--maybe-initialize-session-buffer) fires
- **AND** detects the session file pattern (*/agents/*/session.md)
- **AND** loads preset, sets buffer-local vars, enables gptel-mode
- **AND** buffer is ready for continued conversation

#### Scenario: scope-plan.yml updated timestamp is NOT managed by auto-save
- **WHEN** agent buffer auto-saves
- **THEN** the system only saves session.md
- **AND** does NOT update scope-plan.yml's updated timestamp
- **AND** scope-plan.yml timestamp is managed by scope commands, not persistence

### Requirement: Error handling and recovery

The system SHALL gracefully handle errors with consistent overlay cleanup and structured error reporting to parent.

Error handling SHALL:
- Delete overlay on all error paths
- Return structured error messages to parent callback
- Handle network/API errors, user abort, filesystem errors
- Catch exceptions in callback to prevent crashes

#### Scenario: Network/API error cleanup
- **WHEN** gptel-request callback receives resp=nil (network failure)
- **THEN** the system:
  1. Deletes overlay from parent buffer
  2. Invokes parent callback with formatted error: "Error: Network failure\n%S" (includes :error plist)

#### Scenario: User abort handled gracefully
- **WHEN** user denies tool confirmation (resp='abort)
- **THEN** the system:
  1. Deletes overlay from parent buffer
  2. Invokes parent callback with message: "Error: User aborted agent"

#### Scenario: Missing preset.md reports clear error
- **WHEN** jf/gptel--load-preset-from-file returns nil (file missing or unparseable)
- **THEN** the system raises error: "Failed to load preset from %s"
- **AND** does NOT silently fall back to defaults

#### Scenario: Parent validation failure prevents creation
- **WHEN** PersistentAgent invoked in buffer without jf/gptel--session-dir
- **THEN** the system raises user-error before any directory creation
- **AND** prevents partial state (no orphaned directories)

#### Scenario: Filesystem errors logged and reported
- **WHEN** directory creation, file copy, or metadata write fails
- **THEN** the system logs error via jf/gptel--log at 'error level
- **AND** propagates error to caller (does not silently continue)

#### Scenario: Exception handling in callback
- **WHEN** callback encounters unexpected exception during response processing
- **THEN** the system should catch and handle gracefully
- **AND** log error with context for debugging
- **AND** invoke parent callback with error message (not crash)

#### Scenario: Overlay always cleaned up
- **WHEN** ANY terminal state is reached (success, error, abort)
- **THEN** the callback MUST call (delete-overlay ov)
- **AND** overlay is removed from parent buffer
- **AND** no dangling overlays persist after completion

### Requirement: Integration with sessions subsystem

The PersistentAgent tool SHALL use the sessions filesystem, metadata, and registry modules for directory management and session tracking.

Dependencies SHALL include:
- gptel-session-filesystem: Directory creation, path resolution
- gptel-session-metadata: Preset loading, scope-plan.yml reading
- gptel-session-registry: Session registration and global tracking
- gptel-session-logging: Structured logging

#### Scenario: Agent directory created via filesystem module
- **WHEN** creating an agent session
- **THEN** the system calls jf/gptel--create-agent-directory from filesystem module
- **AND** passes parent-branch-dir, preset, and description
- **AND** returns agent directory path: `<parent-branch-dir>/agents/<preset>-<timestamp>-<slug>/`

#### Scenario: Preset template copied via filesystem helper
- **WHEN** initializing agent configuration
- **THEN** the system calls jf/gptel--copy-preset-template
- **AND** copies from `config/gptel/presets/<preset>.md` to `<agent-dir>/preset.md`

#### Scenario: Preset loaded via metadata module
- **WHEN** loading agent configuration
- **THEN** the system calls jf/gptel--load-preset-from-file
- **AND** parses YAML frontmatter from `<agent-dir>/preset.md`
- **AND** returns plist with :backend, :model, :temperature, :tools, etc.

#### Scenario: Session metadata read from scope-plan.yml
- **WHEN** registering agent session
- **THEN** the system calls jf/gptel--read-session-metadata
- **AND** parses scope-plan.yml for :created, :type, :parent-session-id, :preset
- **AND** uses metadata for registry entry

#### Scenario: Session registered via registry module
- **WHEN** agent buffer is initialized
- **THEN** the system calls jf/gptel--register-session
- **AND** creates hash table entry with key `"<session-id>/main"`
- **AND** stores plist with session paths and buffer reference

#### Scenario: Logging via logging module
- **WHEN** performing operations (directory creation, preset loading, etc.)
- **THEN** the system calls jf/gptel--log with level and message
- **AND** logs at INFO level for successful operations
- **AND** logs at ERROR level for failures

#### Scenario: Path resolution via constants
- **WHEN** constructing file paths for agent session
- **THEN** the system uses constants from gptel-session-constants module
- **AND** calls helpers like jf/gptel--context-file-path, jf/gptel--preset-file-path
- **AND** does NOT hardcode filenames

### Requirement: Integration with scope subsystem

The PersistentAgent tool SHALL integrate with the scope system through preset-based configuration and path inheritance.

Scope integration SHALL:
- Load scope patterns from agent's preset.md YAML frontmatter
- Update preset.md with paths section (read, write, deny)
- Support inheriting parent's allowed read paths
- Validate agent tools against agent's scope (not parent's scope)

#### Scenario: Agent scope loaded from preset.md
- **WHEN** agent buffer is initialized and preset applied
- **THEN** the scope system reads paths section from agent's preset.md:
```yaml
paths:
  read:
    - "/path/to/project/**"
  write:
    - "/tmp/**"
  deny:
    - "**/.git/**"
```
- **AND** validates agent tool calls against these patterns

#### Scenario: Preset.md updated with path controls
- **WHEN** creating agent with allowed_paths ["/project/**"]
- **THEN** the system calls jf/gptel-scope--update-preset-paths
- **AND** writes or updates the paths section in agent's preset.md
- **AND** includes allowed (read), write defaults, and deny defaults

#### Scenario: Agent inherits parent allowed paths
- **WHEN** creating agent without specifying allowed_paths
- **THEN** the system:
  1. Reads parent's preset.md YAML frontmatter
  2. Extracts paths.read patterns via jf/gptel-scope--parse-preset-config
  3. Passes inherited patterns to jf/gptel-scope--update-preset-paths
  4. Writes to agent's preset.md

#### Scenario: scope-plan.yml contains metadata only
- **WHEN** writing scope-plan.yml for agent
- **THEN** the file contains session metadata (session_id, created, updated, type, parent_session_id, preset)
- **AND** does NOT contain scope configuration (paths, org_roam_patterns, shell_commands)
- **AND** scope config lives exclusively in preset.md (file format contract)

#### Scenario: Agent tools validated against agent scope
- **WHEN** agent executes a tool like Read or Edit
- **THEN** the scope system checks buffer-local jf/gptel--branch-dir
- **AND** loads scope config from agent's preset.md (not parent's)
- **AND** validates path against agent's allowed/denied patterns

#### Scenario: Meta tool bypasses scope validation
- **WHEN** parent session invokes PersistentAgent tool
- **THEN** the scope system categorizes PersistentAgent as meta
- **AND** allows invocation without checking parent's scope patterns
- **AND** agent's own tools still respect agent's scope

### Requirement: Integration with gptel package

The PersistentAgent tool SHALL extend gptel's FSM, request system, and hook infrastructure for async execution and persistence.

gptel integration SHALL:
- Use gptel-request for async API calls
- Extend FSM with custom WAIT and TOOL state handlers
- Hook into gptel-post-response-functions for auto-save
- Use gptel-with-preset for dynamic scope configuration
- Leverage gptel-include-tool-results for conversation preservation
- Use gptel-make-tool for tool registration

#### Scenario: Custom FSM handlers registered
- **WHEN** creating FSM for agent request
- **THEN** the system passes jf/gptel-persistent-agent--fsm-handlers to gptel-make-fsm
- **AND** FSM contains tuples: (STATE UI-HANDLER CORE-HANDLER)
  - WAIT: (jf/gptel-persistent-agent--indicate-wait, gptel--handle-wait)
  - TOOL: (jf/gptel-persistent-agent--indicate-tool-call, gptel--handle-tool-use)

#### Scenario: gptel-request executes async
- **WHEN** agent launches
- **THEN** the system calls gptel-request with nil prompt (reads buffer)
- **AND** passes :buffer, :position, :context, :fsm, :callback
- **AND** request executes asynchronously (non-blocking)

#### Scenario: gptel-post-response-functions hook used
- **WHEN** agent buffer receives a response
- **THEN** gptel triggers gptel-post-response-functions hook
- **AND** jf/gptel--auto-save-session-buffer runs as buffer-local hook
- **AND** buffer is saved automatically

#### Scenario: gptel-with-preset creates clean scope
- **WHEN** executing agent request
- **THEN** the system uses gptel-with-preset macro
- **AND** passes plist with :use-tools t, :include-tool-results t, :tools (captured list)
- **AND** preset scope applies only during gptel-request (dynamic binding)

#### Scenario: gptel-include-tool-results preserves tools
- **WHEN** agent preset includes :include-tool-results t
- **THEN** gptel inserts tool calls and results into buffer as text
- **AND** full conversation with tools is preserved in session.md

#### Scenario: Tool registered via gptel-make-tool
- **WHEN** PersistentAgent module loads
- **THEN** the system calls gptel-make-tool with:
  - :name "PersistentAgent"
  - :description (tool description with usage guidance)
  - :function #'jf/gptel-persistent-agent--task
  - :args (preset, description, prompt, allowed_paths, denied_paths)
  - :category "gptel-persistent"
  - :async t, :confirm t, :include t

### Requirement: Critical behavioral invariants

The system SHALL maintain these invariants across all operations.

#### Invariant: Parent session required
- **AT ALL TIMES** when PersistentAgent is invoked
- **IT MUST BE TRUE** that buffer-local jf/gptel--session-dir is non-nil
- **VIOLATION** causes user-error before any side effects

#### Invariant: Buffer-local vars set before preset scope
- **AT ALL TIMES** when initializing agent buffer
- **IT MUST BE TRUE** that session vars (jf/gptel--session-id, jf/gptel--session-dir, jf/gptel--branch-name, jf/gptel--branch-dir) are set BEFORE calling jf/gptel--apply-session-preset
- **REASON** session vars must persist outside preset's dynamic scope

#### Invariant: Tool results included
- **AT ALL TIMES** when agent executes with tools
- **IT MUST BE TRUE** that :include-tool-results t is set in preset
- **REASON** tool I/O must be saved for complete conversation history

#### Invariant: Auto-save triggers on response
- **AT ALL TIMES** after agent receives API response
- **IT MUST BE TRUE** that gptel-post-response-functions hook fires and saves buffer
- **REASON** incremental persistence enables resumption from any point

#### Invariant: Prompt inserted to buffer, request reads from buffer
- **AT ALL TIMES** when launching agent
- **IT MUST BE TRUE** that prompt is inserted to buffer BEFORE gptel-request(nil)
- **REASON** prompt becomes part of persistent conversation, not ephemeral parameter

#### Invariant: Tools captured in agent buffer context
- **AT ALL TIMES** when setting up agent request
- **IT MUST BE TRUE** that agent-tools is set INSIDE (with-current-buffer agent-buffer)
- **REASON** gptel-tools is buffer-local, must be captured in correct context

#### Invariant: Overlay cleaned up on all paths
- **AT ALL TIMES** when callback reaches terminal state (success, error, abort)
- **IT MUST BE TRUE** that (delete-overlay ov) is called
- **REASON** prevents dangling overlays in parent buffer

#### Invariant: Zero configuration inheritance
- **AT ALL TIMES** when loading agent configuration
- **IT MUST BE TRUE** that config comes ONLY from agent's preset.md file
- **VIOLATION** would be reading from parent's buffer-local vars or in-memory state

#### Invariant: Agent directory under parent branch
- **AT ALL TIMES** when creating agent directory
- **IT MUST BE TRUE** that path is `<parent-branch-dir>/agents/<preset>-<timestamp>-<slug>/`
- **REASON** agents are scoped to parent's branch, not session root

#### Invariant: Agents do not branch
- **AT ALL TIMES** for agent sessions
- **IT MUST BE TRUE** that no `branches/` subdirectory or `current` symlink exists
- **REASON** agents are single-timeline, no branching support

## Critical Integration Boundaries

### With Sessions Subsystem
- Uses filesystem module for directory creation: `jf/gptel--create-agent-directory`, `jf/gptel--agents-dir-path`
- Uses metadata module for preset loading: `jf/gptel--load-preset-from-file`, `jf/gptel--read-session-metadata`
- Uses registry module for session tracking: `jf/gptel--register-session`
- Follows same directory structure conventions: preset.md, session.md, scope-plan.yml

### With Scope Subsystem
- Loads scope configuration from agent's preset.md YAML frontmatter
- Updates preset.md with path controls via `jf/gptel-scope--update-preset-paths`
- Agent tools validated against agent's scope (not parent's)
- Can inherit parent's allowed paths if not explicitly provided
- Meta tool categorization bypasses scope checks for tool invocation

### With gptel Package (upstream)
- Extends FSM with custom WAIT and TOOL state handlers
- Uses `gptel-request` for async execution with callback
- Hooks into `gptel-post-response-functions` for auto-save
- Uses `gptel-with-preset` for dynamic scope configuration
- Leverages `gptel-include-tool-results` for conversation persistence
- Uses `gptel-make-tool` for tool registration

## Lifecycle State Diagram

```
1. Creation
   ├─ Validate parent session exists
   ├─ Create directory: <parent-branch-dir>/agents/<preset>-<timestamp>-<slug>/
   ├─ Copy preset template → preset.md
   ├─ Update preset.md with paths section
   ├─ Write scope-plan.yml (metadata: type="agent", parent_session_id)
   ├─ Create empty session.md
   └─ Register in global registry

2. Execution
   ├─ Create agent buffer (markdown-mode, gptel-mode)
   ├─ Set buffer-local session vars (BEFORE preset scope)
   ├─ Apply preset from file (backend, model, tools, system message)
   ├─ Capture tools from agent buffer context
   ├─ Register auto-save hook (gptel-post-response-functions)
   ├─ Insert prompt to buffer
   ├─ Associate buffer with session.md
   └─ Launch gptel-request (async)

3. Progress
   ├─ Create overlay in parent buffer
   ├─ WAIT state → update overlay ("Waiting...")
   ├─ TOOL state → update overlay ("Tools (+N)")
   └─ Check overlay validity before updates

4. Persistence
   ├─ Dual-duty callback: insert to buffer + accumulate for parent
   ├─ Auto-save after EACH response (not just final)
   ├─ Tool results included in saved conversation
   └─ Session.md fully resumable

5. Completion
   ├─ Accumulate final result string
   ├─ Delete overlay
   ├─ Invoke parent callback with result
   └─ Registry tracks session for discovery

6. Error/Abort
   ├─ Network error → delete overlay, return error to parent
   ├─ User abort → delete overlay, return 'abort
   ├─ Filesystem error → log and propagate error
   └─ Overlay always cleaned up

7. Resumption
   ├─ Open session.md via find-file
   ├─ Auto-initialization detects agent session pattern
   ├─ Load preset, set buffer-local vars, enable gptel-mode
   └─ Continue conversation from any point
```

## Verification Checklist

After reviewing implementation:

- [x] Spec captures tool invocation and validation (parent session requirement, meta categorization)
- [x] Spec covers agent session creation (directory under parent branch, no branching support)
- [x] Spec enforces configuration isolation (zero inheritance, preset-only config)
- [x] Spec describes execution lifecycle (FSM states, overlay updates, dual-duty callback)
- [x] Spec documents parent-child communication (overlay system, result accumulation)
- [x] Spec covers persistence and resumption (auto-save, registry, find-file resumption)
- [x] Spec handles error cases (network, abort, filesystem, overlay cleanup)
- [x] Spec cross-references sessions-persistence.md (directory structure, metadata format)
- [x] Spec cross-references scope.md (preset-based config, path inheritance)
- [x] Spec uses SHALL language for requirements
- [x] Critical invariants explicitly stated (tool capture, overlay cleanup, zero inheritance)

## Test Questions

**Q: If I wanted to add a new agent preset, what contract must I follow?**
A: Create `config/gptel/presets/<preset-name>.md` with YAML frontmatter (backend, model, tools, temperature, etc.) and system message body. Add preset name to :enum in PersistentAgent :args. Preset will be copied to agent directory and loaded from file.

**Q: What happens if parent buffer is killed while agent is running?**
A: FSM handlers check `(overlay-buffer overlay)` before updates. If nil (buffer killed), handlers skip update. This prevents crashes from dangling overlay references.

**Q: Can an agent spawn another agent?**
A: Yes, if the spawned agent is itself a persistent session (has jf/gptel--session-dir set). PersistentAgent is categorized as meta, so agents with the PersistentAgent tool can spawn sub-agents. However, check preset definitions - not all presets include PersistentAgent.

**Q: How does agent resume work?**
A: Open `<agent-dir>/session.md` via find-file. Auto-initialization hook detects pattern `*/agents/*/session.md`, loads preset, sets buffer-local vars, enables gptel-mode. Full conversation history (including tools) is loaded, session is ready to continue.

**Q: What's the difference between agent and branch sessions?**
A: Agents live under `<parent-branch-dir>/agents/`, have `type: "agent"` in scope-plan.yml, include `parent_session_id`, and do NOT support branching (no branches/ subdirectory, no current symlink). Branch sessions live under `<session-dir>/branches/`, support branching and merging, and have `type: "branch"` (or unset).

## Related Specs

- **sessions-persistence.md** - Session directory structure, metadata format, registry, auto-save behavior
- **sessions-branching.md** - Branch operations, not applicable to agents (agents don't branch)
- **scope.md** - Preset-based scope configuration, path validation, tool categorization (meta tools)
- **architecture.md** - Overall subsystem boundaries and integration points (when created)
