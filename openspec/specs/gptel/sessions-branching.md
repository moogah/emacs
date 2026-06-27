# GPTEL Sessions Branching

Behavioral specification for gptel session branching operations, enabling divergent conversation paths with full context preservation and metadata tracking.

**Scope:** This spec covers branching operations only. Session persistence fundamentals are documented in sessions-persistence.md. PersistentAgent integration is documented separately.

## ADDED Requirements

### Requirement: Branch creation model

The system SHALL support creating new branches from existing branches, enabling divergent conversation paths while preserving shared history.

A branch operation SHALL:
1. Identify a **branch point** - a specific user prompt in the source branch
2. Copy context from the source branch up to (and optionally including) the branch point
3. Create a new branch directory with its own session.org, metadata, and agent state
4. Preserve lineage via branch-metadata.yml tracking parent branch and branch point

Branches SHALL be first-class session objects with independent evolution.

#### Scenario: Branch as first-class session object
- **WHEN** a branch is created from a parent branch
- **THEN** the new branch SHALL have its own directory under `branches/`
- **AND** contain a complete session.org file (not a reference or delta) whose file-level `:PROPERTIES:` drawer carries the inherited preset, scope keys, and (when applicable) parent-session-id
- **AND** contain its own branch-metadata.yml recording parent branch and branch point
- **AND** be registered independently in the session registry
- **AND** support further branching (branches can have child branches)

#### Scenario: Shared history preservation
- **WHEN** branching from a conversation at a specific user prompt
- **THEN** all conversation history before the branch point SHALL be copied to the new branch
- **AND** both branches share identical history up to the branch point
- **AND** subsequent edits to either branch SHALL NOT affect the other

### Requirement: Branch point selection

The system SHALL provide interactive branch point selection based on **outer `#+begin_user` blocks** in a `gptel-chat-mode` session buffer.

Branch point selection SHALL:
1. Parse the source buffer via `gptel-chat-parse-buffer` to obtain the turn list (or equivalently: enumerate outer `#+begin_user` blocks with their buffer positions)
2. Present a numbered list of user turns for selection (showing the first line of each user block as the display label)
3. Allow the user to choose whether to include or exclude the selected user turn in the new branch
4. Return a buffer position marking the branch point:
   - **Include** → position immediately after the `#+end_user` line that closes the selected user turn (so the new branch ends with the selected user turn complete, awaiting an assistant response)
   - **Exclude** → position immediately before the `#+begin_user` line that opens the selected user turn (so the new branch ends before the selected user turn, awaiting a fresh user prompt)

Only outer `#+begin_user` blocks SHALL be valid branch points. Assistant blocks (`#+begin_assistant`), nested tool blocks (`#+begin_tool`), and non-block content (headings, prose, drawers) SHALL NOT be selectable.

#### Scenario: Interactive turn selection
- **WHEN** user invokes `jf/gptel-branch-session` in an active chat-mode session buffer
- **THEN** the system scans for all outer `#+begin_user` blocks via `gptel-chat-parse-buffer`
- **AND** presents a numbered selection interface showing each user block's first line
- **AND** allows the user to select a turn by number
- **AND** asks whether to include or exclude the selected turn

#### Scenario: Include selected turn in branch
- **WHEN** user selects a user turn and chooses INCLUDE
- **THEN** the branch point position is immediately after the `#+end_user` line of the selected turn
- **AND** the new branch contains the selected turn
- **AND** the next assistant response (if any) is truncated from the new branch

#### Scenario: Exclude selected turn from branch
- **WHEN** user selects a user turn and chooses EXCLUDE
- **THEN** the branch point position is immediately before the `#+begin_user` line of the selected turn
- **AND** the new branch does NOT contain the selected turn
- **AND** the user can author a different turn in the new branch

#### Scenario: Tool blocks and assistant blocks are not valid branch points
- **WHEN** scanning the buffer for branch points
- **THEN** `#+begin_assistant` and `#+begin_tool` blocks SHALL NOT appear in the selection list
- **AND** headings and prose outside turn blocks SHALL NOT appear

#### Scenario: No valid branch points
- **WHEN** a chat-mode session buffer contains no outer `#+begin_user` blocks (empty conversation or only a single assistant block)
- **THEN** the system SHALL report no available branch points
- **AND** NOT allow branch creation

### Requirement: Context truncation

The system SHALL copy conversation history from the source branch to the new branch, truncating the buffer at the selected branch point position.

Context truncation SHALL:
1. Copy buffer content from `point-min` up to the branch point position, verbatim
2. Write the truncated content to the new branch's `session.org`
3. Not attempt to filter, rewrite, or normalize the content — the chat-mode block structure in the source buffer is already the canonical form

Context truncation operates at the buffer-content level. Because chat-mode has no `gptel--bounds` text properties, no bounds-filtering step exists. Block-delimiter integrity is guaranteed by construction: the branch point is always on a line boundary outside any open block.

#### Scenario: Copying content up to branch point
- **WHEN** creating a branch with a branch point at position 5420 (immediately after a `#+end_user`)
- **THEN** the new branch's `session.org` SHALL contain buffer content from position 1 to 5419
- **AND** the content is well-formed chat-mode (parseable by `gptel-chat-parse-buffer`)
- **AND** no truncated / half-open block exists at the end

#### Scenario: Empty branch from first-turn exclude
- **WHEN** the selected branch point is the first user turn
- **AND** the user chooses EXCLUDE
- **THEN** the new branch's `session.org` SHALL contain buffer content from position 1 up to the start of the first `#+begin_user`
- **AND** the result is a valid empty chat-mode session

#### Scenario: Branch preserves org commentary
- **WHEN** the source buffer contains org headings or prose between turns
- **AND** the branch point is after one of those commentary regions
- **THEN** the new branch's `session.org` SHALL include the commentary verbatim
- **AND** the chat-mode parser ignores it on message construction (per chat-mode's blocks-only model)

#### Scenario: Full context copy for late branch point
- **WHEN** the selected branch point is near the end of a long conversation
- **THEN** the new branch SHALL contain nearly all conversation history
- **AND** diverge only at the final exchanges

### Requirement: Branch naming convention

The system SHALL generate branch names using the format `<timestamp>-<user-provided-name>`.

Branch naming SHALL:
- Use YYYYMMDDHHMMSS format for timestamps
- Generate timestamp at branch creation time
- Use user-provided names as-is (no automatic slugification)
- Prevent naming collisions via timestamp prefix
- Enable chronological ordering of branches

The "main" branch is reserved for the initial branch created during session creation.

**Note:** User-provided names are used verbatim without slugification. Users should provide filesystem-safe names (lowercase, hyphens, no spaces or special characters). Session IDs are slugified (see sessions-persistence.md), but branch names within a session are not.

#### Scenario: Timestamped branch name generation
- **WHEN** user creates a branch with name "refactoring-approach"
- **AND** current time is 2026-01-28 15:30:42
- **THEN** the branch name SHALL be "20260128153042-refactoring-approach"
- **AND** the branch directory SHALL be `branches/20260128153042-refactoring-approach/`

#### Scenario: User-provided name used as-is
- **WHEN** user provides a branch name "Feature123"
- **THEN** the branch name SHALL be "<timestamp>-Feature123"
- **AND** the name is NOT automatically slugified or lowercased
- **AND** users are responsible for providing filesystem-safe names

#### Scenario: Main branch reserved name
- **WHEN** a session is created via `jf/gptel-persistent-session`
- **THEN** the initial branch SHALL be named "main"
- **AND** have directory path `branches/main/`
- **AND** subsequent branches SHALL use timestamped naming

#### Scenario: Branch name collision prevention
- **WHEN** creating multiple branches with the same user-provided name
- **THEN** each branch receives a unique timestamp prefix
- **AND** no two branches within a session can have identical names
- **AND** branches are distinguishable by creation time

#### Scenario: Chronological branch ordering
- **WHEN** listing branches in a session directory
- **THEN** sorting branch names alphabetically also sorts them chronologically
- **AND** enables easy identification of branch creation sequence

### Requirement: Branch lineage tracking

The system SHALL maintain parent-child relationships between branches via branch-metadata.yml.

Branch metadata SHALL record:
- `parent_branch` - Name of the branch from which this branch was created
- `created` - ISO8601 timestamp of branch creation
- `branch_point_position` - Buffer position where the branch diverged (optional)

The main branch SHALL NOT have a branch-metadata.yml file (no parent).

#### Scenario: Child branch metadata
- **WHEN** creating a branch "20260128153042-feature" from parent "main" at position 5420
- **THEN** the new branch's branch-metadata.yml SHALL contain:
  ```yaml
  parent_branch: main
  created: 2026-01-28T15:30:42Z
  branch_point_position: 5420
  ```

#### Scenario: Main branch has no parent
- **WHEN** examining the "main" branch directory
- **THEN** there SHALL NOT be a branch-metadata.yml file
- **AND** main is considered the root of the branch tree

#### Scenario: Branch from child branch
- **WHEN** creating a branch from an existing child branch "20260128153042-feature"
- **THEN** the new branch's parent_branch SHALL be "20260128153042-feature"
- **AND** enable arbitrary depth branch trees (grandchildren, etc.)

#### Scenario: Lineage tracing
- **WHEN** reading branch-metadata.yml for a branch
- **THEN** the system can determine the immediate parent
- **AND** recursively traverse to find the root (main or ancestor branch)

### Requirement: Agent replication

The system SHALL replicate agent state from the parent branch to the new branch.

Agent replication SHALL:
1. Identify all agent subdirectories in the parent branch's `agents/` directory
2. Recursively copy each agent directory to the new branch's `agents/` directory
3. Preserve agent state files (output files, context, metadata)

**Current implementation (MVP):** Copies ALL agent directories from parent branch.

**Future enhancement:** Copy only agents invoked before the branch point position.

#### Scenario: Copying all agent directories
- **WHEN** creating a branch from a parent with agents `agents/search-agent/` and `agents/code-generator/`
- **THEN** both agent directories SHALL be recursively copied to the new branch
- **AND** preserve all files within each agent directory
- **AND** maintain directory structure and permissions

#### Scenario: Empty agents directory
- **WHEN** the parent branch has no agents/ directory or it is empty
- **THEN** the new branch SHALL have an empty agents/ directory
- **AND** NOT fail branch creation

#### Scenario: Agent state isolation
- **WHEN** an agent writes output in a child branch
- **THEN** the agent state SHALL be isolated to that branch's `agents/` directory
- **AND** NOT affect the parent branch or sibling branches

#### Scenario: Future optimization - selective agent copying
- **WHEN** the system implements selective agent copying (post-MVP)
- **THEN** only agents invoked before the branch point SHALL be copied
- **AND** reduce unnecessary state replication
- **AND** maintain agent isolation guarantees

### Requirement: Configuration inheritance via drawer

The system SHALL inherit the parent branch's preset, scope, and session-level configuration into the new branch via the file-level `:PROPERTIES:` drawer carried at `point-min` of the parent's `session.org`.

Configuration inheritance SHALL:
- Copy the parent's `session.org` content (drawer + conversation history) verbatim up to the branch point — see "Requirement: Context truncation"
- Preserve all inherited drawer keys (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:` when present, the upstream-compatible chat-mode snapshot keys, and the `:GPTEL_SCOPE_*:` keys) in the new branch's `session.org`
- Set the new branch's own identity keys in its drawer: `:GPTEL_SESSION_ID:` equal to the shared session id, and `:GPTEL_BRANCH:` equal to the new branch name (the inherited `:GPTEL_BRANCH:` from the copied parent content SHALL be overwritten with the new branch's name)
- NOT write any session-level sidecar (`scope.yml`, `metadata.yml`, `scope-plan.yml` are dead — see sessions-persistence.md)

Subsequent changes to the drawer in either branch SHALL NOT affect the other, since each branch owns its own `session.org`.

The branch lineage (parent branch name, branch point position) is the only data the new branch records SEPARATELY from the drawer — it lives in `branch-metadata.yml` and is documented in "Requirement: Branch metadata" below.

**Session identity** lives in the drawer (`:GPTEL_SESSION_ID:`), not in the directory path. All branches of one session share the same `:GPTEL_SESSION_ID:` value; they are distinguished by their per-branch `:GPTEL_BRANCH:`. (The shared `<session-dir>` remains a storage convention but is no longer the source of session identity.)

#### Scenario: Drawer is preserved verbatim across branch creation
- **WHEN** creating a new branch from a parent whose `session.org` carries `:GPTEL_PRESET: executor`, `:GPTEL_SCOPE_READ:` patterns, and chat-mode snapshot keys
- **THEN** the new branch's `session.org` SHALL contain the same `:PROPERTIES:` drawer at `point-min` with all those configuration keys intact
- **AND** no separate sidecar file SHALL be written for preset or scope state
- **AND** subsequent drawer edits in either branch SHALL NOT propagate to the other

#### Scenario: Session ID consistency via the drawer
- **WHEN** a session has multiple branches (main, feature-1, feature-2)
- **THEN** every branch's `session.org` drawer SHALL carry the same `:GPTEL_SESSION_ID:` value
- **AND** each branch's drawer SHALL carry its own `:GPTEL_BRANCH:` value
- **AND** the registry key `"<session-id>/<branch-name>"` is sourced from those drawer values

### Requirement: User-facing command

The system SHALL provide `jf/gptel-branch-session` as the primary user-facing command for creating branches.

The command SHALL:
1. Verify the current buffer is a gptel session buffer (session-initialized)
2. Invoke branch point selection
3. Prompt for a user-provided branch name
4. Orchestrate branch creation (directory, metadata, context, agents, config)
5. Open the new branch's session.org in a buffer

The command SHALL be interactive and invocable via M-x or keybinding.

#### Scenario: Successful branch creation via command
- **WHEN** user invokes `M-x jf/gptel-branch-session` in an active session buffer
- **THEN** the system prompts for branch point selection
- **AND** prompts for a branch name
- **AND** creates the new branch directory and files
- **AND** opens the new branch's session.org in a buffer
- **AND** logs successful branch creation at INFO level

#### Scenario: Command invoked in non-session buffer
- **WHEN** user invokes `jf/gptel-branch-session` in a buffer that is NOT session-initialized
- **THEN** the system SHALL display an error message
- **AND** NOT attempt branch creation

#### Scenario: Command with optional branch name argument
- **WHEN** invoking `jf/gptel-branch-session` with a programmatic branch name argument
- **THEN** the system SHALL skip the branch name prompt
- **AND** use the provided argument for branch naming

### Requirement: Registry integration

The system SHALL register new branches in the in-memory session registry when the branch buffer is opened.

Registry integration SHALL:
1. Add an entry with key `"<session-id>/<branch-name>"`, sourced from the branch drawer's `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:`
2. Store session-dir, branch-dir, and buffer reference
3. Enable O(1) lookup for branch buffers

The registry enables fast session/branch lookup without filesystem scanning.

**Note:** Branch creation does NOT explicitly register the branch. Registration happens implicitly when the new branch's `session.org` is opened: content-addressed activation flips the buffer to `gptel-chat-mode`, and the `gptel-chat-mode-hook` binder registers it. This ensures the buffer exists before registration and avoids race conditions.

#### Scenario: Implicit registration via buffer opening
- **WHEN** a new branch `20260128153042-feature` is created in session `my-session-20260205`
- **AND** the branch's `session.org` is opened via `find-file`
- **THEN** content-addressed activation flips the buffer to `gptel-chat-mode`
- **AND** the mode-hook binder registers the branch with key `"my-session-20260205/20260128153042-feature"` from the drawer
- **AND** stores the branch directory path and buffer reference

#### Scenario: Registry lookup after branch buffer opened
- **WHEN** the new branch buffer has been opened and bound by the `gptel-chat-mode-hook` binder
- **THEN** the registry entry SHALL be available for lookup
- **AND** enable fast lookup of the branch by session-id and branch-name
- **AND** return the buffer reference for the active branch buffer

### Requirement: Error handling and validation

The system SHALL validate branch creation preconditions and handle errors gracefully.

Validation SHALL ensure:
- Source buffer is session-initialized
- Branch point selection succeeds
- Parent branch has a valid `session.org` with a `:PROPERTIES:` drawer at `point-min`
- Filesystem operations succeed (directory creation, file copying)

Errors SHALL be logged at ERROR level and reported to the user.

#### Scenario: Non-session buffer validation
- **WHEN** attempting to branch from a buffer without session-local variables
- **THEN** the system SHALL abort branch creation
- **AND** display an error message: "Not a gptel session buffer"

#### Scenario: Missing parent session.org
- **WHEN** the parent branch directory lacks a `session.org`, or its `session.org` lacks the file-level `:PROPERTIES:` drawer at `point-min`
- **THEN** the system SHALL abort branch creation
- **AND** log an ERROR identifying the missing file or malformed drawer
- **AND** report failure to the user

#### Scenario: Filesystem error handling
- **WHEN** directory creation fails (permissions, disk space)
- **THEN** the system SHALL log an ERROR with the exception
- **AND** NOT leave partial branch artifacts
- **AND** report failure to the user

#### Scenario: Branch point selection cancellation
- **WHEN** the user cancels branch point selection or branch name input
- **THEN** the system SHALL abort branch creation gracefully
- **AND** NOT create any filesystem artifacts

### Requirement: Logging for branch operations

The system SHALL log branch creation operations for observability.

Branch logging SHALL include:
- **INFO**: Branch creation start, branch directory creation, context truncation summary, agent copying summary, branch creation completion
- **DEBUG**: Individual agent directory copies
- **ERROR**: Validation failures, filesystem errors, metadata parsing errors

Logs SHALL include session-id and branch-name for traceability.

**Note:** Branch registration is NOT logged in branching.el since registration occurs implicitly via the `gptel-chat-mode-hook` binder when the branch buffer is opened. Bounds filtering is NOT explicitly logged.

#### Scenario: Logging branch creation
- **WHEN** creating a branch "20260128153042-feature"
- **THEN** the system logs at INFO level:
  - "Creating branch: 20260128153042-feature from parent: main"
  - "Created branch directory: <path>" (logged by filesystem.el)
  - "Copied truncated context: <chars> -> <position> chars, bounds: <count> types"
  - "Copied <N> agent directories to branch"
  - "Branch created successfully: 20260128153042-feature"

#### Scenario: Logging context truncation
- **WHEN** copying and truncating context to a new branch
- **THEN** the system logs at INFO level (not DEBUG):
  - "Copied truncated context: <source-size> -> <branch-position> chars, bounds: <type-count> types"
- **AND** bounds filtering details are NOT explicitly logged

#### Scenario: Logging agent replication
- **WHEN** copying agent directories to a new branch
- **THEN** the system logs at DEBUG level for each agent:
  - "Copied agent directory: search-agent"
  - "Copied agent directory: code-generator"
- **AND** logs summary at INFO level:
  - "Copied <N> agent directories to branch"

### Requirement: Integration with session persistence

The system SHALL build on session persistence fundamentals, reusing core infrastructure.

Branching SHALL depend on:
- Directory structure (`branches/` subdirectory, `session.org` with file-level `:PROPERTIES:` drawer)
- Lineage format (`branch-metadata.yml` — the only sidecar; preset and scope live in the drawer)
- Path resolution functions (branch-dir-path, context-file-path)
- Registry for session/branch tracking
- Content-addressed activation and the `gptel-chat-mode-hook` binder

Branching SHALL NOT duplicate persistence logic; it orchestrates existing infrastructure.

#### Scenario: Reusing path resolution
- **WHEN** creating branch directories and file paths
- **THEN** the system SHALL use path resolution functions from filesystem.el
- **AND** NOT hardcode file paths or names

#### Scenario: Inheriting preset and scope via session.org copy
- **WHEN** propagating configuration to a new branch
- **THEN** the system SHALL copy the parent's `session.org` content (drawer + body) via the context-truncation step
- **AND** NOT read or write any preset/scope sidecar file (none exist post-drawer-as-source-of-truth)

#### Scenario: Reusing registry for branch tracking
- **WHEN** registering a new branch
- **THEN** the system SHALL use registry functions from registry.el
- **AND** follow the same key format: `"<session-id>/<branch-name>"`

#### Scenario: Reusing content-addressed activation
- **WHEN** a new branch buffer is opened
- **THEN** the `magic-mode-alist` signature and the `gptel-chat-mode-hook` binder SHALL handle activation and binding
- **AND** branching does NOT require a separate initialization path

### Requirement: Load order dependencies

The system SHALL enforce correct module load order for branching functionality.

Branching module SHALL load AFTER:
- Constants (required for file name constants)
- Logging (required for branch operation logging)
- Filesystem (required for path resolution and directory operations)
- Registry (required for branch registration)
- Metadata (required for reading/writing metadata files)

Branching module SHALL load BEFORE:
- Commands (which exposes `jf/gptel-branch-session` to users)

This ordering is enforced in `config/gptel/gptel.org`.

#### Scenario: Module load order validation
- **WHEN** loading the gptel subsystem
- **THEN** branching.el SHALL be loaded after registry.el and metadata.el
- **AND** before commands.el
- **AND** ensure all required functions are available

#### Scenario: Missing dependency detection
- **WHEN** branching.el is loaded without required dependencies
- **THEN** the system SHALL fail with a clear error message
- **AND** indicate which dependency is missing
