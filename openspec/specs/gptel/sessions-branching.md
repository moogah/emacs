# GPTEL Sessions Branching

Behavioral specification for gptel session branching operations, enabling divergent conversation paths with full context preservation and metadata tracking.

**Scope:** This spec covers branching operations only. Session persistence fundamentals are documented in sessions-persistence.md. PersistentAgent integration is documented separately.

## ADDED Requirements

### Requirement: Branch creation model

The system SHALL support creating new branches from existing branches, enabling divergent conversation paths while preserving shared history.

A branch operation SHALL:
1. Identify a **branch point** - a specific user prompt in the source branch
2. Copy context from the source branch up to (and optionally including) the branch point
3. Create a new branch directory with its own session.md, metadata, and agent state
4. Preserve lineage via branch-metadata.yml tracking parent branch and branch point
5. Update the `current` symlink to point to the newly created branch

Branches SHALL be first-class session objects with independent evolution.

#### Scenario: Branch as first-class session object
- **WHEN** a branch is created from a parent branch
- **THEN** the new branch SHALL have its own directory under `branches/`
- **AND** contain a complete session.md file (not a reference or delta)
- **AND** contain its own scope-plan.yml, preset.md, and branch-metadata.yml
- **AND** be registered independently in the session registry
- **AND** support further branching (branches can have child branches)

#### Scenario: Shared history preservation
- **WHEN** branching from a conversation at a specific user prompt
- **THEN** all conversation history before the branch point SHALL be copied to the new branch
- **AND** both branches share identical history up to the branch point
- **AND** subsequent edits to either branch SHALL NOT affect the other

### Requirement: Branch point selection

The system SHALL provide interactive branch point selection based on user prompts in the conversation.

Branch point selection SHALL:
1. Scan the source buffer for user input regions (text without gptel properties)
2. Present a numbered list of user prompts for selection
3. Allow the user to choose whether to include or exclude the selected prompt in the new branch
4. Return the buffer position marking the branch point

Only user prompts SHALL be valid branch points. Assistant responses and tool outputs SHALL NOT be selectable as branch points.

#### Scenario: Interactive prompt selection
- **WHEN** user invokes `jf/gptel-branch-session` in an active session buffer
- **THEN** the system scans for all user prompts in the buffer
- **AND** presents a numbered selection interface
- **AND** allows the user to select a prompt by number
- **AND** asks whether to include or exclude the selected prompt

#### Scenario: Include selected prompt in branch
- **WHEN** user selects a branch point and chooses to INCLUDE the prompt
- **THEN** the branch point position is the END of the selected prompt region
- **AND** the new branch contains the selected prompt
- **AND** the next assistant response would be after this prompt

#### Scenario: Exclude selected prompt from branch
- **WHEN** user selects a branch point and chooses to EXCLUDE the prompt
- **THEN** the branch point position is the BEGINNING of the selected prompt region
- **AND** the new branch does NOT contain the selected prompt
- **AND** the user can provide a different prompt to continue the conversation

#### Scenario: User prompt detection
- **WHEN** scanning the buffer for user prompts
- **THEN** the system SHALL identify regions without `gptel` text properties
- **AND** exclude assistant responses (regions with gptel="response")
- **AND** exclude tool outputs (regions with gptel property values)

#### Scenario: No valid branch points
- **WHEN** a session buffer contains only assistant responses or is empty
- **THEN** the system SHALL report no available branch points
- **AND** NOT allow branch creation

### Requirement: Context truncation

The system SHALL copy conversation history from the source branch to the new branch, truncating at the selected branch point.

Context truncation SHALL:
1. Copy all buffer content from the beginning up to the branch point position
2. Preserve text properties and formatting
3. Exclude content at or after the branch point position
4. Write truncated content to the new branch's session.md file

Context truncation SHALL operate at the buffer level, not the semantic message level.

#### Scenario: Copying content up to branch point
- **WHEN** creating a branch with a branch point at position 5420
- **THEN** the new branch's session.md SHALL contain buffer content from position 1 to 5419
- **AND** preserve all text properties (gptel markers, tool outputs)
- **AND** exclude all content from position 5420 onward

#### Scenario: Empty branch from early branch point
- **WHEN** the selected branch point is the first user prompt
- **AND** the user chooses to exclude the prompt
- **THEN** the new branch's session.md SHALL be empty or contain only initial setup
- **AND** represent a fresh conversation starting point

#### Scenario: Full context copy for late branch point
- **WHEN** the selected branch point is near the end of a long conversation
- **THEN** the new branch SHALL contain nearly all conversation history
- **AND** diverge only at the final exchanges

### Requirement: Bounds filtering

The system SHALL filter gptel--bounds to match the truncated context, ensuring consistency between conversation content and metadata.

Bounds filtering SHALL:
1. Preserve the gptel--bounds data structure (alist of type keys to region lists)
2. Include only bounds regions that START before the branch point position
3. Exclude regions that start at or after the branch point position
4. Validate bounds structure before and after filtering

gptel--bounds tracks positions of assistant responses, tool outputs, and other marked regions in the conversation buffer. The structure is an alist where each entry is `(type . regions)` and each region is `(start-pos end-pos)` or `(start-pos end-pos id)`.

#### Scenario: Filtering bounds before branch point
- **WHEN** creating a branch with branch point at position 5420
- **AND** source buffer has gptel--bounds:
  ```elisp
  ((response . ((1000 2000) (3000 4000) (6000 7000)))
   (tool . ((2500 2800))))
  ```
- **THEN** the filtered bounds SHALL be:
  ```elisp
  ((response . ((1000 2000) (3000 4000)))
   (tool . ((2500 2800))))
  ```
- **AND** the response region (6000 7000) is excluded (starts after branch point)
- **AND** type keys are preserved in the alist structure

#### Scenario: Partial region exclusion
- **WHEN** a bounds region overlaps the branch point (starts before, ends after)
- **THEN** the region SHALL be INCLUDED if it starts before the branch point
- **AND** the filtering only checks the start position, not the end position
- **NOTE:** Current implementation includes partial overlaps (only checks start position)

#### Scenario: Bounds validation
- **WHEN** filtering bounds for a new branch
- **THEN** the system SHALL validate bounds structure before filtering
- **AND** validate again after filtering
- **AND** ensure all regions are (start-pos end-pos) pairs

#### Scenario: Empty bounds result
- **WHEN** all bounds regions start at or after the branch point
- **THEN** the filtered bounds SHALL be an empty list
- **AND** represent a branch with no assistant responses or tool outputs yet

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

### Requirement: Configuration inheritance

The system SHALL copy configuration files from the parent branch to the new branch.

Configuration inheritance SHALL copy:
- `scope-plan.yml` - Session metadata (session_id, created, type, preset)
- `preset.md` - Model configuration and system message

Configuration files SHALL be copied as-is, preserving content exactly.

The new branch's scope-plan.yml SHALL maintain the same `session_id`, ensuring all branches share a common session identifier.

#### Scenario: Copying scope-plan.yml
- **WHEN** creating a new branch from a parent
- **THEN** the parent's scope-plan.yml SHALL be copied to the new branch directory
- **AND** the session_id field SHALL remain unchanged
- **AND** the created and updated timestamps SHALL be inherited

#### Scenario: Copying preset.md
- **WHEN** creating a new branch from a parent
- **THEN** the parent's preset.md SHALL be copied to the new branch directory
- **AND** the system message and model configuration SHALL be identical
- **AND** subsequent changes to preset in either branch SHALL NOT affect the other

#### Scenario: Session ID consistency
- **WHEN** a session has multiple branches (main, feature-1, feature-2)
- **THEN** all branches SHALL have the same session_id in their scope-plan.yml
- **AND** enable identification of branches belonging to the same logical session

### Requirement: Symlink management

The system SHALL update the `current` symlink to point to the newly created branch upon successful branch creation.

Symlink update SHALL:
1. Remove or overwrite the existing `current` symlink in the session directory
2. Create a new symlink pointing to `branches/<new-branch-name>` (relative path)
3. Enable the session to track which branch is currently active

The current symlink enables tools to reference the active branch without hardcoding branch names.

#### Scenario: Updating current symlink on branch creation
- **WHEN** creating a new branch "20260128153042-feature"
- **THEN** the `current` symlink SHALL be updated to point to `branches/20260128153042-feature`
- **AND** replace any previous current symlink value

#### Scenario: Relative symlink path
- **WHEN** creating the current symlink
- **THEN** it SHALL use a relative path (e.g., `branches/feature-name`)
- **AND** NOT use an absolute path
- **AND** enable session directory portability

#### Scenario: Current symlink consistency
- **WHEN** opening a session via the `current` symlink path
- **THEN** the system SHALL resolve to the active branch
- **AND** initialize the buffer with that branch's context

### Requirement: User-facing command

The system SHALL provide `jf/gptel-branch-session` as the primary user-facing command for creating branches.

The command SHALL:
1. Verify the current buffer is a gptel session buffer (session-initialized)
2. Invoke branch point selection
3. Prompt for a user-provided branch name
4. Orchestrate branch creation (directory, metadata, context, agents, config)
5. Open the new branch's session.md in a buffer

The command SHALL be interactive and invocable via M-x or keybinding.

#### Scenario: Successful branch creation via command
- **WHEN** user invokes `M-x jf/gptel-branch-session` in an active session buffer
- **THEN** the system prompts for branch point selection
- **AND** prompts for a branch name
- **AND** creates the new branch directory and files
- **AND** opens the new branch's session.md in a buffer
- **AND** logs successful branch creation at INFO level

#### Scenario: Command invoked in non-session buffer
- **WHEN** user invokes `jf/gptel-branch-session` in a buffer that is NOT session-initialized
- **THEN** the system SHALL display an error message
- **AND** NOT attempt branch creation

#### Scenario: Command with optional branch name argument
- **WHEN** invoking `jf/gptel-branch-session` with a programmatic branch name argument
- **THEN** the system SHALL skip the branch name prompt
- **AND** use the provided argument for branch naming

### Requirement: Auto-initialization of new branches

The system SHALL auto-initialize new branch buffers when opened via find-file-hook.

Auto-initialization for branches SHALL:
1. Detect files matching pattern `*/branches/*/session.md`
2. Extract session-id and branch-name from the file path
3. Set buffer-local variables (session-id, session-dir, branch-name, branch-dir)
4. Load preset configuration from preset.md
5. Enable gptel-mode and auto-save

This behavior is inherited from the persistence system's find-file-hook.

#### Scenario: Opening new branch via find-file
- **WHEN** the user opens `~/.gptel/sessions/my-session/branches/20260128153042-feature/session.md`
- **THEN** the find-file-hook SHALL trigger auto-initialization
- **AND** extract session-id "my-session" and branch-name "20260128153042-feature"
- **AND** set buffer-local session variables
- **AND** enable gptel-mode

#### Scenario: Opening new branch via branch creation command
- **WHEN** `jf/gptel-branch-session` completes branch creation
- **THEN** it opens the new branch file with `find-file`
- **AND** triggers auto-initialization automatically
- **AND** the user sees the new branch ready for interaction

### Requirement: Registry integration

The system SHALL register new branches in the in-memory session registry when the branch buffer is opened.

Registry integration SHALL:
1. Add an entry with key `"<session-id>/<branch-name>"`
2. Store session-dir, branch-dir, and buffer reference
3. Enable O(1) lookup for branch buffers

The registry enables fast session/branch lookup without filesystem scanning.

**Note:** Branch creation in `branching.el` does NOT explicitly register the branch. Registration happens implicitly when the new branch's `session.md` file is opened, triggering the find-file-hook in `commands.el` which calls auto-initialization. This ensures the buffer exists before registration and avoids race conditions.

#### Scenario: Implicit registration via buffer opening
- **WHEN** a new branch "20260128153042-feature" is created in session "my-session-20260205"
- **AND** the branch's session.md file is opened via `find-file`
- **THEN** the find-file-hook triggers auto-initialization
- **AND** auto-initialization registers the branch with key `"my-session-20260205/20260128153042-feature"`
- **AND** stores the branch directory path and buffer reference

#### Scenario: Registry lookup after branch buffer opened
- **WHEN** the new branch buffer has been opened and auto-initialized
- **THEN** the registry entry SHALL be available for lookup
- **AND** enable fast lookup of the branch by session-id and branch-name
- **AND** return the buffer reference for the active branch buffer

### Requirement: Error handling and validation

The system SHALL validate branch creation preconditions and handle errors gracefully.

Validation SHALL ensure:
- Source buffer is session-initialized
- Branch point selection succeeds
- Parent branch has required metadata files (scope-plan.yml, preset.md)
- Filesystem operations succeed (directory creation, file copying)

Errors SHALL be logged at ERROR level and reported to the user.

#### Scenario: Non-session buffer validation
- **WHEN** attempting to branch from a buffer without session-local variables
- **THEN** the system SHALL abort branch creation
- **AND** display an error message: "Not a gptel session buffer"

#### Scenario: Missing parent metadata
- **WHEN** the parent branch is missing scope-plan.yml or preset.md
- **THEN** the system SHALL abort branch creation
- **AND** log an ERROR with details of missing files
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

**Note:** Branch registration is NOT logged in branching.el since registration occurs implicitly via find-file-hook in commands.el. Bounds filtering is NOT explicitly logged.

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
- Directory structure (branches/ subdirectory, session.md, metadata files)
- Metadata formats (scope-plan.yml, preset.md, branch-metadata.yml)
- Path resolution functions (branch-dir-path, context-file-path)
- Registry for session/branch tracking
- Auto-initialization via find-file-hook

Branching SHALL NOT duplicate persistence logic; it orchestrates existing infrastructure.

#### Scenario: Reusing path resolution
- **WHEN** creating branch directories and file paths
- **THEN** the system SHALL use path resolution functions from filesystem.el
- **AND** NOT hardcode file paths or names

#### Scenario: Reusing metadata functions
- **WHEN** copying configuration to a new branch
- **THEN** the system SHALL use metadata reading/writing functions from metadata.el
- **AND** NOT reimplement YAML parsing

#### Scenario: Reusing registry for branch tracking
- **WHEN** registering a new branch
- **THEN** the system SHALL use registry functions from registry.el
- **AND** follow the same key format: `"<session-id>/<branch-name>"`

#### Scenario: Reusing auto-initialization
- **WHEN** a new branch buffer is opened
- **THEN** the existing find-file-hook from commands.el SHALL handle initialization
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
