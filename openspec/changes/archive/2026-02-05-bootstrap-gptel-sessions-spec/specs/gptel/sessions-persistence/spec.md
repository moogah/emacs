# GPTEL Sessions Persistence

Behavioral specification for gptel session lifecycle management, file-based persistence, metadata tracking, and auto-save behavior.

**Scope:** This spec covers persistence fundamentals only. Branching operations and PersistentAgent integration are documented in separate specs.

## ADDED Requirements

### Requirement: Session directory structure

The system SHALL maintain a hierarchical directory structure for session storage with the following layout:

```
~/.gptel/sessions/
└── <session-id>/
    ├── branches/
    │   └── <branch-name>/
    │       ├── session.md          (conversation history, markdown)
    │       ├── scope-plan.yml      (session metadata)
    │       ├── preset.md           (configuration with YAML frontmatter)
    │       ├── branch-metadata.yml (branch-specific metadata)
    │       ├── tools.org           (tool execution log, org-mode)
    │       └── system-prompts.org  (system prompt change log, org-mode)
    └── current → branches/<branch-name>  (symlink to active branch)
```

The root directory SHALL be configurable via `jf/gptel-sessions-directory`.

#### Scenario: Valid session directory structure
- **WHEN** a session exists on disk
- **THEN** it MUST contain a `branches/` subdirectory
- **AND** at least one branch directory under `branches/`
- **AND** each branch directory MUST contain `session.md`

#### Scenario: Current branch symlink
- **WHEN** a session has an active branch
- **THEN** the `current` symlink MUST point to `branches/<branch-name>` using a relative path
- **AND** following the symlink SHALL resolve to the active branch directory

### Requirement: Session identification

The system SHALL generate unique session identifiers using the format `<slug>-<timestamp>` where:
- `<slug>` is a URL-friendly version of the session name
- `<timestamp>` is in YYYYMMDDHHMMSS format

Session IDs SHALL be immutable after creation.

#### Scenario: Session ID generation
- **WHEN** creating a new session with name "React Refactoring"
- **THEN** the system generates an ID like `react-refactoring-20260205143052`
- **AND** the ID uniquely identifies the session for its lifetime

#### Scenario: Session ID immutability
- **WHEN** a session is created with a specific ID
- **THEN** that ID MUST NOT change even if the session name or metadata is modified

### Requirement: File format contracts

The system SHALL use the following file formats for session persistence:

**session.md (Conversation History)**
- Format: Markdown
- Purpose: Store conversation content in gptel's native format
- Location: `<session-dir>/branches/<branch-name>/session.md`

**scope-plan.yml (Session Metadata)**
- Format: YAML
- Required fields: `session_id`, `created`, `updated`, `preset`
- Optional fields: `type` ("agent" or "branch"), `parent_session_id`
- Location: `<session-dir>/branches/<branch-name>/scope-plan.yml`

**preset.md (Configuration)**
- Format: Markdown with YAML frontmatter
- Frontmatter fields: `backend`, `model`, `temperature`, `include-tool-results`, `tools`
- Body: System message content
- Location: `<session-dir>/branches/<branch-name>/preset.md`

**branch-metadata.yml (Branch Metadata)**
- Format: YAML
- Fields: `parent_branch`, `created`, `branch_point_position` (optional)
- Location: `<session-dir>/branches/<branch-name>/branch-metadata.yml`

#### Scenario: scope-plan.yml required fields
- **WHEN** reading session metadata from scope-plan.yml
- **THEN** the file MUST contain `session_id`, `created`, `updated`, and `preset` fields
- **AND** `session_id` MUST match the session directory name
- **AND** `created` and `updated` MUST be ISO8601 timestamps

#### Scenario: preset.md YAML frontmatter
- **WHEN** reading configuration from preset.md
- **THEN** the YAML frontmatter MUST contain `backend` and `model` fields
- **AND** the markdown body contains the system message

#### Scenario: Backward compatibility with agent_type
- **WHEN** reading an older scope-plan.yml that uses `agent_type` instead of `preset`
- **THEN** the system SHALL accept `agent_type` as a fallback for the `preset` field

### Requirement: Registry and filesystem separation

The system SHALL maintain a clear separation between in-memory registry and on-disk persistence:

**Registry (Memory)**
- Hash table with keys: `"<session-id>/<branch-name>"`
- Values: Plists containing `:session-id`, `:session-dir`, `:branch-name`, `:branch-dir`, `:buffer`
- Purpose: O(1) lookup and buffer association

**Filesystem (Source of Truth)**
- Directory structure under `~/.gptel/sessions/`
- Metadata files: scope-plan.yml, preset.md, branch-metadata.yml
- Conversation history: session.md
- Purpose: Persistent storage and source of truth

Metadata SHALL be read from disk on-demand, NOT cached in the registry.

#### Scenario: Registry lookup performance
- **WHEN** looking up a session by session-id and branch-name
- **THEN** the registry SHALL provide O(1) lookup time
- **AND** return a plist with session-dir, branch-dir, and buffer reference

#### Scenario: Filesystem as source of truth
- **WHEN** the system needs session metadata
- **THEN** it MUST read from disk (scope-plan.yml, preset.md)
- **AND** NOT rely on cached values in the registry

#### Scenario: Registry initialization from disk
- **WHEN** Emacs starts or registry is refreshed
- **THEN** the system SHALL scan `~/.gptel/sessions/` for all session directories
- **AND** discover all branches within each session
- **AND** populate the registry with session-id, branch-name, and paths
- **AND** initialize buffer references to nil

### Requirement: Session lifecycle - Create

The system SHALL support creating new sessions via `jf/gptel-persistent-session` or activities integration.

On session creation, the system SHALL:
1. Generate a unique session ID
2. Create directory structure: `<session-id>/branches/main/`
3. Copy preset template to `preset.md`
4. Write scope-plan.yml with session_id, created timestamp, and preset name
5. Create empty `session.md` file
6. Create `current` symlink pointing to `branches/main`
7. Register the session in the in-memory registry

#### Scenario: Standalone session creation
- **WHEN** user invokes `jf/gptel-persistent-session` with name "API Integration"
- **THEN** the system creates `~/.gptel/sessions/api-integration-<timestamp>/branches/main/`
- **AND** writes scope-plan.yml with `session_id: "api-integration-<timestamp>"`
- **AND** copies the default preset template to preset.md
- **AND** creates empty session.md
- **AND** creates symlink `current → branches/main`
- **AND** opens session.md in a buffer

#### Scenario: Activities-integrated session creation
- **WHEN** creating a session via `activities-ext-create`
- **THEN** the session SHALL be created at `~/emacs-activities/<activity-slug-date>/session/`
- **AND** follow the same directory structure with `branches/main/`
- **AND** be automatically registered and opened

### Requirement: Session lifecycle - Open

The system SHALL auto-initialize session buffers when opening session.md files via find-file-hook.

Auto-initialization SHALL:
1. Detect files matching pattern `*/branches/*/session.md`
2. Extract session-id and branch-name from path
3. Look up session in registry (or create entry if missing)
4. Load preset configuration from preset.md
5. Set buffer-local variables: `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`
6. Enable gptel-mode
7. Set `jf/gptel-autosave-enabled` to t

#### Scenario: Opening existing session via find-file
- **WHEN** user opens `~/.gptel/sessions/my-session-20260205/branches/main/session.md`
- **THEN** the auto-initialization hook detects the session file
- **AND** extracts session-id "my-session-20260205" and branch-name "main"
- **AND** loads preset configuration from preset.md
- **AND** sets buffer-local session variables
- **AND** enables gptel-mode
- **AND** enables auto-save

#### Scenario: Opening session with Local Variables
- **WHEN** opening an existing session.md that contains Local Variables footer
- **THEN** the system loads the preset system message
- **AND** applies buffer-local settings from Local Variables
- **AND** does NOT duplicate system message in Local Variables

#### Scenario: Fast guard for non-session files
- **WHEN** opening a file that is not a session.md
- **THEN** the auto-initialization hook SHALL exit early (before expensive checks)
- **AND** NOT impact file-open performance

### Requirement: Session lifecycle - Save

The system SHALL persist session state when the buffer is saved.

On save, the system SHALL:
1. Trigger `gptel--save-state` hook to persist conversation history to session.md
2. Write Local Variables footer with buffer-local settings (gptel-backend, gptel-model, gptel-tools)
3. NOT persist system message in Local Variables (managed via preset.md only)
4. NOT update scope-plan.yml (metadata is immutable except for `updated` timestamp)

#### Scenario: Manual save via C-x C-s
- **WHEN** user saves a session buffer with C-x C-s
- **THEN** conversation history is written to session.md
- **AND** Local Variables footer is updated with current buffer settings
- **AND** system message is NOT included in Local Variables
- **AND** scope-plan.yml is NOT modified

#### Scenario: System message isolation
- **WHEN** saving a session with a large system message (>4000 chars)
- **THEN** the system MUST NOT write the system message to Local Variables
- **AND** prevent duplicate Local Variables blocks
- **AND** keep system message managed via preset.md only

### Requirement: Auto-save behavior

The system SHALL support automatic saving of session buffers after idle time.

Auto-save SHALL be controlled by:
- `jf/gptel-autosave-idle-time` (default 0.5 seconds) - idle delay before auto-save
- `jf/gptel-autosave-enabled` (buffer-local) - flag controlling whether auto-save is active

Auto-save SHALL be enabled automatically when a session buffer is initialized.

#### Scenario: Auto-save after idle time
- **WHEN** a session buffer is modified and Emacs is idle for 0.5 seconds
- **THEN** the system automatically saves the buffer
- **AND** triggers the same save behavior as manual save

#### Scenario: Disabling auto-save
- **WHEN** user sets `jf/gptel-autosave-idle-time` to 0
- **THEN** auto-save SHALL be disabled
- **AND** only manual saves (C-x C-s) persist changes

#### Scenario: Auto-save buffer-local control
- **WHEN** `jf/gptel-autosave-enabled` is nil for a buffer
- **THEN** that buffer SHALL NOT auto-save even if global idle time is configured

### Requirement: Metadata persistence and retrieval

The system SHALL provide on-demand metadata reading from YAML files.

Metadata reading SHALL:
- Parse scope-plan.yml for session-level metadata (session_id, created, updated, type, parent_session_id, preset)
- Parse preset.md YAML frontmatter for configuration (backend, model, temperature, tools)
- Parse branch-metadata.yml for branch lineage (parent_branch, created)
- Convert snake_case YAML keys to kebab-case keyword plists
- Return nil for missing or unparseable files

#### Scenario: Reading session metadata
- **WHEN** the system calls `jf/gptel--read-session-metadata` for a branch directory
- **THEN** it reads scope-plan.yml and returns a plist
- **AND** converts YAML keys like `session_id` to `:session-id`
- **AND** returns nil if the file is missing or cannot be parsed

#### Scenario: Reading preset configuration
- **WHEN** the system calls `jf/gptel--read-preset-metadata` for a branch directory
- **THEN** it reads the YAML frontmatter from preset.md
- **AND** returns a plist with `:backend` and `:model`
- **AND** returns nil if the file is missing

#### Scenario: Type checking for agent sessions
- **WHEN** the system calls `jf/gptel--is-agent-session-p` for a session
- **THEN** it reads scope-plan.yml and checks if `:type` equals "agent"
- **AND** returns t if type is "agent", nil otherwise

### Requirement: Session discovery

The system SHALL support discovering all sessions and branches on disk.

Discovery operations SHALL:
- List all session directories under `~/.gptel/sessions/`
- List all branch directories within a session
- Filter out hidden files and invalid directories
- Validate directory structure (sessions must have `branches/`, branches must have `session.md`)

#### Scenario: Discovering all sessions
- **WHEN** the system calls `jf/gptel--list-session-directories`
- **THEN** it returns a list of absolute paths to all valid session directories
- **AND** excludes hidden directories (starting with ".")
- **AND** excludes directories lacking a `branches/` subdirectory

#### Scenario: Discovering branches within a session
- **WHEN** the system calls `jf/gptel--list-branches` for a session directory
- **THEN** it returns a list of branch names (e.g., "main", "20260128153042-feature")
- **AND** excludes hidden directories and non-directories

#### Scenario: Comprehensive branch discovery with agents
- **WHEN** the system calls `jf/gptel--find-all-branches-with-agents`
- **THEN** it scans all sessions and all branches
- **AND** returns plists with `:session-dir`, `:session-id`, `:branch-dir`, `:branch-name`, `:agent-dirs`
- **AND** discovers agent subdirectories within each branch's `agents/` directory

### Requirement: Logging and observability

The system SHALL provide leveled logging for session operations with four levels: debug, info, warn, error.

Logging SHALL:
- Be controlled by `jf/gptel-log-level` (default: debug)
- Write to Emacs `*Messages*` buffer (always)
- Optionally write to `<session-dir>/session.log` when `jf/gptel-log-to-file` is enabled
- Prefix messages with timestamp (for file log) and level (e.g., `[GPTEL-INFO]`)

Key logged events:
- **INFO**: Session/branch registration, directory creation, preset application
- **DEBUG**: Current symlink updates, individual branch discovery, buffer initialization
- **WARN**: Existing directory overwrites, missing metadata files, duplicate hooks
- **ERROR**: Metadata parsing failures, session initialization errors

#### Scenario: Logging session creation
- **WHEN** creating a new session directory
- **THEN** the system logs at INFO level: "Created session directory: <path>"

#### Scenario: Logging registry initialization
- **WHEN** initializing the registry from disk
- **THEN** the system logs at INFO level: "Initialized registry with N branches"

#### Scenario: Logging metadata parsing errors
- **WHEN** scope-plan.yml cannot be parsed
- **THEN** the system logs at ERROR level: "Failed to parse scope-plan.yml in <dir>: <error>"

#### Scenario: Logging level filtering
- **WHEN** `jf/gptel-log-level` is set to `info`
- **THEN** DEBUG messages SHALL NOT be logged
- **AND** INFO, WARN, and ERROR messages SHALL be logged

### Requirement: Defensive behaviors

The system SHALL detect and correct common configuration issues.

Defensive behaviors SHALL include:
- Detecting duplicate save hooks in `gptel--save-state`
- Cleaning duplicate Local Variables blocks in session.md
- Validating directory structure before operations

#### Scenario: Duplicate save hook detection
- **WHEN** a buffer has multiple `gptel--save-state` hooks registered
- **THEN** the system logs a WARNING with the count of duplicates
- **AND** removes the duplicate hooks
- **AND** retains only one save hook

#### Scenario: Duplicate Local Variables cleanup
- **WHEN** session.md contains multiple Local Variables blocks
- **THEN** the cleanup utility removes duplicates
- **AND** retains only the last valid Local Variables block

#### Scenario: Directory structure validation
- **WHEN** checking if a path is a valid session directory
- **THEN** the system verifies it contains a `branches/` subdirectory
- **AND** returns nil if the structure is invalid

### Requirement: Buffer-local state variables

The system SHALL maintain buffer-local state to track session context in each gptel buffer.

Required buffer-local variables:
- `jf/gptel--session-id` - Unique session identifier
- `jf/gptel--session-dir` - Absolute path to session directory
- `jf/gptel--branch-name` - Current branch name (e.g., "main")
- `jf/gptel--branch-dir` - Absolute path to branch directory
- `jf/gptel-autosave-enabled` - Auto-save activation flag

All five variables SHALL be set together during session initialization.

#### Scenario: Buffer initialization sets all variables
- **WHEN** auto-initialization runs for a session buffer
- **THEN** all five buffer-local variables MUST be set
- **AND** `jf/gptel--session-id` matches the session directory name
- **AND** `jf/gptel--session-dir` is the absolute path to the session
- **AND** `jf/gptel--branch-name` matches the branch directory name
- **AND** `jf/gptel--branch-dir` is the absolute path to the branch
- **AND** `jf/gptel-autosave-enabled` is set to t

#### Scenario: Buffer-local variables mark session initialization
- **WHEN** all five buffer-local variables are set in a buffer
- **THEN** the buffer is considered "session-initialized"
- **AND** subsequent auto-init hook invocations SHALL skip re-initialization

### Requirement: Path resolution

The system SHALL provide deterministic path construction for all session files.

Path resolution SHALL:
- Use computed paths (never hardcoded)
- Accept directory paths as input, return absolute paths
- Use constants for file names (session.md, scope-plan.yml, preset.md, etc.)

Key path resolution functions:
- `jf/gptel--branches-dir-path(session-dir)` → `session-dir/branches`
- `jf/gptel--branch-dir-path(session-dir, branch-name)` → `session-dir/branches/branch-name`
- `jf/gptel--context-file-path(branch-dir)` → `branch-dir/session.md`
- `jf/gptel--scope-plan-file-path(branch-dir)` → `branch-dir/scope-plan.yml`
- `jf/gptel--preset-file-path(branch-dir)` → `branch-dir/preset.md`

#### Scenario: Computing branch directory path
- **WHEN** the system needs the branch directory path for session "my-session" and branch "main"
- **THEN** it calls `jf/gptel--branch-dir-path(session-dir, "main")`
- **AND** returns `<session-dir>/branches/main` as an absolute path

#### Scenario: Computing context file path
- **WHEN** the system needs the conversation history file for a branch
- **THEN** it calls `jf/gptel--context-file-path(branch-dir)`
- **AND** returns `<branch-dir>/session.md`

#### Scenario: File name constants
- **WHEN** constructing any session file path
- **THEN** the system MUST use constants from `constants.el`
- **AND** NOT hardcode filenames like "session.md" in path construction logic
