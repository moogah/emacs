## MODIFIED Requirements

### Requirement: Session directory structure

The system SHALL maintain a hierarchical directory structure for session storage with the following layout:

```
~/.gptel/sessions/
└── <session-id>/
    ├── branches/
    │   └── <branch-name>/
    │       ├── session.md          (conversation history, markdown)
    │       ├── scope.yml           (mutable scope configuration)
    │       ├── metadata.yml        (session metadata)
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

### Requirement: File format contracts

The system SHALL use the following file formats for session persistence:

**session.md (Conversation History)**
- Format: Markdown with Local Variables footer
- Purpose: Store conversation content in gptel's native format
- Local Variables SHALL include `gptel--preset` (symbol) and any override values
- Location: `<session-dir>/branches/<branch-name>/session.md`

**metadata.yml (Session Metadata)**
- Format: YAML
- Required fields: `session_id`, `created`, `updated`, `preset`
- Optional fields: `type` ("agent" or "branch"), `parent_session_id`
- Location: `<session-dir>/branches/<branch-name>/metadata.yml`

**scope.yml (Scope Configuration)**
- Format: YAML (plain, no frontmatter)
- Sections: `paths` (read/write/deny), `org_roam_patterns`, `shell_commands`
- Mutable: updated by scope expansion during session
- Location: `<session-dir>/branches/<branch-name>/scope.yml`

**branch-metadata.yml (Branch Metadata)**
- Format: YAML
- Fields: `parent_branch`, `created`, `branch_point_position` (optional)
- Location: `<session-dir>/branches/<branch-name>/branch-metadata.yml`

#### Scenario: metadata.yml required fields
- **WHEN** reading session metadata from metadata.yml
- **THEN** the file MUST contain `session_id`, `created`, `updated`, and `preset` fields
- **AND** `session_id` MUST match the session directory name
- **AND** `created` and `updated` MUST be ISO8601 timestamps

#### Scenario: Backward compatibility with scope-plan.yml
- **WHEN** opening a session with `scope-plan.yml` but no `metadata.yml`
- **THEN** the system SHALL read metadata from `scope-plan.yml` as a fallback
- **AND** log a warning suggesting migration

#### Scenario: Backward compatibility with agent_type
- **WHEN** reading an older metadata file that uses `agent_type` instead of `preset`
- **THEN** the system SHALL accept `agent_type` as a fallback for the `preset` field

### Requirement: Session lifecycle - Create

The system SHALL support creating new sessions via `jf/gptel-persistent-session` or activities integration.

On session creation, the system SHALL:
1. Generate a unique session ID
2. Create directory structure: `<session-id>/branches/main/`
3. Apply preset by name via `gptel--apply-preset` with buffer-local setter
4. Create `scope.yml` from preset's scope profile or scope defaults
5. Write metadata.yml with session_id, created timestamp, and preset name
6. Create empty `session.md` file
7. Create `current` symlink pointing to `branches/main`
8. Register the session in the in-memory registry

#### Scenario: Standalone session creation
- **WHEN** user invokes `jf/gptel-persistent-session` with name "API Integration"
- **THEN** the system creates `~/.gptel/sessions/api-integration-<timestamp>/branches/main/`
- **AND** writes metadata.yml with `session_id: "api-integration-<timestamp>"` and `preset: "executor"`
- **AND** creates scope.yml from the preset's scope profile
- **AND** creates empty session.md
- **AND** applies preset via `(gptel--apply-preset 'executor setter)` with buffer-local setter
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
4. Set buffer-local variables: `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`
5. Determine session state: **new** (no Local Variables) vs **existing** (has Local Variables)
6. Apply preset configuration (path depends on session state — see scenarios below)
7. Load scope configuration from `scope.yml`
8. Set `jf/gptel-autosave-enabled` to t

The open hook handles two distinct sub-cases:
- **Existing sessions** (reopened, has Local Variables with `gptel--preset`): Enable gptel-mode, which triggers upstream's `gptel--restore-state` to apply the preset and overlay saved overrides. The session hook SHALL NOT call custom preset loading functions.
- **New sessions** (just created, no Local Variables yet): Apply the preset directly via `gptel--apply-preset` with a buffer-local setter (`(lambda (var val) (set (make-local-variable var) val))`), then enable gptel-mode. On first save, upstream writes `gptel--preset` to Local Variables.

#### Scenario: Reopening existing session (has Local Variables)
- **WHEN** opening a session.md that contains Local Variables footer with `gptel--preset: executor`
- **THEN** the auto-initialization hook detects the session file
- **AND** sets buffer-local session variables
- **AND** enables gptel-mode (triggering `gptel--restore-state` which applies preset from Local Variables)
- **AND** overlays any explicitly saved overrides (temperature, model changes, etc.)
- **AND** loads scope configuration from `scope.yml`
- **AND** enables auto-save
- **AND** the session hook does NOT call custom preset loading functions

#### Scenario: Opening newly created session (no Local Variables)
- **WHEN** opening a session.md that has no Local Variables footer (just created by session creation flow)
- **AND** the session's metadata.yml contains a `preset` field
- **THEN** the hook reads the preset name from metadata.yml
- **AND** applies the preset via `gptel--apply-preset` with buffer-local setter
- **AND** enables gptel-mode
- **AND** loads scope configuration from `scope.yml`
- **AND** on first save, upstream writes `gptel--preset` and `gptel--bounds` to Local Variables

#### Scenario: Opening legacy session with preset.md
- **WHEN** opening a session that has `preset.md` but no `gptel--preset` in Local Variables
- **THEN** the system detects the legacy format
- **AND** reads preset.md YAML frontmatter to determine the preset name (matching against registered presets)
- **AND** applies the preset via `gptel--apply-preset` with buffer-local setter
- **AND** creates `scope.yml` from preset.md's scope sections (paths, org_roam_patterns, shell_commands)
- **AND** enables gptel-mode
- **AND** on next save, upstream writes `gptel--preset` to Local Variables (completing migration)
- **AND** logs a warning indicating the legacy session was auto-migrated

#### Scenario: Legacy session with unknown preset
- **WHEN** opening a legacy session whose preset.md does not match any registered preset name
- **THEN** the system logs a warning with the unmatched preset details
- **AND** falls back to applying settings directly from preset.md (backend, model, tools, system message)
- **AND** does NOT set `gptel--preset` (no matching registered preset)
- **AND** the session operates without upstream preset differential save

#### Scenario: Fast guard for non-session files
- **WHEN** opening a file that is not a session.md
- **THEN** the auto-initialization hook SHALL exit early (before expensive checks)
- **AND** NOT impact file-open performance

### Requirement: Session lifecycle - Save

The system SHALL rely on upstream's differential save for persisting session state.

On save, upstream's `gptel--save-state` SHALL:
1. Write `gptel--preset` as a Local Variable
2. For each setting (model, backend, tools, temperature, etc.), only write a Local Variable if the value differs from the preset (using `gptel--preset-mismatch-value`)
3. System message: only written if it differs from the preset's `:system` value

The session system SHALL NOT apply advice to prevent system message saving. Upstream's `gptel--preset-mismatch-value` handles this automatically — if the system message matches the registered preset, it is omitted; if modified during the session, it is saved as an override. This aligns session behavior with upstream's intended save semantics.

#### Scenario: Manual save via C-x C-s
- **WHEN** user saves a session buffer with C-x C-s
- **THEN** conversation history is written to session.md
- **AND** `gptel--preset` is written as a Local Variable
- **AND** only overridden settings are written as Local Variables
- **AND** system message is NOT written if it matches the preset

#### Scenario: Save after temperature change
- **WHEN** user changes temperature from 0.5 (preset default) to 0.8
- **AND** saves the session
- **THEN** `gptel-temperature: 0.8` is written as a Local Variable
- **AND** `gptel--preset: executor` is written as a Local Variable
- **AND** backend, model, tools, system message are NOT written (match preset)

#### Scenario: Save after system message modification
- **WHEN** an agent or user modifies the system message during a session
- **AND** the modified message differs from the preset's `:system` value
- **THEN** the modified system message IS written as a Local Variable override
- **AND** on next restore, the override takes precedence over the preset's system message

#### Scenario: Save with no changes from preset
- **WHEN** user saves a session without changing any settings from the preset
- **THEN** only `gptel--preset` and `gptel--bounds` are written as Local Variables
- **AND** no other gptel settings are stored

### Requirement: Registry and filesystem separation

The system SHALL maintain a clear separation between in-memory registry and on-disk persistence:

**Registry (Memory)**
- Hash table with keys: `"<session-id>/<branch-name>"`
- Values: Plists containing `:session-id`, `:session-dir`, `:branch-name`, `:branch-dir`, `:buffer`
- Purpose: O(1) lookup and buffer association

**Filesystem (Source of Truth)**
- Directory structure under `~/.gptel/sessions/`
- Metadata files: metadata.yml, scope.yml, branch-metadata.yml
- Conversation history: session.md
- Purpose: Persistent storage and source of truth

Metadata SHALL be read from disk on-demand, NOT cached in the registry.

#### Scenario: Registry lookup performance
- **WHEN** looking up a session by session-id and branch-name
- **THEN** the registry SHALL provide O(1) lookup time
- **AND** return a plist with session-dir, branch-dir, and buffer reference

#### Scenario: Filesystem as source of truth
- **WHEN** the system needs session metadata
- **THEN** it MUST read from disk (metadata.yml, scope.yml)
- **AND** NOT rely on cached values in the registry

### Requirement: Metadata persistence and retrieval

The system SHALL provide on-demand metadata reading from YAML files.

Metadata reading SHALL:
- Parse metadata.yml for session-level metadata (session_id, created, updated, type, parent_session_id, preset)
- Parse scope.yml for scope configuration (paths, org_roam_patterns, shell_commands)
- Parse branch-metadata.yml for branch lineage (parent_branch, created)
- Convert snake_case YAML keys to kebab-case keyword plists
- Return nil for missing or unparseable files
- Fall back to scope-plan.yml if metadata.yml does not exist (backward compatibility)

#### Scenario: Reading session metadata
- **WHEN** the system calls `jf/gptel--read-session-metadata` for a branch directory
- **THEN** it reads metadata.yml and returns a plist
- **AND** converts YAML keys like `session_id` to `:session-id`
- **AND** returns nil if the file is missing or cannot be parsed

#### Scenario: Reading scope configuration
- **WHEN** the system needs scope configuration for a session
- **THEN** it reads scope.yml from the branch directory
- **AND** returns a plist with `:paths`, `:org-roam-patterns`, `:shell-commands`

#### Scenario: Fallback to scope-plan.yml
- **WHEN** metadata.yml does not exist but scope-plan.yml does
- **THEN** the system reads scope-plan.yml for metadata
- **AND** logs a deprecation warning

### Requirement: Path resolution

The system SHALL provide deterministic path construction for all session files.

Key path resolution functions:
- `jf/gptel--branches-dir-path(session-dir)` → `session-dir/branches`
- `jf/gptel--branch-dir-path(session-dir, branch-name)` → `session-dir/branches/branch-name`
- `jf/gptel--context-file-path(branch-dir)` → `branch-dir/session.md`
- `jf/gptel--metadata-file-path(branch-dir)` → `branch-dir/metadata.yml`
- `jf/gptel--scope-file-path(branch-dir)` → `branch-dir/scope.yml`

#### Scenario: Computing scope file path
- **WHEN** the system needs the scope configuration file for a branch
- **THEN** it calls `jf/gptel--scope-file-path(branch-dir)`
- **AND** returns `<branch-dir>/scope.yml`

#### Scenario: Computing metadata file path
- **WHEN** the system needs the metadata file for a branch
- **THEN** it calls `jf/gptel--metadata-file-path(branch-dir)`
- **AND** returns `<branch-dir>/metadata.yml`

#### Scenario: File name constants
- **WHEN** constructing any session file path
- **THEN** the system MUST use constants from `constants.el`
- **AND** NOT hardcode filenames like "scope.yml" in path construction logic

## REMOVED Requirements

### Requirement: preset.md in session directories
**Reason**: Preset configuration is now stored in `gptel--known-presets` (registered at init time) and referenced by name via the `gptel--preset` Local Variable. Upstream's save/restore handles preset application.
**Migration**: Sessions using preset.md are detected by the legacy fallback in the Open lifecycle. The preset name is read from the old preset.md or scope-plan.yml and used to set `gptel--preset`. On next save, the Local Variable is written and preset.md is no longer needed.

### Requirement: System message save prevention
**Reason**: Upstream's differential save (`gptel--preset-mismatch-value`) automatically omits the system message when it matches the registered preset. No advice is needed.
**Migration**: Remove advice on `gptel--save-state` that prevented system message persistence.
