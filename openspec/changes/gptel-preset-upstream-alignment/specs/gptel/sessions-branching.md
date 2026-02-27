## MODIFIED Requirements

### Requirement: Configuration inheritance

The system SHALL copy configuration files from the parent branch to the new branch.

Configuration inheritance SHALL copy:
- `metadata.yml` - Session metadata (session_id, created, type, preset)
- `scope.yml` - Mutable scope configuration

Configuration files SHALL be copied as-is, preserving content exactly.

The new branch's metadata.yml SHALL maintain the same `session_id`, ensuring all branches share a common session identifier.

#### Scenario: Copying metadata.yml
- **WHEN** creating a new branch from a parent
- **THEN** the parent's metadata.yml SHALL be copied to the new branch directory
- **AND** the session_id field SHALL remain unchanged
- **AND** the created and updated timestamps SHALL be inherited

#### Scenario: Copying scope.yml
- **WHEN** creating a new branch from a parent
- **THEN** the parent's scope.yml SHALL be copied to the new branch directory
- **AND** scope permissions (paths, org-roam patterns, shell commands) are inherited
- **AND** subsequent scope expansion in either branch SHALL NOT affect the other

#### Scenario: Session ID consistency
- **WHEN** a session has multiple branches (main, feature-1, feature-2)
- **THEN** all branches SHALL have the same session_id in their metadata.yml
- **AND** enable identification of branches belonging to the same logical session

#### Scenario: Backward compatibility with legacy config files
- **WHEN** creating a branch from a parent that has scope-plan.yml and preset.md (legacy format)
- **THEN** the system copies scope-plan.yml and preset.md as-is (preserving legacy format)
- **AND** does NOT attempt to migrate during branch creation

### Requirement: Auto-initialization of new branches

The system SHALL auto-initialize new branch buffers when opened via find-file-hook.

Auto-initialization for branches SHALL:
1. Detect files matching pattern `*/branches/*/session.md`
2. Extract session-id and branch-name from the file path
3. Set buffer-local variables (session-id, session-dir, branch-name, branch-dir)
4. Enable gptel-mode (which triggers upstream's `gptel--restore-state` for preset application)
5. Load scope configuration from `scope.yml`
6. Enable auto-save

This behavior is inherited from the persistence system's find-file-hook.

#### Scenario: Opening new branch via find-file
- **WHEN** the user opens `~/.gptel/sessions/my-session/branches/20260128153042-feature/session.md`
- **THEN** the find-file-hook SHALL trigger auto-initialization
- **AND** extract session-id "my-session" and branch-name "20260128153042-feature"
- **AND** set buffer-local session variables
- **AND** enable gptel-mode (triggering upstream `gptel--restore-state`)
- **AND** load scope from `scope.yml`

### Requirement: Error handling and validation

The system SHALL validate branch creation preconditions and handle errors gracefully.

Validation SHALL ensure:
- Source buffer is session-initialized
- Branch point selection succeeds
- Parent branch has required metadata files (metadata.yml or scope-plan.yml, scope.yml or preset.md)
- Filesystem operations succeed (directory creation, file copying)

Errors SHALL be logged at ERROR level and reported to the user.

#### Scenario: Missing parent metadata
- **WHEN** the parent branch is missing both metadata.yml and scope-plan.yml
- **THEN** the system SHALL abort branch creation
- **AND** log an ERROR with details of missing files
- **AND** report failure to the user

### Requirement: Integration with session persistence

The system SHALL build on session persistence fundamentals, reusing core infrastructure.

Branching SHALL depend on:
- Directory structure (branches/ subdirectory, session.md, metadata files)
- Metadata formats (metadata.yml, scope.yml, branch-metadata.yml)
- Path resolution functions (branch-dir-path, context-file-path, scope-file-path, metadata-file-path)
- Registry for session/branch tracking
- Auto-initialization via find-file-hook

Branching SHALL NOT duplicate persistence logic; it orchestrates existing infrastructure.

#### Scenario: Reusing path resolution
- **WHEN** creating branch directories and file paths
- **THEN** the system SHALL use path resolution functions from filesystem.el
- **AND** NOT hardcode file paths or names

#### Scenario: Reusing auto-initialization
- **WHEN** a new branch buffer is opened
- **THEN** the existing find-file-hook from commands.el SHALL handle initialization
- **AND** branching does NOT require a separate initialization path
