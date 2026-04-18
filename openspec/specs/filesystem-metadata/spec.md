# Filesystem Metadata Gathering

## Purpose

Collects file contextual metadata (git status, existence, type) before scope validation to enable future git-based policies, richer expansion UI, and separation of concerns between validation and execution.

## ADDED Requirements

### Requirement: Metadata gathering before validation

The system SHALL gather file metadata before scope validation and pass it through the validation pipeline.

**Implementation**: `config/gptel/scope/scope-metadata.el` - metadata gathering functions

#### Scenario: Metadata gathered for read_file
- **WHEN** read_file tool is called with filepath
- **THEN** system gathers metadata before validation
- **AND** metadata includes git status, existence, and type

#### Scenario: Metadata gathered for write_file_in_scope
- **WHEN** write_file_in_scope tool is called with filepath
- **THEN** system gathers metadata before validation
- **AND** metadata includes git status, existence, and type

#### Scenario: Metadata gathered for edit_file_in_scope
- **WHEN** edit_file_in_scope tool is called with filepath
- **THEN** system gathers metadata before validation
- **AND** metadata includes git status, existence, and type

### Requirement: Metadata structure

Metadata SHALL be a flat plist containing file context information.

#### Scenario: Metadata plist structure
- **WHEN** metadata is gathered for a file
- **THEN** it contains `:path` (expanded absolute), `:real-path` (symlink-resolved), `:exists` (boolean), `:git-tracked` (boolean), `:git-repo` (repo root or nil), `:type` (file/directory/other)

#### Scenario: File in git repository
- **WHEN** file is tracked by git
- **THEN** metadata `:git-tracked` is `t`
- **AND** `:git-repo` is absolute path to repository root

#### Scenario: File not in git repository
- **WHEN** file is not in a git repository
- **THEN** metadata `:git-tracked` is `nil`
- **AND** `:git-repo` is `nil`

#### Scenario: File does not exist
- **WHEN** file does not exist
- **THEN** metadata `:exists` is `nil`
- **AND** other fields are populated based on path analysis

#### Scenario: Symlink resolution
- **WHEN** file is accessed via symlink
- **THEN** metadata `:path` is the absolute symlink path
- **AND** `:real-path` is the symlink-resolved target path

### Requirement: Validation receives metadata

Validation functions SHALL accept metadata parameter and propagate it through validation pipeline.

**Breaking change**: Function signatures change to require metadata

#### Scenario: validate-path-tool accepts metadata
- **WHEN** jf/gptel-scope--validate-path-tool is called
- **THEN** signature is `(tool-name args category config metadata)`
- **AND** metadata is accessible within validation logic

#### Scenario: check-tool-permission accepts metadata
- **WHEN** jf/gptel-scope--check-tool-permission is called
- **THEN** signature is `(config tool-name args metadata)`
- **AND** metadata is passed to specific validators

### Requirement: Metadata available for future policies

The metadata SHALL be structured to enable future git-based scope policies without requiring code changes.

#### Scenario: Git-tracked requirement (future capability)
- **WHEN** scope.yml includes `require_git_tracked: true` for write operations
- **THEN** system can check metadata `:git-tracked` field
- **AND** deny writes to non-git-tracked files

#### Scenario: Repository-scoped policies (future capability)
- **WHEN** scope.yml includes repository-specific rules
- **THEN** system can use metadata `:git-repo` field
- **AND** apply different policies per repository

### Requirement: Expansion UI displays metadata

When scope violation occurs, expansion UI SHALL display relevant file metadata to aid user decision.

#### Scenario: Git status shown in expansion UI
- **WHEN** expansion UI is triggered for file operation
- **THEN** UI displays git tracking status
- **AND** UI shows "(git-tracked)" or "(not git-tracked)" indicator

#### Scenario: File existence shown in expansion UI
- **WHEN** expansion UI is triggered for non-existent file
- **THEN** UI indicates "(file does not exist)"

### Requirement: Git check removed from tool body

The edit_file_in_scope tool SHALL NOT check git-tracked status in tool body, as metadata gathering provides this information before validation.

**Breaking change**: Removes git check from lines 148-152 of scope-filesystem-tools.el

#### Scenario: edit_file no longer checks git in tool body
- **WHEN** edit_file_in_scope tool body executes
- **THEN** it does NOT call jf/gptel--file-is-git-tracked-p
- **AND** it relies on metadata gathered before validation

#### Scenario: Metadata gathered regardless of git status
- **WHEN** edit_file_in_scope is called on non-git-tracked file
- **THEN** metadata `:git-tracked` is `nil`
- **AND** validation can use this information
- **AND** tool body does not re-check git status
