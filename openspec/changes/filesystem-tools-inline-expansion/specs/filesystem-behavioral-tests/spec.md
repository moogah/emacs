# Filesystem Tools Behavioral Tests

## Purpose

Provides comprehensive behavioral test coverage for scoped filesystem tools (read_file, write_file_in_scope, edit_file_in_scope), testing scope expansion workflows, validation logic, allow-once lifecycle, and error handling.

## ADDED Requirements

### Requirement: Test organization

Behavioral tests for filesystem tools SHALL be organized in single spec file covering all three tools with nested describe blocks.

**Implementation**: `config/gptel/tools/test/behavioral/filesystem-tools-scope-expansion-spec.el`

#### Scenario: Single spec file for all tools
- **WHEN** behavioral tests are organized
- **THEN** all three tools (read_file, write_file_in_scope, edit_file_in_scope) are in one file
- **AND** each tool has dedicated `describe` block

#### Scenario: Test framework is Buttercup
- **WHEN** behavioral tests are written
- **THEN** tests use Buttercup framework (matching run_bash_command tests)
- **AND** use `describe`, `it`, `expect` syntax

### Requirement: Shared test infrastructure

Filesystem behavioral tests SHALL reuse existing helpers-spec.el infrastructure from run_bash_command tests.

#### Scenario: Reuse session setup helpers
- **WHEN** tests need mock gptel session
- **THEN** they use `helpers-spec-setup-session` and `helpers-spec-teardown-session`

#### Scenario: Reuse scope config helpers
- **WHEN** tests need scope configuration
- **THEN** they use `helpers-spec-make-minimal-scope` and `helpers-spec-load-scope-config`

#### Scenario: Reuse custom matchers
- **WHEN** tests check validation results
- **THEN** they use custom matchers like `:to-be-validation-success` and `:to-be-validation-error`

### Requirement: Test coverage for read_file

Tests SHALL cover read_file path validation, inline expansion workflow, allow-once lifecycle, and error handling.

#### Scenario: Read in read scope succeeds
- **WHEN** read_file accesses path in `paths.read`
- **THEN** validation passes
- **AND** file contents are returned

#### Scenario: Read in write scope succeeds (hierarchical)
- **WHEN** read_file accesses path in `paths.write`
- **THEN** validation passes (write scope includes read capability)
- **AND** file contents are returned

#### Scenario: Read out of scope triggers expansion
- **WHEN** read_file accesses path outside scope
- **THEN** validation fails
- **AND** inline expansion UI is triggered

#### Scenario: Read expansion approved with add-to-scope
- **WHEN** user approves read via add-to-scope
- **THEN** scope.yml `paths.read` is updated
- **AND** validation retry succeeds
- **AND** file contents are returned

#### Scenario: Read expansion approved with allow-once
- **WHEN** user approves read via allow-once
- **THEN** allow-once permission is granted
- **AND** validation retry succeeds
- **AND** file contents are returned
- **AND** permission is consumed

#### Scenario: Read expansion denied
- **WHEN** user denies read expansion
- **THEN** scope violation error is returned
- **AND** no file access occurs

#### Scenario: Read file not found
- **WHEN** read_file accesses non-existent file (in scope)
- **THEN** error `:file_not_found` is returned

### Requirement: Test coverage for write_file_in_scope

Tests SHALL cover write_file_in_scope path validation, write-specific permissions, directory creation, and inline expansion workflow.

#### Scenario: Write in write scope succeeds
- **WHEN** write_file_in_scope accesses path in `paths.write`
- **THEN** validation passes
- **AND** file is written

#### Scenario: Write in read-only scope fails
- **WHEN** write_file_in_scope accesses path only in `paths.read`
- **THEN** validation fails (read scope does NOT allow writes)
- **AND** expansion UI is triggered

#### Scenario: Write out of scope triggers expansion
- **WHEN** write_file_in_scope accesses path outside scope
- **THEN** validation fails
- **AND** inline expansion UI is triggered

#### Scenario: Write expansion approved with add-to-scope
- **WHEN** user approves write via add-to-scope
- **THEN** scope.yml `paths.write` is updated
- **AND** validation retry succeeds
- **AND** file is written

#### Scenario: Write expansion approved with allow-once
- **WHEN** user approves write via allow-once
- **THEN** allow-once permission is granted
- **AND** validation retry succeeds
- **AND** file is written
- **AND** permission is consumed

#### Scenario: Write creates parent directories
- **WHEN** write_file_in_scope writes to non-existent directory (in scope)
- **THEN** parent directories are created
- **AND** file is written successfully

#### Scenario: Directory creation respects scope
- **WHEN** write_file_in_scope creates parent directories
- **THEN** directory path must match write scope patterns

### Requirement: Test coverage for edit_file_in_scope

Tests SHALL cover edit_file_in_scope path validation, git-tracked validation, string replacement, and inline expansion workflow.

#### Scenario: Edit in write scope succeeds
- **WHEN** edit_file_in_scope edits git-tracked file in `paths.write`
- **THEN** validation passes
- **AND** string replacement succeeds

#### Scenario: Edit in read-only scope fails
- **WHEN** edit_file_in_scope edits file only in `paths.read`
- **THEN** validation fails (read scope does NOT allow edits)
- **AND** expansion UI is triggered

#### Scenario: Edit out of scope triggers expansion
- **WHEN** edit_file_in_scope edits path outside scope
- **THEN** validation fails
- **AND** inline expansion UI is triggered

#### Scenario: Edit git-tracked file succeeds
- **WHEN** edit_file_in_scope edits file with metadata `:git-tracked t`
- **THEN** validation passes
- **AND** edit proceeds

#### Scenario: Edit non-git-tracked file behavior
- **WHEN** edit_file_in_scope edits file with metadata `:git-tracked nil`
- **THEN** metadata is available to validation
- **AND** future git-based policies can use this information

#### Scenario: Edit file not found
- **WHEN** edit_file_in_scope edits non-existent file (in scope)
- **THEN** error `:file_not_found` is returned

#### Scenario: Edit string not found
- **WHEN** edit_file_in_scope searches for non-existent string (in scope)
- **THEN** error `:string_not_found` is returned
- **AND** file is not modified

#### Scenario: Edit expansion approved with add-to-scope
- **WHEN** user approves edit via add-to-scope
- **THEN** scope.yml `paths.write` is updated
- **AND** validation retry succeeds
- **AND** edit proceeds

#### Scenario: Edit expansion approved with allow-once
- **WHEN** user approves edit via allow-once
- **THEN** allow-once permission is granted
- **AND** validation retry succeeds
- **AND** edit proceeds
- **AND** permission is consumed

### Requirement: Transient action handler tests

Tests SHALL verify transient menu action handlers (deny, add-to-scope, allow-once) for filesystem tools.

#### Scenario: Add-to-scope updates paths.read for read_file
- **WHEN** user selects add-to-scope for read_file violation
- **THEN** scope.yml `paths.read` is updated with filepath pattern
- **AND** callback receives success response

#### Scenario: Add-to-scope updates paths.write for write_file
- **WHEN** user selects add-to-scope for write_file_in_scope violation
- **THEN** scope.yml `paths.write` is updated with filepath pattern
- **AND** callback receives success response

#### Scenario: Add-to-scope updates paths.write for edit_file
- **WHEN** user selects add-to-scope for edit_file_in_scope violation
- **THEN** scope.yml `paths.write` is updated with filepath pattern
- **AND** callback receives success response

#### Scenario: Deny action returns user_denied
- **WHEN** user selects deny for any filesystem tool
- **THEN** callback receives `:success nil`, `:user_denied t`

#### Scenario: Allow-once grants temporary permission
- **WHEN** user selects allow-once for any filesystem tool
- **THEN** permission added to allow-once list
- **AND** callback receives `:success t`, `:allowed_once t`

### Requirement: Metadata display in expansion UI tests

Tests SHALL verify that file metadata (git status) is displayed in expansion UI during scope violations.

#### Scenario: Git-tracked status shown
- **WHEN** expansion UI triggered for git-tracked file
- **THEN** UI displays "(git-tracked)" indicator

#### Scenario: Non-git-tracked status shown
- **WHEN** expansion UI triggered for non-git-tracked file
- **THEN** UI displays "(not git-tracked)" indicator

#### Scenario: File existence shown
- **WHEN** expansion UI triggered for non-existent file
- **THEN** UI displays "(file does not exist)" indicator

### Requirement: Allow-once lifecycle tests

Tests SHALL verify allow-once permission granting, consumption, and expiration for filesystem tools.

#### Scenario: Allow-once permission format
- **WHEN** allow-once granted for filesystem tool
- **THEN** stored as `(tool-name . filepath)` pair

#### Scenario: Allow-once consumed after execution
- **WHEN** tool executes with allow-once permission
- **THEN** permission removed from list
- **AND** subsequent call requires re-approval

#### Scenario: Allow-once cleared after LLM turn
- **WHEN** LLM response completes
- **THEN** all allow-once permissions cleared
