# No-Op Command Allowance

## Purpose

Allow commands that produce no file system operations to execute by default without requiring explicit command allowlists. This enables safe introspection commands (version checks, help displays, status queries) to work immediately while maintaining strict validation for commands that interact with files.

## ADDED Requirements

### Requirement: Commands with no file operations are allowed by default

The system SHALL allow execution of commands that extract no file operations from bash-parser semantic analysis, without requiring the command to be in any allowlist category.

#### Scenario: Version check command allowed
- **WHEN** command is `python3 --version`
- **AND** bash-parser extracts zero file operations
- **THEN** validation pipeline allows execution without checking command categories

#### Scenario: Help flag command allowed
- **WHEN** command is `node --help`
- **AND** bash-parser extracts zero file operations
- **THEN** validation pipeline allows execution

#### Scenario: Unknown command with no operations allowed
- **WHEN** command is `my-custom-tool --info`
- **AND** command is not in any allowlist or deny list
- **AND** bash-parser extracts zero file operations
- **THEN** validation pipeline allows execution

#### Scenario: No-op command in deny list still blocked
- **WHEN** command is `sudo --version`
- **AND** bash-parser extracts zero file operations
- **AND** command is in bash_tools.deny list
- **THEN** validation pipeline denies execution with "command_denied" error

### Requirement: No-op validation occurs before file operation validation

The system SHALL check for no-op commands (zero file operations) before attempting to validate individual file operations against path permissions.

#### Scenario: No-op check short-circuits validation
- **WHEN** command extracts zero file operations
- **THEN** validation pipeline returns success without entering file operation validation stage

#### Scenario: Commands with operations proceed to path validation
- **WHEN** command is `python3 script.py`
- **AND** bash-parser extracts one or more file operations
- **THEN** validation pipeline proceeds to file operation validation stage

### Requirement: Empty operations list defines no-op commands

The system SHALL consider a command to be no-op if and only if the bash-parser semantics extraction returns an empty list of file operations for the :filesystem domain.

#### Scenario: Empty filesystem operations list
- **WHEN** semantics extraction returns `{:domains {:filesystem []}}`
- **THEN** command is classified as no-op

#### Scenario: Non-empty filesystem operations list
- **WHEN** semantics extraction returns `{:domains {:filesystem [{:operation :read ...}]}}`
- **THEN** command is NOT classified as no-op

#### Scenario: No filesystem domain present
- **WHEN** semantics extraction returns `{:domains {}}`
- **THEN** command is classified as no-op (no filesystem operations)

### Requirement: No-op allowance is independent of command name

The system SHALL allow no-op commands regardless of whether the command name appears in any configuration (allowlists, deny lists, or categories), except when the command is explicitly in the deny list.

#### Scenario: Unknown command name with no operations
- **WHEN** command name has never been seen before
- **AND** command extracts zero file operations
- **AND** command is not in deny list
- **THEN** validation allows execution

#### Scenario: Command previously required allowlist
- **WHEN** command is `ruby --version`
- **AND** in old system, `ruby` required being in read_only category
- **AND** command extracts zero file operations
- **THEN** validation allows execution without checking categories

### Requirement: No-op check applies to all pipeline commands

The system SHALL apply no-op allowance check to the entire command pipeline, not individual commands within the pipeline.

#### Scenario: Simple command pipeline with no operations
- **WHEN** command is `echo "hello" | wc -l`
- **AND** entire pipeline extracts zero file operations
- **THEN** validation allows execution

#### Scenario: Pipeline with file operations in later stage
- **WHEN** command is `echo "test" > /tmp/file.txt`
- **AND** pipeline extracts file operations (write to /tmp/file.txt)
- **THEN** validation proceeds to file operation validation (not classified as no-op)

### Requirement: Error messages distinguish no-op allowance from category validation

The system SHALL provide clear error messages that indicate whether a command was allowed via no-op allowance or denied due to operations requiring validation.

#### Scenario: No-op command success message
- **WHEN** command allowed via no-op allowance
- **THEN** validation returns success without mentioning categories or paths

#### Scenario: Operation validation failure message
- **WHEN** command has file operations that fail path validation
- **THEN** error message references specific operation and path, not command name or categories
