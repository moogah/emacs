# Scope Validation Pipelines - Delta Spec

## MODIFIED Requirements

### Requirement: All pipeline commands validated independently

The system SHALL validate each command in a pipeline against bash_tools deny list and file operation semantics, replacing category-based validation with operation-first validation.

#### Scenario: All pipeline commands with no operations allowed
- **WHEN** command is "python3 --version | head -1"
- **AND** entire pipeline extracts zero file operations
- **THEN** validation passes via no-op allowance

#### Scenario: Pipeline commands validated by deny list only
- **WHEN** command is "ls -la | grep foo"
- **AND** neither "ls" nor "grep" are in deny list
- **AND** file operations are extracted
- **THEN** deny list check passes and proceeds to file operation validation

#### Scenario: Second pipeline command in deny list
- **WHEN** command is "ls | xargs rm"
- **AND** "rm" is in deny list
- **THEN** validation fails with "command_denied" error for "rm" at position 2

#### Scenario: Pipeline command not in deny list with file operations
- **WHEN** command is "cat file.txt | sh script.sh"
- **AND** "sh" is not in deny list
- **AND** bash-parser extracts :execute operation for "script.sh"
- **THEN** deny list check passes and file operation validation checks paths.execute

### Requirement: Pipeline validation combined with file operation validation

The system SHALL validate both pipeline commands (deny list) AND extracted file operations (path permissions) for complete security coverage, removing category-based command validation.

#### Scenario: Pipeline with file operations validated together
- **WHEN** command is "cat /etc/passwd | grep root"
- **AND** commands pass deny list check
- **THEN** system validates extracted file operations against paths configuration

#### Scenario: Pipeline command allowed but file path denied
- **WHEN** command is "cat /etc/shadow | head"
- **AND** commands not in deny list
- **AND** /etc/shadow in paths.deny
- **THEN** validation fails on path_denied (not command categories)

#### Scenario: File path allowed but pipeline command denied
- **WHEN** command is "cat /workspace/file.txt | sudo head"
- **AND** /workspace/file.txt in paths.read
- **AND** "sudo" in deny list
- **THEN** validation fails on command_denied for "sudo"

## ADDED Requirements

### Requirement: Validation pipeline includes no-op check stage

The system SHALL add a validation stage that checks for no-op commands (zero file operations) before file operation validation, allowing commands with no file system impact to execute by default.

#### Scenario: No-op pipeline bypasses file validation
- **WHEN** command is "echo 'hello' | wc -c"
- **AND** bash-parser extracts zero file operations
- **THEN** validation succeeds at no-op check stage without entering file operation validation

#### Scenario: Pipeline with operations proceeds to file validation
- **WHEN** command is "cat file.txt | grep pattern"
- **AND** bash-parser extracts file operations
- **THEN** validation proceeds past no-op check to file operation validation stage

### Requirement: Validation stages execute in specific order

The system SHALL execute validation stages in this order: (1) Parse completeness, (2) Deny list check, (3) Extract semantics, (4) No-op check, (5) File operation validation.

#### Scenario: Deny list checked before no-op
- **WHEN** command is "sudo --version"
- **AND** command would be no-op
- **AND** "sudo" is in deny list
- **THEN** validation fails at deny list stage before reaching no-op check

#### Scenario: No-op check before file validation
- **WHEN** command is "python3 --version"
- **AND** command extracts zero file operations
- **THEN** validation succeeds at no-op check without attempting file operation validation

#### Scenario: Parse completeness checked first
- **WHEN** command has syntax errors
- **THEN** validation fails at parse completeness stage before checking deny list or operations

## REMOVED Requirements

### Requirement: All pipeline commands validated independently against categories

**Reason**: Category-based validation (read_only, safe_write, dangerous) removed in favor of operation-first validation model.

**Migration**: Commands now validated via deny list check (Stage 2) and file operation validation (Stage 5). No category configuration needed.
