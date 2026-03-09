# Purpose

Validate all commands in bash pipelines and chains, not just the base command. Uses bash-parser to extract all commands from pipelines, command chains (&&, ||, ;), and validates each command independently against scope.yml bash_tools configuration.

# Requirements

## Pipeline command extraction
The system SHALL use bash-parser to extract all commands from bash pipelines.

#### Scenario: Extract commands from pipe
- **WHEN** parsing "ls -la | grep foo"
- **THEN** system extracts two commands: "ls -la" and "grep foo"

#### Scenario: Extract commands from multi-stage pipeline
- **WHEN** parsing "cat file.txt | grep pattern | head -10"
- **THEN** system extracts three commands: "cat file.txt", "grep pattern", "head -10"

#### Scenario: Extract commands from command chain with semicolon
- **WHEN** parsing "cd /tmp; ls -la"
- **THEN** system extracts two commands: "cd /tmp" and "ls -la"

#### Scenario: Extract commands from AND chain
- **WHEN** parsing "mkdir foo && cd foo"
- **THEN** system extracts two commands: "mkdir foo" and "cd foo"

#### Scenario: Extract commands from OR chain
- **WHEN** parsing "test -f file.txt || touch file.txt"
- **THEN** system extracts two commands: "test -f file.txt" and "touch file.txt"

## All pipeline commands validated independently
The system SHALL validate each command in a pipeline against bash_tools deny list and file operation semantics.

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

## Pipeline validation closes security bypass
The system SHALL prevent dangerous commands from bypassing validation by appearing in non-base positions.

#### Scenario: Prevent xargs rm bypass
- **WHEN** command is "find . -name '*.tmp' | xargs rm"
- **AND** "rm" is in deny list
- **THEN** system rejects with clear error identifying "rm" in pipeline position 2

#### Scenario: Prevent chmod bypass
- **WHEN** command is "find . -type f | xargs chmod 777"
- **AND** "chmod" is in deny list
- **THEN** system rejects with security warning

## Command name extraction from pipeline segments
The system SHALL extract base command name from each pipeline segment for categorization.

#### Scenario: Extract command from segment with arguments
- **WHEN** pipeline segment is "grep -rn 'pattern' ."
- **THEN** system extracts "grep" as command name

#### Scenario: Extract command from segment with flags
- **WHEN** pipeline segment is "ls -la"
- **THEN** system extracts "ls" as command name

#### Scenario: Extract git subcommand
- **WHEN** pipeline segment is "git log --oneline"
- **THEN** system extracts "git log" as command name (with subcommand)

## Structured error identifies pipeline position
The system SHALL include pipeline position in error messages to help identify which command failed.

#### Scenario: Error identifies second command in pipeline
- **WHEN** "ls | rm" fails validation on "rm"
- **THEN** error includes :pipeline_position 1 (0-indexed) and :command "rm"

#### Scenario: Error shows full pipeline context
- **WHEN** pipeline validation fails
- **THEN** error includes :full_command with entire pipeline and :failed_segment with failing command

#### Scenario: Error message explains pipeline validation
- **WHEN** pipeline command fails
- **THEN** message explains "All commands in pipeline are validated independently"

## Parse completeness required for pipeline validation
The system SHALL reject commands when bash-parser cannot fully parse the pipeline structure.

#### Scenario: Incomplete parse of pipeline
- **WHEN** bash-parser sets :parse-complete nil for pipeline
- **THEN** system rejects with "incomplete_parse" error

#### Scenario: Syntax error in pipeline
- **WHEN** command has unmatched quotes: "ls | grep 'unclosed"
- **THEN** system rejects with parse error details

#### Scenario: Complete parse enables validation
- **WHEN** bash-parser successfully parses entire pipeline
- **THEN** system proceeds with per-command validation

## Pipeline validation combined with file operation validation
The system SHALL validate both pipeline commands (deny list) AND extracted file operations (path permissions) for complete security coverage.

#### Scenario: Pipeline with file operations validated together
- **WHEN** command is "cat /etc/passwd | grep root"
- **AND** commands pass deny list check
- **THEN** system validates extracted file operations against paths configuration

#### Scenario: Pipeline command allowed but file path denied
- **WHEN** command is "cat /etc/shadow | head"
- **AND** commands not in deny list
- **AND** /etc/shadow in paths.deny
- **THEN** validation fails on path_denied (not command)

#### Scenario: File path allowed but pipeline command denied
- **WHEN** command is "cat /workspace/file.txt | sudo head"
- **AND** /workspace/file.txt in paths.read
- **AND** "sudo" in deny list
- **THEN** validation fails on command_denied for "sudo"

## Validation pipeline includes no-op check stage
The system SHALL add a validation stage that checks for no-op commands (zero file operations) before file operation validation, allowing commands with no file system impact to execute by default.

#### Scenario: No-op pipeline bypasses file validation
- **WHEN** command is "echo 'hello' | wc -c"
- **AND** bash-parser extracts zero file operations
- **THEN** validation succeeds at no-op check stage without entering file operation validation

#### Scenario: Pipeline with operations proceeds to file validation
- **WHEN** command is "cat file.txt | grep pattern"
- **AND** bash-parser extracts file operations
- **THEN** validation proceeds past no-op check to file operation validation stage

## Validation stages execute in specific order
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
