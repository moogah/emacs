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
The system SHALL validate each command in a pipeline against bash_tools categories and deny list.

#### Scenario: All pipeline commands allowed
- **WHEN** command is "ls -la | grep foo"
- **AND** both "ls" and "grep" are in read_only category
- **THEN** validation passes

#### Scenario: Second pipeline command denied
- **WHEN** command is "ls | xargs rm"
- **AND** "ls" is in read_only but "rm" is in deny list
- **THEN** validation fails with "command_denied" error for "rm"

#### Scenario: Middle command in chain not allowed
- **WHEN** command is "cat file.txt | sh | head -10"
- **AND** "cat" and "head" allowed but "sh" not in any category
- **THEN** validation fails with "command_not_allowed" error for "sh"

## Pipeline validation closes security bypass
The system SHALL prevent dangerous commands from bypassing validation by appearing in non-base positions.

#### Scenario: Prevent xargs rm bypass
- **WHEN** command is "find . -name '*.tmp' | xargs rm"
- **AND** "rm" is in deny list
- **THEN** system rejects with clear error identifying "rm" in pipeline position 2

#### Scenario: Prevent sh execution bypass
- **WHEN** command is "grep pattern . | sh"
- **AND** "sh" not in allowed categories
- **THEN** system rejects identifying shell execution risk

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

## Pipeline validation combined with file path validation
The system SHALL validate both pipeline commands AND file paths for complete security coverage.

#### Scenario: Pipeline with file paths validated together
- **WHEN** command is "cat /etc/passwd | grep root"
- **THEN** system validates both "cat" and "grep" commands AND validates /etc/passwd path

#### Scenario: Pipeline command allowed but file path denied
- **WHEN** command is "cat /etc/shadow | head"
- **AND** "cat" and "head" are allowed but /etc/shadow in paths.deny
- **THEN** validation fails on path_denied (not command)

#### Scenario: File path allowed but pipeline command denied
- **WHEN** command is "cat /workspace/file.txt | sh"
- **AND** /workspace/file.txt in paths.read but "sh" not allowed
- **THEN** validation fails on command_not_allowed for "sh"
