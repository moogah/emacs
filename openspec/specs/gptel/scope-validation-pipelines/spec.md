# Purpose

Validate all commands in bash pipelines and chains, not just the base command. Uses bash-parser to extract all file operations from pipelines, command chains (&&, ||, ;), and validates each operation independently against scope.yml path permissions.

# Requirements

## Pipeline command extraction
The system SHALL use bash-parser to extract all commands and their file operations from bash pipelines.

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

## Pipeline file operations validated independently
The system SHALL validate each file operation extracted from pipeline commands against the appropriate path section (read/write/execute/modify/deny).

#### Scenario: Pipeline with no file operations allowed via no-op
- **WHEN** command is "python3 --version | head -1"
- **AND** entire pipeline extracts zero file operations
- **THEN** validation passes via no-op allowance

#### Scenario: Pipeline commands validated by extracted file operations
- **WHEN** command is "cat /workspace/file.txt | grep pattern"
- **AND** bash-parser extracts a :read operation for /workspace/file.txt
- **THEN** system validates /workspace/file.txt against paths.read

#### Scenario: Pipeline with mixed operations validated
- **WHEN** command is "cat /workspace/in.txt | tee /workspace/out.txt"
- **AND** bash-parser extracts :read for in.txt and :write for out.txt
- **THEN** system validates in.txt against paths.read and out.txt against paths.write

## Pipeline validation closes security bypass
The system SHALL prevent dangerous operations from bypassing validation by appearing in non-base pipeline positions.

#### Scenario: Prevent xargs rm bypass
- **WHEN** command is "find . -name '*.tmp' | xargs rm /workspace/**"
- **AND** paths extracted from rm are outside paths.write
- **THEN** system rejects with path-scope violation for the rm target

#### Scenario: Second pipeline command writes to denied path
- **WHEN** command is "cat /workspace/data.txt | tee /etc/config"
- **AND** /etc/config is not in paths.write
- **THEN** validation fails on the write operation at pipeline position 2

## Parse completeness required for pipeline validation
The system SHALL reject commands when bash-parser cannot fully parse the pipeline structure.

#### Scenario: Incomplete parse of pipeline
- **WHEN** bash-parser sets :parse-complete nil for pipeline
- **AND** security.enforce_parse_complete is true
- **THEN** system rejects with "parse_incomplete" error

#### Scenario: Syntax error in pipeline
- **WHEN** command has unmatched quotes: "ls | grep 'unclosed"
- **THEN** system rejects with parse error details

#### Scenario: Complete parse enables validation
- **WHEN** bash-parser successfully parses entire pipeline
- **THEN** system proceeds with per-operation validation

## Validation pipeline includes no-op check stage
The system SHALL check for no-op commands (zero file operations) before file operation validation, allowing commands with no file system impact to execute by default.

#### Scenario: No-op pipeline bypasses file validation
- **WHEN** command is "echo 'hello' | wc -c"
- **AND** bash-parser extracts zero file operations
- **THEN** validation succeeds at no-op check stage without entering file operation validation

#### Scenario: Pipeline with operations proceeds to file validation
- **WHEN** command is "cat file.txt | grep pattern"
- **AND** bash-parser extracts file operations
- **THEN** validation proceeds past no-op check to file operation validation stage

## Validation stages execute in specific order
The system SHALL execute validation stages in this order: (1) Parse completeness, (2) No-op check, (3) File operation validation, (4) Cloud auth policy, (5) Coverage threshold.

#### Scenario: Parse completeness checked first
- **WHEN** command has syntax errors
- **AND** security.enforce_parse_complete is true
- **THEN** validation fails at parse completeness stage before reaching later stages

#### Scenario: No-op check before file validation
- **WHEN** command is "python3 --version"
- **AND** command extracts zero file operations
- **THEN** validation succeeds at no-op check without attempting file operation validation

#### Scenario: Cloud auth checked after file operations
- **WHEN** command has both file operations and cloud auth side-effects
- **THEN** file operations are validated before cloud auth policy is applied
