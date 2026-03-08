# Purpose

Validate file paths extracted from bash commands against operation-specific scope patterns. Uses bash-parser file operations plugin to extract read/write/execute/modify operations and validates each file path against the corresponding scope.yml paths section.

# Requirements

## File operations extraction from commands
The system SHALL use bash-parser to extract file operations with operation types and file paths from bash command strings.

#### Scenario: Extract read operation
- **WHEN** parsing command "cat /workspace/file.txt"
- **THEN** system extracts file operation with :operation :read and :path "/workspace/file.txt"

#### Scenario: Extract write operation
- **WHEN** parsing command "touch /tmp/output.txt"
- **THEN** system extracts file operation with :operation :write and :path "/tmp/output.txt"

#### Scenario: Extract execute operation
- **WHEN** parsing command "python /workspace/scripts/deploy.py"
- **THEN** system extracts file operation with :operation :execute and :path "/workspace/scripts/deploy.py"

#### Scenario: Extract modify operation
- **WHEN** parsing command "sed -i 's/foo/bar/' /workspace/config/app.yml"
- **THEN** system extracts file operation with :operation :modify and :path "/workspace/config/app.yml"

#### Scenario: Extract multiple operations from single command
- **WHEN** parsing command "cp /workspace/source.txt /tmp/dest.txt"
- **THEN** system extracts two operations: :read for source.txt and :write for dest.txt

## Path resolution relative to working directory
The system SHALL resolve relative file paths to absolute paths using the working directory argument.

#### Scenario: Resolve relative path
- **WHEN** command is "cat ./file.txt" with directory "/workspace"
- **THEN** system resolves path to "/workspace/file.txt"

#### Scenario: Resolve parent directory reference
- **WHEN** command is "cat ../other/file.txt" with directory "/workspace/project"
- **THEN** system resolves path to "/workspace/other/file.txt"

#### Scenario: Absolute path unchanged
- **WHEN** command is "cat /etc/passwd" with directory "/workspace"
- **THEN** system keeps path as "/etc/passwd"

#### Scenario: Resolve symlinks in path
- **WHEN** resolved path contains symlinks
- **THEN** system uses file-truename to resolve to real path

## Operation-specific scope validation
The system SHALL validate file paths against operation-specific scope patterns (paths.read, paths.write, paths.execute, paths.modify).

#### Scenario: Read operation matches paths.read
- **WHEN** file operation is :read "/workspace/file.txt"
- **AND** scope.yml has paths.read: ["/workspace/**"]
- **THEN** validation passes

#### Scenario: Read operation matches paths.write
- **WHEN** file operation is :read "/tmp/file.txt"
- **AND** scope.yml has paths.write: ["/tmp/**"]
- **THEN** validation passes (write scope includes read)

#### Scenario: Write operation requires paths.write
- **WHEN** file operation is :write "/workspace/output.txt"
- **AND** scope.yml has paths.read: ["/workspace/**"]
- **THEN** validation fails with "path_out_of_scope" error

#### Scenario: Execute operation requires paths.execute
- **WHEN** file operation is :execute "/workspace/scripts/deploy.py"
- **AND** scope.yml has paths.read: ["/workspace/**"] but no paths.execute
- **THEN** validation fails with "path_out_of_scope" error

#### Scenario: Modify operation requires paths.modify
- **WHEN** file operation is :modify "/workspace/config/app.yml"
- **AND** scope.yml has paths.write: ["/workspace/**"] but no paths.modify
- **THEN** validation fails with "path_out_of_scope" error

## Deny patterns take precedence
The system SHALL reject file operations on paths matching paths.deny regardless of operation type.

#### Scenario: Read operation on denied path
- **WHEN** file operation is :read "/etc/passwd"
- **AND** scope.yml has paths.deny: ["/etc/**"]
- **THEN** validation fails with "path_denied" error

#### Scenario: Deny overrides read scope
- **WHEN** file operation is :read "/workspace/secrets/api-key.txt"
- **AND** scope.yml has paths.read: ["/workspace/**"] and paths.deny: ["/workspace/secrets/**"]
- **THEN** validation fails with "path_denied" error

## Multiple file operations validated independently
The system SHALL validate each file operation from a command independently.

#### Scenario: Copy with both paths in scope
- **WHEN** command "cp /workspace/source.txt /tmp/dest.txt"
- **AND** paths.read: ["/workspace/**"] and paths.write: ["/tmp/**"]
- **THEN** both operations validate and command allowed

#### Scenario: Copy with destination out of scope
- **WHEN** command "cp /workspace/source.txt /etc/dest.txt"
- **AND** paths.read: ["/workspace/**"] but no write scope for /etc
- **THEN** validation fails with details about /etc/dest.txt

#### Scenario: First operation fails, report immediately
- **WHEN** validating multiple operations and first fails
- **THEN** system reports first failure without validating remaining operations

## Structured error responses with path details
The system SHALL return detailed error information identifying which path and operation failed validation.

#### Scenario: Path out of scope error structure
- **WHEN** file path fails validation
- **THEN** error includes :error "path_out_of_scope", :path, :operation, :required_scope, :allowed_patterns

#### Scenario: Path denied error structure
- **WHEN** file path matches deny pattern
- **THEN** error includes :error "path_denied", :path, :operation, :deny_pattern

#### Scenario: Error message suggests scope expansion
- **WHEN** path validation fails
- **THEN** error message suggests using request_scope_expansion with appropriate path pattern

## Glob pattern matching for scope validation
The system SHALL use glob pattern matching to check if file paths match scope patterns.

#### Scenario: Single-level wildcard match
- **WHEN** path is "/workspace/file.txt"
- **AND** pattern is "/workspace/*"
- **THEN** path matches pattern

#### Scenario: Recursive wildcard match
- **WHEN** path is "/workspace/deep/nested/file.txt"
- **AND** pattern is "/workspace/**"
- **THEN** path matches pattern

#### Scenario: Extension pattern match
- **WHEN** path is "/workspace/config.yml"
- **AND** pattern is "/workspace/*.yml"
- **THEN** path matches pattern

#### Scenario: Multiple patterns checked in order
- **WHEN** validating path against multiple patterns
- **THEN** system checks each pattern until match found or all fail
