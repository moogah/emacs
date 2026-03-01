## ADDED Requirements

### Requirement: Extract file operations from parsed commands
The system SHALL extract all file operations from a parsed bash command, returning a list of file operation plists with file path, operation type, confidence level, and source.

#### Scenario: Simple read command
- **WHEN** extracting operations from "cat /workspace/foo.txt"
- **THEN** system returns one operation: file "/workspace/foo.txt", operation `:read`, confidence `:high`, source `:positional-arg`

#### Scenario: Command with multiple file arguments
- **WHEN** extracting operations from "cp source.txt dest.txt"
- **THEN** system returns two operations: source.txt with `:read`, dest.txt with `:write`

#### Scenario: Command with no file operations
- **WHEN** extracting operations from "echo hello"
- **THEN** system returns empty list of operations

### Requirement: Extract operations from redirections
The system SHALL extract file operations from all redirection operators (>, >>, <, 2>, etc.) with high confidence.

#### Scenario: Output redirection
- **WHEN** extracting operations from "echo test > output.txt"
- **THEN** system returns operation: file "output.txt", operation `:write`, confidence `:high`, source `:redirection`

#### Scenario: Append redirection
- **WHEN** extracting operations from "cat input.txt >> output.txt"
- **THEN** system includes operation: file "output.txt", operation `:append`, confidence `:high`, source `:redirection`

#### Scenario: Input redirection
- **WHEN** extracting operations from "grep pattern < input.txt"
- **THEN** system includes operation: file "input.txt", operation `:read`, confidence `:high`, source `:redirection`

#### Scenario: Multiple redirections
- **WHEN** extracting operations from "cmd > out.txt 2> err.txt"
- **THEN** system returns two operations: out.txt with `:write` and err.txt with `:write`

### Requirement: Extract operations from find exec blocks
The system SHALL extract file operations from parsed find -exec blocks, treating the exec command separately.

#### Scenario: Find with exec rm
- **WHEN** extracting operations from "find . -name '*.log' -exec rm {} \;"
- **THEN** system returns operations from both find (`:read-directory` on ".") and rm (`:delete` on matched files)

#### Scenario: Find with multiple exec blocks
- **WHEN** extracting operations from command with multiple -exec blocks
- **THEN** system returns operations from each exec block separately

### Requirement: Handle multi-command constructs
The system SHALL extract file operations from all commands in pipelines and chains, treating each command independently.

#### Scenario: Pipeline with multiple file operations
- **WHEN** extracting operations from "cat file.txt | grep pattern > output.txt"
- **THEN** system returns operations from cat (read file.txt) and grep's redirection (write output.txt)

#### Scenario: Command chain
- **WHEN** extracting operations from "rm temp.txt && touch new.txt"
- **THEN** system returns operations from both rm (delete temp.txt) and touch (create new.txt)

### Requirement: Confidence level classification
The system SHALL assign confidence levels (`:high`, `:medium`, `:low`, `:unknown`) to each extracted file operation based on command knowledge and context clarity.

#### Scenario: High confidence from known command
- **WHEN** extracting operations from well-known command like "cat file.txt"
- **THEN** confidence level is `:high`

#### Scenario: High confidence from redirection
- **WHEN** extracting operations from any command with redirections
- **THEN** redirection-based operations have confidence `:high`

#### Scenario: Unknown command
- **WHEN** extracting operations from unrecognized command
- **THEN** confidence level is `:unknown` or `:low`

### Requirement: Operation metadata
The system SHALL include metadata with each extracted operation: source type, command name, and original parsed data.

#### Scenario: Metadata for positional arg operation
- **WHEN** extracting operation from positional argument
- **THEN** result includes `:source :positional-arg`, `:command` name, and `:metadata` with full context

#### Scenario: Metadata for redirection operation
- **WHEN** extracting operation from redirection
- **THEN** result includes `:source :redirection`, `:command` name, and `:metadata` with redirection details

### Requirement: Deduplicate operations
The system SHALL deduplicate extracted file operations when the same file and operation appear multiple times.

#### Scenario: Duplicate file operations
- **WHEN** same file appears with same operation type multiple times
- **THEN** system returns single operation entry for that file

#### Scenario: Same file with different operations
- **WHEN** same file appears with different operation types (read and write)
- **THEN** system returns separate operation entries for each type

### Requirement: Handle glob patterns in file arguments
The system SHALL preserve glob patterns (*, **, ?, []) in file paths without expanding them, marking them as patterns.

#### Scenario: Simple glob pattern
- **WHEN** extracting operations from "rm *.txt"
- **THEN** file path is "*.txt" with pattern indicator

#### Scenario: Recursive glob pattern
- **WHEN** extracting operations from "cat config/**/*.json"
- **THEN** file path is "config/**/*.json" with pattern indicator

### Requirement: Handle relative and absolute paths
The system SHALL extract both relative and absolute file paths as they appear in the command, without path resolution.

#### Scenario: Absolute path
- **WHEN** extracting operations from "cat /workspace/file.txt"
- **THEN** file path is "/workspace/file.txt"

#### Scenario: Relative path
- **WHEN** extracting operations from "cat ./file.txt"
- **THEN** file path is "./file.txt"

### Requirement: Main extraction function
The system SHALL provide a main entry point function that accepts a parsed command and returns all file operations.

#### Scenario: Extract from simple command
- **WHEN** calling extraction function with parsed simple command
- **THEN** function returns list of file operation plists

#### Scenario: Extract from pipeline
- **WHEN** calling extraction function with parsed pipeline
- **THEN** function returns operations from all commands in pipeline

#### Scenario: Extract from chain
- **WHEN** calling extraction function with parsed chain
- **THEN** function returns operations from all commands in chain
