## ADDED Requirements

### Requirement: File operation extraction integration
The bash-parser module SHALL integrate file operation extraction capabilities while maintaining backward compatibility with existing parser API.

#### Scenario: Existing parse function unchanged
- **WHEN** calling existing `jf/bash-parse` function
- **THEN** function returns same structure as before with no breaking changes

#### Scenario: New extraction function available
- **WHEN** file operation detection is loaded
- **THEN** new function `jf/bash-extract-file-operations` is available

### Requirement: Leverage existing parsed data
The file operation extraction SHALL leverage existing parsed data structures including redirections, exec-blocks, and positional-args without re-parsing.

#### Scenario: Use parsed redirections
- **WHEN** extracting file operations from parsed command
- **THEN** extraction uses `:redirections` field from parse result

#### Scenario: Use parsed exec blocks
- **WHEN** extracting operations from find command
- **THEN** extraction uses `:exec-blocks` field from parse result

#### Scenario: Use positional args
- **WHEN** extracting operations from command semantics
- **THEN** extraction uses `:positional-args` field from parse result

### Requirement: Module organization
The file operation capabilities SHALL be added as new sections in bash-parser.org literate source file.

#### Scenario: New sections added
- **WHEN** file operation detection is implemented
- **THEN** bash-parser.org contains new sections for command semantics, extraction, and security

#### Scenario: Existing sections preserved
- **WHEN** new sections are added
- **THEN** existing parsing sections remain unchanged

### Requirement: Test corpus extension
The test corpus SHALL be extended with file operation test cases while preserving existing parser tests.

#### Scenario: Existing tests unmodified
- **WHEN** file operation tests are added
- **THEN** existing bash-parser test cases continue to pass

#### Scenario: New file operation tests
- **WHEN** test corpus is extended
- **THEN** new test cases cover file operation extraction and security checking

### Requirement: Command injection detection
The parser SHALL detect SHELL command execution patterns (bash -c, sh -c, zsh -c, env -S) that accept nested shell command strings as arguments.

**NOTE**: This requirement covers SHELL interpreters only. Non-shell interpreters like `python -c`, `node -e`, `ruby -e`, etc. execute code in their own language (Python, JavaScript, Ruby) and cannot directly execute bash commands. These require language-specific parsing and are excluded from bash command injection detection.

#### Scenario: Detect bash -c pattern
- **WHEN** parsing "bash -c 'rm file.txt'"
- **THEN** parser marks this as `:command-injection t` with nested command string

#### Scenario: Detect sh -c pattern
- **WHEN** parsing "sh -c 'cat file.txt'"
- **THEN** parser marks this as `:command-injection t`

#### Scenario: Detect zsh -c pattern
- **WHEN** parsing "zsh -c 'cat file.txt'"
- **THEN** parser marks this as `:command-injection t`

#### Scenario: Detect env -S pattern
- **WHEN** parsing "env -S 'bash -c cmd'"
- **THEN** parser marks this as `:command-injection t`

#### Scenario: Python -c is NOT bash injection
- **WHEN** parsing "python -c 'import os; os.remove(file)'"
- **THEN** parser does NOT mark this as `:command-injection` because Python code cannot directly execute bash commands

### Requirement: Nested command parsing
The system SHALL recursively parse nested command strings from command injection patterns.

#### Scenario: Parse nested bash command
- **WHEN** encountering "bash -c 'rm /workspace/file.txt'"
- **THEN** system recursively parses 'rm /workspace/file.txt' as separate command

#### Scenario: Parse nested command with quotes
- **WHEN** encountering command with single or double quoted nested command
- **THEN** system extracts and parses the quoted string as nested command

#### Scenario: Parse nested command with variables
- **WHEN** encountering "bash -c \"rm $FILE\""
- **THEN** system parses nested command and preserves variable references

### Requirement: Indirect operation marking
The system SHALL mark file operations from nested commands with `:indirect t` metadata to enable stricter security policies.

#### Scenario: Mark indirect operation
- **WHEN** extracting operations from "bash -c 'rm file.txt'"
- **THEN** delete operation on file.txt includes `:indirect t`

#### Scenario: Direct operation unmarked
- **WHEN** extracting operations from "rm file.txt"
- **THEN** delete operation does not include `:indirect` flag (or `:indirect nil`)

#### Scenario: Nested indirect operations
- **WHEN** extracting operations from "bash -c 'sh -c rm file.txt'"
- **THEN** operation marked `:indirect t` with `:nesting-depth 2`

### Requirement: Command injection argument extraction
The system SHALL identify which argument position contains the nested command string for each injection pattern.

#### Scenario: bash -c uses first argument after flag
- **WHEN** parsing "bash -c 'command' arg2 arg3"
- **THEN** 'command' is identified as nested command string

#### Scenario: Handle flags before injection
- **WHEN** parsing "bash -x -e -c 'command'"
- **THEN** 'command' is correctly identified despite preceding flags

### Requirement: Quote stripping for nested commands
The system SHALL strip outer quotes from nested command strings before recursive parsing.

#### Scenario: Strip single quotes
- **WHEN** nested command is "'rm file.txt'"
- **THEN** parser strips outer quotes, parses "rm file.txt"

#### Scenario: Strip double quotes
- **WHEN** nested command is "\"cat file.txt\""
- **THEN** parser strips outer quotes, parses "cat file.txt"

#### Scenario: Preserve inner quotes
- **WHEN** nested command is "'grep \"pattern\" file.txt'"
- **THEN** inner quotes preserved in parsed command
