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
