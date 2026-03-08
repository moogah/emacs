## MODIFIED Requirements

### Requirement: Extract file operations from parsed commands
The system SHALL extract all file operations from a parsed bash command as a plugin, returning plugin result structure with operations and claimed tokens.

#### Scenario: Simple read command
- **WHEN** extracting operations from "cat /workspace/foo.txt"
- **THEN** plugin returns result with domain :filesystem, one operation: file "/workspace/foo.txt", operation `:read`, confidence `:high`, source `:positional-arg`

#### Scenario: Command with multiple file arguments
- **WHEN** extracting operations from "cp source.txt dest.txt"
- **THEN** plugin returns result with two operations: source.txt with `:read`, dest.txt with `:write`

#### Scenario: Command with no file operations
- **WHEN** extracting operations from "echo hello"
- **THEN** plugin returns nil or empty operations list

#### Scenario: Plugin claims relevant tokens
- **WHEN** extracting file operations
- **THEN** plugin result includes claimed-token-ids for command name and file path tokens

## ADDED Requirements

### Requirement: Filesystem plugin implementation
The system SHALL implement filesystem extraction as a plugin conforming to plugin protocol.

#### Scenario: Plugin registered with priority
- **WHEN** filesystem plugin is registered
- **THEN** plugin has priority value and universal predicate (always applicable)

#### Scenario: Plugin returns result structure
- **WHEN** filesystem plugin completes extraction
- **THEN** result includes :domain :filesystem, :operations list, :claimed-token-ids list, :metadata plist

#### Scenario: Plugin claims command token
- **WHEN** command has file operation semantics
- **THEN** plugin claims command-name token ID

#### Scenario: Plugin claims file path tokens
- **WHEN** extracting file operations from positional args
- **THEN** plugin claims positional-arg token IDs for file paths

#### Scenario: Plugin claims flag tokens
- **WHEN** command uses flag-dependent operations (e.g., sed -i)
- **THEN** plugin claims flag token IDs that control operations

#### Scenario: Plugin claims redirection tokens
- **WHEN** extracting operations from redirections
- **THEN** plugin claims redirection token IDs

### Requirement: Token claiming algorithm
The system SHALL identify which tokens contributed to extracted file operations and claim them for coverage.

#### Scenario: Claim tokens for simple file operation
- **WHEN** extracting "cat file.txt"
- **THEN** claimed tokens include command-name "cat" and positional-arg "file.txt"

#### Scenario: Claim tokens for redirection
- **WHEN** extracting "echo test > output.txt"
- **THEN** claimed tokens include redirection token and file path

#### Scenario: Claim tokens for complex command
- **WHEN** extracting "find . -name '*.log' -exec rm {} \\;"
- **THEN** claimed tokens include find command, flags, arguments, exec block tokens

#### Scenario: No token claiming for non-file commands
- **WHEN** extracting from "echo hello" (no file operations)
- **THEN** claimed-token-ids is empty

### Requirement: Backward compatible wrapping
The system SHALL provide backward-compatible wrapper function preserving existing jf/bash-extract-file-operations API.

#### Scenario: Legacy API calls plugin
- **WHEN** calling jf/bash-extract-file-operations with parsed command
- **THEN** function invokes filesystem plugin and returns :operations only

#### Scenario: Legacy API returns operations list
- **WHEN** calling jf/bash-extract-file-operations
- **THEN** return value is operations list (not full plugin result)

#### Scenario: Legacy API maintains signature
- **WHEN** calling jf/bash-extract-file-operations with var-context
- **THEN** function signature matches original (parsed-command &optional var-context)
