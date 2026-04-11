## ADDED Requirements

### Requirement: Single path operation validator

The validation module SHALL provide a single `validate-path-operation` function that checks whether a given path is allowed for a given operation type against the scope configuration. This function SHALL be the sole path validation implementation used by both filesystem tool validation and bash pipeline stage 6 (file operation validation).

#### Scenario: Path allowed for read operation
- **WHEN** `validate-path-operation` is called with path "/workspace/file.txt", operation `read`, and scope config containing `paths.read: ["/workspace/**"]`
- **THEN** it returns `(:allowed t)`

#### Scenario: Path allowed via write scope for read operation
- **WHEN** `validate-path-operation` is called with path "/workspace/file.txt", operation `read`, and scope config containing only `paths.write: ["/workspace/**"]`
- **THEN** it returns `(:allowed t)` because write scope includes read capability

#### Scenario: Path denied by deny pattern
- **WHEN** `validate-path-operation` is called with path "/etc/passwd", operation `read`, and scope config containing `paths.deny: ["/etc/**"]`
- **THEN** it returns `(:allowed nil :error "denied-pattern" :resource "/etc/passwd" :message ...)`

#### Scenario: Deny patterns take precedence over allow patterns
- **WHEN** path matches both `paths.read` and `paths.deny`
- **THEN** deny takes precedence and returns `(:allowed nil :error "denied-pattern" ...)`

#### Scenario: Path not in any allow list
- **WHEN** `validate-path-operation` is called with path "/other/file.txt", operation `read`, and scope config with no matching patterns
- **THEN** it returns `(:allowed nil :error "not-in-scope" :resource "/other/file.txt" :message ...)`

#### Scenario: Symlinks resolved before matching
- **WHEN** path contains symlinks
- **THEN** both the absolute path and the resolved real path are checked against patterns
- **AND** access is allowed if either form matches

#### Scenario: Filesystem tools use validate-path-operation
- **WHEN** a filesystem tool (read_file, write_file, edit_file) is validated
- **THEN** the wrapper extracts the path and operation type and calls `validate-path-operation`

#### Scenario: Bash pipeline stage 6 uses validate-path-operation
- **WHEN** the bash validation pipeline reaches stage 6 (file operation validation)
- **THEN** for each extracted file operation, it calls `validate-path-operation` with the extracted path and operation type
- **AND** uses the same glob matching and error codes as filesystem tool validation

### Requirement: Single glob pattern matching implementation

The validation module SHALL provide exactly one `glob-to-regex` function and one `path-matches-patterns` function. All path matching throughout the scope system SHALL use these functions.

#### Scenario: Double-star matches path separators
- **WHEN** pattern is "/workspace/**" and path is "/workspace/sub/deep/file.txt"
- **THEN** the pattern matches

#### Scenario: Single-star does not match path separators
- **WHEN** pattern is "/workspace/*.el" and path is "/workspace/sub/init.el"
- **THEN** the pattern does not match

#### Scenario: Question mark matches single character
- **WHEN** pattern is "/tmp/file?" and path is "/tmp/file1"
- **THEN** the pattern matches

#### Scenario: Mid-path double-star matches zero or more directories
- **WHEN** pattern is "**/.git/**" and path is "/workspace/.git/config"
- **THEN** the pattern matches

#### Scenario: Same implementation used by filesystem and bash validators
- **WHEN** a filesystem tool checks path "/workspace/file.txt" against pattern "/workspace/**"
- **AND** a bash command extracting file operation for "/workspace/file.txt" checks against the same pattern
- **THEN** both use the identical glob-to-regex function and produce the same result

### Requirement: Consolidated error codes

The validation module SHALL use a single set of canonical error codes for path validation. Both filesystem tool validation and bash file operation validation SHALL produce identical error codes for the same denial reason.

#### Scenario: Deny pattern produces consistent error code
- **WHEN** a path is denied by a deny pattern via filesystem tool validation
- **THEN** the error code is "denied-pattern"
- **AND** when the same path is denied by the same deny pattern via bash pipeline validation
- **THEN** the error code is also "denied-pattern"

#### Scenario: Out-of-scope produces consistent error code
- **WHEN** a path is not in any allow list via filesystem tool validation
- **THEN** the error code is "not-in-scope"
- **AND** when the same path is not in any allow list via bash pipeline validation
- **THEN** the error code is also "not-in-scope"

### Requirement: Allow-once mechanism

The validation module SHALL provide allow-once temporary permissions with a single check point. The allow-once check SHALL occur exactly once per validation request.

#### Scenario: Allow-once grants temporary access
- **WHEN** tool and resource are in the allow-once list
- **THEN** validation returns `(:allowed t)` without checking scope config

#### Scenario: Allow-once permission consumed before execution
- **WHEN** validation succeeds via allow-once
- **THEN** the permission is removed from the list immediately
- **AND** a subsequent call for the same tool and resource goes through normal validation

#### Scenario: Allow-once checked exactly once
- **WHEN** a tool call enters the validation system
- **THEN** allow-once is checked in exactly one location (not both macro and validator)

#### Scenario: Allow-once cleared after LLM response
- **WHEN** the LLM response completes
- **THEN** all remaining allow-once permissions are cleared via `gptel-post-response-functions`

#### Scenario: Allow-once works without scope config
- **WHEN** tool has allow-once permission but no scope.yml exists
- **THEN** validation succeeds (allow-once bypasses config requirement)

### Requirement: Violation-info building

The validation module SHALL transform validator error plists into violation-info format for the expansion UI, handling all canonical error codes.

#### Scenario: Path denial produces violation-info
- **WHEN** path validator returns denial with error code "denied-pattern" or "not-in-scope"
- **THEN** `build-violation-info` extracts `:resource` from the `:resource` field

#### Scenario: Command denial produces violation-info
- **WHEN** bash validator returns denial with error code "command_denied"
- **THEN** `build-violation-info` extracts `:resource` from the `:command` field

#### Scenario: Cloud auth denial produces violation-info
- **WHEN** bash validator returns denial with error code "cloud_auth_denied" or "cloud_provider_denied"
- **THEN** `build-violation-info` extracts `:resource` from the `:provider` field

#### Scenario: Parse incomplete produces violation-info
- **WHEN** bash validator returns denial with error code "parse_incomplete"
- **THEN** `build-violation-info` extracts `:resource` from the `:command` field

#### Scenario: All canonical error codes handled
- **WHEN** any canonical error code is produced by any validator
- **THEN** `build-violation-info` has a branch that handles it (no fallthrough to nil resource)

### Requirement: Bash validation pipeline preserved

The validation module SHALL preserve the seven-stage bash validation pipeline. The pipeline SHALL call `validate-path-operation` at stage 6 rather than maintaining its own path matching logic.

#### Scenario: Seven stages execute in order
- **WHEN** a bash command enters validation
- **THEN** the pipeline executes: parse → completeness → deny list → extract semantics → no-op → file operations → cloud auth

#### Scenario: Early exit on failure
- **WHEN** any stage fails
- **THEN** the pipeline returns immediately without executing subsequent stages

#### Scenario: Stage 6 delegates to shared path validator
- **WHEN** file operations are extracted at stage 4
- **AND** the command is not a no-op (stage 5)
- **THEN** stage 6 calls `validate-path-operation` for each extracted (path, operation) pair

#### Scenario: No-op commands bypass path validation
- **WHEN** semantic extraction finds zero file operations
- **THEN** the command is allowed without checking path scope

### Requirement: Schema loading in YAML module

The validation module SHALL obtain scope configuration through scope-yaml.el, which SHALL provide schema loading with safe defaults merging.

#### Scenario: Schema loaded with defaults
- **WHEN** scope config is loaded from scope.yml
- **THEN** scope-yaml provides defaults for missing sections (empty path lists, cloud auth "warn", security enforcement on)

#### Scenario: Deprecated categories section rejected
- **WHEN** scope.yml contains a `bash_tools.categories` section
- **THEN** schema loading returns a malformed-config error

#### Scenario: Snake-case to kebab-case normalization
- **WHEN** scope.yml uses snake_case keys (e.g., `auth_detection`)
- **THEN** the parsed config uses kebab-case keys (e.g., `:auth-detection`)
