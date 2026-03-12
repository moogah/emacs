## ADDED Requirements

### Requirement: Command semantics database
The system SHALL maintain a database mapping command names to their file operation semantics, defining how each command interacts with file arguments.

#### Scenario: Lookup simple read command
- **WHEN** looking up semantics for "cat"
- **THEN** system returns operation type `:read` and source `:positional-args`

#### Scenario: Lookup command with skip rules
- **WHEN** looking up semantics for "grep"
- **THEN** system returns operation type `:read`, source `:positional-args`, and skip-first rule (pattern argument)

#### Scenario: Lookup unknown command
- **WHEN** looking up semantics for unrecognized command
- **THEN** system returns nil or empty semantics

### Requirement: Multi-operation command support
The system SHALL support commands that perform multiple operation types on different arguments (read source, write destination).

#### Scenario: Copy command with read and write operations
- **WHEN** extracting semantics for "cp"
- **THEN** system returns two operations: `:read` for indices 0 to N-2, `:write` for index -1

#### Scenario: Move command with delete and write operations
- **WHEN** extracting semantics for "mv"
- **THEN** system returns `:delete` for source arguments and `:write` for destination argument

### Requirement: Subcommand-specific semantics
The system SHALL support commands with subcommands where file operation semantics vary by subcommand (git, docker, npm).

#### Scenario: Git add subcommand
- **WHEN** extracting semantics for "git" with subcommand "add"
- **THEN** system returns operation type `:read` for positional args

#### Scenario: Git checkout subcommand
- **WHEN** extracting semantics for "git" with subcommand "checkout"
- **THEN** system returns operation type `:modify` for positional args

### Requirement: Operation type classification
The system SHALL classify file operations into distinct types: `:read`, `:write`, `:delete`, `:modify`, `:create`, `:create-or-modify`.

#### Scenario: Read operation classification
- **WHEN** command only reads file contents
- **THEN** operation type is `:read`

#### Scenario: Write operation classification
- **WHEN** command creates new file or overwrites existing
- **THEN** operation type is `:write`

#### Scenario: Delete operation classification
- **WHEN** command removes file or directory
- **THEN** operation type is `:delete`

#### Scenario: Modify operation classification
- **WHEN** command changes existing file without replacing it
- **THEN** operation type is `:modify`

### Requirement: Core command coverage
The system SHALL include semantics for at least 20 core commands covering read, write, delete, copy, move, and directory operations.

#### Scenario: File reading commands
- **WHEN** database is queried for file reading commands
- **THEN** semantics exist for cat, head, tail, less, grep, wc

#### Scenario: File writing commands
- **WHEN** database is queried for file writing commands
- **THEN** semantics exist for touch, echo (via redirection), tee

#### Scenario: File deletion commands
- **WHEN** database is queried for file deletion commands
- **THEN** semantics exist for rm, rmdir

#### Scenario: File manipulation commands
- **WHEN** database is queried for file manipulation commands
- **THEN** semantics exist for cp, mv, ln, chmod, chown

### Requirement: Positional argument indexing
The system SHALL support flexible positional argument indexing including single index, index ranges, and special indices (first, last, all-but-last).

#### Scenario: Single index specification
- **WHEN** semantics specify index 0
- **THEN** only first positional argument is considered

#### Scenario: Last argument specification
- **WHEN** semantics specify index -1
- **THEN** only last positional argument is considered

#### Scenario: Range specification
- **WHEN** semantics specify indices 0-to-N-2
- **THEN** all positional arguments except last are considered

### Requirement: Flag-dependent operations
The system SHALL support commands where operation type depends on flags (sed -i makes in-place edits, tar -c creates vs -x extracts).

#### Scenario: In-place editing flag
- **WHEN** "sed" command has "-i" flag
- **THEN** operation type is `:modify` for positional args

#### Scenario: Sed without in-place flag
- **WHEN** "sed" command lacks "-i" flag
- **THEN** operation type is `:read` for positional args
