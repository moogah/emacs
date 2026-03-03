## MODIFIED Requirements

### Requirement: Operation type classification
The system SHALL classify file operations into distinct types: `:read`, `:write`, `:delete`, `:modify`, `:create`, `:create-or-modify`, `:append`, `:execute`.

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

#### Scenario: Execute operation classification
- **WHEN** command executes file as code or script
- **THEN** operation type is `:execute`

## ADDED Requirements

### Requirement: Interpreter command semantics
The system SHALL include semantics entries for interpreter commands that execute script files, marking the first positional argument with `:execute` operation.

#### Scenario: Python interpreter semantics
- **WHEN** looking up semantics for "python" or "python3"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Node interpreter semantics
- **WHEN** looking up semantics for "node"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Bash interpreter semantics
- **WHEN** looking up semantics for "bash"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Shell interpreter semantics
- **WHEN** looking up semantics for "sh" or "zsh"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Source built-in semantics
- **WHEN** looking up semantics for "source" or "."
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

### Requirement: Subcommand-based interpreter semantics
The system SHALL include semantics entries for commands where execution depends on subcommand (go run vs go build).

#### Scenario: Go run subcommand semantics
- **WHEN** looking up semantics for "go" with subcommand "run"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Go test subcommand semantics
- **WHEN** looking up semantics for "go" with subcommand "test"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Go build subcommand semantics
- **WHEN** looking up semantics for "go" with subcommand "build"
- **THEN** system returns operation type `:read`, source `:positional-args`, index 0

### Requirement: Single index extraction for interpreters
The system SHALL support `:index 0` specification to extract only the first positional argument (the script file), not subsequent arguments.

#### Scenario: Extract first argument only
- **WHEN** semantics specify `:index 0`
- **THEN** only first positional argument is extracted as file operation

#### Scenario: Skip remaining arguments
- **WHEN** command has multiple positional arguments and semantics specify `:index 0`
- **THEN** arguments after index 0 are not extracted as file operations
