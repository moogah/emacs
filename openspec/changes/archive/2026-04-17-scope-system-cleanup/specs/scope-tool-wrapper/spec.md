## ADDED Requirements

### Requirement: Thin tool wrapper macro

The tool wrapper SHALL provide a `gptel-make-scoped-tool` macro that is the single entry point for scope validation. The macro SHALL be thin — it loads config, calls the validation module, and handles the result. It SHALL NOT contain validation logic itself.

#### Scenario: Tool declares operation type at definition site
- **WHEN** a scoped tool is defined using `gptel-make-scoped-tool`
- **THEN** the definition includes an `:operation` keyword (read, write, execute, modify)
- **AND** this operation type is passed to the validation module

#### Scenario: Wrapper loads config fresh each call
- **WHEN** a scoped tool is invoked
- **THEN** the wrapper reads scope.yml from the buffer's branch directory
- **AND** does NOT use cached configuration

#### Scenario: Missing config returns structured error
- **WHEN** no scope.yml exists in the branch directory
- **THEN** the wrapper returns a "no_scope_config" error to the tool caller

#### Scenario: Wrapper normalizes arguments
- **WHEN** tool arguments arrive as JSON vectors
- **THEN** the wrapper converts them to Elisp lists before passing to validation

#### Scenario: Wrapper calls validation module
- **WHEN** config is loaded and arguments are normalized
- **THEN** the wrapper calls the validation module's entry point with tool-name, operation, args, and config
- **AND** does NOT perform any validation logic itself

#### Scenario: Successful validation executes tool body
- **WHEN** the validation module returns `(:allowed t)`
- **THEN** the wrapper executes the tool's body function

#### Scenario: Failed validation returns structured error
- **WHEN** the validation module returns a denial
- **THEN** the wrapper formats it as a JSON error for the LLM with `:success nil`, `:error`, `:message`

#### Scenario: Failed async validation triggers expansion UI
- **WHEN** validation fails for a tool defined with `:async t`
- **THEN** the wrapper triggers the expansion UI with violation-info
- **AND** queues the expansion for user interaction

### Requirement: No central tool categorization

The tool wrapper SHALL NOT maintain a central mapping of tool names to validation types. Each tool declares its own operation type at definition time.

#### Scenario: Operation type declared at tool definition
- **WHEN** `read_file` tool is defined
- **THEN** its definition specifies `:operation read`
- **AND** no central registry needs updating

#### Scenario: New tool requires no registry update
- **WHEN** a new scoped tool is added to the system
- **THEN** only the tool's own definition file needs to specify its operation type
- **AND** no other file in the scope system needs modification

### Requirement: Validation type routing

The tool wrapper SHALL route to the appropriate validator based on the tool's characteristics, not a lookup table.

#### Scenario: Path-based tool routed to path validator
- **WHEN** a tool with `:operation read` (or write/execute/modify) is invoked with a file path argument
- **THEN** the wrapper routes to `validate-path-operation`

#### Scenario: Bash tool routed to bash pipeline
- **WHEN** `run_bash_command` is invoked with a command string and working directory
- **THEN** the wrapper routes to the bash validation pipeline

#### Scenario: Meta tool bypasses validation
- **WHEN** a tool is defined with `:meta t`
- **THEN** the wrapper allows execution without calling the validation module

### Requirement: Metadata gathering for path tools

The tool wrapper SHALL gather file metadata before validation for path-based tools.

#### Scenario: Metadata gathered for path operations
- **WHEN** a path-based tool is invoked
- **THEN** the wrapper calls `gather-file-metadata` on the target path
- **AND** passes the metadata to the validation module

#### Scenario: Metadata not gathered for bash tools
- **WHEN** a bash tool is invoked
- **THEN** metadata is nil (bash pipeline handles its own context)
