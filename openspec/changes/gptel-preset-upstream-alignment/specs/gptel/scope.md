## MODIFIED Requirements

### Requirement: Configuration loading from scope document
The scope system SHALL load scope configuration from `scope.yml` located in the session's branch directory.

#### Scenario: Configuration loaded from scope.yml
- **WHEN** a tool executes and needs scope validation
- **THEN** the system reads `scope.yml` from the buffer's branch directory

#### Scenario: Missing scope.yml handled gracefully
- **WHEN** no `scope.yml` exists in the branch directory
- **THEN** the system checks for legacy `preset.md` as a fallback
- **AND** if neither exists, returns a "no_scope_config" error to the tool

#### Scenario: Legacy preset.md fallback
- **WHEN** `scope.yml` does not exist but `preset.md` does in the branch directory
- **THEN** the system reads scope configuration from `preset.md` YAML frontmatter
- **AND** logs a deprecation warning

#### Scenario: Buffer context determines directory
- **WHEN** a tool executes in a gptel buffer
- **THEN** the system uses the buffer-local `jf/gptel--branch-dir` variable to locate `scope.yml`

### Requirement: Macro-based tool wrapping
The scope system SHALL provide a macro that wraps tool definitions to automatically handle validation without boilerplate.

#### Scenario: Macro loads config automatically
- **WHEN** a scoped tool is invoked via the macro
- **THEN** the macro loads scope configuration from `scope.yml` without tool code needing to do so

#### Scenario: Macro normalizes arguments
- **WHEN** tool arguments arrive as JSON vectors
- **THEN** the macro converts them to lists before passing to validation and tool body

#### Scenario: Macro checks permissions before body
- **WHEN** a scoped tool is invoked
- **THEN** the macro validates permissions before executing the tool body

#### Scenario: Tool body executes only if allowed
- **WHEN** permission check passes
- **THEN** the macro executes the tool body and returns its result

#### Scenario: Macro returns formatted error if denied
- **WHEN** permission check fails
- **THEN** the macro prevents tool body execution and returns structured error response

#### Scenario: Macro handles exceptions
- **WHEN** validation or tool execution throws an exception
- **THEN** the macro catches it and returns :error "tool_exception" with error message
