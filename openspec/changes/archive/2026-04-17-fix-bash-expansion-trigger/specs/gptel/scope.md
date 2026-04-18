## MODIFIED Requirements

### Requirement: Bash tool categorization

The scope system SHALL support bash validation type for tools that execute shell commands with semantic validation.

**Config source:** `scope.yml` in the session's branch directory, under the `bash_tools` top-level key with deny list only (no categories).

**Tool:** `run_bash_command` - Executes shell commands with seven-stage validation pipeline using bash-parser integration.

**Module:** `scope-shell-tools` (config/gptel/scope/scope-shell-tools.el) - Implements the run_bash_command tool using bash-parser semantic validation.

#### Scenario: Bash tool categorized
- **WHEN** a tool executes arbitrary shell commands
- **THEN** the system categorizes it with validation strategy "bash" and operation type "write"

#### Scenario: Bash validator dispatched
- **WHEN** scope system validates a bash tool
- **THEN** it routes to jf/gptel-scope--validate-bash-tool function using seven-stage pipeline

### Requirement: Macro-level validation responsibility

The `gptel-make-scoped-tool` macro SHALL be the single location where validation, expansion triggering, and retry logic occurs. Individual tool bodies SHALL NOT perform their own validation or expansion. By the time a tool body executes, validation has already passed at the macro level.

**Implementation**: `config/gptel/scope/scope-core.org` - `gptel-make-scoped-tool` macro

#### Scenario: Tool body executes only after macro validation passes
- **WHEN** a scope-aware tool is invoked
- **THEN** the macro validates the tool call before executing the tool body
- **AND** the tool body is never reached if validation fails

#### Scenario: Bash validation runs at macro level
- **WHEN** a bash tool's validation is dispatched via `check-tool-permission`
- **THEN** `validate-bash-tool` runs the full 7-stage semantic validation pipeline
- **AND** returns `:allowed nil` with error details on validation failure
- **AND** the macro triggers expansion UI on failure (for async tools)

#### Scenario: Tool body does not duplicate validation
- **WHEN** a tool body executes
- **THEN** it assumes validation has already passed
- **AND** it does not call `validate-command-semantics` or any other validation function
- **AND** it only contains execution logic (running the command, formatting output)

#### Scenario: Expansion trigger is macro-only
- **WHEN** validation fails for any tool type (path, pattern, or bash)
- **THEN** the macro's expansion branch triggers `jf/gptel-scope--trigger-inline-expansion`
- **AND** no other code path triggers expansion

### Requirement: Bash tool validator performs semantic validation

The `validate-bash-tool` function SHALL perform the full seven-stage semantic validation pipeline and return `:allowed nil` on failure, enabling the macro's expansion trigger to fire for bash denials.

**Implementation**: `config/gptel/scope/scope-core.org` or `scope-shell-tools.org` - `jf/gptel-scope--validate-bash-tool`

#### Scenario: Validate-bash-tool runs semantic pipeline
- **WHEN** `validate-bash-tool` is called with tool-name, args, config, metadata
- **THEN** it calls `jf/gptel-scope--validate-command-semantics` with command, directory, and config
- **AND** returns `:allowed t` if validation passes
- **AND** returns `:allowed nil` with the validation error details if validation fails

#### Scenario: Missing bash_tools config denies all commands
- **WHEN** config has no `bash_tools` section
- **THEN** `validate-bash-tool` returns `:allowed nil` with error "command-not-allowed"

#### Scenario: Legacy categories section rejected
- **WHEN** config has `bash_tools.categories` section
- **THEN** `validate-bash-tool` returns `:allowed nil` with error "malformed-config" and migration message

#### Scenario: Validation error fields propagated
- **WHEN** semantic validation fails (path_out_of_scope, command_denied, cloud_auth_denied, etc.)
- **THEN** `validate-bash-tool` propagates the error plist fields (`:error`, `:message`, `:path`, `:command`, etc.) in its return value
- **AND** the macro can build violation-info from these fields

### Requirement: Violation-info transformation

The scope system SHALL transform validator error plists into a unified violation-info format for the expansion UI. The transformation SHALL read `:error` and `:message` from all validator types without fallback chains.

#### Scenario: Violation-info produced from bash semantic validation denial
- **WHEN** bash validator returns `:allowed nil` with `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`
- **AND** the macro triggers expansion UI with the violation-info

#### Scenario: Violation-info produced from path validator denial
- **WHEN** path validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info produced from bash pipeline denial
- **WHEN** bash pipeline validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info produced from pattern validator denial
- **WHEN** pattern validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info reason is always human-readable
- **WHEN** `build-violation-info` extracts the `:reason` for the output plist
- **THEN** it reads from the input `:message` field (human-readable text)
- **AND** it does NOT use the `:error` field (machine code) as the reason

#### Scenario: Resource extracted from validator-specific fields
- **WHEN** `build-violation-info` extracts `:resource`
- **THEN** it reads from `:path` (for path_out_of_scope, path_denied), `:command` (for command_denied), or `:provider` (for cloud_auth_denied)
