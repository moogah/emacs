## MODIFIED Requirements

### Requirement: Integration with scope expansion flow

The bash tools system SHALL automatically trigger inline scope expansion UI when validation fails, allowing commands to execute immediately upon user approval within the same tool call. The `request_scope_expansion` meta-tool remains available for pre-emptive permission requests.

#### Scenario: Validation failure triggers inline expansion UI
- **WHEN** `run_bash_command` validation fails
- **THEN** the system automatically triggers the expansion transient menu
- **AND** user sees 3-choice UI (deny/add-to-scope/allow-once)
- **AND** if user approves, validation is retried and command executes in same tool call
- **AND** if user denies, error is returned to LLM

#### Scenario: User approves with allow-once in inline flow
- **WHEN** validation fails and user selects "Allow once"
- **THEN** permission is added to allow-once list
- **AND** validation is retried immediately
- **AND** command executes successfully in same tool call

#### Scenario: User approves with add-to-scope in inline flow
- **WHEN** validation fails and user selects "Add to scope"
- **THEN** pattern is added to appropriate paths section in scope.yml
- **AND** validation is retried immediately
- **AND** command executes successfully in same tool call

#### Scenario: User denies inline expansion
- **WHEN** validation fails and user selects "Deny"
- **THEN** error is returned to LLM with structured error format
- **AND** command does not execute

#### Scenario: LLM requests path pattern expansion pre-emptively
- **WHEN** `request_scope_expansion` is called for run_bash_command before attempting command
- **THEN** the system infers validation type and presents transient menu to user
- **AND** approval adds to allow-once list or scope.yml for subsequent command

#### Scenario: User approves path pattern permanently via request_scope_expansion
- **WHEN** user selects "Add to scope" in expansion menu via `request_scope_expansion`
- **THEN** the system adds path pattern to appropriate paths section in scope.yml (read/write/execute/modify)

#### Scenario: User approves path once via request_scope_expansion
- **WHEN** user selects "Allow once" in expansion menu via `request_scope_expansion`
- **THEN** the system adds to allow-once list for current turn

#### Scenario: LLM requests command expansion for denied command
- **WHEN** `request_scope_expansion` is called for command in deny list
- **THEN** the system presents option to remove from deny list (dangerous operation)

## MODIFIED Requirements

### Requirement: Structured error responses with expansion guidance

The bash tools system SHALL return structured errors when inline expansion is denied, maintaining the same error format that previously guided LLMs to use `request_scope_expansion`.

#### Scenario: Command denied error structure
- **WHEN** a command is in deny list
- **THEN** the system returns :allowed nil with :reason "denied-command", tool, command, and security warning

#### Scenario: Parse incomplete error structure
- **WHEN** command cannot be fully parsed
- **THEN** error includes :error "incomplete_parse", :parse_errors, :partial_tokens

#### Scenario: Path out of scope error with operation detail
- **WHEN** file path validation fails and user denies expansion
- **THEN** error includes :error "path_out_of_scope", :path, :operation, :required_scope, :allowed_patterns

#### Scenario: Pipeline command denied with position
- **WHEN** pipeline command fails deny list validation
- **THEN** error includes :pipeline_position, :failed_command, :full_pipeline

#### Scenario: Cloud auth denied error with provider
- **WHEN** cloud auth command denied
- **THEN** error includes :error "cloud_auth_denied", :provider, :allowed_providers

#### Scenario: Coverage metrics in successful responses
- **WHEN** command executes successfully
- **THEN** result may include :coverage with :total_tokens, :claimed_tokens, :coverage_ratio

#### Scenario: Error messages no longer suggest request_scope_expansion
- **WHEN** any scope violation occurs and user denies inline expansion
- **THEN** the error message does NOT suggest using request_scope_expansion tool
- **AND** error indicates user denied the expansion request
