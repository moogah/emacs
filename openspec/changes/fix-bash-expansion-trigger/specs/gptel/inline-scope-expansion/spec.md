## MODIFIED Requirements

### Requirement: Inline expansion UI on validation failure

The scope validation system SHALL automatically trigger the expansion UI when a scope-aware tool's validation fails, allowing the tool to execute immediately upon user approval within the same tool call. This applies to ALL validation types: path, pattern, and bash.

#### Scenario: Validation failure triggers expansion UI automatically
- **WHEN** a scope-aware tool's validation fails
- **THEN** the system automatically triggers the expansion transient menu
- **AND** the user sees the same 3-choice UI (deny/add-to-scope/allow-once) as `request_scope_expansion`

#### Scenario: Bash validation failure triggers expansion UI
- **WHEN** a bash tool's semantic validation fails (path_out_of_scope, command_denied, cloud_auth_denied, etc.)
- **THEN** the macro triggers expansion UI with violation-info built from the validation error
- **AND** the user can approve the operation via allow-once or add-to-scope

#### Scenario: User approves with allow-once in inline flow
- **WHEN** validation fails and triggers inline expansion UI
- **AND** user selects "Allow once (temporary)"
- **THEN** permission is added to allow-once list
- **AND** validation is retried immediately
- **AND** validation succeeds via allow-once permission
- **AND** tool body executes in the same tool call
- **AND** tool returns success response to LLM

#### Scenario: User approves with add-to-scope in inline flow
- **WHEN** validation fails and triggers inline expansion UI
- **AND** user selects "Add to scope (permanent)"
- **THEN** pattern is added to scope.yml
- **AND** validation is retried immediately
- **AND** validation succeeds via updated scope.yml
- **AND** tool body executes in the same tool call
- **AND** tool returns success response to LLM

#### Scenario: User denies inline expansion request
- **WHEN** validation fails and triggers inline expansion UI
- **AND** user selects "Deny (reject tool call)"
- **THEN** tool returns error to LLM
- **AND** error structure matches current validation error format
- **AND** tool body does not execute

#### Scenario: Inline expansion preserves violation context
- **WHEN** validation fails and triggers inline expansion UI
- **THEN** violation info includes tool name, resource, reason (human-readable), and validation type
- **AND** transient menu displays full context to user
- **AND** context is sufficient for user to make informed decision

#### Scenario: Expansion UI receives human-readable reason from all validator types
- **WHEN** path, pattern, or bash validator triggers expansion UI
- **THEN** the `:reason` field in violation-info contains human-readable text (from validator's `:message` field)
- **AND** the UI never displays machine codes like "denied-pattern" or "command-not-allowed" as the reason

### Requirement: Async-capable scope validation wrapper

The scope validation wrapper SHALL support async operation to enable inline expansion UI while maintaining backward compatibility with synchronous validation. The macro is the single point of validation and expansion for all tool types.

#### Scenario: Scope wrapper detects async tool capability
- **WHEN** a scope-aware tool is registered with `:async t`
- **THEN** the scope validation wrapper operates in async mode
- **AND** validation failures trigger inline expansion UI

#### Scenario: Synchronous tools maintain current behavior
- **WHEN** a scope-aware tool is registered without `:async t`
- **THEN** the scope validation wrapper operates in sync mode
- **AND** validation failures return error immediately without UI

#### Scenario: Validation retry after approval
- **WHEN** user approves expansion (allow-once or add-to-scope)
- **THEN** the scope validation wrapper retries validation automatically
- **AND** retry uses the updated allow-once list or scope.yml
- **AND** retry success triggers tool body execution

#### Scenario: All validation types handled uniformly by macro
- **WHEN** validation fails for path, pattern, or bash tool type
- **THEN** the macro handles all three uniformly via `check-tool-permission` → `trigger-inline-expansion`
- **AND** no tool-type-specific expansion code exists outside the macro
