## ADDED Requirements

### Requirement: Inline expansion UI on validation failure

The scope validation system SHALL automatically trigger the expansion UI when a scope-aware tool's validation fails, allowing the tool to execute immediately upon user approval within the same tool call.

#### Scenario: Validation failure triggers expansion UI automatically
- **WHEN** a scope-aware tool's validation fails
- **THEN** the system automatically triggers the expansion transient menu
- **AND** the user sees the same 3-choice UI (deny/add-to-scope/allow-once) as `request_scope_expansion`

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
- **THEN** violation info includes tool name, resource, operation, and validation type
- **AND** transient menu displays full context to user
- **AND** context is sufficient for user to make informed decision

### Requirement: Async-capable scope validation wrapper

The scope validation wrapper SHALL support async operation to enable inline expansion UI while maintaining backward compatibility with synchronous validation.

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

### Requirement: Violation info construction from validation errors

The system SHALL construct violation info suitable for expansion UI from validation error plists returned by the validation pipeline.

#### Scenario: Path out of scope error builds violation info
- **WHEN** validation fails with `:error "path_out_of_scope"`
- **THEN** violation info includes `:tool`, `:resource` (path), `:operation`, `:validation-type 'bash`
- **AND** resource is the failed path from error plist

#### Scenario: Command denied error builds violation info
- **WHEN** validation fails with `:error "command_denied"`
- **THEN** violation info includes `:tool`, `:resource` (command name), `:validation-type 'bash`
- **AND** resource is the denied command name

#### Scenario: Cloud auth denied error builds violation info
- **WHEN** validation fails with `:error "cloud_auth_denied"`
- **THEN** violation info includes `:tool`, `:resource` (provider), `:validation-type 'bash`
- **AND** resource is the cloud provider name

#### Scenario: Incomplete parse error builds violation info
- **WHEN** validation fails with `:error "incomplete_parse"`
- **THEN** violation info includes `:tool`, `:resource` (command), `:validation-type 'bash`
- **AND** resource is the original command string

### Requirement: Backward compatibility with request_scope_expansion

The `request_scope_expansion` meta-tool SHALL remain available for LLM to pre-emptively request permissions before attempting tool calls.

#### Scenario: request_scope_expansion still works for pre-emptive requests
- **WHEN** LLM calls `request_scope_expansion` with tool name and patterns
- **THEN** expansion UI is triggered
- **AND** user sees the same 3-choice menu
- **AND** approval adds to allow-once list or scope.yml
- **AND** subsequent tool calls can use the granted permission

#### Scenario: LLM can choose between inline and pre-emptive expansion
- **WHEN** LLM anticipates needing permission
- **THEN** LLM can call `request_scope_expansion` first (pre-emptive)
- **OR** LLM can call tool directly and let inline expansion trigger (reactive)
- **AND** both approaches result in the same outcome
