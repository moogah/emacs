# Filesystem Tools Inline Expansion

## Purpose

Enables inline scope expansion for scoped filesystem tools (read_file, write_file_in_scope, edit_file_in_scope), providing automatic UI-driven approval workflow when scope validation fails, matching the user experience of run_bash_command.

## ADDED Requirements

### Requirement: Async tool signature with callback

The filesystem tools SHALL use async (callback-first) signature to enable inline expansion workflow.

**Implementation**: Add `:async` keyword to gptel-make-scoped-tool macro invocation

#### Scenario: read_file uses async signature
- **WHEN** read_file tool is defined
- **THEN** it includes `:async` keyword in gptel-make-scoped-tool
- **AND** function signature is `(lambda (callback filepath) ...)`

#### Scenario: write_file_in_scope uses async signature
- **WHEN** write_file_in_scope tool is defined
- **THEN** it includes `:async` keyword in gptel-make-scoped-tool
- **AND** function signature is `(lambda (callback filepath content) ...)`

#### Scenario: edit_file_in_scope uses async signature
- **WHEN** edit_file_in_scope tool is defined
- **THEN** it includes `:async` keyword in gptel-make-scoped-tool
- **AND** function signature is `(lambda (callback filepath old_string new_string) ...)`

### Requirement: Inline expansion trigger on validation failure

When scope validation fails for filesystem tools, the system SHALL automatically trigger the inline expansion UI without requiring separate LLM tool call.

#### Scenario: read_file validation fails triggers expansion UI
- **WHEN** read_file validation fails (path not in scope)
- **THEN** system calls `jf/gptel-scope--trigger-inline-expansion` automatically
- **AND** expansion UI displays with filepath and violation details

#### Scenario: write_file validation fails triggers expansion UI
- **WHEN** write_file_in_scope validation fails (path not in write scope)
- **THEN** system calls `jf/gptel-scope--trigger-inline-expansion` automatically
- **AND** expansion UI displays with filepath and violation details

#### Scenario: edit_file validation fails triggers expansion UI
- **WHEN** edit_file_in_scope validation fails (path not in write scope)
- **THEN** system calls `jf/gptel-scope--trigger-inline-expansion` automatically
- **AND** expansion UI displays with filepath and violation details

### Requirement: Three-choice expansion menu

The expansion UI SHALL present three action choices: deny, add-to-scope (permanent), and allow-once (temporary).

#### Scenario: User denies expansion
- **WHEN** user selects "Deny" in expansion UI
- **THEN** tool returns scope violation error to LLM
- **AND** no permission is granted

#### Scenario: User adds to scope permanently
- **WHEN** user selects "Add to scope (permanent)" in expansion UI
- **THEN** system updates scope.yml with new path pattern
- **AND** tool retries validation
- **AND** tool executes if retry succeeds

#### Scenario: User allows once temporarily
- **WHEN** user selects "Allow once (temporary)" in expansion UI
- **THEN** system adds permission to allow-once list
- **AND** tool retries validation
- **AND** tool executes if retry succeeds

### Requirement: Validation retry after approval

After user approval (add-to-scope or allow-once), the system SHALL retry validation and execute tool if validation passes.

#### Scenario: Add-to-scope retry succeeds
- **WHEN** user approves with add-to-scope
- **AND** scope.yml is updated with new pattern
- **AND** validation is retried
- **THEN** validation passes
- **AND** tool body executes

#### Scenario: Allow-once retry succeeds
- **WHEN** user approves with allow-once
- **AND** permission is added to allow-once list
- **AND** validation is retried
- **THEN** validation passes (via allow-once check)
- **AND** tool body executes

#### Scenario: Retry fails after approval
- **WHEN** user approves expansion
- **AND** validation retry still fails
- **THEN** tool returns validation error to LLM

### Requirement: Allow-once permission format

The system SHALL store allow-once permissions as `(tool-name . filepath)` pairs where filepath is the expanded absolute path.

#### Scenario: read_file allow-once format
- **WHEN** read_file granted allow-once for "/tmp/file.txt"
- **THEN** permission stored as `("read_file" . "/tmp/file.txt")`

#### Scenario: write_file allow-once format
- **WHEN** write_file_in_scope granted allow-once for "/workspace/output.txt"
- **THEN** permission stored as `("write_file_in_scope" . "/workspace/output.txt")`

#### Scenario: Allow-once consumed after use
- **WHEN** tool executes with allow-once permission
- **THEN** permission is removed from allow-once list
- **AND** subsequent call to same tool with same path requires re-approval

### Requirement: Error message consistency

Filesystem tools SHALL return error messages in same format as run_bash_command for consistency.

#### Scenario: Scope violation error format
- **WHEN** filesystem tool validation fails
- **THEN** error includes `:success nil`, `:error "scope_violation"`, `:tool` name, `:resource` filepath, `:allowed_patterns` list
