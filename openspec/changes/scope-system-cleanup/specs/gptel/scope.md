## REMOVED Requirements

### Requirement: Tool categorization system

**Reason**: Tool categorization as a central const is replaced by per-tool operation type declaration at the macro call site. The central mapping created coupling — adding a tool required updating a separate const in scope-core.el. The new design has each tool declare its own operation type.

**Migration**: Each tool definition using `gptel-make-scoped-tool` now includes `:operation` keyword (read, write, execute, modify) directly. Remove `jf/gptel-scope--tool-categories` const entirely.

### Requirement: Pattern-based validation

**Reason**: Org-roam tools have no proper specifications, are unused in practice, and add complexity to the validation system (separate validation path, separate error codes, separate expansion UI handling). The pattern validation type exists solely for org-roam and has no other consumers.

**Migration**: Remove `scope-org-roam-tools.el/org` entirely. Remove `validate-pattern-tool` function. Remove `org_roam_patterns` handling from expansion UI. Remove `add-pattern-to-scope` from scope-expansion.el. Remove "pattern" from validation types in interfaces.el.

### Requirement: Validation function signatures accept metadata

**Reason**: The function signatures described (check-tool-permission, validate-path-tool, validate-pattern-tool, validate-bash-tool) no longer exist in their current form. The new validation module has a different interface: `validate-path-operation` and the bash pipeline entry point. Metadata is passed through the new interface.

**Migration**: Use the new validation module's entry points which accept metadata as part of their interface.

### Requirement: Scoped tool macro gathers metadata

**Reason**: This requirement is superseded by the scope-tool-wrapper spec which defines metadata gathering as part of the thin wrapper's responsibilities.

**Migration**: See scope-tool-wrapper spec, "Metadata gathering for path tools" requirement.

## MODIFIED Requirements

### Requirement: Configuration loading from scope.yml

The scope system SHALL load scope configuration from scope.yml in session's branch directory.

**CRITICAL**: Configuration is **NOT cached** - scope.yml is read fresh on every tool call. Filesystem is source of truth.

Schema loading with safe defaults merging SHALL be provided by scope-yaml.el (not scope-shell-tools.el).

#### Scenario: Configuration loaded from scope.yml
- **WHEN** tool executes and needs scope validation
- **THEN** system reads scope configuration via scope-yaml.el from scope.yml in buffer's branch directory
- **AND** does NOT use cached configuration (filesystem is source of truth)
- **AND** scope-yaml merges safe defaults for missing sections

#### Scenario: Missing configuration handled gracefully
- **WHEN** no scope.yml exists in branch directory
- **THEN** system returns "no_scope_config" error to tool

#### Scenario: Buffer context determines directory
- **WHEN** tool executes in gptel buffer
- **THEN** system uses buffer-local `jf/gptel--branch-dir` variable to locate scope.yml

### Requirement: Macro-based tool wrapping

The scope system SHALL wrap scope-aware tools using a thin macro that intercepts arguments, delegates to the validation module, and returns structured errors on denial. The macro SHALL NOT contain validation logic.

#### Scenario: Wrapped tool extracts first argument
- **WHEN** scope-wrapped tool called
- **THEN** wrapper extracts first argument as resource identifier
- **AND** normalizes from JSON vectors to Elisp lists

#### Scenario: Tool declares operation type
- **WHEN** a scoped tool is defined
- **THEN** the macro call includes `:operation` specifying read, write, execute, or modify

#### Scenario: Validation delegated to validation module
- **WHEN** wrapped tool called
- **THEN** wrapper calls validation module entry point
- **AND** wrapper does NOT contain path matching, error code checking, or other validation logic

#### Scenario: Structured error on denial
- **WHEN** scope validation fails
- **THEN** wrapper returns JSON with `:success nil`, `:error` (machine code), `:message` (human text)

#### Scenario: Meta tools skip validation wrapper
- **WHEN** tool defined with `:meta t`
- **THEN** wrapper allows call without scope checks

### Requirement: Error message formatting

The scope system SHALL return structured error messages using a unified format across all validator types. All validators SHALL use `:error` for machine-readable error codes and `:message` for human-readable text.

#### Scenario: All validators use :error and :message fields
- **WHEN** any validator (path, bash) denies an operation
- **THEN** the return plist contains `:error` with a machine-readable code
- **AND** the return plist contains `:message` with human-readable text

#### Scenario: Path denial includes allowed patterns
- **WHEN** path validation fails
- **THEN** error includes `:allowed_patterns` list from the relevant paths section

#### Scenario: Deny pattern error distinguishes from allow failure
- **WHEN** resource explicitly denied
- **THEN** error uses `:denied_patterns` list (not allowed_patterns)

### Requirement: Violation-info transformation

The scope system SHALL transform validator error plists into a unified violation-info format for the expansion UI. The transformation SHALL handle all canonical error codes without missing branches.

#### Scenario: Violation-info produced from path validator denial
- **WHEN** path validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info produced from bash pipeline denial
- **WHEN** bash pipeline validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: All canonical error codes handled
- **WHEN** any canonical error code is produced
- **THEN** `build-violation-info` has a handling branch (including cloud_provider_denied and parse_incomplete)

### Requirement: Allow-once temporary permissions

The scope system SHALL support temporary single-use permissions lasting only for current LLM response turn.

**Consumption semantics**: Permission is consumed BEFORE tool body executes. Allow-once is checked in exactly one location (the validation module), not in both the macro and the validator.

#### Scenario: Allow-once grants temporary access
- **WHEN** tool and resource added to allow-once list
- **THEN** next validation check for exact tool and resource succeeds

#### Scenario: Allow-once permission consumed BEFORE execution
- **WHEN** tool validation succeeds via allow-once permission
- **THEN** permission removed from allow-once list immediately (before tool body executes)

#### Scenario: Allow-once cleared after response
- **WHEN** LLM response completes
- **THEN** system clears all allow-once permissions via `gptel-post-response-functions` hook

#### Scenario: Allow-once checked in single location
- **WHEN** validating tool call
- **THEN** allow-once is checked exactly once, in the validation module
- **AND** the wrapper macro does NOT independently check allow-once

#### Scenario: Allow-once works without config
- **WHEN** tool granted allow-once permission but no scope config exists
- **THEN** tool succeeds (allow-once bypasses missing config check)
