# Interface Preservation

## ADDED Requirements

### Requirement: Public API compatibility

The public API `jf/bash-extract-semantics` MUST preserve its return structure to ensure the gptel scope validation system continues working without modification.

#### Scenario: Function signature
- **WHEN** calling `jf/bash-extract-semantics` with a parsed command
- **THEN** the function SHALL accept a parsed-command plist and optional var-context
- **AND** the argument structure SHALL match the existing contract (`:tokens`, `:parse-complete`, etc.)

#### Scenario: Return structure
- **WHEN** `jf/bash-extract-semantics` completes extraction
- **THEN** the return value SHALL be a plist with these keys: `:domains`, `:coverage`, `:parse-complete`
- **AND** `:domains` SHALL be an alist of `(domain . operations)` pairs
- **AND** `:coverage` SHALL be a coverage plist from `jf/bash-calculate-coverage`
- **AND** `:parse-complete` SHALL be a boolean passed through from input

#### Scenario: Scope validation system compatibility
- **WHEN** the gptel scope validation system calls `jf/bash-extract-semantics`
- **THEN** the return value SHALL contain all data needed for validation
- **AND** no changes SHALL be required in `config/gptel/scope/scope-shell-tools.el`

### Requirement: Command handler API stability

The command handler registration and dispatch API MUST remain stable.

#### Scenario: Handler registration unchanged
- **WHEN** registering a command handler using `jf/bash-register-command-handler`
- **THEN** the function SHALL accept `:command`, `:domain`, and `:handler` keyword args
- **AND** registered handlers SHALL be callable during semantic extraction

#### Scenario: Handler function contract
- **WHEN** a handler is invoked during extraction
- **THEN** the handler SHALL receive a `parsed-command` plist
- **AND** the handler SHALL return a plist with `:domain`, `:operations`, `:claimed-token-ids`, `:metadata`

### Requirement: Coverage tracking unchanged

Token coverage tracking MUST continue to function with the same interface and behavior.

#### Scenario: Token claiming from handlers
- **WHEN** command handlers return `:claimed-token-ids`
- **THEN** those IDs SHALL be included in coverage calculation
- **AND** `jf/bash-calculate-coverage` SHALL receive all claimed token IDs

#### Scenario: Coverage threshold checking
- **WHEN** the scope validation system checks coverage thresholds
- **THEN** the `:coverage` plist SHALL contain `:total-tokens`, `:claimed-tokens`, `:coverage-ratio`
- **AND** coverage percentage calculation SHALL produce consistent results

### Requirement: Parsing interface unchanged

The `jf/bash-parse` function and its return structure MUST remain unchanged.

#### Scenario: Parser output structure
- **WHEN** calling `jf/bash-parse` with a bash command string
- **THEN** the return value SHALL contain `:tokens`, `:parse-complete`, `:command-name`, and other existing fields
- **AND** command handlers SHALL receive this exact structure

#### Scenario: Parser is independent of handlers
- **WHEN** the parser processes a command
- **THEN** parsing SHALL complete before any handler execution
- **AND** handlers SHALL NOT affect parser behavior or output structure

### Requirement: Scope validation tests pass

All existing scope validation tests MUST pass without modification after architectural changes.

#### Scenario: File path validation tests
- **WHEN** running existing scope validation tests for file path operations
- **THEN** all tests SHALL pass with identical results

#### Scenario: Cloud authentication tests
- **WHEN** running existing scope validation tests for cloud authentication
- **THEN** all tests SHALL pass with identical results

#### Scenario: Pipeline validation tests
- **WHEN** running existing scope validation tests for command pipelines
- **THEN** all tests SHALL pass with identical results
