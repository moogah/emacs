# Interface Preservation

## ADDED Requirements

### Requirement: Public API compatibility

The public API `jf/bash-extract-semantics` MUST preserve its signature and return structure to ensure the gptel scope validation system continues working without modification.

#### Scenario: Function signature unchanged
- **WHEN** calling `jf/bash-extract-semantics` with a parsed command
- **THEN** the function SHALL accept exactly one argument: the parsed-command plist
- **AND** the argument structure SHALL match the existing contract (`:tokens`, `:parse-complete`, etc.)

#### Scenario: Return structure preserved
- **WHEN** `jf/bash-extract-semantics` completes extraction
- **THEN** the return value SHALL be a plist with exactly these keys: `:domains`, `:coverage`, `:parse-complete`, `:plugin-results`
- **AND** `:domains` SHALL be an alist of `(domain . operations)` pairs
- **AND** `:coverage` SHALL be a coverage plist from `jf/bash-calculate-coverage`
- **AND** `:parse-complete` SHALL be a boolean passed through from input
- **AND** `:plugin-results` SHALL be a list of raw plugin results

#### Scenario: Scope validation system compatibility
- **WHEN** the gptel scope validation system calls `jf/bash-extract-semantics`
- **THEN** the return value SHALL contain all data needed for validation
- **AND** no changes SHALL be required in `config/gptel/tools/scope-shell-tools.el`

### Requirement: Plugin infrastructure compatibility

The existing plugin infrastructure in `bash-parser-plugins.el` MUST remain functional and be enhanced to work with command handlers.

#### Scenario: Universal plugins still execute
- **WHEN** extracting semantics for any command
- **THEN** universal plugins (redirections, variables) SHALL execute as before
- **AND** universal plugin results SHALL be included in `:domains` output

#### Scenario: Plugin registration continues working
- **WHEN** registering a domain-based plugin using `jf/bash-register-plugin`
- **THEN** the plugin SHALL be registered and executed during semantic extraction
- **AND** both command-based handlers and domain-based plugins SHALL coexist

#### Scenario: Plugin execution order maintained
- **WHEN** both universal plugins and command handlers produce results
- **THEN** plugins SHALL execute in priority order as before
- **AND** command handlers SHALL execute after or alongside plugins (implementation detail)
- **AND** results from both SHALL be merged by domain

### Requirement: Coverage tracking unchanged

Token coverage tracking MUST continue to function with the same interface and behavior.

#### Scenario: Token claiming from handlers
- **WHEN** command handlers return `:claimed-token-ids`
- **THEN** those IDs SHALL be included in coverage calculation
- **AND** `jf/bash-calculate-coverage` SHALL receive all claimed token IDs
- **AND** the `:coverage` section of the return value SHALL be identical to current behavior

#### Scenario: Coverage threshold checking
- **WHEN** the scope validation system checks coverage thresholds
- **THEN** the `:coverage` plist SHALL contain the same fields as before
- **AND** coverage percentage calculation SHALL produce identical results

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

All existing scope validation tests MUST pass without modification after the refactoring.

#### Scenario: File path validation tests
- **WHEN** running existing scope validation tests for file path operations
- **THEN** all tests SHALL pass with identical results
- **AND** no test modifications SHALL be required

#### Scenario: Cloud authentication tests
- **WHEN** running existing scope validation tests for cloud authentication
- **THEN** all tests SHALL pass with identical results
- **AND** cloud auth detection SHALL work exactly as before

#### Scenario: Pipeline validation tests
- **WHEN** running existing scope validation tests for command pipelines
- **THEN** all tests SHALL pass with identical results
- **AND** multi-command validation SHALL work exactly as before

#### Scenario: Coverage enforcement tests
- **WHEN** running existing tests that check parse coverage thresholds
- **THEN** all tests SHALL pass with identical behavior
- **AND** coverage warnings and errors SHALL trigger at the same thresholds
