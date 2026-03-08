## ADDED Requirements

### Requirement: Token inventory in parsed output
The parser SHALL include granular token inventory in parsed command structure for coverage tracking.

#### Scenario: Parse result includes tokens
- **WHEN** jf/bash-parse completes successfully
- **THEN** result includes :tokens list with token plists

#### Scenario: Each token has required fields
- **WHEN** tokens are generated
- **THEN** each token includes :id, :type, :value, :start, :end fields

#### Scenario: Tokens cover entire command
- **WHEN** parsing complete command
- **THEN** token inventory represents all parsed syntactic elements

### Requirement: Parse completeness flag
The parser SHALL include :parse-complete flag indicating whether syntax was fully understood.

#### Scenario: Successful parse
- **WHEN** parser successfully parses entire command syntax
- **THEN** :parse-complete is t

#### Scenario: Syntax error
- **WHEN** parser encounters unmatched quotes or invalid syntax
- **THEN** :parse-complete is nil and :parse-errors contains error messages

#### Scenario: Partial parse
- **WHEN** parser handles prefix but cannot parse remainder
- **THEN** :parse-complete is nil

### Requirement: Parse error tracking
The parser SHALL include :parse-errors list when parse-complete is false.

#### Scenario: Parse errors empty on success
- **WHEN** parse completes successfully
- **THEN** :parse-errors is nil or empty list

#### Scenario: Parse errors populated on failure
- **WHEN** parse fails with syntax error
- **THEN** :parse-errors contains descriptive error messages

#### Scenario: Multiple parse errors
- **WHEN** parse encounters multiple syntax issues
- **THEN** :parse-errors lists all detected errors
