## ADDED Requirements

### Requirement: Token inventory structure
The system SHALL track granular token inventory with unique IDs, types, values, and positions for each parsed element.

#### Scenario: Token has unique ID
- **WHEN** parser creates tokens
- **THEN** each token has unique integer :id field

#### Scenario: Token has type classification
- **WHEN** parser creates tokens
- **THEN** each token has :type field indicating syntactic category

#### Scenario: Token has value
- **WHEN** parser creates tokens
- **THEN** each token has :value field with actual content

#### Scenario: Token has position
- **WHEN** parser creates tokens
- **THEN** each token has :start and :end fields indicating character positions

### Requirement: Token types
The system SHALL classify tokens into syntactic categories for coverage analysis.

#### Scenario: Command name token
- **WHEN** parsing command name
- **THEN** token type is :command-name

#### Scenario: Positional argument token
- **WHEN** parsing positional argument
- **THEN** token type is :positional-arg

#### Scenario: Flag token
- **WHEN** parsing flag (e.g., --flag or -f)
- **THEN** token type is :flag

#### Scenario: Flag argument token
- **WHEN** parsing argument following a flag
- **THEN** token type is :flag-arg with :associated-flag reference

#### Scenario: Command substitution token
- **WHEN** parsing $(command) or `command`
- **THEN** token type is :command-substitution

#### Scenario: Separator token
- **WHEN** parsing separator like "--"
- **THEN** token type is :separator

#### Scenario: Redirection token
- **WHEN** parsing redirection operator
- **THEN** token type is :redirection

#### Scenario: Pipe token
- **WHEN** parsing pipe operator
- **THEN** token type is :pipe

### Requirement: Pipeline segment tracking
The system SHALL track which pipeline segment each token belongs to for multi-command analysis.

#### Scenario: First command segment
- **WHEN** token is in first command of pipeline
- **THEN** token has :pipe-segment 0

#### Scenario: Second command segment
- **WHEN** token is in second command of pipeline
- **THEN** token has :pipe-segment 1

#### Scenario: Command after separator
- **WHEN** token follows "--" separator
- **THEN** token has updated :pipe-segment value

### Requirement: Flag association
The system SHALL link flag arguments to their associated flags for semantic understanding.

#### Scenario: Flag argument linked to flag
- **WHEN** parsing "--log-group-name /aws/lambda/logs"
- **THEN** flag-arg token has :associated-flag pointing to flag token ID

#### Scenario: Multiple flag arguments
- **WHEN** flag accepts multiple arguments
- **THEN** each flag-arg token links to same flag token ID

### Requirement: Nested structure tracking
The system SHALL track whether nested structures (command substitutions, subshells) were parsed.

#### Scenario: Parsed command substitution
- **WHEN** command substitution contents are parsed
- **THEN** token has :parsed-inner t

#### Scenario: Unparsed command substitution
- **WHEN** command substitution contents are not parsed (e.g., non-shell language)
- **THEN** token has :parsed-inner nil

### Requirement: Token inventory in parsed output
The system SHALL include token inventory in parsed command structure output.

#### Scenario: Parsed command includes tokens
- **WHEN** jf/bash-parse completes
- **THEN** result includes :tokens list with all token plists

#### Scenario: Tokens maintain parse order
- **WHEN** tokens are returned
- **THEN** tokens are ordered by position in command string

### Requirement: Position tracking for debugging
The system SHALL track character positions for each token to enable source mapping and visualization.

#### Scenario: Token start position
- **WHEN** token is created
- **THEN** :start field indicates first character position (0-indexed)

#### Scenario: Token end position
- **WHEN** token is created
- **THEN** :end field indicates position after last character (exclusive)

#### Scenario: Position enables substring extraction
- **WHEN** using :start and :end positions
- **THEN** substring of original command yields token :value
