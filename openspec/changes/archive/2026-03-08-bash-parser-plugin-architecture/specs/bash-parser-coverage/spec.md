## ADDED Requirements

### Requirement: Coverage calculation
The system SHALL calculate semantic coverage by comparing total tokens to claimed tokens.

#### Scenario: Full coverage
- **WHEN** all tokens are claimed by at least one plugin
- **THEN** coverage ratio is 1.0 (100%)

#### Scenario: Partial coverage
- **WHEN** 7 out of 10 tokens are claimed
- **THEN** coverage ratio is 0.7 (70%)

#### Scenario: No coverage
- **WHEN** no tokens are claimed by any plugin
- **THEN** coverage ratio is 0.0 (0%)

#### Scenario: Over 100% with shared claiming
- **WHEN** multiple plugins claim same tokens (shared claiming)
- **THEN** coverage ratio is still between 0.0 and 1.0 (not > 1.0)

### Requirement: Coverage by token type
The system SHALL provide coverage breakdown by token type for diagnostic analysis.

#### Scenario: Coverage for command-name tokens
- **WHEN** calculating coverage
- **THEN** result includes coverage ratio for :command-name token type

#### Scenario: Coverage for flag tokens
- **WHEN** calculating coverage
- **THEN** result includes coverage ratio for :flag token type

#### Scenario: Coverage for all token types
- **WHEN** calculating coverage
- **THEN** result includes coverage ratio for each token type present in command

### Requirement: Unclaimed token identification
The system SHALL identify and list all tokens not claimed by any plugin (blindspots).

#### Scenario: List unclaimed tokens
- **WHEN** calculating coverage with unclaimed tokens
- **THEN** result includes :unclaimed-tokens list with full token plists

#### Scenario: Empty unclaimed list at full coverage
- **WHEN** all tokens are claimed
- **THEN** :unclaimed-tokens list is empty

#### Scenario: Unclaimed token details
- **WHEN** listing unclaimed tokens
- **THEN** each token includes :id, :type, :value, :start, :end fields

### Requirement: Coverage metrics structure
The system SHALL provide structured coverage metrics including total, claimed, ratio, and unclaimed tokens.

#### Scenario: Coverage includes total tokens
- **WHEN** calculating coverage
- **THEN** result includes :total-tokens count

#### Scenario: Coverage includes claimed tokens
- **WHEN** calculating coverage
- **THEN** result includes :claimed-tokens count

#### Scenario: Coverage includes ratio
- **WHEN** calculating coverage
- **THEN** result includes :coverage-ratio as float between 0.0 and 1.0

#### Scenario: Coverage includes unclaimed tokens
- **WHEN** calculating coverage
- **THEN** result includes :unclaimed-tokens list

#### Scenario: Coverage includes type breakdown
- **WHEN** calculating coverage
- **THEN** result includes :coverage-by-type alist

### Requirement: Zero token handling
The system SHALL handle commands with zero tokens gracefully without division errors.

#### Scenario: Empty command coverage
- **WHEN** calculating coverage for command with zero tokens
- **THEN** coverage ratio is 1.0 (perfect coverage of nothing)

### Requirement: Coverage visualization
The system SHALL provide human-readable coverage visualization for debugging and analysis.

#### Scenario: Display coverage summary
- **WHEN** visualizing coverage
- **THEN** output shows parse complete status, coverage percentage, and token counts

#### Scenario: Show unclaimed tokens
- **WHEN** visualizing coverage with blindspots
- **THEN** output lists unclaimed tokens with type, value, and position

#### Scenario: Show coverage by type
- **WHEN** visualizing coverage
- **THEN** output shows bar chart or table of coverage by token type

### Requirement: Integration with semantic extraction
The system SHALL integrate coverage calculation into semantic extraction workflow.

#### Scenario: Coverage included in semantic result
- **WHEN** jf/bash-extract-semantics completes
- **THEN** result includes :coverage plist with metrics

#### Scenario: Coverage uses claimed tokens from plugins
- **WHEN** calculating coverage after plugin execution
- **THEN** coverage uses aggregated claimed-token-ids from all plugin results
