## MODIFIED Requirements

### Requirement: Command semantics extraction
The system SHALL provide `jf/bash-extract-command-semantics` to execute all registered handlers for a parsed simple command and collect results by domain. This function is called by the grammar extraction layer for each simple command encountered during compound decomposition.

#### Scenario: Extract semantics for simple command from compound
- **WHEN** the grammar layer decomposes `cat file.txt && rm temp.txt` into two simple commands
- **AND** dispatches each to `jf/bash-extract-command-semantics`
- **THEN** the function SHALL execute handlers for "cat" and "rm" independently
- **AND** each call SHALL return domain-specific operations for that command only

#### Scenario: Extract semantics for single-domain command
- **WHEN** calling `jf/bash-extract-command-semantics` with a parsed "cat" command
- **AND** "cat" has one handler registered for `:filesystem`
- **THEN** the function SHALL execute the filesystem handler
- **AND** the return value SHALL contain `:domains` with `(:filesystem . [operations])`

#### Scenario: Extract semantics for multi-domain command
- **WHEN** calling `jf/bash-extract-command-semantics` with a parsed "aws" command
- **AND** "aws" has handlers for `:filesystem`, `:authentication`, and `:network`
- **THEN** the function SHALL execute all three handlers
- **AND** the return value SHALL contain `:domains` with all three domain entries

#### Scenario: Handle handler execution errors gracefully
- **WHEN** calling `jf/bash-extract-command-semantics` with a command
- **AND** one of the handlers signals an error
- **THEN** the system SHALL log the error with domain, command name, and error message
- **AND** the system SHALL continue executing remaining handlers
- **AND** the failed handler's results SHALL NOT be included in the output

#### Scenario: Simple command with no registered handlers
- **WHEN** calling `jf/bash-extract-command-semantics` with a command that has no registered handlers (e.g., "echo")
- **THEN** the function SHALL return empty domains
- **AND** grammar-level operations (redirections) SHALL still be provided by Layer 0

#### Scenario: Collect claimed token IDs from all handlers
- **WHEN** multiple handlers claim different token IDs
- **THEN** the return value SHALL contain `:claimed-token-ids` with all claimed IDs
- **AND** duplicate token IDs SHALL be removed from the list
