# Handler Registry

## ADDED Requirements

### Requirement: Registry data structure

The handler registry SHALL use a hash table mapping command names to domain-handler mappings, replacing the monolithic `jf/bash-command-file-semantics` database.

#### Scenario: Registry initialization
- **WHEN** the bash-parser-semantics module is loaded
- **THEN** `jf/bash-command-handlers` SHALL be initialized as a hash table with test mode `equal`
- **AND** the hash table SHALL be empty before any handlers are registered

#### Scenario: Registry stores command-to-domain mapping
- **WHEN** handlers are registered for command "aws" with domains `:filesystem` and `:authentication`
- **THEN** `jf/bash-command-handlers` SHALL contain an entry for "aws"
- **AND** the entry SHALL map to a hash table containing both `:filesystem` and `:authentication` keys

#### Scenario: Registry stores multiple handlers per domain
- **WHEN** two handlers are registered for command "aws" and domain `:filesystem`
- **THEN** the `:filesystem` key SHALL map to a list containing both handler functions in registration order

### Requirement: Handler lookup interface

The system SHALL provide `jf/bash-lookup-command-handlers` to retrieve all registered handlers for a command.

#### Scenario: Lookup handlers for registered command
- **WHEN** calling `jf/bash-lookup-command-handlers` with command name "aws"
- **AND** handlers have been registered for "aws"
- **THEN** the function SHALL return a hash table mapping domains to handler lists

#### Scenario: Lookup handlers for unregistered command
- **WHEN** calling `jf/bash-lookup-command-handlers` with command name "nonexistent"
- **AND** no handlers have been registered for "nonexistent"
- **THEN** the function SHALL return `nil`

#### Scenario: Registry lookup preserves handler order
- **WHEN** three handlers were registered for "aws" `:filesystem` in order A, B, C
- **AND** calling `jf/bash-lookup-command-handlers` with "aws"
- **THEN** the `:filesystem` domain SHALL map to a list `[A, B, C]` in that exact order

### Requirement: Command semantics extraction

The system SHALL provide `jf/bash-extract-command-semantics` to execute all registered handlers for a parsed command and collect results by domain.

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

#### Scenario: Merge operations from multiple handlers
- **WHEN** two handlers for the same domain both return operations
- **THEN** the `:domains` alist SHALL contain one entry for that domain
- **AND** the operations list SHALL contain operations from both handlers in execution order

#### Scenario: Collect claimed token IDs from all handlers
- **WHEN** multiple handlers claim different token IDs
- **THEN** the return value SHALL contain `:claimed-token-ids` with all claimed IDs
- **AND** duplicate token IDs SHALL be removed from the list

### Requirement: Registry replaces monolithic database

The handler registry SHALL replace `jf/bash-command-file-semantics` with no backwards compatibility.

#### Scenario: Monolithic database removed
- **WHEN** the refactoring is complete
- **THEN** `jf/bash-command-file-semantics` variable SHALL NOT exist
- **AND** `jf/bash-lookup-command-semantics` function SHALL NOT exist
- **AND** all command semantics SHALL be defined via handler registration

#### Scenario: No fallback to old database
- **WHEN** extracting semantics for a command
- **THEN** the system SHALL use ONLY the handler registry
- **AND** the system SHALL NOT attempt to look up commands in any database structure
