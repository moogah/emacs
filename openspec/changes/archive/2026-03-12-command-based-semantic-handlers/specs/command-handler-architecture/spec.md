# Command Handler Architecture

## ADDED Requirements

### Requirement: Commands are first-class entities

The bash-parser system SHALL organize semantic extraction by command rather than by semantic domain. Each command SHALL be defined in its own file and SHALL be able to contribute to multiple semantic domains.

#### Scenario: Single-domain command registration
- **WHEN** a command handler file (e.g., `cat.el`) registers handlers for one domain
- **THEN** the handler registry SHALL contain that command mapped to the single domain
- **AND** calling `jf/bash-lookup-command-handlers` with that command name SHALL return the registered handler

#### Scenario: Multi-domain command registration
- **WHEN** a command handler file (e.g., `aws.el`) registers handlers for multiple domains (`:filesystem`, `:authentication`, `:network`)
- **THEN** the handler registry SHALL contain that command mapped to all three domains
- **AND** calling `jf/bash-lookup-command-handlers` SHALL return a hash table with all three domain mappings

#### Scenario: Command file locality
- **WHEN** examining command-specific behavior for `aws`
- **THEN** all semantic extraction logic for `aws` SHALL be located in `commands/aws.el`
- **AND** no other files SHALL contain aws-specific semantic logic

### Requirement: Handler registration API

The system SHALL provide `jf/bash-register-command-handler` for registering command handlers to semantic domains.

#### Scenario: Register handler with required parameters
- **WHEN** calling `jf/bash-register-command-handler` with `:command "cat"`, `:domain :filesystem`, and `:handler #'func`
- **THEN** the handler SHALL be registered in the handler registry
- **AND** the handler SHALL be callable during semantic extraction

#### Scenario: Register handler missing command parameter
- **WHEN** calling `jf/bash-register-command-handler` without `:command` parameter
- **THEN** the system SHALL signal an error with message "Command handler registration requires :command string"

#### Scenario: Register handler missing domain parameter
- **WHEN** calling `jf/bash-register-command-handler` without `:domain` parameter
- **THEN** the system SHALL signal an error with message "Command handler registration requires :domain keyword"

#### Scenario: Register handler missing handler function
- **WHEN** calling `jf/bash-register-command-handler` without `:handler` parameter
- **THEN** the system SHALL signal an error with message "Command handler registration requires :handler function"

#### Scenario: Register multiple handlers for same command and domain
- **WHEN** registering two handlers for command "aws" and domain `:filesystem`
- **THEN** both handlers SHALL be registered and executed in registration order during extraction

### Requirement: Handler interface contract

Each command handler MUST accept a `parsed-command` plist and MUST return a result plist containing `:domain`, `:operations`, `:claimed-token-ids`, and `:metadata`.

#### Scenario: Handler receives parsed command
- **WHEN** a registered handler is invoked during semantic extraction
- **THEN** the handler SHALL receive a `parsed-command` plist with `:tokens`, `:command-name`, `:positional-args`, and other parsing metadata

#### Scenario: Handler returns valid result
- **WHEN** a handler completes successfully
- **THEN** the handler SHALL return a plist with `:domain` keyword
- **AND** the plist SHALL contain `:operations` list
- **AND** the plist SHALL contain `:claimed-token-ids` list (may be empty)
- **AND** the plist SHALL contain `:metadata` plist (may be empty)

#### Scenario: Handler returns nil when not applicable
- **WHEN** a handler determines it cannot extract semantics from the parsed command
- **THEN** the handler MAY return `nil`
- **AND** the system SHALL continue processing other handlers without error

### Requirement: Auto-discovery system

The system SHALL automatically discover and load all command handler files from the `commands/` directory without requiring manual registration in an index.

#### Scenario: Load command handler on initialization
- **WHEN** the bash-parser system initializes and `commands/index.el` is required
- **THEN** the system SHALL discover all `.el` files in `commands/` directory (except `index.el`)
- **AND** the system SHALL load each discovered file
- **AND** each file's handler registrations SHALL be active

#### Scenario: Add new command without code changes elsewhere
- **WHEN** a developer creates `commands/terraform.el` with handler registrations
- **THEN** on next system initialization, Terraform handlers SHALL be automatically loaded
- **AND** no modifications to other files SHALL be required

#### Scenario: Handle load error gracefully
- **WHEN** loading a command handler file fails with an error
- **THEN** the system SHALL log the error with the file name and error message
- **AND** the system SHALL continue loading remaining command files

### Requirement: Directory structure organization

The bash-parser SHALL organize command handlers in a `commands/` directory with one file per command or command family.

#### Scenario: Simple command file structure
- **WHEN** examining the directory structure
- **THEN** `bash-parser/commands/cat.el` SHALL exist for the cat command
- **AND** `bash-parser/commands/grep.el` SHALL exist for the grep command
- **AND** each file SHALL contain only handler logic for that specific command

#### Scenario: Command family organization
- **WHEN** a command has many subcommands (e.g., `git`)
- **THEN** the command MAY be organized as a single file `commands/git.el`
- **OR** the command MAY be organized as a directory `commands/git/` with subcommand files

#### Scenario: Index file location
- **WHEN** examining the commands directory
- **THEN** `bash-parser/commands/index.el` SHALL exist
- **AND** the index file SHALL provide the auto-discovery mechanism
