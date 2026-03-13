# Bash Command Semantics

## Purpose

Define how each command interacts with file arguments and other semantic domains. Semantic extraction is organized by command (not by domain), with each command defined in its own handler file.

**Supersedes:** The monolithic `jf/bash-command-file-semantics` database has been replaced by the command handler registry system. See [command-handlers.md](command-handlers.md), [handler-registry.md](handler-registry.md), [multi-domain.md](multi-domain.md), and [interface-preservation.md](interface-preservation.md) for detailed specs.

## Responsibilities

- Provide per-command handler files in `config/bash-parser/commands/`
- Support simple commands (cat, rm, cp) with operation types and argument sources
- Support commands with skip rules (grep skips pattern argument)
- Support multi-operation commands (cp reads source, writes destination)
- Support subcommand-specific semantics (git add vs git checkout)
- Support flag-dependent operations (sed -i makes in-place edits)
- Support interpreter commands (python, node, bash execute scripts)
- Support multi-domain commands (aws contributes to filesystem, authentication, network)
- Classify operations into distinct types
- Auto-discover command handlers from `commands/` directory

## Architecture

Command handlers are organized as individual `.el` files in `config/bash-parser/commands/`, one per command or command family. Each file self-registers via `jf/bash-register-command-handler`. The `commands/index.el` file provides auto-discovery.

The handler registry (`jf/bash-command-handlers`) is a nested hash table: `{command-name => {domain => [handler-fn, ...]}}`, providing O(1) command lookup.

## Key Invariants

- Each command has defined operation types (read, write, delete, modify, create, append, execute)
- Commands can contribute to multiple semantic domains (filesystem, authentication, network)
- Handlers execute independently per domain — failure in one domain does not affect others
- Handler results merge by domain in the orchestrator output
- Public API `jf/bash-extract-semantics` preserves signature and return structure for gptel scope validation
- Grammar-level extraction (redirections, variables) coexists with command handlers in the two-layer architecture

## Requirements

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

### Requirement: Operation type classification
The system SHALL classify file operations into distinct types: `:read`, `:write`, `:delete`, `:modify`, `:create`, `:create-or-modify`, `:append`, `:execute`.

#### Scenario: Read operation classification
- **WHEN** command only reads file contents
- **THEN** operation type is `:read`

#### Scenario: Write operation classification
- **WHEN** command creates new file or overwrites existing
- **THEN** operation type is `:write`

#### Scenario: Delete operation classification
- **WHEN** command removes file or directory
- **THEN** operation type is `:delete`

#### Scenario: Modify operation classification
- **WHEN** command changes existing file without replacing it
- **THEN** operation type is `:modify`

#### Scenario: Execute operation classification
- **WHEN** command executes file as code or script
- **THEN** operation type is `:execute`

### Requirement: Core command coverage
The system SHALL include handlers for at least 20 core commands covering read, write, delete, copy, move, and directory operations.

#### Scenario: File reading commands
- **WHEN** examining registered command handlers
- **THEN** handlers exist for cat, head, tail, less, grep, wc

#### Scenario: File writing commands
- **WHEN** examining registered command handlers
- **THEN** handlers exist for touch, tee

#### Scenario: File deletion commands
- **WHEN** examining registered command handlers
- **THEN** handlers exist for rm, rmdir

#### Scenario: File manipulation commands
- **WHEN** examining registered command handlers
- **THEN** handlers exist for cp, mv, ln, chmod, chown

## Integration Points

- **Orchestrator** (`bash-parser-orchestrator.el`): Dispatches to handlers per-simple-command and merges results with grammar-level extraction
- **Coverage System**: Receives claimed-token-ids from handlers for coverage calculation
- **Scope Validation** (`scope-shell-tools.el`): Primary consumer via `jf/bash-extract-semantics`
