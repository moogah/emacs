# Grammar Extraction

## Purpose

Unconditional grammar-level extraction layer that decomposes compound bash structures and extracts shell I/O operations (redirections) independently of any domain plugin or command handler.

## Responsibilities

- Recursively decompose compound bash structures (pipelines, chains, loops, conditionals, subshells, substitutions) into simple commands
- Extract file operations from shell redirections for every simple command
- Dispatch each simple command to the command handler layer for domain-specific extraction
- Operate unconditionally without predicate gating

## Key Invariants

- Grammar extraction runs unconditionally on all commands — no whitelist or predicate checks
- Redirections are extracted as shell grammar concerns, not command semantic concerns
- Command handlers receive only simple commands, never compound structures
- Variable/directory context propagates across chain commands

## Requirements

### Requirement: Unconditional compound decomposition
The grammar extraction layer SHALL recursively decompose all compound bash structures into simple commands without any predicate gating. This layer operates on shell grammar, not command semantics.

#### Scenario: Pipeline decomposition
- **WHEN** grammar layer receives a parsed command with `:type :pipeline`
- **THEN** the layer SHALL recursively process each command in `:all-commands`
- **AND** operations from all pipeline stages SHALL be collected

#### Scenario: Chain decomposition
- **WHEN** grammar layer receives a parsed command with `:type :chain`
- **THEN** the layer SHALL recursively process each command in `:all-commands`
- **AND** variable assignments SHALL propagate across chain commands
- **AND** directory context (`cd`, `pushd`, `popd`) SHALL be tracked across chain commands

#### Scenario: Conditional decomposition
- **WHEN** grammar layer receives a parsed command with `:type :conditional`
- **THEN** the layer SHALL recursively process test, then, and else branches
- **AND** operations SHALL be annotated with `:conditional t` and `:branch` metadata

#### Scenario: Loop decomposition
- **WHEN** grammar layer receives a for-loop command
- **THEN** the layer SHALL parse and recursively process the loop body
- **AND** the loop variable SHALL be bound in the body context
- **AND** operations SHALL be annotated with `:loop-context t`

#### Scenario: Subshell decomposition
- **WHEN** grammar layer receives a parsed command with `:type :subshell`
- **THEN** the layer SHALL recursively process the subshell body with an isolated variable context
- **AND** operations SHALL be annotated with `:subshell-context t`

#### Scenario: Command substitution decomposition
- **WHEN** grammar layer encounters `:command-substitutions` in a parsed command
- **THEN** the layer SHALL recursively parse and process each substitution
- **AND** operations SHALL be annotated with `:from-substitution t`

#### Scenario: Nested command injection decomposition
- **WHEN** grammar layer detects command injection patterns (`bash -c`, `sh -c`, `eval`)
- **THEN** the layer SHALL recursively parse and process the nested command string
- **AND** operations SHALL be annotated with `:indirect t` and `:nesting-depth`

#### Scenario: No predicate gating
- **WHEN** grammar layer receives any parsed command (simple or compound)
- **THEN** compound decomposition SHALL execute unconditionally
- **AND** there SHALL be no command-name whitelist check
- **AND** there SHALL be no token-type predicate check

### Requirement: Unconditional redirection extraction
The grammar extraction layer SHALL extract file operations from shell redirections for every simple command encountered during compound decomposition. Redirections are a shell grammar concern, not a command semantic concern.

#### Scenario: Output redirection produces write operation
- **WHEN** a simple command has a `>` redirection to a file path
- **THEN** the layer SHALL produce a `:filesystem` domain operation with `:operation :write` and `:source :redirection`

#### Scenario: Append redirection produces append operation
- **WHEN** a simple command has a `>>` redirection to a file path
- **THEN** the layer SHALL produce a `:filesystem` domain operation with `:operation :append` and `:source :redirection`

#### Scenario: Input redirection produces read operation
- **WHEN** a simple command has a `<` redirection from a file path
- **THEN** the layer SHALL produce a `:filesystem` domain operation with `:operation :read` and `:source :redirection`

#### Scenario: Stderr redirection produces write operation
- **WHEN** a simple command has a `2>` redirection to a file path
- **THEN** the layer SHALL produce a `:filesystem` domain operation with `:operation :write` and `:source :redirection`

#### Scenario: Redirection extraction for non-filesystem commands
- **WHEN** a command not in any handler registry has redirections (e.g., `echo hello > file.txt`)
- **THEN** the layer SHALL still extract the redirection as a `:filesystem` operation
- **AND** extraction SHALL NOT depend on the command name

#### Scenario: Redirection extraction in compound structures
- **WHEN** a pipeline or chain contains commands with redirections (e.g., `aws s3 ls > log.txt && echo done`)
- **THEN** the layer SHALL extract redirections from each subcommand during compound decomposition

#### Scenario: Redirection with variable paths
- **WHEN** a redirection target contains variables (e.g., `> $OUTPUT_DIR/file.txt`)
- **THEN** the layer SHALL resolve variables from context if available
- **AND** unresolved variables SHALL produce operations with `:confidence :medium` and `:unresolved t`

### Requirement: Simple command dispatch to handler layer
The grammar extraction layer SHALL dispatch each simple command it encounters to the command handler layer for domain-specific extraction.

#### Scenario: Simple command dispatched to handlers
- **WHEN** grammar layer encounters a simple command during compound decomposition
- **THEN** the layer SHALL call the command handler layer with that simple command
- **AND** the handler layer SHALL execute all registered handlers for that command's name

#### Scenario: Compound subcommands each dispatched independently
- **WHEN** grammar layer decomposes `cat file.txt && rm temp.txt`
- **THEN** the handler layer SHALL be called separately for `cat file.txt` and for `rm temp.txt`
- **AND** each call SHALL use the appropriate accumulated variable/directory context

#### Scenario: Unregistered commands produce no handler results
- **WHEN** grammar layer dispatches a command with no registered handlers (e.g., `echo`)
- **THEN** the handler layer SHALL return empty results for that command
- **AND** grammar-level operations (redirections) SHALL still be present

### Requirement: Result merge across layers
The orchestrator SHALL merge grammar-level operations with command-handler operations into a unified result. No plugin results are involved.

#### Scenario: Merge redirections with handler operations
- **WHEN** `cat input.txt > output.txt` is processed
- **THEN** the merged result SHALL contain `:read` from the `cat` handler (positional arg) AND `:write` from grammar extraction (redirection)

#### Scenario: Merge across compound subcommands
- **WHEN** `cp src.txt dst.txt && echo done > log.txt` is processed
- **THEN** the merged result SHALL contain `:read` and `:write` from `cp` handler, AND `:write` from `echo`'s redirection

#### Scenario: Multi-domain merge
- **WHEN** `aws s3 cp s3://bucket/key local.txt > log.txt 2>&1` is processed
- **THEN** the merged result SHALL contain `:filesystem` operations from both the `aws` handler and grammar redirections, plus `:authentication` and `:network` operations from the `aws` command handler

## Integration Points

- **Orchestrator** (`bash-parser-orchestrator.el`): Entry point for grammar extraction layer
- **File Operations** (`bash-parser-file-ops.el`): Provides redirection extraction primitives
- **Command Handlers** (`bash-parser-semantics.el`): Receives dispatched simple commands
- **Coverage System**: Consumes claimed token IDs from merged results
