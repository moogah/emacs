# File Operations Extraction

## Purpose

Extract filesystem operations from parsed bash commands, identifying files being read, written, deleted, or modified. Provides extraction primitives consumed by the orchestrator and command handlers.

## Responsibilities

- Extract file operations from shell redirections (grammar-level, consumed by orchestrator Layer 0)
- Resolve variable references in file paths using variable context
- Classify operation types (read, write, delete, modify, create, append, execute, match-pattern, read-directory, read-metadata)
- Track operation confidence levels and source (positional-arg, redirection, flag-arg)
- Provide deprecated `jf/bash-extract-file-operations` wrapper for legacy callers

## Key Invariants

- Redirection extraction is unconditional — it does not inspect command names
- Each file operation includes file path, operation type, confidence level, and source
- Variable resolution produces `:unresolved t` metadata when variables cannot be resolved
- The deprecated wrapper delegates to the internal implementation, not to `jf/bash-extract-semantics`

## Requirements

### Requirement: Extract file operations from parsed commands
The system SHALL extract all file operations from a parsed bash command. File operations come from two sources: grammar-level redirection extraction (unconditional) and command handler positional argument extraction (handler-based). These are merged in the orchestrator, not in a filesystem plugin.

#### Scenario: Simple read command
- **WHEN** extracting operations from "cat /workspace/foo.txt"
- **THEN** result contains domain `:filesystem` with one operation: file "/workspace/foo.txt", operation `:read`, confidence `:high`, source `:positional-arg`

#### Scenario: Command with redirection
- **WHEN** extracting operations from "echo 'content' > output.txt"
- **THEN** result contains domain `:filesystem` with one operation: file "output.txt", operation `:write`, confidence `:high`, source `:redirection`

#### Scenario: Command with both positional args and redirection
- **WHEN** extracting operations from "cat input.txt > output.txt"
- **THEN** result contains domain `:filesystem` with two operations: file "input.txt" `:read` from `:positional-arg`, and file "output.txt" `:write` from `:redirection`

#### Scenario: Command with no file operations
- **WHEN** extracting operations from "echo hello"
- **THEN** result contains no `:filesystem` domain operations

#### Scenario: Non-filesystem command with redirection
- **WHEN** extracting operations from "aws s3 ls > log.txt"
- **THEN** result contains domain `:filesystem` with operation: file "log.txt", operation `:write`, source `:redirection`
- **AND** the `aws` command handler MAY contribute additional domain operations

#### Scenario: Compound structure with mixed commands
- **WHEN** extracting operations from "rm temp.txt && echo done"
- **THEN** result contains domain `:filesystem` with operation: file "temp.txt", operation `:delete`, source `:positional-arg`
- **AND** "echo done" contributes no filesystem operations (no redirection, no handler)

### Requirement: Extract file operations from redirections
The system SHALL extract file operations from shell I/O redirections for any simple command.

#### Scenario: Output redirection produces write operation
- **WHEN** extracting from a simple command with `> file.txt`
- **THEN** result contains operation with `:file "file.txt"`, `:operation :write`, `:source :redirection`

#### Scenario: Append redirection produces append operation
- **WHEN** extracting from a simple command with `>> file.txt`
- **THEN** result contains operation with `:file "file.txt"`, `:operation :append`, `:source :redirection`

#### Scenario: Input redirection produces read operation
- **WHEN** extracting from a simple command with `< file.txt`
- **THEN** result contains operation with `:file "file.txt"`, `:operation :read`, `:source :redirection`

#### Scenario: Stderr redirection produces write operation
- **WHEN** extracting from a simple command with `2> errors.log`
- **THEN** result contains operation with `:file "errors.log"`, `:operation :write`, `:source :redirection`

#### Scenario: Redirection extraction is command-agnostic
- **WHEN** extracting redirections from "echo hello > file.txt"
- **THEN** result contains the `:write` operation for file.txt
- **AND** extraction does NOT depend on whether "echo" has a registered handler

### Requirement: Variable resolution in file paths
The system SHALL resolve variable references in file paths when variable context is available.

#### Scenario: Resolve known variable
- **WHEN** extracting with `$DIR/file.txt` and context `((DIR . "/workspace"))`
- **THEN** resolved path is "/workspace/file.txt" with `:confidence :high`

#### Scenario: Mark unresolved variables
- **WHEN** extracting with `$UNKNOWN/file.txt` and no matching context
- **THEN** operation has `:unresolved t` and `:confidence :medium`

### Requirement: Operation confidence levels
The system SHALL assign confidence levels based on extraction source reliability.

#### Scenario: High confidence for known commands
- **WHEN** operation comes from a registered handler for a known command
- **THEN** confidence is `:high`

#### Scenario: High confidence for redirections
- **WHEN** operation comes from shell redirection extraction
- **THEN** confidence is `:high`

#### Scenario: Medium confidence for unresolved variables
- **WHEN** file path contains unresolved variables
- **THEN** confidence is `:medium`

#### Scenario: Low confidence for heuristics
- **WHEN** operation comes from self-execution detection (path-based commands)
- **THEN** confidence is `:low`

### Requirement: Operation source tracking
The system SHALL track where each file operation was extracted from.

#### Scenario: Positional argument source
- **WHEN** operation comes from command handler processing positional args
- **THEN** source is `:positional-arg`

#### Scenario: Redirection source
- **WHEN** operation comes from shell redirection
- **THEN** source is `:redirection`

#### Scenario: Flag argument source
- **WHEN** operation comes from flag-based extraction (e.g., `sed -i --file=script.sed`)
- **THEN** source is `:flag-arg`

### Requirement: Backward compatible wrapper
The system SHALL provide `jf/bash-extract-file-operations` as a deprecated wrapper.

#### Scenario: Legacy API signature preserved
- **WHEN** calling `jf/bash-extract-file-operations` with `(parsed-command &optional var-context)`
- **THEN** function signature matches the original API

#### Scenario: Legacy API returns operations list
- **WHEN** calling `jf/bash-extract-file-operations`
- **THEN** return value is a flat list of operation plists (not the full semantics result)

## Operation Types

```elisp
(defconst jf/bash-valid-operation-types
  '(:read :write :delete :modify :create :create-or-modify :append
    :match-pattern :read-directory :read-metadata :execute))
```

## Integration Points

- **Orchestrator Layer 0**: Calls `jf/bash-extract-operations-from-redirections` per simple command
- **Orchestrator**: Calls `jf/bash--resolve-handler-filesystem-ops` to resolve handler result paths
- **Command Handlers**: Independent — handlers produce their own filesystem operations
- **gptel Scope System**: Legacy consumer through deprecated wrapper (should migrate to `jf/bash-extract-semantics`)
