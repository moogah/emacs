# Bash Command Semantics Database

## Purpose

Maintain a database mapping command names to their file operation semantics, defining how each command interacts with file arguments. Used by the filesystem plugin to extract file operations accurately.

## Responsibilities

- Provide lookup table for command-to-semantics mapping
- Support simple commands (cat, rm, cp) with operation types and argument sources
- Support commands with skip rules (grep skips pattern argument)
- Support multi-operation commands (cp reads source, writes destination)
- Support subcommand-specific semantics (git add vs git checkout)
- Support flag-dependent operations (sed -i makes in-place edits)
- Support interpreter commands (python, node, bash execute scripts)
- Classify operations into distinct types

## Key Invariants

- Each command has defined operation types (read, write, delete, modify, create, append, execute)
- Positional argument indexing supports single index, ranges, and special indices (first, last, all-but-last)
- Flag-dependent operations override base semantics when flags are present
- Subcommands create separate semantic entries (git-add, git-checkout as distinct entries)

## Requirements

### Requirement: Command semantics database
The system SHALL maintain a database mapping command names to their file operation semantics, defining how each command interacts with file arguments.

#### Scenario: Lookup simple read command
- **WHEN** looking up semantics for "cat"
- **THEN** system returns operation type `:read` and source `:positional-args`

#### Scenario: Lookup command with skip rules
- **WHEN** looking up semantics for "grep"
- **THEN** system returns operation type `:read`, source `:positional-args`, and skip-first rule (pattern argument)

#### Scenario: Lookup unknown command
- **WHEN** looking up semantics for unrecognized command
- **THEN** system returns nil or empty semantics

### Requirement: Multi-operation command support
The system SHALL support commands that perform multiple operation types on different arguments (read source, write destination).

#### Scenario: Copy command with read and write operations
- **WHEN** extracting semantics for "cp"
- **THEN** system returns two operations: `:read` for indices 0 to N-2, `:write` for index -1

#### Scenario: Move command with delete and write operations
- **WHEN** extracting semantics for "mv"
- **THEN** system returns `:delete` for source arguments and `:write` for destination argument

### Requirement: Subcommand-specific semantics
The system SHALL support commands with subcommands where file operation semantics vary by subcommand (git, docker, npm).

#### Scenario: Git add subcommand
- **WHEN** extracting semantics for "git" with subcommand "add"
- **THEN** system returns operation type `:read` for positional args

#### Scenario: Git checkout subcommand
- **WHEN** extracting semantics for "git" with subcommand "checkout"
- **THEN** system returns operation type `:modify` for positional args

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
The system SHALL include semantics for at least 20 core commands covering read, write, delete, copy, move, and directory operations.

#### Scenario: File reading commands
- **WHEN** database is queried for file reading commands
- **THEN** semantics exist for cat, head, tail, less, grep, wc

#### Scenario: File writing commands
- **WHEN** database is queried for file writing commands
- **THEN** semantics exist for touch, echo (via redirection), tee

#### Scenario: File deletion commands
- **WHEN** database is queried for file deletion commands
- **THEN** semantics exist for rm, rmdir

#### Scenario: File manipulation commands
- **WHEN** database is queried for file manipulation commands
- **THEN** semantics exist for cp, mv, ln, chmod, chown

### Requirement: Positional argument indexing
The system SHALL support flexible positional argument indexing including single index, index ranges, and special indices (first, last, all-but-last).

#### Scenario: Single index specification
- **WHEN** semantics specify index 0
- **THEN** only first positional argument is considered

#### Scenario: Last argument specification
- **WHEN** semantics specify index -1
- **THEN** only last positional argument is considered

#### Scenario: Range specification
- **WHEN** semantics specify indices 0-to-N-2
- **THEN** all positional arguments except last are considered

### Requirement: Flag-dependent operations
The system SHALL support commands where operation type depends on flags (sed -i makes in-place edits, tar -c creates vs -x extracts).

#### Scenario: In-place editing flag
- **WHEN** "sed" command has "-i" flag
- **THEN** operation type is `:modify` for positional args

#### Scenario: Sed without in-place flag
- **WHEN** "sed" command lacks "-i" flag
- **THEN** operation type is `:read` for positional args

### Requirement: Interpreter command semantics
The system SHALL include semantics entries for interpreter commands that execute script files, marking the first positional argument with `:execute` operation.

#### Scenario: Python interpreter semantics
- **WHEN** looking up semantics for "python" or "python3"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Node interpreter semantics
- **WHEN** looking up semantics for "node"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Bash interpreter semantics
- **WHEN** looking up semantics for "bash"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Shell interpreter semantics
- **WHEN** looking up semantics for "sh" or "zsh"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Source built-in semantics
- **WHEN** looking up semantics for "source" or "."
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

### Requirement: Subcommand-based interpreter semantics
The system SHALL include semantics entries for commands where execution depends on subcommand (go run vs go build).

#### Scenario: Go run subcommand semantics
- **WHEN** looking up semantics for "go" with subcommand "run"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Go test subcommand semantics
- **WHEN** looking up semantics for "go" with subcommand "test"
- **THEN** system returns operation type `:execute`, source `:positional-args`, index 0

#### Scenario: Go build subcommand semantics
- **WHEN** looking up semantics for "go" with subcommand "build"
- **THEN** system returns operation type `:read`, source `:positional-args`, index 0

### Requirement: Single index extraction for interpreters
The system SHALL support `:index 0` specification to extract only the first positional argument (the script file), not subsequent arguments.

#### Scenario: Extract first argument only
- **WHEN** semantics specify `:index 0`
- **THEN** only first positional argument is extracted as file operation

#### Scenario: Skip remaining arguments
- **WHEN** command has multiple positional arguments and semantics specify `:index 0`
- **THEN** arguments after index 0 are not extracted as file operations

## Database Structure

```elisp
(defconst jf/bash-command-semantics
  '(;; Simple read commands
    ("cat"  :operation :read :source :positional-args :index :all)
    ("grep" :operation :read :source :positional-args :index :all :skip-first t)

    ;; Multi-operation commands
    ("cp"   :operations ((:operation :read :indices (0 . -2))
                         (:operation :write :indices (-1))))
    ("mv"   :operations ((:operation :delete :indices (0 . -2))
                         (:operation :write :indices (-1))))

    ;; Flag-dependent operations
    ("sed"  :operation :read :flag-dependent (("-i" :operation :modify)))

    ;; Subcommand-specific
    ("git" :subcommands
           (("add" :operation :read)
            ("checkout" :operation :modify)
            ("rm" :operation :delete)))

    ;; Interpreters
    ("python" :operation :execute :index 0)
    ("node"   :operation :execute :index 0)
    ("bash"   :operation :execute :index 0)))
```

## Integration Points

- **Filesystem Plugin**: Primary consumer - uses semantics to extract file operations
- **Coverage System**: Indirectly benefits from accurate operation extraction

## Example Usage

```elisp
;; Lookup simple command
(jf/bash-lookup-semantics "cat")
;; => (:operation :read :source :positional-args :index :all)

;; Lookup subcommand
(jf/bash-lookup-semantics "git" :subcommand "add")
;; => (:operation :read :source :positional-args)

;; Lookup flag-dependent
(jf/bash-lookup-semantics "sed" :flags '("-i"))
;; => (:operation :modify :source :positional-args)
```
