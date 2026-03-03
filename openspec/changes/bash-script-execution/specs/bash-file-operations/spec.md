## ADDED Requirements

### Requirement: Detect self-executing commands
The system SHALL detect when the command-name itself is a path-based reference to a file and extract `:execute` operation on the command-name.

#### Scenario: Relative path command
- **WHEN** extracting operations from command where command-name starts with "./"
- **THEN** system extracts operation: file is command-name, operation `:execute`, source `:command-name`, self-executing `t`

#### Scenario: Absolute path command
- **WHEN** extracting operations from command where command-name starts with "/"
- **THEN** system extracts operation: file is command-name, operation `:execute`, source `:command-name`, self-executing `t`

#### Scenario: Parent path command
- **WHEN** extracting operations from command where command-name starts with "../"
- **THEN** system extracts operation: file is command-name, operation `:execute`, source `:command-name`, self-executing `t`

#### Scenario: Non-path command not detected as self-executing
- **WHEN** extracting operations from command where command-name is "cat" or other non-path value
- **THEN** system does NOT extract self-execution operation

### Requirement: Capture script arguments metadata
The system SHALL capture positional arguments following an executed script as `:script-args` metadata in the execute operation.

#### Scenario: Execute operation with script arguments
- **WHEN** extracting execute operation from "python script.py arg1 arg2"
- **THEN** operation includes `:script-args ("arg1" "arg2")`

#### Scenario: Execute operation with no script arguments
- **WHEN** extracting execute operation from "python script.py"
- **THEN** operation includes `:script-args ()` (empty list)

#### Scenario: Self-executing command with script arguments
- **WHEN** extracting execute operation from "./deploy.sh staging us-west-2"
- **THEN** operation includes `:script-args ("staging" "us-west-2")`

### Requirement: Low confidence for self-executing detection
The system SHALL assign `:low` confidence to self-executing operations since detection is heuristic.

#### Scenario: Low confidence for path-based execution
- **WHEN** extracting self-executing operation from "./script.sh"
- **THEN** confidence level is `:low`

#### Scenario: High confidence maintained for known interpreters
- **WHEN** extracting execute operation from "python script.py"
- **THEN** confidence level is `:high` (known interpreter, not self-executing)

### Requirement: Self-executing metadata flag
The system SHALL include `:self-executing t` metadata flag for operations extracted from command-name to distinguish from interpreter-based execution.

#### Scenario: Self-executing flag present for path commands
- **WHEN** extracting operation from "./script.sh"
- **THEN** operation includes `:self-executing t`

#### Scenario: Self-executing flag absent for interpreter commands
- **WHEN** extracting operation from "python script.py"
- **THEN** operation does not include `:self-executing` flag (or `:self-executing nil`)

### Requirement: Script arguments for all execute operations
The system SHALL capture `:script-args` metadata for all execute operations, whether from interpreters or self-execution.

#### Scenario: Script args captured for interpreter execution
- **WHEN** extracting execute operation from interpreter command with arguments
- **THEN** operation includes `:script-args` with remaining positional arguments

#### Scenario: Script args captured for self-execution
- **WHEN** extracting execute operation from self-executing command with arguments
- **THEN** operation includes `:script-args` with all positional arguments

#### Scenario: Empty script args when no arguments present
- **WHEN** extracting execute operation from command with no arguments
- **THEN** operation includes `:script-args ()` (empty list)
