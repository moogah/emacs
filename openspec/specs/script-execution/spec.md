## ADDED Requirements

### Requirement: Extract execute operations from interpreter commands
The system SHALL extract `:execute` operations when commands execute script files through interpreters (python, node, bash, etc.), using the first positional argument as the executed file.

#### Scenario: Python interpreter execution
- **WHEN** extracting operations from "python script.py"
- **THEN** system returns operation: file "script.py", operation `:execute`, confidence `:high`, source `:positional-arg`

#### Scenario: Node interpreter execution
- **WHEN** extracting operations from "node server.js"
- **THEN** system returns operation: file "server.js", operation `:execute`, confidence `:high`, source `:positional-arg`

#### Scenario: Bash interpreter execution
- **WHEN** extracting operations from "bash deploy.sh"
- **THEN** system returns operation: file "deploy.sh", operation `:execute`, confidence `:high`, source `:positional-arg`

#### Scenario: Shell built-in source execution
- **WHEN** extracting operations from "source config.sh"
- **THEN** system returns operation: file "config.sh", operation `:execute`, confidence `:high`, source `:positional-arg`

#### Scenario: Dot source execution
- **WHEN** extracting operations from ". config.sh"
- **THEN** system returns operation: file "config.sh", operation `:execute`, confidence `:high`, source `:positional-arg`

### Requirement: Extract execute operations from subcommand-based interpreters
The system SHALL extract `:execute` operations from commands where execution depends on subcommand (go run, go test).

#### Scenario: Go run subcommand
- **WHEN** extracting operations from "go run main.go"
- **THEN** system returns operation: file "main.go", operation `:execute`, confidence `:high`, source `:positional-arg`, subcommand "run"

#### Scenario: Go test subcommand
- **WHEN** extracting operations from "go test ./..."
- **THEN** system returns operation: file "./...", operation `:execute`, confidence `:high`, source `:positional-arg`, subcommand "test"

#### Scenario: Go build subcommand does not execute
- **WHEN** extracting operations from "go build main.go"
- **THEN** system returns operation: file "main.go", operation `:read`, confidence `:high` (not `:execute`)

### Requirement: Detect self-executing commands
The system SHALL detect when the command-name itself is a path to an executable file and extract `:execute` operation on the command-name.

#### Scenario: Relative path execution
- **WHEN** extracting operations from "./script.sh arg1 arg2"
- **THEN** system returns operation: file "./script.sh", operation `:execute`, confidence `:low`, source `:command-name`, self-executing `t`

#### Scenario: Absolute path execution
- **WHEN** extracting operations from "/usr/local/bin/tool"
- **THEN** system returns operation: file "/usr/local/bin/tool", operation `:execute`, confidence `:low`, source `:command-name`, self-executing `t`

#### Scenario: Parent directory path execution
- **WHEN** extracting operations from "../bin/runner"
- **THEN** system returns operation: file "../bin/runner", operation `:execute`, confidence `:low`, source `:command-name`, self-executing `t`

#### Scenario: Non-path command not self-executing
- **WHEN** extracting operations from "cat file.txt"
- **THEN** system does NOT extract execute operation on "cat" itself

### Requirement: Capture script arguments metadata
The system SHALL capture remaining positional arguments after the executed script as `:script-args` metadata for potential future analysis.

#### Scenario: Script with no arguments
- **WHEN** extracting operations from "python script.py"
- **THEN** operation includes `:script-args ()` (empty list)

#### Scenario: Script with file arguments
- **WHEN** extracting operations from "python process.py input.csv output.json"
- **THEN** operation includes `:script-args ("input.csv" "output.json")`

#### Scenario: Script with flag arguments
- **WHEN** extracting operations from "node server.js --port 3000 --verbose"
- **THEN** operation includes `:script-args ("--port" "3000" "--verbose")`

#### Scenario: Self-executing script with arguments
- **WHEN** extracting operations from "./deploy.sh staging us-west-2"
- **THEN** operation includes `:script-args ("staging" "us-west-2")`

### Requirement: Confidence levels for execute operations
The system SHALL assign confidence levels to execute operations based on whether the command is a known interpreter or a path-based execution.

#### Scenario: High confidence for known interpreter
- **WHEN** extracting operations from "python script.py"
- **THEN** confidence level is `:high` (known interpreter in semantics database)

#### Scenario: Low confidence for path-based execution
- **WHEN** extracting operations from "./unknown-binary"
- **THEN** confidence level is `:low` (heuristic detection, unknown file type)

#### Scenario: High confidence for known shell built-in
- **WHEN** extracting operations from "source config.sh"
- **THEN** confidence level is `:high` (known built-in in semantics database)

### Requirement: Execute operations work with variable resolution
The system SHALL resolve variables in executed script paths when variable context is provided.

#### Scenario: Execute with resolved variable
- **WHEN** extracting operations from "python $SCRIPT" with context SCRIPT="deploy.py"
- **THEN** file path is resolved to "deploy.py", operation `:execute`

#### Scenario: Execute with unresolved variable
- **WHEN** extracting operations from "python $UNKNOWN_SCRIPT" without variable context
- **THEN** operation marked with `:unresolved t`, file path is "$UNKNOWN_SCRIPT"

### Requirement: Execute operations in pipelines and chains
The system SHALL extract execute operations from all commands in pipelines and chains.

#### Scenario: Execute in pipeline
- **WHEN** extracting operations from "python generate.py | node process.js"
- **THEN** system returns two execute operations: "generate.py" and "process.js"

#### Scenario: Execute in command chain
- **WHEN** extracting operations from "bash setup.sh && python main.py"
- **THEN** system returns two execute operations: "setup.sh" and "main.py"

### Requirement: Execute operations from nested commands
The system SHALL extract execute operations from nested command strings (bash -c, sh -c) and mark them with `:indirect t`.

#### Scenario: Nested python execution
- **WHEN** extracting operations from "bash -c 'python script.py'"
- **THEN** system returns operation: file "script.py", operation `:execute`, indirect `t`

#### Scenario: Nested self-execution
- **WHEN** extracting operations from "sh -c './deploy.sh'"
- **THEN** system returns operation: file "./deploy.sh", operation `:execute`, indirect `t`, self-executing `t`

### Requirement: Interpreter command coverage
The system SHALL include semantics entries for common interpreters and runtimes that execute scripts.

#### Scenario: Python interpreters
- **WHEN** database is queried for python interpreters
- **THEN** semantics exist for "python", "python3"

#### Scenario: JavaScript runtimes
- **WHEN** database is queried for JavaScript runtimes
- **THEN** semantics exist for "node"

#### Scenario: Shell interpreters
- **WHEN** database is queried for shell interpreters
- **THEN** semantics exist for "bash", "sh", "zsh"

#### Scenario: Shell built-ins
- **WHEN** database is queried for shell built-ins
- **THEN** semantics exist for "source", "."

#### Scenario: Go runtime
- **WHEN** database is queried for go runtime with subcommands
- **THEN** semantics exist for "go" with subcommand handlers for "run" and "test"
