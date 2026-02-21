## ADDED Requirements

### Requirement: Bash tool categorization
The scope system SHALL support bash validation type for tools that execute shell commands with directory-scoped validation.

#### Scenario: Bash tool categorized
- **WHEN** a tool executes arbitrary shell commands in a specified directory
- **THEN** the system categorizes it with validation strategy "bash" and operation type "write"

#### Scenario: Bash validator dispatched
- **WHEN** scope system validates a bash tool
- **THEN** it routes to jf/gptel-scope--validate-bash-tool function

### Requirement: Bash-based validation
The scope system SHALL validate bash tools against both command category and directory path requirements.

#### Scenario: Command categorized and directory validated
- **WHEN** a bash tool executes
- **THEN** the system categorizes the command (read_only, safe_write, dangerous) and validates the directory against the category's path requirement

#### Scenario: Read-only command requires read paths
- **WHEN** a bash command is categorized as read_only
- **THEN** the system validates the directory matches paths.read or paths.write patterns

#### Scenario: Safe write command requires write paths
- **WHEN** a bash command is categorized as safe_write
- **THEN** the system validates the directory matches paths.write patterns

#### Scenario: Dangerous command denied by default
- **WHEN** a bash command is categorized as dangerous
- **THEN** the system denies it unless explicitly in bash_tools.dangerous allow list

#### Scenario: Deny list overrides category
- **WHEN** a bash command appears in bash_tools.deny
- **THEN** the system denies it regardless of category or directory scope

#### Scenario: Unknown command denied
- **WHEN** a bash command is not in any category allow list
- **THEN** the system denies with "command_not_allowed" error

## MODIFIED Requirements

### Requirement: Tool categorization system
The scope system SHALL maintain a categorization mapping for all scope-aware tools that defines their validation strategy and operation type.

#### Scenario: Path-based read tool categorized
- **WHEN** a tool performs file read operations
- **THEN** the system categorizes it with validation strategy "path" and operation type "read"

#### Scenario: Path-based write tool categorized
- **WHEN** a tool performs file write operations
- **THEN** the system categorizes it with validation strategy "path" and operation type "write"

#### Scenario: Pattern-based tool categorized
- **WHEN** a tool performs org-roam operations
- **THEN** the system categorizes it with validation strategy "pattern" and operation type based on the specific operation

#### Scenario: Command-based tool categorized
- **WHEN** a tool executes shell commands (deprecated - replaced by bash validation)
- **THEN** the system categorizes it with validation strategy "command" and operation type "write"

#### Scenario: Bash-based tool categorized
- **WHEN** a tool executes arbitrary shell commands with directory scoping
- **THEN** the system categorizes it with validation strategy "bash" and operation type "write"

#### Scenario: Meta tool bypasses validation
- **WHEN** a tool is categorized as meta (e.g., request_scope_expansion, inspect_scope_plan)
- **THEN** the system categorizes it with validation strategy "meta" and allows it without scope checks
