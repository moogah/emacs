## CONTEXT: Scoped Bash Command Execution

This delta spec describes modifications to the scope-expansion system to support bash command expansion.

**Integration with existing system:**
- Leverages existing transient menu UI (Deny, Add to scope, Allow once)
- Extends routing logic to handle bash validation type
- Adds YAML writer for bash_tools section
- Integrates with scope.yml mutation flow

**Related specs:** See `bash-tools/spec.md` for bash validation behavior, `scope.md` for tool categorization.

---

## MODIFIED Requirements

### Requirement: Scope expansion routing supports bash validation type
The scope expansion system SHALL route bash command expansion requests to the appropriate handler.

#### Scenario: Bash validation type routed to bash handler
- **WHEN** request_scope_expansion is called with tool_name "run_bash_command"
- **THEN** the system infers validation type as 'bash
- **AND** routes to jf/gptel-scope--add-bash-to-scope

#### Scenario: Bash resource is directory path
- **WHEN** bash expansion resource is a directory path (contains "/" or ends with "/")
- **THEN** the system delegates to jf/gptel-scope--add-path-to-scope
- **AND** adds directory pattern to paths.read or paths.write based on command category

#### Scenario: Bash resource is command name
- **WHEN** bash expansion resource is a command name (no "/" characters)
- **THEN** the system adds command to bash_tools.categories section
- **AND** categorizes based on operation type (read_only or safe_write)

### Requirement: Bash command addition to scope.yml
The scope expansion system SHALL write approved bash commands to the bash_tools section in scope.yml.

#### Scenario: Read-only command added to scope
- **WHEN** user approves a read-only command (e.g., "tree")
- **THEN** the system adds command to bash_tools.categories.read_only.commands list
- **AND** preserves existing commands in list
- **AND** writes updated structure to scope.yml

#### Scenario: Safe write command added to scope
- **WHEN** user approves a safe write command (e.g., "git commit")
- **THEN** the system adds command to bash_tools.categories.safe_write.commands list
- **AND** preserves existing commands in list
- **AND** writes updated structure to scope.yml

#### Scenario: Command already in category list
- **WHEN** user approves a command already in the category list
- **THEN** the system does not duplicate the command
- **AND** confirms expansion completed successfully

#### Scenario: Bash tools section missing from scope.yml
- **WHEN** scope.yml exists but has no bash_tools section
- **THEN** the system creates bash_tools section with default structure
- **AND** adds approved command to appropriate category
- **AND** writes complete bash_tools section to scope.yml

### Requirement: YAML serialization of bash_tools structure
The scope expansion system SHALL correctly serialize bash_tools structure to YAML with nested categories.

#### Scenario: Nested category structure preserved
- **WHEN** writing bash_tools to scope.yml
- **THEN** the system preserves nested structure: bash_tools → categories → read_only/safe_write → commands
- **AND** uses snake_case keys in YAML (bash_tools, read_only, safe_write)
- **AND** serializes command lists as YAML sequences

#### Scenario: Existing bash_tools structure updated
- **WHEN** scope.yml already has bash_tools section with commands
- **THEN** the system reads existing structure
- **AND** appends new command to appropriate category list
- **AND** preserves other categories and deny list
- **AND** writes complete updated structure

### Requirement: Allow-once for bash commands
The scope expansion system SHALL support temporary allow-once permissions for bash commands.

#### Scenario: Allow once for bash command adds to buffer-local list
- **WHEN** user selects "Allow once" for bash command
- **THEN** the system adds (tool-name . resource) to allow-once list
- **AND** resource format is "command:directory" (e.g., "grep:/Users/jefffarr/emacs")
- **AND** permission is valid only for current LLM turn
- **AND** permission is consumed on first use

#### Scenario: Allow-once cleared after response
- **WHEN** LLM response completes
- **THEN** the system clears buffer-local allow-once list
- **AND** temporary permissions no longer valid

### Requirement: Category inference from tool arguments
The scope expansion system SHALL infer bash command category from the command name and context.

#### Scenario: Command category inferred for scope writing
- **WHEN** adding bash command to scope.yml
- **THEN** the system checks command against known read_only and safe_write lists
- **AND** defaults to safe_write if command is unknown
- **AND** writes to appropriate category section

#### Scenario: Directory scope requirement determined by category
- **WHEN** bash command requires directory expansion
- **THEN** the system determines required scope (read vs write) based on command category
- **AND** adds pattern to paths.read for read_only commands
- **AND** adds pattern to paths.write for safe_write commands
