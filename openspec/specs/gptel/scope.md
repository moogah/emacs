# Gptel Scope System - Core Behavioral Spec

## Purpose

The scope system provides fine-grained permission control for gptel tools, enforcing access policies through preset-defined patterns. This spec describes the core validation engine, configuration loading, permission checking, and tool wrapping mechanisms.

## Requirements

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
- **WHEN** a tool executes shell commands
- **THEN** the system categorizes it with validation strategy "command" and operation type "write"

#### Scenario: Meta tool bypasses validation
- **WHEN** a tool is categorized as meta (e.g., request_scope_expansion, inspect_scope_plan)
- **THEN** the system categorizes it with validation strategy "meta" and allows it without scope checks

### Requirement: Configuration loading from preset
The scope system SHALL load scope configuration from the YAML frontmatter of preset.md files located in the session's branch directory.

#### Scenario: Configuration loaded from preset frontmatter
- **WHEN** a tool executes and needs scope validation
- **THEN** the system reads the YAML frontmatter from preset.md in the buffer's branch directory

#### Scenario: Missing configuration handled gracefully
- **WHEN** no preset.md exists in the branch directory
- **THEN** the system returns a "no_scope_config" error to the tool

#### Scenario: Buffer context determines directory
- **WHEN** a tool executes in a gptel buffer
- **THEN** the system uses the buffer-local jf/gptel--branch-dir variable to locate preset.md

### Requirement: Path-based validation
The scope system SHALL validate path-based tools against read/write/deny path lists using glob pattern matching.

#### Scenario: Deny patterns have highest priority
- **WHEN** a path matches both allow and deny patterns
- **THEN** the system denies access (deny takes precedence)

#### Scenario: Read operation matches read patterns
- **WHEN** a read tool accesses a path matching paths.read patterns
- **THEN** the system allows the operation

#### Scenario: Write operation matches write patterns
- **WHEN** a write tool accesses a path matching paths.write patterns
- **THEN** the system allows the operation

#### Scenario: Path fails to match allowed patterns
- **WHEN** a path does not match any allowed patterns for its operation type
- **THEN** the system denies access and returns allowed_patterns in the error

#### Scenario: Glob patterns support wildcards
- **WHEN** patterns use ** (match any including /), * (match any except /), or ? (match single char)
- **THEN** the system correctly matches paths using converted regex patterns

#### Scenario: Symlinks resolved before matching
- **WHEN** validating a path that is or contains symlinks
- **THEN** the system resolves symlinks to real paths before pattern matching

### Requirement: Pattern-based validation
The scope system SHALL validate org-roam tools against org_roam_patterns section with subdirectory, tags, and node_ids patterns.

#### Scenario: Create node with subdirectory pattern
- **WHEN** create_roam_node_in_scope specifies a subdirectory matching org_roam_patterns.subdirectory
- **THEN** the system allows the operation

#### Scenario: Create node with tag pattern
- **WHEN** create_roam_node_in_scope specifies tags matching org_roam_patterns.tags
- **THEN** the system allows the operation

#### Scenario: Add tags with matching pattern
- **WHEN** add_roam_tags_in_scope specifies tags matching org_roam_patterns.tags
- **THEN** the system allows the operation

#### Scenario: Link nodes with wildcard permission
- **WHEN** link_roam_nodes_in_scope is called and org_roam_patterns.node_ids contains "*"
- **THEN** the system allows linking any nodes

#### Scenario: Pattern validation fails
- **WHEN** an org-roam tool's arguments do not match any allowed patterns
- **THEN** the system denies access and returns "not-in-org-roam-patterns" reason

### Requirement: Command-based validation
The scope system SHALL validate shell commands against shell_commands allowlist and denylist using substring matching.

#### Scenario: Deny patterns checked first
- **WHEN** a command matches both allow and deny patterns
- **THEN** the system denies the command (deny takes precedence)

#### Scenario: Command name in allowlist
- **WHEN** the command's base name (first word) is in shell_commands.allow
- **THEN** the system allows the command

#### Scenario: Wildcard allowlist grants all commands
- **WHEN** shell_commands.allow contains "*"
- **THEN** the system allows any command not explicitly denied

#### Scenario: Command not in allowlist
- **WHEN** a command's base name is not in the allowlist and no wildcard is present
- **THEN** the system denies access and returns "not-in-allowlist" reason with allowed_patterns

#### Scenario: Deny pattern substring match
- **WHEN** a deny pattern appears anywhere in the full command string
- **THEN** the system denies the command with "denied-command" reason

### Requirement: Allow-once temporary permissions
The scope system SHALL support temporary single-use permissions that last only for the current LLM response turn.

#### Scenario: Allow-once grants temporary access
- **WHEN** a tool and resource are added to the allow-once list
- **THEN** the next validation check for that exact tool and resource succeeds

#### Scenario: Allow-once permission consumed on use
- **WHEN** a tool succeeds via allow-once permission
- **THEN** the permission is removed from the allow-once list (single-use)

#### Scenario: Allow-once cleared after response
- **WHEN** an LLM response completes
- **THEN** the system clears all allow-once permissions via gptel-post-response-functions hook

#### Scenario: Allow-once checked before config
- **WHEN** validating a tool call
- **THEN** the system checks allow-once list before checking the preset configuration

#### Scenario: Allow-once works without config
- **WHEN** a tool is granted allow-once permission but no preset config exists
- **THEN** the tool succeeds (allow-once bypasses missing config check)

### Requirement: Permission checking priority
The scope system SHALL check permissions in priority order: allow-once, deny patterns, allow patterns, default policy.

#### Scenario: Allow-once bypasses all other checks
- **WHEN** a tool call has allow-once permission
- **THEN** the system allows it without checking deny patterns or config

#### Scenario: Deny patterns block allowed patterns
- **WHEN** a resource matches both deny and allow patterns
- **THEN** the system denies access (deny takes precedence)

#### Scenario: Allow patterns checked after deny
- **WHEN** a resource passes deny checks
- **THEN** the system checks if it matches the appropriate allow patterns for the operation type

#### Scenario: Default policy deny when no match
- **WHEN** a resource does not match any allow patterns and no default policy is specified
- **THEN** the system denies access (secure default)

### Requirement: Structured error responses
The scope system SHALL return structured error responses to tools that include sufficient information for the LLM to request expansion or adjust its approach.

#### Scenario: Scope violation error structure
- **WHEN** a tool is denied due to scope violation
- **THEN** the system returns :success nil, :error type, :tool name, :resource identifier, and :message

#### Scenario: Pattern information included for path tools
- **WHEN** a path-based tool is denied for not matching patterns
- **THEN** the error includes :allowed_patterns showing what patterns would be acceptable

#### Scenario: No config error
- **WHEN** validation fails because preset.md does not exist
- **THEN** the system returns :error "no_scope_config" with explanatory message

#### Scenario: Error messages guide LLM to expansion
- **WHEN** any scope violation occurs
- **THEN** the error message suggests using request_scope_expansion tool

### Requirement: Macro-based tool wrapping
The scope system SHALL provide a macro that wraps tool definitions to automatically handle validation without boilerplate.

#### Scenario: Macro loads config automatically
- **WHEN** a scoped tool is invoked via the macro
- **THEN** the macro loads scope configuration from preset.md without tool code needing to do so

#### Scenario: Macro normalizes arguments
- **WHEN** tool arguments arrive as JSON vectors
- **THEN** the macro converts them to lists before passing to validation and tool body

#### Scenario: Macro checks permissions before body
- **WHEN** a scoped tool is invoked
- **THEN** the macro validates permissions before executing the tool body

#### Scenario: Tool body executes only if allowed
- **WHEN** permission check passes
- **THEN** the macro executes the tool body and returns its result

#### Scenario: Macro returns formatted error if denied
- **WHEN** permission check fails
- **THEN** the macro prevents tool body execution and returns structured error response

#### Scenario: Macro handles exceptions
- **WHEN** validation or tool execution throws an exception
- **THEN** the macro catches it and returns :error "tool_exception" with error message

### Requirement: Tool-specific first argument convention
The scope system SHALL extract the primary resource identifier from the first argument of each tool based on its validation strategy.

#### Scenario: Path tools use first arg as filepath
- **WHEN** validating a path-based tool
- **THEN** the system treats the first argument as the filepath to validate

#### Scenario: Pattern tools extract resource from args
- **WHEN** validating create_roam_node_in_scope
- **THEN** the system extracts subdirectory (2nd arg) and tags (3rd arg) for validation

#### Scenario: Bash tools use command and directory args
- **WHEN** validating run_bash_command
- **THEN** the system treats the first argument as the command string and second argument as the working directory

#### Scenario: Resource extraction for allow-once
- **WHEN** adding a tool to the allow-once list
- **THEN** the system extracts the resource identifier using the same convention as validation
