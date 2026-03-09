# Bash Tools - Delta Spec

## MODIFIED Requirements

### Requirement: Complete validation pipeline

The bash tools system SHALL execute a multi-step validation pipeline for each command using bash-parser for semantic extraction, replacing category-based validation with operation-first validation and no-op allowance.

#### Scenario: Validation pipeline for command execution
- **WHEN** run_bash_command is called with command and directory
- **THEN** the system executes these steps in order:
  1. Parse command using bash-parser (jf/bash-parse) to get AST with tokens
  2. Check parse completeness (:parse-complete flag)
  3. Check deny list (bash_tools.deny) for all pipeline commands
  4. Extract semantics using plugin system (jf/bash-extract-semantics)
  5. Check if no file operations extracted (no-op allowance)
  6. If file operations exist, validate each operation against paths configuration
  7. Extract file paths and resolve to absolute paths relative to working directory
  8. Validate each file path against operation-specific scope patterns
  9. Check cloud authentication detection (using cloud-auth plugin)
  10. Enforce cloud authentication policy
  11. Execute command with timeout and output truncation

#### Scenario: No-op command allowed without category check
- **WHEN** command is `python3 --version`
- **AND** bash-parser extracts zero file operations
- **THEN** command passes validation at no-op check stage without checking categories

#### Scenario: Command with operations proceeds to path validation
- **WHEN** command is `python3 script.py`
- **AND** bash-parser extracts :execute operation
- **THEN** validation skips no-op check and proceeds to file operation validation

#### Scenario: Parse completeness enforced
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is true
- **THEN** system rejects command with "incomplete_parse" error

#### Scenario: Deny list checked before no-op allowance
- **WHEN** command is `sudo --version`
- **AND** command would extract zero file operations
- **AND** "sudo" is in bash_tools.deny list
- **THEN** system rejects at deny list stage before reaching no-op check

### Requirement: Configuration loading from scope document

The bash tools system SHALL load bash command configuration from `scope.yml` located in the session's branch directory, loading only the deny list without categories.

#### Scenario: Bash tools configuration loaded from scope.yml
- **WHEN** a bash command executes
- **THEN** the system loads bash_tools section from scope.yml in buffer's branch directory
- **AND** uses plain YAML parsing (no frontmatter extraction)
- **AND** normalizes YAML keys from snake_case to kebab-case

#### Scenario: Missing bash tools section handled
- **WHEN** scope.yml exists but has no bash_tools section
- **THEN** the system uses empty deny list (allow all commands by default, subject to operation validation)

#### Scenario: Missing scope.yml errors
- **WHEN** scope.yml does not exist in the branch directory
- **THEN** the system returns error "no_scope_config"

#### Scenario: Deny list parsed without categories
- **WHEN** bash_tools configuration is loaded
- **THEN** the system parses bash_tools.deny list for globally denied commands
- **AND** does NOT parse or expect categories section

#### Scenario: Operation-specific path sections loaded (v4)
- **WHEN** scope.yml v4 is loaded
- **THEN** the system loads paths.read, paths.write, paths.execute, paths.modify, and paths.deny sections
- **AND** each section contains list of glob patterns for that operation type

#### Scenario: Cloud configuration loaded (v4)
- **WHEN** scope.yml v4 has cloud section
- **THEN** the system loads cloud.auth_detection mode ("allow", "warn", or "deny")
- **AND** loads cloud.allowed_providers list of provider names

#### Scenario: Security configuration loaded (v4)
- **WHEN** scope.yml v4 has security section
- **THEN** the system loads security.enforce_parse_complete boolean flag
- **AND** loads security.max_coverage_threshold numeric value (0.0-1.0)

#### Scenario: Missing v4 sections get defaults
- **WHEN** scope.yml v4 is missing cloud or security sections
- **THEN** the system applies safe defaults (cloud.auth_detection: "warn", security.enforce_parse_complete: true)

#### Scenario: Invalid v4 schema values rejected
- **WHEN** scope.yml v4 has invalid values (e.g., cloud.auth_detection: "invalid")
- **THEN** the system returns schema validation error at load time

### Requirement: Structured error responses with expansion guidance

The bash tools system SHALL return structured errors that guide the LLM to request scope expansion when needed, focusing on operation-based errors rather than category membership.

#### Scenario: Command denied error structure
- **WHEN** a command is in deny list
- **THEN** the system returns :allowed nil with :reason "denied-command", tool, command, and security warning

#### Scenario: Parse incomplete error structure
- **WHEN** command cannot be fully parsed
- **THEN** error includes :error "incomplete_parse", :parse_errors, :partial_tokens

#### Scenario: Path out of scope error with operation detail
- **WHEN** file path validation fails
- **THEN** error includes :error "path_out_of_scope", :path, :operation, :required_scope, :allowed_patterns

#### Scenario: Pipeline command denied with position
- **WHEN** pipeline command fails deny list validation
- **THEN** error includes :pipeline_position, :failed_command, :full_pipeline

#### Scenario: Cloud auth denied error with provider
- **WHEN** cloud auth command denied
- **THEN** error includes :error "cloud_auth_denied", :provider, :allowed_providers

#### Scenario: Coverage metrics in successful responses
- **WHEN** command executes successfully
- **THEN** result may include :coverage with :total_tokens, :claimed_tokens, :coverage_ratio

#### Scenario: Error messages suggest expansion
- **WHEN** any scope violation occurs
- **THEN** the error message suggests using request_scope_expansion tool

### Requirement: Integration with scope expansion flow

The bash tools system SHALL integrate with the existing scope expansion mechanism for permission requests, focusing on path patterns and deny list additions rather than category membership.

#### Scenario: LLM requests path pattern expansion
- **WHEN** request_scope_expansion is called for run_bash_command with path_out_of_scope error
- **THEN** the system infers validation type and presents transient menu to user

#### Scenario: User approves path pattern permanently
- **WHEN** user selects "Add to scope" in expansion menu
- **THEN** the system adds path pattern to appropriate paths section in scope.yml (read/write/execute/modify)

#### Scenario: User approves path once
- **WHEN** user selects "Allow once" in expansion menu
- **THEN** the system adds to allow-once list for current turn

#### Scenario: LLM requests command expansion for denied command
- **WHEN** request_scope_expansion is called for command in deny list
- **THEN** the system presents option to remove from deny list (dangerous operation)

## REMOVED Requirements

### Requirement: Command categorization by operation type

**Reason**: Category-based command validation (read_only, safe_write, dangerous) replaced by operation-first validation. Commands no longer categorized by name.

**Migration**: Remove category checks from validation pipeline. Commands now validated via: (1) deny list check, (2) no-op allowance for commands with no file operations, (3) file operation validation against paths configuration for commands with operations.

### Requirement: Directory scope validation per category

**Reason**: Directory validation no longer tied to command categories. Validation now based on extracted file operations and operation-specific path scoping.

**Migration**: Remove category-based directory validation. File operations extracted by bash-parser are validated against operation-specific paths (paths.read, paths.write, paths.execute, paths.modify, paths.deny). Working directory still required but not validated against categories.

### Requirement: Configuration loading parses categories structure

**Reason**: Categories section removed from bash_tools configuration schema.

**Migration**: Remove category parsing logic. Only parse bash_tools.deny list. Commands with no operations allowed by default. Commands with operations validated against paths configuration.

### Requirement: Unknown command denied by default

**Reason**: Default-deny for unknown commands no longer needed. Commands allowed if they extract no file operations (no-op allowance) or if their file operations are within scope.

**Migration**: Remove "command_not_allowed" error for unknown commands. Unknown commands now: (1) allowed if no file operations extracted, (2) validated via file operations if operations extracted, (3) denied only if in deny list.

## ADDED Requirements

### Requirement: No-op commands allowed by default

The bash tools system SHALL allow commands that extract no file operations to execute without requiring command name in any allowlist.

#### Scenario: Version check allowed without allowlist
- **WHEN** command is `ruby --version`
- **AND** bash-parser extracts zero file operations
- **AND** "ruby" not in any configuration
- **THEN** command allowed via no-op allowance

#### Scenario: Help command allowed without allowlist
- **WHEN** command is `gcc --help`
- **AND** bash-parser extracts zero file operations
- **THEN** command allowed

#### Scenario: Unknown tool with no operations allowed
- **WHEN** command is `my-custom-tool --info`
- **AND** tool not in deny list
- **AND** bash-parser extracts zero file operations
- **THEN** command allowed

#### Scenario: No-op command in deny list still blocked
- **WHEN** command is `sudo --version`
- **AND** bash-parser extracts zero file operations
- **AND** "sudo" in bash_tools.deny list
- **THEN** command denied at deny list stage

### Requirement: Deny list checked for all pipeline commands

The bash tools system SHALL check all commands in pipelines and chains against the deny list, independent of file operations.

#### Scenario: Pipeline with denied command rejected
- **WHEN** command is `echo "test" | sudo tee file.txt`
- **AND** "sudo" in bash_tools.deny list
- **THEN** validation fails at deny list stage with command_denied error

#### Scenario: Pipeline with all non-denied commands proceeds
- **WHEN** command is `ls | grep foo`
- **AND** neither "ls" nor "grep" in deny list
- **THEN** deny list check passes and proceeds to semantics extraction
