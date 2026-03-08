# Purpose

The bash tools system provides controlled command execution with semantic validation using bash-parser integration. Commands are parsed into AST, semantic operations extracted (file operations, cloud auth), and validated against operation-specific scope patterns.

This integrates with bash-parser (`config/bash-parser/`) for tree-sitter parsing, scope profiles for configuration, and preset registration for deployment. Execution boundaries defined in `scope.yml` v4 files include operation-specific path scoping (read/write/execute/modify), cloud authentication detection, and parse completeness enforcement.

# Requirements

## Command categorization by operation type
The bash tools system SHALL categorize commands into read_only, safe_write, and dangerous categories based on their operational impact.

### Scenario: Read-only command categorized
- **WHEN** a command only reads data (ls, grep, find, cat, head, tail, wc, file, git log, git show, git diff)
- **THEN** the system categorizes it as read_only requiring paths.read access

### Scenario: Safe write command categorized
- **WHEN** a command creates or modifies safely (mkdir, touch, echo, git add, git commit)
- **THEN** the system categorizes it as safe_write requiring paths.write access

### Scenario: Dangerous command categorized
- **WHEN** a command is explicitly listed in bash_tools.categories.dangerous
- **THEN** the system categorizes it as dangerous, always denies it, and requires explicit user approval via request_scope_expansion

### Scenario: Denied command rejected immediately
- **WHEN** a command appears in bash_tools.deny list
- **THEN** the system rejects it without checking category or directory scope

### Scenario: Unknown command denied by default
- **WHEN** a command is not in any category allow list
- **THEN** the system denies access with "command_not_allowed" error

## Directory scope validation per category
The bash tools system SHALL validate that the working directory matches the command category's path scope requirement.

### Scenario: Read-only command in read path
- **WHEN** a read_only command executes with directory matching paths.read patterns
- **THEN** the system allows execution

### Scenario: Read-only command in write path
- **WHEN** a read_only command executes with directory matching paths.write patterns
- **THEN** the system allows execution (write scope includes read capability)

### Scenario: Safe write command in write path
- **WHEN** a safe_write command executes with directory matching paths.write patterns
- **THEN** the system allows execution

### Scenario: Safe write command in read-only path
- **WHEN** a safe_write command executes with directory matching only paths.read patterns
- **THEN** the system denies with "directory_not_in_scope" error including required_scope and allowed_patterns

### Scenario: Directory matches deny pattern
- **WHEN** a directory matches paths.deny patterns
- **THEN** the system denies regardless of category (deny takes precedence)

### Scenario: Directory outside all patterns
- **WHEN** a directory does not match any paths.read or paths.write patterns
- **THEN** the system denies with "directory_not_in_scope" error

## Explicit directory required for all commands
The bash tools system SHALL require an explicit directory argument for every command execution.

### Scenario: Command with explicit directory
- **WHEN** run_bash_command is called with both command and directory arguments
- **THEN** the system resolves the directory to absolute path and validates it

### Scenario: Relative directory path resolved
- **WHEN** a relative directory path is provided
- **THEN** the system expands it to an absolute path using expand-file-name

### Scenario: Symlink directory resolved
- **WHEN** a directory contains symlinks
- **THEN** the system resolves to real path using file-truename before validation

## Shell composition features with full validation
The bash tools system SHALL parse shell composition features (pipes, redirects, command substitution) using bash-parser and validate all commands in pipelines.

### Scenario: Piped commands all validated
- **WHEN** command is "ls | grep foo"
- **THEN** the system validates both "ls" and "grep" against bash_tools categories

### Scenario: Dangerous command in pipeline rejected
- **WHEN** command is "ls | xargs rm"
- **AND** "rm" is in deny list
- **THEN** the system rejects with "command_denied" error identifying "rm" in pipeline position

### Scenario: Redirected output allowed after validation
- **WHEN** command is "cat file.txt > output.txt"
- **THEN** the system extracts "cat" command, validates it, and validates file paths

### Scenario: Command substitution parsed and validated
- **WHEN** command is "echo $(pwd)"
- **THEN** the system parses command substitution and validates inner command

### Scenario: Complex pipeline fully validated
- **WHEN** command is "find . -name '*.el' | xargs grep -l 'defun' | head -10"
- **THEN** the system validates "find", "xargs grep", and "head" commands

## Complete validation pipeline
The bash tools system SHALL execute a multi-step validation pipeline for each command using bash-parser for semantic extraction.

### Scenario: Validation pipeline for command execution
- **WHEN** run_bash_command is called with command and directory
- **THEN** the system executes these steps in order:
  1. Parse command using bash-parser (jf/bash-parse) to get AST with tokens
  2. Extract semantics using plugin system (jf/bash-extract-semantics)
  3. Check parse completeness (:parse-complete flag)
  4. Validate all commands in pipeline/chain (not just base command)
  5. Categorize each command (check deny → read_only → safe_write → dangerous)
  6. Extract file operations from command (using file-ops plugin)
  7. Resolve file paths to absolute paths relative to working directory
  8. Validate each file path against operation-specific scope patterns
  9. Check cloud authentication detection (using cloud-auth plugin)
  10. Enforce cloud authentication policy
  11. Execute command with timeout and output truncation

### Scenario: Parse completeness enforced
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is true
- **THEN** system rejects command with "incomplete_parse" error

### Scenario: Parse completeness optional
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is false
- **THEN** system proceeds with validation but includes warning

### Scenario: Directory resolution with symlinks
- **WHEN** validating a directory path containing symlinks
- **THEN** the system resolves symlinks to the real path before pattern matching
- **AND** pattern matching occurs against the fully resolved path

### Scenario: Relative directory resolution
- **WHEN** a relative directory path is provided
- **THEN** the system expands it to absolute path relative to buffer's default-directory
- **AND** then resolves any symlinks to real path

## Safe command execution with timeouts
The bash tools system SHALL execute commands with timeout protection to prevent runaway processes.

### Scenario: Command completes within timeout
- **WHEN** a command executes and completes within 30 seconds
- **THEN** the system returns success with output and exit code

### Scenario: Command exceeds timeout
- **WHEN** a command runs longer than 30 seconds
- **THEN** the system terminates it and returns error "timeout" with explanatory message

### Scenario: Command execution captures output
- **WHEN** a command executes successfully
- **THEN** the system captures stdout/stderr in :output field

### Scenario: Command execution captures exit code
- **WHEN** a command executes
- **THEN** the system captures the exit code in :exit_code field

### Scenario: Non-zero exit code reported
- **WHEN** a command exits with non-zero status
- **THEN** the system returns :success nil and includes exit_code in result

## Output truncation for large results
The bash tools system SHALL limit output size to prevent overwhelming the LLM context.

### Scenario: Output within limit returned fully
- **WHEN** command output is less than maximum characters
- **THEN** the system returns complete output

### Scenario: Output exceeding limit truncated
- **WHEN** command output exceeds maximum characters
- **THEN** the system truncates output and appends truncation notice with character count

### Scenario: Truncation notice suggests filtering
- **WHEN** output is truncated
- **THEN** the notice suggests using filters like 'head', 'grep', or 'tail' to narrow results

## Absolute path warning in command arguments
The bash tools system SHALL warn when command arguments contain absolute paths that bypass directory scope validation.

### Scenario: Command with relative paths
- **WHEN** command is "grep -r 'TODO' ./src"
- **THEN** the system executes without warnings

### Scenario: Command with absolute path argument
- **WHEN** command is "grep -r 'TODO' /Users/jefffarr/other-project"
- **THEN** the system includes :warnings field noting absolute path bypasses scope validation

### Scenario: Warning suggests relative paths
- **WHEN** an absolute path warning is issued
- **THEN** the warning message suggests using relative paths for proper scope validation

## File path validation with operation-specific scoping
The bash tools system SHALL validate all file paths extracted from commands against operation-specific scope patterns.

### Scenario: Read operation validated against paths.read
- **WHEN** command is "cat /workspace/file.txt"
- **AND** file operations plugin extracts :read operation for /workspace/file.txt
- **THEN** system validates path against paths.read and paths.write patterns

### Scenario: Write operation requires paths.write
- **WHEN** command is "touch /workspace/output.txt"
- **AND** file operations plugin extracts :write operation
- **AND** paths.read: ["/workspace/**"] but no paths.write
- **THEN** system rejects with "path_out_of_scope" error

### Scenario: Execute operation requires paths.execute
- **WHEN** command is "python /workspace/scripts/deploy.py"
- **AND** file operations plugin extracts :execute operation
- **AND** paths.read and paths.write cover /workspace but no paths.execute
- **THEN** system rejects with "path_out_of_scope" error

### Scenario: Modify operation requires paths.modify
- **WHEN** command is "sed -i 's/foo/bar/' /workspace/config.yml"
- **AND** file operations plugin extracts :modify operation
- **AND** no paths.modify configured
- **THEN** system rejects with "path_out_of_scope" error

### Scenario: File path denied takes precedence
- **WHEN** command operates on path matching paths.deny
- **THEN** system rejects regardless of operation type or other path scopes

### Scenario: Multiple file operations validated independently
- **WHEN** command is "cp /workspace/source.txt /tmp/dest.txt"
- **THEN** system validates source.txt against :read scope and dest.txt against :write scope

## Cloud authentication detection and enforcement
The bash tools system SHALL detect cloud authentication commands and enforce cloud policy from scope.yml.

### Scenario: Cloud auth detected in allow mode
- **WHEN** command is "aws-vault exec prod -- aws s3 ls"
- **AND** cloud.auth_detection: "allow"
- **THEN** command executes without restrictions

### Scenario: Cloud auth detected in warn mode
- **WHEN** command is "gcloud auth login"
- **AND** cloud.auth_detection: "warn"
- **THEN** command executes with warning in result

### Scenario: Cloud auth detected in deny mode
- **WHEN** command is "az login"
- **AND** cloud.auth_detection: "deny"
- **AND** "azure" not in cloud.allowed_providers
- **THEN** system rejects with "cloud_auth_denied" error

### Scenario: Allowed provider in deny mode
- **WHEN** command uses cloud provider in allowed_providers list
- **AND** cloud.auth_detection: "deny"
- **THEN** command allowed

## Coverage-based validation warnings
The bash tools system SHALL optionally warn when semantic coverage is below configured threshold.

### Scenario: High coverage passes silently
- **WHEN** bash-parser coverage is >= max_coverage_threshold
- **THEN** no coverage warnings generated

### Scenario: Low coverage generates warning
- **WHEN** bash-parser coverage is < max_coverage_threshold
- **AND** max_coverage_threshold is configured
- **THEN** result includes warning about low coverage ratio

### Scenario: Coverage warning includes metrics
- **WHEN** low coverage warning generated
- **THEN** warning includes total tokens, claimed tokens, and coverage ratio

## Structured error responses with expansion guidance
The bash tools system SHALL return structured errors that guide the LLM to request scope expansion when needed.

### Scenario: Command not allowed error structure
- **WHEN** a command is not in any allow list
- **THEN** the system returns :allowed nil with :reason "command-not-allowed", tool, command, and message fields

### Scenario: Command denied error structure
- **WHEN** a command is in deny list
- **THEN** the system returns :allowed nil with :reason "denied-command", tool, command, and security warning

### Scenario: Directory not in scope error structure
- **WHEN** directory does not match category's path requirement
- **THEN** the system returns :allowed nil with :reason "directory-not-in-scope", directory, required_scope, allowed_patterns, and message

### Scenario: Parse incomplete error structure
- **WHEN** command cannot be fully parsed
- **THEN** error includes :error "incomplete_parse", :parse_errors, :partial_tokens

### Scenario: Path out of scope error with operation detail
- **WHEN** file path validation fails
- **THEN** error includes :error "path_out_of_scope", :path, :operation, :required_scope, :allowed_patterns

### Scenario: Pipeline command denied with position
- **WHEN** pipeline command fails validation
- **THEN** error includes :pipeline_position, :failed_command, :full_pipeline

### Scenario: Cloud auth denied error with provider
- **WHEN** cloud auth command denied
- **THEN** error includes :error "cloud_auth_denied", :provider, :allowed_providers

### Scenario: Coverage metrics in successful responses
- **WHEN** command executes successfully
- **THEN** result may include :coverage with :total_tokens, :claimed_tokens, :coverage_ratio

### Scenario: Error messages suggest expansion
- **WHEN** any scope violation occurs
- **THEN** the error message suggests using request_scope_expansion tool

## Integration with scope expansion flow
The bash tools system SHALL integrate with the existing scope expansion mechanism for permission requests.

### Scenario: LLM requests command expansion
- **WHEN** request_scope_expansion is called for run_bash_command with a denied command
- **THEN** the system infers validation type and presents transient menu to user

### Scenario: User approves command permanently
- **WHEN** user selects "Add to scope" in expansion menu
- **THEN** the system adds command to appropriate category in scope.yml

### Scenario: User approves command once
- **WHEN** user selects "Allow once" in expansion menu
- **THEN** the system adds to allow-once list for current turn

### Scenario: LLM requests directory expansion
- **WHEN** request_scope_expansion is called with directory pattern
- **THEN** the system adds pattern to appropriate paths section (read or write)

## Configuration loading from scope document
The bash tools system SHALL load bash command configuration from `scope.yml` located in the session's branch directory.

### Scenario: Bash tools configuration loaded from scope.yml
- **WHEN** a bash command executes
- **THEN** the system loads bash_tools section from scope.yml in buffer's branch directory
- **AND** uses plain YAML parsing (no frontmatter extraction)
- **AND** normalizes YAML keys from snake_case to kebab-case

### Scenario: Missing bash tools section handled
- **WHEN** scope.yml exists but has no bash_tools section
- **THEN** the system uses empty allow lists (deny all commands by default)

### Scenario: Missing scope.yml errors
- **WHEN** scope.yml does not exist in the branch directory
- **THEN** the system returns error "no_scope_config"

### Scenario: Category structure parsed
- **WHEN** bash_tools configuration is loaded
- **THEN** the system parses bash_tools.categories.read_only, bash_tools.categories.safe_write, and bash_tools.categories.dangerous lists from YAML (using snake_case keys)
- **AND** normalizes keys to kebab-case during parsing: {:bash-tools {:categories {:read-only [...] :safe-write [...]} :deny [...]}}
- **NOTE:** YAML uses snake_case (bash_tools, read_only), Elisp uses kebab-case (:bash-tools, :read-only)

### Scenario: Deny list parsed
- **WHEN** bash_tools configuration is loaded
- **THEN** the system parses bash_tools.deny list for globally denied commands

### Scenario: Operation-specific path sections loaded (v4)
- **WHEN** scope.yml v4 is loaded
- **THEN** the system loads paths.read, paths.write, paths.execute, paths.modify, and paths.deny sections
- **AND** each section contains list of glob patterns for that operation type

### Scenario: Cloud configuration loaded (v4)
- **WHEN** scope.yml v4 has cloud section
- **THEN** the system loads cloud.auth_detection mode ("allow", "warn", or "deny")
- **AND** loads cloud.allowed_providers list of provider names

### Scenario: Security configuration loaded (v4)
- **WHEN** scope.yml v4 has security section
- **THEN** the system loads security.enforce_parse_complete boolean flag
- **AND** loads security.max_coverage_threshold numeric value (0.0-1.0)

### Scenario: Missing v4 sections get defaults
- **WHEN** scope.yml v4 is missing cloud or security sections
- **THEN** the system applies safe defaults (cloud.auth_detection: "warn", security.enforce_parse_complete: true)

### Scenario: Invalid v4 schema values rejected
- **WHEN** scope.yml v4 has invalid values (e.g., cloud.auth_detection: "invalid")
- **THEN** the system returns schema validation error at load time
