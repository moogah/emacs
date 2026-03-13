# Purpose

The bash tools system provides controlled command execution with semantic validation using bash-parser integration. Commands are parsed into AST, semantic operations extracted (file operations, cloud auth), and validated against operation-specific scope patterns.

This integrates with bash-parser (`config/bash-parser/`) for tree-sitter parsing, scope profiles for configuration, and preset registration for deployment. Execution boundaries defined in `scope.yml` v4 files include operation-specific path scoping (read/write/execute/modify), cloud authentication detection, and parse completeness enforcement.

# Requirements

## Shell composition features with full validation
The bash tools system SHALL parse shell composition features (pipes, redirects, command substitution) using bash-parser and validate all commands in pipelines.

### Scenario: Piped commands all validated
- **WHEN** command is "ls | grep foo"
- **THEN** the system validates both "ls" and "grep" against bash_tools deny list

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
The bash tools system SHALL execute a multi-step validation pipeline for each command using bash-parser for semantic extraction, replacing category-based validation with operation-first validation and no-op allowance.

### Scenario: Validation pipeline for command execution
- **WHEN** run_bash_command is called with a command
- **THEN** the system executes these steps in order:
  1. Parse command using bash-parser (jf/bash-parse) to get AST with tokens
  2. Check parse completeness (:parse-complete flag)
  3. Check deny list (bash_tools.deny) for all pipeline commands
  4. Extract semantics using plugin system (jf/bash-extract-semantics)
  5. Check if no file operations extracted (no-op allowance)
  6. If file operations exist, validate each operation against paths configuration
  7. Extract file paths and resolve to absolute paths relative to default-directory
  8. Validate each file path against operation-specific scope patterns
  9. Check cloud authentication detection (using cloud-auth plugin)
  10. Enforce cloud authentication policy
  11. Execute command with timeout and output truncation

### Scenario: No-op command allowed without category check
- **WHEN** command is `python3 --version`
- **AND** bash-parser extracts zero file operations
- **THEN** command passes validation at no-op check stage without checking categories

### Scenario: Command with operations proceeds to path validation
- **WHEN** command is `python3 script.py`
- **AND** bash-parser extracts :execute operation
- **THEN** validation skips no-op check and proceeds to file operation validation

### Scenario: Parse completeness enforced
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is true
- **THEN** system rejects command with "incomplete_parse" error

### Scenario: Deny list checked before no-op allowance
- **WHEN** command is `sudo --version`
- **AND** command would extract zero file operations
- **AND** "sudo" is in bash_tools.deny list
- **THEN** system rejects at deny list stage before reaching no-op check

### Scenario: Parse completeness optional
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is false
- **THEN** system proceeds with validation but includes warning

### Scenario: Working directory symlink resolution
- **WHEN** default-directory contains symlinks
- **THEN** the system resolves symlinks to the real path before pattern matching
- **AND** pattern matching occurs against the fully resolved path

## No-op commands allowed by default
The bash tools system SHALL allow commands that extract no file operations to execute without requiring command name in any allowlist.

### Scenario: Version check allowed without allowlist
- **WHEN** command is `ruby --version`
- **AND** bash-parser extracts zero file operations
- **AND** "ruby" not in any configuration
- **THEN** command allowed via no-op allowance

### Scenario: Help command allowed without allowlist
- **WHEN** command is `gcc --help`
- **AND** bash-parser extracts zero file operations
- **THEN** command allowed

### Scenario: Unknown tool with no operations allowed
- **WHEN** command is `my-custom-tool --info`
- **AND** tool not in deny list
- **AND** bash-parser extracts zero file operations
- **THEN** command allowed

### Scenario: No-op command in deny list still blocked
- **WHEN** command is `sudo --version`
- **AND** bash-parser extracts zero file operations
- **AND** "sudo" in bash_tools.deny list
- **THEN** command denied at deny list stage

## Deny list checked for all pipeline commands
The bash tools system SHALL check all commands in pipelines and chains against the deny list, independent of file operations.

### Scenario: Pipeline with denied command rejected
- **WHEN** command is `echo "test" | sudo tee file.txt`
- **AND** "sudo" in bash_tools.deny list
- **THEN** validation fails at deny list stage with command_denied error

### Scenario: Pipeline with all non-denied commands proceeds
- **WHEN** command is `ls | grep foo`
- **AND** neither "ls" nor "grep" in deny list
- **THEN** deny list check passes and proceeds to semantics extraction

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
The bash tools system SHALL return structured errors when inline expansion is denied, maintaining the same error format that previously guided LLMs to use `request_scope_expansion`.

### Scenario: Command denied error structure
- **WHEN** a command is in deny list
- **THEN** the system returns :allowed nil with :reason "denied-command", tool, command, and security warning

### Scenario: Parse incomplete error structure
- **WHEN** command cannot be fully parsed
- **THEN** error includes :error "incomplete_parse", :parse_errors, :partial_tokens

### Scenario: Path out of scope error with operation detail
- **WHEN** file path validation fails and user denies expansion
- **THEN** error includes :error "path_out_of_scope", :path, :operation, :required_scope, :allowed_patterns

### Scenario: Pipeline command denied with position
- **WHEN** pipeline command fails deny list validation
- **THEN** error includes :pipeline_position, :failed_command, :full_pipeline

### Scenario: Cloud auth denied error with provider
- **WHEN** cloud auth command denied
- **THEN** error includes :error "cloud_auth_denied", :provider, :allowed_providers

### Scenario: Coverage metrics in successful responses
- **WHEN** command executes successfully
- **THEN** result may include :coverage with :total_tokens, :claimed_tokens, :coverage_ratio

### Scenario: Error messages no longer suggest request_scope_expansion
- **WHEN** any scope violation occurs and user denies inline expansion
- **THEN** the error message does NOT suggest using request_scope_expansion tool
- **AND** error indicates user denied the expansion request

## Integration with scope expansion flow
The bash tools system SHALL automatically trigger inline scope expansion UI when validation fails, allowing commands to execute immediately upon user approval within the same tool call. The `request_scope_expansion` meta-tool remains available for pre-emptive permission requests.

### Scenario: Validation failure triggers inline expansion UI
- **WHEN** `run_bash_command` validation fails
- **THEN** the system automatically triggers the expansion transient menu
- **AND** user sees 3-choice UI (deny/add-to-scope/allow-once)
- **AND** if user approves, validation is retried and command executes in same tool call
- **AND** if user denies, error is returned to LLM

### Scenario: User approves with allow-once in inline flow
- **WHEN** validation fails and user selects "Allow once"
- **THEN** permission is added to allow-once list
- **AND** validation is retried immediately
- **AND** command executes successfully in same tool call

### Scenario: User approves with add-to-scope in inline flow
- **WHEN** validation fails and user selects "Add to scope"
- **THEN** pattern is added to appropriate paths section in scope.yml
- **AND** validation is retried immediately
- **AND** command executes successfully in same tool call

### Scenario: User denies inline expansion
- **WHEN** validation fails and user selects "Deny"
- **THEN** error is returned to LLM with structured error format
- **AND** command does not execute

### Scenario: LLM requests path pattern expansion pre-emptively
- **WHEN** `request_scope_expansion` is called for run_bash_command before attempting command
- **THEN** the system infers validation type and presents transient menu to user
- **AND** approval adds to allow-once list or scope.yml for subsequent command

### Scenario: User approves path pattern permanently via request_scope_expansion
- **WHEN** user selects "Add to scope" in expansion menu via `request_scope_expansion`
- **THEN** the system adds path pattern to appropriate paths section in scope.yml (read/write/execute/modify)

### Scenario: User approves path once via request_scope_expansion
- **WHEN** user selects "Allow once" in expansion menu via `request_scope_expansion`
- **THEN** the system adds to allow-once list for current turn

### Scenario: LLM requests command expansion for denied command
- **WHEN** `request_scope_expansion` is called for command in deny list
- **THEN** the system presents option to remove from deny list (dangerous operation)

## Configuration loading from scope document
The bash tools system SHALL load bash command configuration from `scope.yml` located in the session's branch directory, loading only the deny list without categories.

### Scenario: Bash tools configuration loaded from scope.yml
- **WHEN** a bash command executes
- **THEN** the system loads bash_tools section from scope.yml in buffer's branch directory
- **AND** uses plain YAML parsing (no frontmatter extraction)
- **AND** normalizes YAML keys from snake_case to kebab-case

### Scenario: Missing bash tools section handled
- **WHEN** scope.yml exists but has no bash_tools section
- **THEN** the system uses empty deny list (allow all commands by default, subject to operation validation)

### Scenario: Missing scope.yml errors
- **WHEN** scope.yml does not exist in the branch directory
- **THEN** the system returns error "no_scope_config"

### Scenario: Deny list parsed without categories
- **WHEN** bash_tools configuration is loaded
- **THEN** the system parses bash_tools.deny list for globally denied commands
- **AND** does NOT parse or expect categories section

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
