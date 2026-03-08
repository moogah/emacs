# Delta Spec for gptel/bash-tools

This delta spec documents changes to the bash-tools capability as part of bash-parser integration.

## MODIFIED Requirements

### Requirement: Complete validation pipeline
The bash tools system SHALL execute a multi-step validation pipeline for each command using bash-parser for semantic extraction.

#### Scenario: Validation pipeline for command execution
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

#### Scenario: Parse completeness enforced
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is true
- **THEN** system rejects command with "incomplete_parse" error

#### Scenario: Parse completeness optional
- **WHEN** bash-parser cannot fully parse command (:parse-complete nil)
- **AND** security.enforce_parse_complete is false
- **THEN** system proceeds with validation but includes warning

#### Scenario: Directory resolution with symlinks
- **WHEN** validating a directory path containing symlinks
- **THEN** the system resolves symlinks to the real path before pattern matching
- **AND** pattern matching occurs against the fully resolved path

#### Scenario: Relative directory resolution
- **WHEN** a relative directory path is provided
- **THEN** the system expands it to absolute path relative to buffer's default-directory
- **AND** then resolves any symlinks to real path

### Requirement: Shell composition features with full validation
The bash tools system SHALL parse shell composition features (pipes, redirects, command substitution) and validate all commands in pipelines.

#### Scenario: Piped commands all validated
- **WHEN** command is "ls | grep foo"
- **THEN** system validates both "ls" and "grep" against bash_tools categories

#### Scenario: Dangerous command in pipeline rejected
- **WHEN** command is "ls | xargs rm"
- **AND** "rm" is in deny list
- **THEN** system rejects with "command_denied" error identifying "rm" in pipeline position

#### Scenario: Redirected output allowed after validation
- **WHEN** command is "cat file.txt > output.txt"
- **THEN** system extracts "cat" command, validates it, and validates file paths

#### Scenario: Command substitution parsed and validated
- **WHEN** command is "echo $(pwd)"
- **THEN** system parses command substitution and validates inner command

#### Scenario: Complex pipeline fully validated
- **WHEN** command is "find . -name '*.el' | xargs grep -l 'defun' | head -10"
- **THEN** system validates "find", "xargs grep", and "head" commands

## ADDED Requirements

### Requirement: File path validation with operation-specific scoping
The bash tools system SHALL validate all file paths extracted from commands against operation-specific scope patterns.

#### Scenario: Read operation validated against paths.read
- **WHEN** command is "cat /workspace/file.txt"
- **AND** file operations plugin extracts :read operation for /workspace/file.txt
- **THEN** system validates path against paths.read and paths.write patterns

#### Scenario: Write operation requires paths.write
- **WHEN** command is "touch /workspace/output.txt"
- **AND** file operations plugin extracts :write operation
- **AND** paths.read: ["/workspace/**"] but no paths.write
- **THEN** system rejects with "path_out_of_scope" error

#### Scenario: Execute operation requires paths.execute
- **WHEN** command is "python /workspace/scripts/deploy.py"
- **AND** file operations plugin extracts :execute operation
- **AND** paths.read and paths.write cover /workspace but no paths.execute
- **THEN** system rejects with "path_out_of_scope" error

#### Scenario: Modify operation requires paths.modify
- **WHEN** command is "sed -i 's/foo/bar/' /workspace/config.yml"
- **AND** file operations plugin extracts :modify operation
- **AND** no paths.modify configured
- **THEN** system rejects with "path_out_of_scope" error

#### Scenario: File path denied takes precedence
- **WHEN** command operates on path matching paths.deny
- **THEN** system rejects regardless of operation type or other path scopes

#### Scenario: Multiple file operations validated independently
- **WHEN** command is "cp /workspace/source.txt /tmp/dest.txt"
- **THEN** system validates source.txt against :read scope and dest.txt against :write scope

### Requirement: Cloud authentication detection and enforcement
The bash tools system SHALL detect cloud authentication commands and enforce cloud policy from scope.yml.

#### Scenario: Cloud auth detected in allow mode
- **WHEN** command is "aws-vault exec prod -- aws s3 ls"
- **AND** cloud.auth_detection: "allow"
- **THEN** command executes without restrictions

#### Scenario: Cloud auth detected in warn mode
- **WHEN** command is "gcloud auth login"
- **AND** cloud.auth_detection: "warn"
- **THEN** command executes with warning in result

#### Scenario: Cloud auth detected in deny mode
- **WHEN** command is "az login"
- **AND** cloud.auth_detection: "deny"
- **AND** "azure" not in cloud.allowed_providers
- **THEN** system rejects with "cloud_auth_denied" error

#### Scenario: Allowed provider in deny mode
- **WHEN** command uses cloud provider in allowed_providers list
- **AND** cloud.auth_detection: "deny"
- **THEN** command allowed

### Requirement: Coverage-based validation warnings
The bash tools system SHALL optionally warn when semantic coverage is below configured threshold.

#### Scenario: High coverage passes silently
- **WHEN** bash-parser coverage is >= max_coverage_threshold
- **THEN** no coverage warnings generated

#### Scenario: Low coverage generates warning
- **WHEN** bash-parser coverage is < max_coverage_threshold
- **AND** max_coverage_threshold is configured
- **THEN** result includes warning about low coverage ratio

#### Scenario: Coverage warning includes metrics
- **WHEN** low coverage warning generated
- **THEN** warning includes total tokens, claimed tokens, and coverage ratio

### Requirement: Enhanced error responses with parse details
The bash tools system SHALL return rich error information including parse details, coverage metrics, and semantic extraction results.

#### Scenario: Parse incomplete error structure
- **WHEN** command cannot be fully parsed
- **THEN** error includes :error "incomplete_parse", :parse_errors, :partial_tokens

#### Scenario: Path out of scope error with operation detail
- **WHEN** file path validation fails
- **THEN** error includes :error "path_out_of_scope", :path, :operation, :required_scope, :allowed_patterns

#### Scenario: Pipeline command denied with position
- **WHEN** pipeline command fails validation
- **THEN** error includes :pipeline_position, :failed_command, :full_pipeline

#### Scenario: Cloud auth denied error with provider
- **WHEN** cloud auth command denied
- **THEN** error includes :error "cloud_auth_denied", :provider, :allowed_providers

#### Scenario: Coverage metrics in successful responses
- **WHEN** command executes successfully
- **THEN** result may include :coverage with :total_tokens, :claimed_tokens, :coverage_ratio

## REMOVED Requirements

### Requirement: Base command extraction from complex commands
**Reason**: Replaced by full bash-parser semantic extraction. The simple regex-based extraction (jf/gptel-bash--parse-command) is removed in favor of tree-sitter parsing.

**Migration**: Code using jf/gptel-bash--parse-command should migrate to jf/bash-parse for full parsing or access the :command-name field from parsed results.

### Requirement: Shell composition features allowed
**Reason**: This requirement described the OLD behavior where only the base command was validated. Replaced by "Shell composition features with full validation" which validates all pipeline commands.

**Migration**: Users relying on pipeline bypass (e.g., "ls | xargs rm") must update scope.yml to allow all commands or remove dangerous commands from their pipelines.
