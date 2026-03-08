# Scope System - Core Behavioral Spec

## Purpose

Provides fine-grained permission control for gptel tools, enforcing access policies through scope configuration. This spec describes the core validation engine, configuration loading, permission checking, and tool wrapping mechanisms.

**Current version:** v4 (bash-parser integration)

**Breaking changes from v3:** The v4 scope system uses bash-parser for semantic command validation with operation-specific path scoping, cloud authentication detection, and security settings. v3 schemas are NOT backward compatible and require manual migration. See "Breaking Changes from v3" section below.

## Requirements

### Requirement: Tool categorization system

The scope system SHALL maintain categorization mapping for all scope-aware tools defining validation strategy and operation type.

**Implementation**: `config/gptel/scope/scope-core.org` - tool categories alist

#### Scenario: Path-based read tool categorized
- **WHEN** tool performs file read operations
- **THEN** system categorizes with validation strategy "path" and operation type "read"

#### Scenario: Path-based write tool categorized
- **WHEN** tool performs file write operations
- **THEN** system categorizes with validation strategy "path" and operation type "write"

#### Scenario: Pattern-based tool categorized
- **WHEN** tool performs org-roam operations
- **THEN** system categorizes with validation strategy "pattern"

#### Scenario: Command-based tool categorized
- **WHEN** tool executes shell commands (deprecated - replaced by bash validation)
- **THEN** system categorizes with validation strategy "command" and operation type "write"

#### Scenario: Bash-based tool categorized
- **WHEN** tool executes arbitrary shell commands with directory scoping
- **THEN** system categorizes with validation strategy "bash" and operation type "write"

#### Scenario: Meta tool bypasses validation
- **WHEN** tool categorized as meta (e.g., request_scope_expansion)
- **THEN** system allows without scope checks

#### Scenario: PersistentAgent delegation
- **WHEN** PersistentAgent tool categorized
- **THEN** marked as meta with `:operation delegate` (delegates to spawned agent)

### Requirement: Configuration loading from scope.yml

The scope system SHALL load scope configuration from scope.yml in session's branch directory.

**CRITICAL**: Configuration is **NOT cached** - scope.yml is read fresh on every tool call. Filesystem is source of truth.

**Implementation**: `config/gptel/scope/scope-core.org` - `jf/gptel-scope--load-config` (lines 407-442)

#### Scenario: Configuration loaded from scope.yml
- **WHEN** tool executes and needs scope validation
- **THEN** system reads scope configuration from scope.yml in buffer's branch directory
- **AND** does NOT use cached configuration (filesystem is source of truth)

#### Scenario: Missing configuration handled gracefully
- **WHEN** no scope.yml exists in branch directory
- **THEN** system returns "no_scope_config" error to tool

#### Scenario: Buffer context determines directory
- **WHEN** tool executes in gptel buffer
- **THEN** system uses buffer-local `jf/gptel--branch-dir` variable to locate scope.yml

### Requirement: Path-based validation

The scope system SHALL validate path-based tools against read/write/deny path lists using glob pattern matching.

**Implementation**: `config/gptel/scope/scope-core.org` - path validator

#### Scenario: Deny patterns have highest priority
- **WHEN** path matches both allow and deny patterns
- **THEN** system denies access (deny takes precedence)

#### Scenario: Read operation matches read patterns
- **WHEN** read tool accesses path matching `paths.read` patterns
- **THEN** system allows the operation

#### Scenario: Write operation matches write patterns
- **WHEN** write tool accesses path matching `paths.write` patterns
- **THEN** system allows the operation

#### Scenario: Path fails to match allowed patterns
- **WHEN** path doesn't match any allowed patterns for operation type
- **THEN** system denies access and returns `allowed_patterns` in error

#### Scenario: Glob patterns support wildcards
- **WHEN** patterns use `**` (any including /), `*` (any except /), or `?` (single char)
- **THEN** system correctly matches paths using converted regex patterns

#### Scenario: Symlinks resolved before matching
- **WHEN** validating path that is or contains symlinks
- **THEN** system resolves symlinks to real paths
- **AND** checks BOTH real-path AND absolute-path for pattern matches
- **AND** allows if either matches (patterns may match either form)

**Implementation**: `config/gptel/scope/scope-core.org` - `jf/gptel-scope--matches-pattern` (lines 1098-1108)

### Requirement: Pattern-based validation

The scope system SHALL validate org-roam tools against org_roam_patterns section with subdirectory, tags, and node_ids patterns.

**Implementation**: `config/gptel/scope/scope-core.org` - pattern validator

#### Scenario: Create node with subdirectory pattern
- **WHEN** `create_roam_node_in_scope` called with subdirectory (2nd argument) matching `org_roam_patterns.subdirectory`
- **THEN** system allows the operation

#### Scenario: Create node with tag pattern
- **WHEN** `create_roam_node_in_scope` called with tags (3rd argument) matching `org_roam_patterns.tags`
- **THEN** system allows the operation

#### Scenario: Add tags with matching pattern
- **WHEN** `add_roam_tags_in_scope` called with tags (2nd argument) matching `org_roam_patterns.tags`
- **THEN** system allows the operation

#### Scenario: Link nodes with wildcard permission
- **WHEN** `link_roam_nodes_in_scope` called and `org_roam_patterns.node_ids` contains `"*"`
- **THEN** system allows linking any nodes

#### Scenario: Pattern validation fails
- **WHEN** org-roam tool's arguments don't match any allowed patterns
- **THEN** system denies access with "not-in-org-roam-patterns" reason

**Note**: Org-roam tools use 2nd and 3rd arguments (subdirectory, tags), not just first argument.

### Requirement: Bash tool categorization

The scope system SHALL support bash validation type for tools that execute shell commands with semantic validation.

**Config source:** `scope.yml` in the session's branch directory, under the `bash_tools` top-level key with nested `categories` structure.

**Tool:** `run_bash_command` - Executes shell commands with seven-stage validation pipeline using bash-parser integration.

**Module:** `scope-shell-tools` (config/gptel/tools/scope-shell-tools.el) - Implements the run_bash_command tool using bash-parser semantic validation.

**Related specs:** See delta specs for detailed v4 behavior:
- `scope-validation-pipelines/spec.md` - Pipeline command extraction and validation
- `scope-validation-file-paths/spec.md` - Operation-specific path validation
- `scope-validation-cloud-auth/spec.md` - Cloud authentication policy enforcement
- `scope-schema-v4/spec.md` - v4 schema structure

#### Scenario: Bash tool categorized
- **WHEN** a tool executes arbitrary shell commands
- **THEN** the system categorizes it with validation strategy "bash" and operation type "write"

#### Scenario: Bash validator dispatched
- **WHEN** scope system validates a bash tool
- **THEN** it routes to jf/gptel-scope--validate-bash-tool function using seven-stage pipeline

### Requirement: Seven-stage bash validation pipeline

The scope system SHALL validate bash commands through a seven-stage pipeline with early exit on failure, using bash-parser for semantic extraction.

**Implementation**: `config/gptel/tools/scope-shell-tools.el` - `jf/gptel-scope--validate-bash-tool` (seven-stage pipeline)

**Validation stages:**
1. **Parse** - Use bash-parser (tree-sitter) to extract AST with tokens
2. **Extract semantics** - Run plugins (file-ops, cloud-auth, security) to extract operations
3. **Parse completeness** - Reject if incomplete and `security.enforce_parse_complete: true`
4. **Pipeline validation** - Extract and validate ALL commands in pipelines/chains
5. **Command categorization** - Check deny list, then read_only/safe_write/dangerous
6. **File operation validation** - Match extracted file paths against operation-specific scope patterns
7. **Cloud auth policy** - Enforce `cloud.auth_detection` and `allowed_providers`

#### Scenario: Command passes all seven stages
- **WHEN** bash command completes all validation stages successfully
- **THEN** system allows command execution

#### Scenario: Early exit on first failure
- **WHEN** any validation stage fails
- **THEN** pipeline exits immediately with structured error (does not continue to subsequent stages)

#### Scenario: Parse stage extracts AST
- **WHEN** command enters validation pipeline
- **THEN** bash-parser produces AST with token positions and structure

#### Scenario: Semantic extraction runs plugins
- **WHEN** AST is available
- **THEN** system runs file-ops, cloud-auth, and security plugins to extract operations

### Requirement: Pipeline command validation

The scope system SHALL extract and validate ALL commands in bash pipelines and chains, not just the base command. Closes security bypass where dangerous commands could hide in pipeline positions.

**Implementation**: `config/gptel/tools/scope-shell-tools.el` - `jf/gptel-scope--extract-pipeline-commands`, `jf/gptel-scope--validate-pipeline-commands`

#### Scenario: Extract commands from pipe
- **WHEN** parsing "ls -la | grep foo"
- **THEN** system extracts two commands: "ls" and "grep"

#### Scenario: Extract commands from command chain
- **WHEN** parsing "mkdir foo && cd foo"
- **THEN** system extracts two commands: "mkdir" and "cd"

#### Scenario: All pipeline commands validated independently
- **WHEN** command is "ls | xargs rm"
- **AND** "ls" is in read_only but "rm" is in deny list
- **THEN** validation fails with "command_denied" error for "rm" at pipeline position 1

#### Scenario: Pipeline bypass prevented
- **WHEN** command is "find . -name '*.tmp' | xargs rm"
- **AND** "rm" is in deny list
- **THEN** system rejects with error identifying "rm" in pipeline position 2

**Note**: This requirement DEPRECATED in v3 (regex-based parsing could not extract pipeline commands). Implemented in v4 with bash-parser integration.

### Requirement: Operation-specific path validation

The scope system SHALL validate file paths against operation-specific scope patterns (paths.read, paths.write, paths.execute, paths.modify) based on the operation type extracted from the command.

**Implementation**: `config/gptel/tools/scope-shell-tools.el` - file-ops plugin integration with bash-parser

**v4 schema** (scope.yml):
```yaml
paths:
  read:
    - "/workspace/**"
    - "/tmp/**"
  write:
    - "/workspace/**"
  execute:                    # NEW in v4
    - "/workspace/scripts/**"
  modify:                     # NEW in v4
    - "/workspace/config/**"
  deny:
    - "/etc/**"
    - "~/.ssh/**"
```

**Operation types**:
- `read` - File reads (cat, grep, find) - requires paths.read OR paths.write
- `write` - File writes (echo >, mkdir) - requires paths.write
- `execute` - Script execution (./script.sh, bash script.sh) - requires paths.execute
- `modify` - In-place edits (sed -i, awk -i) - requires paths.modify OR paths.write

#### Scenario: Read operation matches paths.read
- **WHEN** file operation is :read "/workspace/file.txt"
- **AND** scope.yml has paths.read: ["/workspace/**"]
- **THEN** validation passes

#### Scenario: Write scope includes read capability
- **WHEN** file operation is :read "/tmp/file.txt"
- **AND** scope.yml has paths.write: ["/tmp/**"]
- **THEN** validation passes (write scope includes read)

#### Scenario: Execute operation requires paths.execute
- **WHEN** file operation is :execute "/workspace/scripts/deploy.py"
- **AND** scope.yml has paths.read: ["/workspace/**"] but no paths.execute
- **THEN** validation fails with "path_out_of_scope" error

#### Scenario: Deny patterns take precedence
- **WHEN** file operation is :read "/etc/passwd"
- **AND** scope.yml has paths.deny: ["/etc/**"]
- **THEN** validation fails with "path_denied" error (even if paths.read includes /etc)

**Note**: v3 used directory-based validation only. v4 extracts file paths from command arguments and validates against operation-specific patterns.

### Requirement: Cloud authentication detection and policy enforcement

The scope system SHALL detect cloud authentication commands (AWS, GCP, Azure) and enforce configurable policy (allow, warn, deny) with provider filtering.

**Implementation**: `config/gptel/tools/scope-shell-tools.el` - cloud-auth plugin integration with bash-parser

**v4 schema** (scope.yml):
```yaml
cloud:
  auth_detection: "warn"      # "allow", "warn", or "deny"
  allowed_providers:
    - aws
    - gcp
```

**Detected commands**:
- AWS: `aws-vault`, `aws sts`, `aws configure`
- GCP: `gcloud auth`, `gcloud config`
- Azure: `az login`, `az account`

#### Scenario: Warn mode executes with warning
- **WHEN** cloud.auth_detection: "warn"
- **AND** command is "aws-vault exec prod -- aws s3 ls"
- **THEN** command executes successfully
- **AND** result includes :warnings field with cloud auth detection notice

#### Scenario: Deny mode rejects unlisted provider
- **WHEN** cloud.auth_detection: "deny"
- **AND** cloud.allowed_providers: ["aws"]
- **AND** command is "gcloud auth login"
- **THEN** validation fails with "cloud_auth_denied" for GCP

#### Scenario: Allow mode permits all cloud commands
- **WHEN** cloud.auth_detection: "allow"
- **AND** command uses any cloud provider
- **THEN** command executes without warnings or restrictions

**Note**: New in v4. v3 had no cloud authentication awareness.

### Requirement: Command categorization (v3 compatibility)

The scope system SHALL validate bash commands using category-based validation with three categories: read_only, safe_write, dangerous. This requirement maintained from v3 for backward compatibility.

**Implementation**: `config/gptel/tools/scope-shell-tools.el` - command categorization stage (stage 5)

**Configuration format** (scope.yml):
```yaml
bash_tools:
  categories:
    read_only:
      - ls
      - cat
      - grep
      - git log
    safe_write:
      - mkdir
      - touch
      - git add
      - git commit
    dangerous:
      - rm
      - sudo
  deny:
    - rm
    - sudo
    - chmod
```

**Category semantics** (unchanged from v3):

| Category | Meaning |
|----------|---------|
| `read_only` | Read-only commands; categorization only (no path enforcement at category level) |
| `safe_write` | Non-destructive creation |
| `dangerous` | Commands requiring explicit user approval |
| `deny` | Never allowed; blocked even with scope expansion |

#### Scenario: Deny list checked first
- **WHEN** command matches deny list
- **THEN** system denies command (deny takes precedence)
- **AND** returns custom `:message` field in error

#### Scenario: Read-only command allowed
- **WHEN** command in `bash_tools.categories.read_only`
- **THEN** system allows command (path validation handled separately in v4)

#### Scenario: Command not in categories
- **WHEN** command not in any category
- **THEN** system denies with "command_not_allowed" error

**Note**: v4 separates command categorization (stage 5) from path validation (stage 6). v3 combined these into single validation step.

### Requirement: Allow-once temporary permissions

The scope system SHALL support temporary single-use permissions lasting only for current LLM response turn.

**Implementation**: `config/gptel/scope/scope-core.org` - `jf/gptel-scope--allow-once-list` (buffer-local, lines 547-550)

**Consumption semantics**: Permission is consumed BEFORE tool body executes, not after. If tool body errors after permission check, permission is already consumed and cannot be retried with same permission.

#### Scenario: Allow-once grants temporary access
- **WHEN** tool and resource added to allow-once list
- **THEN** next validation check for exact tool and resource succeeds

#### Scenario: Allow-once permission consumed BEFORE execution
- **WHEN** tool validation succeeds via allow-once permission
- **THEN** permission removed from allow-once list immediately (before tool body executes)
- **AND** if tool body errors after check, permission already consumed

**Critical**: This timing matters - failed operations cannot be retried with same allow-once permission.

#### Scenario: Allow-once cleared after response
- **WHEN** LLM response completes
- **THEN** system clears all allow-once permissions via `gptel-post-response-functions` hook

#### Scenario: Allow-once checked before config
- **WHEN** validating tool call
- **THEN** system checks allow-once list before checking scope configuration

#### Scenario: Allow-once works without config
- **WHEN** tool granted allow-once permission but no scope config exists
- **THEN** tool succeeds (allow-once bypasses missing config check)

### Requirement: Permission checking priority

The scope system SHALL check permissions in priority order: allow-once, deny patterns, allow patterns, default policy.

#### Scenario: Allow-once bypasses all other checks
- **WHEN** tool call has allow-once permission
- **THEN** system allows without checking deny patterns or config

#### Scenario: Deny patterns block allowed patterns
- **WHEN** resource matches both deny and allow patterns
- **THEN** deny takes precedence (operation denied)

#### Scenario: Default policy denies unlisted resources
- **WHEN** resource doesn't match any allow patterns
- **THEN** system denies access (deny-by-default policy)

### Requirement: Macro-based tool wrapping

The scope system SHALL wrap scope-aware tools using macros that intercept arguments, validate against scope, and return structured errors on denial.

**Implementation**: `config/gptel/scope/scope-core.org` - defmacro for wrapping

#### Scenario: Wrapped tool extracts first argument
- **WHEN** scope-wrapped tool called
- **THEN** wrapper extracts first argument as resource identifier
- **AND** normalizes from JSON vectors to Elisp lists

#### Scenario: Validation runs before tool body
- **WHEN** wrapped tool called
- **THEN** scope validation runs before tool body executes
- **AND** tool body only executes if validation succeeds

#### Scenario: Structured error on denial
- **WHEN** scope validation fails
- **THEN** wrapper returns JSON with `:success nil`, `:reason` (error type), `:allowed_patterns`, `:denied_patterns`

#### Scenario: Meta tools skip validation wrapper
- **WHEN** tool categorized as meta
- **THEN** wrapper allows call without scope checks

**Note**: Different tools extract resource from different argument positions - path tools use 1st arg, org-roam tools use 2nd/3rd args.

### Requirement: Security configuration (v4)

The scope system SHALL enforce security settings for parse completeness and coverage thresholds.

**v4 schema** (scope.yml):
```yaml
security:
  enforce_parse_complete: true  # Reject unparseable commands
  max_coverage_threshold: 0.8   # Warn if <80% tokens claimed by plugins
```

**Settings**:
- `enforce_parse_complete` - When true, reject commands bash-parser cannot fully parse
- `max_coverage_threshold` - Float 0.0-1.0; warn if semantic plugin coverage below threshold

#### Scenario: Incomplete parse rejected when enforced
- **WHEN** security.enforce_parse_complete: true
- **AND** bash-parser cannot fully parse command (syntax error, unsupported construct)
- **THEN** validation fails with "incomplete_parse" error

#### Scenario: Incomplete parse allowed when not enforced
- **WHEN** security.enforce_parse_complete: false
- **AND** bash-parser cannot fully parse command
- **THEN** validation continues with warning (command may execute)

#### Scenario: Low coverage warning
- **WHEN** security.max_coverage_threshold: 0.8
- **AND** semantic plugins claim <80% of tokens
- **THEN** validation includes warning about incomplete semantic extraction

**Note**: New in v4. v3 had no parse completeness or coverage tracking.

### Requirement: Error message formatting

The scope system SHALL return structured error messages helping LLMs understand denials and request appropriate expansions.

#### Scenario: Path denial includes allowed patterns
- **WHEN** path validation fails
- **THEN** error includes `:allowed_patterns` list from `paths.read` or `paths.write`

#### Scenario: Path out of scope error structure (v4)
- **WHEN** file path fails validation
- **THEN** error includes `:error "path_out_of_scope"`, `:path`, `:operation`, `:required_scope`, `:allowed_patterns`

#### Scenario: Pipeline command denied error structure (v4)
- **WHEN** pipeline command fails validation
- **THEN** error includes `:error "command_denied"`, `:command`, `:pipeline_position`, `:full_command`

#### Scenario: Cloud auth denied error structure (v4)
- **WHEN** cloud auth command denied
- **THEN** error includes `:error "cloud_auth_denied"`, `:provider`, `:command`, `:allowed_providers`

#### Scenario: Bash denial includes custom message
- **WHEN** bash validation fails
- **THEN** error includes custom `:message` field explaining denial reason
- **AND** includes `:allowed_patterns` with bash category commands

#### Scenario: Pattern denial includes allowed patterns
- **WHEN** org-roam validation fails
- **THEN** error includes `:allowed_patterns` from `org_roam_patterns` section

#### Scenario: Deny pattern error distinguishes from allow failure
- **WHEN** resource explicitly denied
- **THEN** error uses `:denied_patterns` list (not allowed_patterns)

**Note**: Bash validator is unique in including custom `:message` fields for user-friendly error messages.

## Breaking Changes from v3

The v4 scope system introduces breaking changes that require manual migration from v3 schemas. No automatic migration or backward compatibility is provided.

### Schema Structure Changes

**v3 schema:**
```yaml
paths:
  read:
    - "/workspace/**"
  write:
    - "/workspace/**"
  deny:
    - "/etc/**"

bash_tools:
  categories:
    read_only:
      commands: ["ls", "cat"]
```

**v4 schema (incompatible):**
```yaml
paths:
  read:
    - "/workspace/**"
  write:
    - "/workspace/**"
  execute:              # NEW - required for script execution
    - "/workspace/scripts/**"
  modify:               # NEW - required for in-place edits
    - "/workspace/config/**"
  deny:
    - "/etc/**"

bash_tools:
  categories:
    read_only:          # Command arrays now at category level
      - ls
      - cat
    safe_write:
      - mkdir
    dangerous:
      - rm
  deny:
    - sudo

cloud:                  # NEW section
  auth_detection: "warn"
  allowed_providers:
    - aws

security:               # NEW section
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
```

### Migration Requirements

**Commands requiring new path sections:**
- Execute operations (e.g., `bash /workspace/script.sh`) → Add `paths.execute`
- Modify operations (e.g., `sed -i 's/foo/bar/' file.txt`) → Add `paths.modify`
- Cloud commands (e.g., `aws-vault exec`) → Add `cloud` section

**Pipeline validation:**
- v3: Only validated base command (e.g., `ls` in `ls | xargs rm`)
- v4: Validates ALL commands in pipeline (closes security bypass)
- Impact: Commands like `find . | xargs rm` now rejected if `rm` in deny list

**File path validation:**
- v3: Validated working directory only
- v4: Extracts and validates file paths from command arguments
- Impact: `cat /etc/passwd` now checks `/etc/passwd` against scope, not just working directory

**bash_tools structure:**
- v3: `categories.read_only.commands: ["ls"]`
- v4: `categories.read_only: ["ls"]` (commands array moved up one level)

### Rollback Strategy

If issues discovered post-deployment:
- Rollback requires `git revert` of bash-parser integration PR
- No dual-mode fallback available
- Users must manually revert scope.yml changes

### No Automatic Migration

Users must manually update scope.yml files. Migration guide available at:
`openspec/changes/bash-parser-integration/migration-guide.md`

## Integration Points

### With Preset System
- Scope configuration extracted during preset registration
- Stored in `jf/gptel-preset--scope-defaults` alist
- Referenced by scope profiles during session creation

### With Session Persistence
- scope.yml written during session creation
- Loaded from session's branch directory on every tool call
- No caching - filesystem is source of truth

### With Scope Expansion
- Validation failures trigger scope expansion UI
- request_scope_expansion meta-tool requests permissions
- Allow-once list managed by expansion UI

### With gptel Package
- Wraps tools registered via gptel-make-tool
- Uses buffer-local variables for session context
- Integrates with gptel-post-response-functions hook

## Summary

The Scope System v4 provides fine-grained tool permissions through:
- **Seven-stage bash validation pipeline** (parse → semantics → completeness → pipelines → categories → file paths → cloud auth)
- **Semantic command analysis** (bash-parser integration for AST extraction and operation detection)
- **Operation-specific path validation** (read, write, execute, modify with glob patterns)
- **Pipeline command validation** (validates ALL commands in pipes and chains, closes v3 security bypass)
- **Cloud authentication detection** (AWS, GCP, Azure with allow/warn/deny policy)
- **Security settings** (parse completeness enforcement, coverage thresholds)
- **Path-based validation** (glob patterns with deny precedence)
- **Pattern-based validation** (org-roam subdirectory/tags/node-ids)
- **Allow-once temporary permissions** (buffer-local, consumed before execution)
- **No caching** (filesystem is source of truth, scope.yml read every call)
- **Structured errors** (help LLMs understand denials and request expansions)

**v4 vs v3:**
- v3: Regex-based command parsing, directory-only validation, pipeline bypass
- v4: Bash-parser semantic analysis, file path extraction, full pipeline validation
- Breaking: v3 schemas NOT compatible, manual migration required
