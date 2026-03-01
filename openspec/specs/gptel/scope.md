# Scope System - Core Behavioral Spec

## Purpose

Provides fine-grained permission control for gptel tools, enforcing access policies through scope configuration. This spec describes the core validation engine, configuration loading, permission checking, and tool wrapping mechanisms.

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

#### Scenario: Bash tool categorized
- **WHEN** tool executes bash commands
- **THEN** system categorizes with validation strategy "bash"

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

### Requirement: Bash tool validation (category-based)

The scope system SHALL validate bash commands using category-based validation with three categories: read_only, safe_write, dangerous.

**Implementation**: `config/gptel/scope/scope-core.org` - bash validator (lines 860-1020)

**Configuration format** (scope.yml):
```yaml
bash_tools:
  categories:
    read_only:
      commands: ["ls", "cat", "grep", "git log"]
    safe_write:
      commands: ["mkdir", "touch", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "sudo"
    - "chmod"
```

**Category semantics**:

| Category | Path Requirement | Meaning |
|----------|------------------|---------|
| `read_only` | `paths.read` (or write) | Read-only commands; write scope includes read |
| `safe_write` | `paths.write` | Non-destructive creation |
| `dangerous` | Both read AND write + user approval | Requires explicit confirmation |
| `deny` | Never allowed | Blocked even with scope expansion |

#### Scenario: Deny list checked first
- **WHEN** command matches deny list
- **THEN** system denies command (deny takes precedence)
- **AND** returns custom `:message` field in error

#### Scenario: Read-only command with read scope
- **WHEN** command in `bash_tools.categories.read_only.commands`
- **AND** buffer has `paths.read` scope
- **THEN** system allows command

#### Scenario: Safe-write command with write scope
- **WHEN** command in `bash_tools.categories.safe_write.commands`
- **AND** buffer has `paths.write` scope
- **THEN** system allows command

#### Scenario: Dangerous command requires both scopes
- **WHEN** command in `bash_tools.categories.dangerous.commands`
- **THEN** system requires both `paths.read` AND `paths.write` scope
- **AND** requires explicit user approval

#### Scenario: Command not in categories
- **WHEN** command not in any category
- **THEN** system denies with "not-in-bash-categories" reason

**Note**: Bash validator includes custom `:message` fields in errors for user-friendly feedback (not present in other validators).

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

### Requirement: Error message formatting

The scope system SHALL return structured error messages helping LLMs understand denials and request appropriate expansions.

#### Scenario: Path denial includes allowed patterns
- **WHEN** path validation fails
- **THEN** error includes `:allowed_patterns` list from `paths.read` or `paths.write`

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

The Scope System provides fine-grained tool permissions through:
- **Category-based bash validation** (read_only, safe_write, dangerous)
- **Path-based validation** (glob patterns with deny precedence)
- **Pattern-based validation** (org-roam subdirectory/tags/node-ids)
- **Allow-once temporary permissions** (buffer-local, consumed before execution)
- **No caching** (filesystem is source of truth, scope.yml read every call)
- **Structured errors** (help LLMs understand denials and request expansions)
