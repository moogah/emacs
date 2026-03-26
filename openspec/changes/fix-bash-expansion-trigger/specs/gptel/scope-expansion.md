## MODIFIED Requirements

### Requirement: Validation type routing for updates

When updating scope, system SHALL route to appropriate updater functions based on validation type. For bash validation types, the system SHALL use v4 path-based expansion (not v3 category-based).

**Implementation**: `config/gptel/scope/scope-expansion.org` - routing via pcase

#### Scenario: Path validation routes to path updater
- **WHEN** adding path-based resource (validation-type: path)
- **THEN** system calls `jf/gptel-scope--add-path-to-scope`

#### Scenario: Pattern validation routes to pattern updater
- **WHEN** adding org-roam resource (validation-type: pattern)
- **THEN** system calls `jf/gptel-scope--add-pattern-to-scope`

#### Scenario: Bash validation routes to path-based expansion
- **WHEN** adding bash resource (validation-type: bash)
- **AND** the resource is a file path (from path_out_of_scope error)
- **THEN** system delegates to `jf/gptel-scope--add-path-to-scope`
- **AND** adds the path pattern to the appropriate paths section based on operation type

#### Scenario: Bash path expansion uses operation type
- **WHEN** bash expansion resource is a file path
- **AND** the operation type is `:read`
- **THEN** system adds pattern to `paths.read`
- **WHEN** the operation type is `:write`
- **THEN** system adds pattern to `paths.write`

#### Scenario: Unknown validation type errors
- **WHEN** validation type not recognized
- **THEN** system signals error rather than updating incorrectly

### Requirement: Bash command addition to scope.yml

The scope expansion system SHALL write approved bash resources to scope.yml using v4 path-based model. Category-based expansion (v3) is no longer supported.

#### Scenario: Path-based bash resource added to scope
- **WHEN** user approves a bash tool denial where the resource is a file path
- **THEN** system adds the path pattern to the appropriate `paths` section
- **AND** determines read vs write based on operation type from violation-info

#### Scenario: Deny list command not addable via expansion
- **WHEN** bash tool denial is due to deny list (command_denied error)
- **THEN** expansion UI shows the denial but "Add to scope" is not meaningful
- **AND** user can only choose "Allow once" or "Deny"

#### Scenario: Cloud auth denial expansion
- **WHEN** bash tool denial is due to cloud auth policy (cloud_auth_denied error)
- **THEN** expansion UI shows the cloud provider and policy
- **AND** "Add to scope" updates cloud.allowed_providers in scope.yml

## REMOVED Requirements

### Requirement: Category inference from tool arguments

**Reason**: v4 scope system removed `bash_tools.categories`. Commands are validated by operation-first model (file operations + path scopes), not category membership. Expansion adds paths, not categories.

**Migration**: Bash expansion now uses path-based addition. When a bash command is denied because a file path is out of scope, expansion adds the path to the appropriate `paths` section.

### Requirement: YAML serialization of bash_tools structure

**Reason**: The nested `bash_tools.categories` structure no longer exists in v4. `bash_tools` contains only `deny` list. Expansion for bash tools now modifies the `paths` section, not `bash_tools`.

**Migration**: No action needed. Expansion code should target `paths` section for bash denials.

### Requirement: Bash command addition to scope.yml (v3 categories)

**Reason**: Replaced by path-based expansion above. v3 added commands to `bash_tools.categories.read_only.commands` or `bash_tools.categories.safe_write.commands`. v4 adds file paths to `paths.read` or `paths.write`.

**Migration**: Expansion code should use `jf/gptel-scope--add-path-to-scope` for bash resource paths.
