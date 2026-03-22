## MODIFIED Requirements

### Requirement: Error message formatting

The scope system SHALL return structured error messages using a unified format across all validator types. All validators SHALL use `:error` for machine-readable error codes and `:message` for human-readable text. The `:reason` field SHALL NOT appear in validator return plists.

#### Scenario: Path denial includes allowed patterns
- **WHEN** path validation fails
- **THEN** error includes `:allowed_patterns` list from `paths.read` or `paths.write`

#### Scenario: Path out of scope error structure (v4)
- **WHEN** file path fails validation
- **THEN** error includes `:error "path_out_of_scope"`, `:path`, `:operation`, `:required_scope`, `:allowed_patterns`, `:message`

#### Scenario: Pipeline command denied error structure (v4)
- **WHEN** pipeline command fails validation
- **THEN** error includes `:error "command_denied"`, `:command`, `:pipeline_position`, `:full_command`, `:message`

#### Scenario: Cloud auth denied error structure (v4)
- **WHEN** cloud auth command denied
- **THEN** error includes `:error "cloud_auth_denied"`, `:provider`, `:command`, `:allowed_providers`, `:message`

#### Scenario: Pattern denial includes allowed patterns
- **WHEN** org-roam validation fails
- **THEN** error includes `:allowed_patterns` from `org_roam_patterns` section

#### Scenario: Deny pattern error distinguishes from allow failure
- **WHEN** resource explicitly denied
- **THEN** error uses `:denied_patterns` list (not allowed_patterns)

#### Scenario: All validators use :error and :message fields
- **WHEN** any validator (path, pattern, bash) denies an operation
- **THEN** the return plist contains `:error` with a machine-readable code (e.g., "path_out_of_scope", "denied-pattern", "command-not-allowed")
- **AND** the return plist contains `:message` with human-readable text explaining the denial
- **AND** the return plist does NOT contain a `:reason` field

### Requirement: Path-based validation

The scope system SHALL validate path-based tools against read/write/deny path lists using glob pattern matching. Path validation denial plists SHALL use `:error` and `:message` fields.

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
- **THEN** system denies access with `:error "not-in-scope"` and `:message` explaining which patterns were checked

#### Scenario: Path denied by deny pattern
- **WHEN** path matches a deny pattern
- **THEN** system denies access with `:error "denied-pattern"` and `:message` explaining which deny pattern matched

#### Scenario: Glob patterns support wildcards
- **WHEN** patterns use `**` (any including /), `*` (any except /), or `?` (single char)
- **THEN** system correctly matches paths using converted regex patterns

#### Scenario: Symlinks resolved before matching
- **WHEN** validating path that is or contains symlinks
- **THEN** system resolves symlinks to real paths
- **AND** checks BOTH real-path AND absolute-path for pattern matches
- **AND** allows if either matches (patterns may match either form)

#### Scenario: Validation receives metadata parameter
- **WHEN** path validation is performed
- **THEN** jf/gptel-scope--validate-path-tool is called with signature `(tool-name args category config metadata)`
- **AND** metadata contains file context (git status, existence, type)

#### Scenario: Metadata available during validation
- **WHEN** path validation logic executes
- **THEN** metadata plist is accessible
- **AND** future policies can use `:git-tracked`, `:git-repo`, `:exists`, `:type` fields

### Requirement: Pattern-based validation

The scope system SHALL validate org-roam tools against org_roam_patterns section. Pattern validation denial plists SHALL use `:error` and `:message` fields.

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
- **THEN** system denies access with `:error "not-in-org-roam-patterns"` and `:message` explaining the denial

### Requirement: Violation-info transformation

The scope system SHALL transform validator error plists into a unified violation-info format for the expansion UI. The transformation SHALL read `:error` and `:message` from all validator types without fallback chains.

#### Scenario: Violation-info produced from path validator denial
- **WHEN** path validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info produced from bash pipeline denial
- **WHEN** bash pipeline validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info produced from pattern validator denial
- **WHEN** pattern validator returns `:error` and `:message`
- **THEN** `build-violation-info` produces plist with `:tool`, `:resource`, `:reason` (from `:message`), `:validation-type`

#### Scenario: Violation-info reason is always human-readable
- **WHEN** `build-violation-info` extracts the `:reason` for the output plist
- **THEN** it reads from the input `:message` field (human-readable text)
- **AND** it does NOT use the `:error` field (machine code) as the reason

#### Scenario: Resource extracted from validator-specific fields
- **WHEN** `build-violation-info` extracts `:resource`
- **THEN** it reads from `:path` (for path_out_of_scope, path_denied), `:command` (for command_denied), or `:provider` (for cloud_auth_denied)

### Requirement: Macro-based tool wrapping

The scope system SHALL wrap scope-aware tools using macros that intercept arguments, validate against scope, and return structured errors on denial.

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
- **THEN** wrapper returns JSON with `:success nil`, `:error` (machine code), `:message` (human text), `:allowed_patterns`, `:denied_patterns`

#### Scenario: Meta tools skip validation wrapper
- **WHEN** tool categorized as meta
- **THEN** wrapper allows call without scope checks
