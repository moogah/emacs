## MODIFIED Requirements

### Requirement: Configuration loading from scope document
The scope system SHALL load scope configuration from `scope.yml` located in the session's branch directory.

**Parser change:** `scope.yml` is plain YAML (no frontmatter delimiters). The parser SHALL read the entire file content and pass it to `yaml-parse-string` directly. This differs from the legacy `preset.md` parser which extracted YAML from between `---` delimiters before parsing. Both paths use `yaml-parse-string` with `:object-type 'plist`, but the input preparation differs.

**Key normalization:** After parsing, YAML snake_case keys SHALL be normalized to kebab-case keywords (e.g., `:org_roam_patterns` → `:org-roam-patterns`, `:shell_commands` → `:shell-commands`). This follows the same convention documented in `preset-registration/spec.md`.

#### Scenario: Configuration loaded from scope.yml
- **WHEN** a tool executes and needs scope validation
- **THEN** the system reads `scope.yml` from the buffer's branch directory
- **AND** parses the entire file as YAML (no frontmatter extraction needed)
- **AND** returns a plist with `:paths`, `:org-roam-patterns`, `:shell-commands`

#### Scenario: Missing scope.yml handled gracefully
- **WHEN** no `scope.yml` exists in the branch directory
- **THEN** the system checks for legacy `preset.md` as a fallback
- **AND** if neither exists, returns a "no_scope_config" error to the tool

#### Scenario: Legacy preset.md fallback
- **WHEN** `scope.yml` does not exist but `preset.md` does in the branch directory
- **THEN** the system reads scope configuration from `preset.md` using frontmatter extraction (between `---` delimiters)
- **AND** extracts only scope-relevant keys (`:paths`, `:org_roam_patterns`, `:shell_commands`)
- **AND** logs a deprecation warning

#### Scenario: Buffer context determines directory
- **WHEN** a tool executes in a gptel buffer
- **THEN** the system uses the buffer-local `jf/gptel--branch-dir` variable to locate `scope.yml`

### Requirement: Macro-based tool wrapping
The scope system SHALL provide a macro that wraps tool definitions to automatically handle validation without boilerplate.

#### Scenario: Macro loads config automatically
- **WHEN** a scoped tool is invoked via the macro
- **THEN** the macro loads scope configuration from `scope.yml` without tool code needing to do so

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
