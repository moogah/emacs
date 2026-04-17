## MODIFIED Requirements

### Requirement: request_scope_expansion tool

The system SHALL expose a regular gptel tool that lets the LLM pre-emptively request user approval before invoking a scoped tool it expects will be denied. The violation-info plist passed to `jf/gptel-scope-prompt-expansion` SHALL carry `:validation-type` derived from the requested tool, not a hardcoded value.

**Implementation**: `config/gptel/scope/scope-shell-tools.org` — §request_scope_expansion Tool

#### Scenario: Tool is registered under the scope category

- **WHEN** `scope-shell-tools` loads
- **THEN** it defines `request_scope_expansion` via `gptel-make-tool` with `:async t` and `:category "scope"`
- **AND** the tool is a regular gptel tool, not routed through any meta validation strategy

#### Scenario: Validation-type resolves from the requested tool

- **WHEN** the LLM invokes `request_scope_expansion` with `tool_name "read_file_in_scope"` (a filesystem tool)
- **THEN** the violation-info carries `:validation-type 'path`
- **AND** the add-to-scope handlers target the `paths.*` section (path router)

#### Scenario: Validation-type resolves to bash for bash-backed tools

- **WHEN** the LLM invokes `request_scope_expansion` with `tool_name "run_bash_command"`
- **THEN** the violation-info carries `:validation-type 'bash`
- **AND** the add-to-scope handlers dispatch through `jf/gptel-scope--add-bash-to-scope` (bash router)

#### Scenario: Unknown tool names are rejected

- **WHEN** the LLM invokes `request_scope_expansion` with a `tool_name` that is neither a registered filesystem tool nor a registered bash tool
- **THEN** the tool returns `:success nil`, a structured error that names the unknown tool, and suggests valid tool names
- **AND** no transient menu is shown

#### Scenario: Approved pre-emptive request

- **WHEN** the user chooses Add to Scope (or a wildcard/custom variant)
- **THEN** `scope.yml` is updated and the LLM's response contains `:success t :patterns_added [...]`
- **AND** the LLM may then invoke the originally intended tool, which will pass validation against the updated `scope.yml`

### Requirement: Section-targeted writes

Add-to-scope variants SHALL use the denied operation to target the correct `paths.*` subsection of `scope.yml` (not the tool category, not the command name). Every code path SHALL invoke the expansion callback exactly once — including refusal paths that do not mutate `scope.yml`.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Path to Scope, §Add Bash to Scope; `config/gptel/scope/scope-validation.org` — §Operation → Scope Section Mapping

#### Scenario: Operation keyword maps to section

- **WHEN** the violation carries `:operation :read`, `:write`, `:modify`, or `:execute`
- **THEN** `jf/gptel-scope--map-operation-to-scope-section` returns `:read`, `:write`, or `:execute` and the pattern is written under `paths.<section>`
- **AND** read-like granular operations (`:read-directory`, `:read-metadata`, `:match-pattern`) collapse to `:read`; write-like granular operations (`:create`, `:create-or-modify`, `:append`, `:delete`, `:modify`) collapse to `:write`

#### Scenario: Bash file-op denials route to path sections (no command-name expansion)

- **WHEN** a bash validation denies a file operation on an absolute path, tilde path, or glob pattern
- **THEN** `jf/gptel-scope--add-bash-to-scope` delegates to `jf/gptel-scope--add-path-to-scope` with the denied operation
- **AND** the denied pattern is written and the callback is funcalled with `:success t :patterns_added [...]`

#### Scenario: Bare command name refusal invokes the callback

- **WHEN** the resource is a bare command name (no `/`, no `~`, no glob characters)
- **AND** the user has chosen an Add-to-Scope variant
- **THEN** `jf/gptel-scope--add-bash-to-scope` does NOT write to `scope.yml`
- **AND** the callback is funcalled with `:success nil`, a machine-readable error indicating "command-name expansion is not supported", and a human-readable message suggesting the LLM request expansion for the underlying file operation's path instead
- **AND** the pending tool invocation resolves cleanly — it never hangs waiting for a callback

#### Scenario: Missing operation falls back safely

- **WHEN** no `:operation` is present on the violation
- **THEN** `jf/gptel-scope--add-path-to-scope` defaults the target section to `:read` (the safest choice for filesystem tools whose category the caller did not pass through)
