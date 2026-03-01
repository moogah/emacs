# Scope Expansion - Permission Request UI

## Purpose

Defines the scope expansion workflow where LLMs request permission to access denied resources through the request_scope_expansion meta-tool. Users respond via transient menu offering temporary (allow-once) or permanent (add to scope) permission grants.

## Requirements

### Requirement: request_scope_expansion meta-tool

The scope system SHALL provide request_scope_expansion tool that LLMs invoke to request permission for denied operations.

**Implementation**: `config/gptel/tools/scope-shell-tools.org`

#### Scenario: LLM requests expansion after denial
- **WHEN** tool returns scope violation error
- **THEN** LLM can invoke request_scope_expansion with tool_name, patterns, and justification

#### Scenario: request_scope_expansion triggers UI
- **WHEN** request_scope_expansion invoked
- **THEN** system displays transient expansion menu

#### Scenario: Meta-tool bypasses scope checks
- **WHEN** request_scope_expansion categorized
- **THEN** has validation strategy "meta" and always passes scope validation

#### Scenario: Expansion request context
- **WHEN** LLM calls request_scope_expansion
- **THEN** provides tool_name, patterns (array), and justification
- **AND** system adds validation-type automatically (not provided by LLM)

**Note**: `validation-type` is inferred from tool_name by `jf/gptel-scope--infer-validation-type`, not provided in LLM request.

### Requirement: Transient menu three-choice UI

Expansion UI SHALL present three choices via transient menu: Deny, Add to scope, Allow once.

#### Scenario: Menu shows violation details
- **WHEN** expansion UI displayed
- **THEN** shows tool name, resource identifier, denial reason

#### Scenario: Deny choice rejects permanently
- **WHEN** user selects "Deny (reject tool call)"
- **THEN** tool fails with `:success nil`, `:user_denied true`

#### Scenario: Add to scope updates scope.yml
- **WHEN** user selects "Add to scope (permanent)"
- **THEN** system updates scope.yml to include resource in appropriate patterns

#### Scenario: Allow once grants temporary permission
- **WHEN** user selects "Allow once (temporary)"
- **THEN** system adds (tool-name . resource) to buffer-local allow-once list

#### Scenario: Edit scope option available
- **WHEN** expansion menu displayed
- **THEN** secondary option allows editing scope.yml manually

#### Scenario: Menu quits without action
- **WHEN** user selects "Cancel" or quits menu
- **THEN** tool call treated as denied

### Requirement: Add to scope updates scope.yml

When user approves permanent addition, system SHALL update scope.yml file to include new pattern.

**Implementation**: `config/gptel/scope/scope-expansion.org`

#### Scenario: Path added to appropriate section
- **WHEN** adding path-based resource to scope
- **THEN** system determines read vs write based on tool operation
- **AND** adds to correct paths subsection

#### Scenario: Pattern added to org-roam section
- **WHEN** adding org-roam resource to scope
- **THEN** system parses resource format (subdirectory:X or tags:Y)
- **AND** adds to correct org_roam_patterns subsection

#### Scenario: Bash command added to bash_tools
- **WHEN** adding bash command to scope
- **THEN** system extracts command name
- **AND** adds to appropriate bash_tools category

#### Scenario: Duplicate patterns avoided
- **WHEN** adding pattern that already exists
- **THEN** system does not duplicate entry

#### Scenario: Directory paths normalized
- **WHEN** adding directory path
- **THEN** system appends `/**` suffix if path ends with `/`

#### Scenario: YAML structure preserved
- **WHEN** updating scope.yml
- **THEN** system preserves existing YAML structure, modifies only relevant section

### Requirement: Allow-once list management

Expansion UI SHALL manage temporary permissions via buffer-local allow-once list with automatic cleanup.

**Implementation**: `config/gptel/scope/scope-core.org` - `jf/gptel-scope--allow-once-list` (buffer-local)

#### Scenario: Allow-once adds to buffer-local list
- **WHEN** user selects "Allow once"
- **THEN** system adds `(tool-name . resource)` pair to `jf/gptel-scope--allow-once-list` (buffer-local variable)

#### Scenario: Allow-once permission consumed
- **WHEN** tool succeeds via allow-once
- **THEN** permission immediately removed from list

#### Scenario: Allow-once cleared after response
- **WHEN** LLM response completes
- **THEN** `gptel-post-response-functions` hook clears allow-once list

#### Scenario: Multiple allow-once in same turn
- **WHEN** LLM requests multiple expansions in one turn
- **THEN** allow-once list can contain multiple tool-resource pairs

#### Scenario: Allow-once survives tool retries
- **WHEN** tool granted allow-once but hasn't executed yet
- **THEN** permission remains until tool consumes it or turn ends

**Note**: Allow-once list is **buffer-local**, cleared via `gptel-post-response-functions` hook after each response.

### Requirement: Expansion callback integration

Expansion UI SHALL integrate with gptel's async callback system to communicate results to LLM.

#### Scenario: Deny invokes callback with failure
- **WHEN** user denies expansion request
- **THEN** callback receives JSON with `:success nil`, `:user_denied true`

#### Scenario: Add to scope invokes callback with success
- **WHEN** user approves permanent addition
- **THEN** callback receives JSON with `:success t`, `:patterns_added` array

#### Scenario: Allow once invokes callback with success
- **WHEN** user approves temporary permission
- **THEN** callback receives JSON with `:success t`, `:allowed_once true`

#### Scenario: Callback quits transient menu
- **WHEN** any choice selected
- **THEN** transient menu closes via `transient-quit-one`

### Requirement: Transient scope data passing

Expansion UI SHALL use transient's scope mechanism to pass violation info and callbacks without buffer-local variables.

**Implementation**: `config/gptel/scope/scope-expansion.org` - `jf/gptel-scope-prompt-expansion`

#### Scenario: Violation data in transient scope
- **WHEN** expansion menu invoked
- **THEN** violation details (`:tool`, `:resource`, `:reason`, `:validation-type`) stored in transient scope

#### Scenario: Callback stored in scope
- **WHEN** expansion menu invoked
- **THEN** gptel async callback function stored in transient scope

#### Scenario: Patterns list stored in scope
- **WHEN** expansion menu invoked
- **THEN** patterns list from request_scope_expansion stored in transient scope
- **AND** used by updater functions to determine what to add

#### Scenario: Suffix commands access scope
- **WHEN** user selects menu option
- **THEN** suffix command retrieves violation and callback from `(transient-scope)`

**Note**: Context directory is NOT passed in transient scope - it's resolved on-demand via fallback chain.

### Requirement: Validation type routing for updates

When updating scope, system SHALL route to appropriate updater functions based on validation type.

**Implementation**: `config/gptel/scope/scope-expansion.org` - routing via pcase

#### Scenario: Path validation routes to path updater
- **WHEN** adding path-based resource (validation-type: path)
- **THEN** system calls `jf/gptel-scope--add-path-to-scope`

#### Scenario: Pattern validation routes to pattern updater
- **WHEN** adding org-roam resource (validation-type: pattern)
- **THEN** system calls `jf/gptel-scope--add-pattern-to-scope`

#### Scenario: Bash validation routes to bash updater
- **WHEN** adding bash command (validation-type: bash)
- **THEN** system calls `jf/gptel-scope--add-bash-to-scope`

#### Scenario: Unknown validation type errors
- **WHEN** validation type not recognized
- **THEN** system signals error rather than updating incorrectly

**Note**: Function names use `-scope` suffix (not `-preset`). Bash validation type is `bash` (not "command").

### Requirement: Scope file validation before update

Expansion UI SHALL validate scope.yml exists and is writable before attempting updates.

#### Scenario: Missing scope file errors cleanly
- **WHEN** attempting to add to scope but scope.yml doesn't exist
- **THEN** system signals user-error with explanatory message

#### Scenario: Context directory resolution
- **WHEN** locating scope.yml for update
- **THEN** system tries:
  1. `jf/gptel--branch-dir` (buffer-local variable)
  2. `buffer-file-name` directory
- **AND** does NOT use transient scope context-dir (doesn't exist)

**Implementation**: `config/gptel/scope/scope-expansion.org` - `jf/gptel-scope--get-scope-file-path`

#### Scenario: YAML parsing errors handled
- **WHEN** scope.yml contains invalid YAML
- **THEN** system reports parsing error rather than corrupting file

### Requirement: Manual scope editing option

Expansion UI SHALL provide option to open scope.yml for manual editing instead of automatic updates.

#### Scenario: Edit scope opens file
- **WHEN** user selects "Edit scope manually"
- **THEN** system opens scope.yml in Emacs buffer

#### Scenario: Edit preserves transient state
- **WHEN** editing scope manually
- **THEN** transient menu closes but violation context available if needed

#### Scenario: Edit does not invoke callback
- **WHEN** user chooses manual editing
- **THEN** expansion callback not invoked (user handles request manually)

### Requirement: Structured error information for LLM

Expansion denials SHALL return structured information helping LLM understand what happened.

#### Scenario: Denial includes user_denied flag
- **WHEN** user denies expansion request
- **THEN** response includes `:user_denied true` to distinguish from system errors

#### Scenario: Success includes patterns_added
- **WHEN** user approves permanent addition
- **THEN** response includes `:patterns_added` array with patterns added to scope.yml

#### Scenario: Allow-once success includes flag
- **WHEN** user approves temporary permission
- **THEN** response includes `:allowed_once true`

## Integration Points

### With Scope Core
- Uses `jf/gptel-scope--allow-once-list` (buffer-local)
- Validation-type inferred via `jf/gptel-scope--infer-validation-type`
- Scope file located via `jf/gptel--branch-dir`

### With gptel Callback System
- Expansion menu receives async callback from gptel
- Invokes callback with JSON response on user choice
- Transient menu integrates with gptel's async flow

### With scope.yml Updates
- Updater functions modify scope.yml directly
- YAML structure preserved during updates
- Deduplication prevents redundant patterns

## Summary

Scope expansion provides user-friendly permission management via transient UI. LLMs request access through request_scope_expansion meta-tool, users respond with deny/add-to-scope/allow-once choices. System routes updates to appropriate functions based on validation type, maintains buffer-local allow-once list, and returns structured results to LLM.
