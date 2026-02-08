## ADDED Requirements

### Requirement: request_scope_expansion meta-tool
The scope system SHALL provide a request_scope_expansion tool that LLMs can invoke to request permission for denied operations.

#### Scenario: LLM requests expansion after denial
- **WHEN** a tool returns a scope violation error
- **THEN** the LLM can invoke request_scope_expansion with tool name, resource, and reason

#### Scenario: request_scope_expansion triggers UI
- **WHEN** request_scope_expansion is invoked
- **THEN** the system displays the transient expansion menu to the user

#### Scenario: Meta-tool bypasses scope checks
- **WHEN** request_scope_expansion is categorized
- **THEN** it has validation strategy "meta" and always passes scope validation

#### Scenario: Expansion request includes context
- **WHEN** LLM calls request_scope_expansion
- **THEN** it provides tool name, resource identifier, and reason for the request

### Requirement: Transient menu three-choice UI
The expansion UI SHALL present exactly three choices via a transient menu: Deny, Add to scope, and Allow once.

#### Scenario: Menu shows violation details
- **WHEN** the expansion UI is displayed
- **THEN** it shows tool name, resource identifier, and denial reason in the description

#### Scenario: Deny choice rejects permanently
- **WHEN** user selects "Deny (reject tool call)"
- **THEN** the tool fails and returns :success nil, :user_denied true to the LLM

#### Scenario: Add to scope updates preset
- **WHEN** user selects "Add to scope (permanent)"
- **THEN** the system updates preset.md to include the resource in appropriate patterns

#### Scenario: Allow once grants temporary permission
- **WHEN** user selects "Allow once (temporary)"
- **THEN** the system adds the tool and resource to the allow-once list for the current turn

#### Scenario: Edit preset option available
- **WHEN** the expansion menu is displayed
- **THEN** a secondary option allows editing preset.md manually

#### Scenario: Menu quits without action
- **WHEN** user selects "Cancel" or quits the menu
- **THEN** the tool call is treated as denied

### Requirement: Add to scope updates preset file
When user approves permanent addition, the system SHALL update the preset.md YAML frontmatter to include the new pattern.

#### Scenario: Path added to appropriate section
- **WHEN** adding a path-based resource to scope
- **THEN** the system determines read vs write based on tool operation and adds to correct paths subsection

#### Scenario: Pattern added to org-roam section
- **WHEN** adding an org-roam resource to scope
- **THEN** the system parses the resource format (subdirectory:X or tags:Y) and adds to correct org_roam_patterns subsection

#### Scenario: Command added to allowlist
- **WHEN** adding a shell command to scope
- **THEN** the system extracts the base command name and adds to shell_commands.allow array

#### Scenario: Duplicate patterns avoided
- **WHEN** adding a pattern that already exists in the preset
- **THEN** the system does not duplicate the entry

#### Scenario: Directory paths normalized
- **WHEN** adding a directory path to scope
- **THEN** the system appends /** suffix if the path ends with /

#### Scenario: YAML structure preserved
- **WHEN** updating preset.md frontmatter
- **THEN** the system preserves existing YAML structure and only modifies the relevant section

### Requirement: Allow-once list management
The expansion UI SHALL manage temporary permissions via the allow-once list with automatic cleanup.

#### Scenario: Allow-once adds to buffer-local list
- **WHEN** user selects "Allow once"
- **THEN** the system adds (tool-name . resource) pair to jf/gptel-scope--allow-once-list

#### Scenario: Allow-once permission single-use
- **WHEN** a tool succeeds via allow-once
- **THEN** the permission is immediately removed from the list

#### Scenario: Allow-once cleared after response
- **WHEN** LLM response completes
- **THEN** the gptel-post-response-functions hook clears jf/gptel-scope--allow-once-list

#### Scenario: Multiple allow-once in same turn
- **WHEN** LLM requests multiple expansions in one turn
- **THEN** the allow-once list can contain multiple tool-resource pairs

#### Scenario: Allow-once survives tool retries
- **WHEN** a tool is granted allow-once but hasn't executed yet
- **THEN** the permission remains until the tool consumes it or the turn ends

### Requirement: Expansion callback integration
The expansion UI SHALL integrate with gptel's async callback system to communicate results back to the LLM.

#### Scenario: Deny invokes callback with failure
- **WHEN** user denies the expansion request
- **THEN** the callback receives JSON with :success nil and :user_denied true

#### Scenario: Add to scope invokes callback with success
- **WHEN** user approves permanent addition
- **THEN** the callback receives JSON with :success t and :patterns_added array

#### Scenario: Allow once invokes callback with success
- **WHEN** user approves temporary permission
- **THEN** the callback receives JSON with :success t and :allowed_once true

#### Scenario: Callback quits transient menu
- **WHEN** any choice is selected
- **THEN** the transient menu closes via transient-quit-one

### Requirement: Transient scope data passing
The expansion UI SHALL use transient's scope mechanism to pass violation info and callbacks without buffer-local variables.

#### Scenario: Violation data in transient scope
- **WHEN** expansion menu is invoked
- **THEN** violation details (:tool, :resource, :reason, :validation-type) are stored in transient scope

#### Scenario: Callback stored in scope
- **WHEN** expansion menu is invoked
- **THEN** the gptel async callback function is stored in transient scope

#### Scenario: Context directory in scope
- **WHEN** expansion menu needs to update preset
- **THEN** the branch directory path is available in transient scope or via buffer-local variable

#### Scenario: Suffix commands access scope
- **WHEN** user selects a menu option
- **THEN** the suffix command retrieves violation and callback from (transient-scope)

### Requirement: Validation type routing for updates
When updating preset, the system SHALL route to appropriate updater functions based on validation type.

#### Scenario: Path validation routes to path updater
- **WHEN** adding a path-based resource (validation-type: path)
- **THEN** the system calls jf/gptel-scope--add-path-to-preset

#### Scenario: Pattern validation routes to pattern updater
- **WHEN** adding an org-roam resource (validation-type: pattern)
- **THEN** the system calls jf/gptel-scope--add-pattern-to-preset

#### Scenario: Command validation routes to command updater
- **WHEN** adding a shell command (validation-type: command)
- **THEN** the system calls jf/gptel-scope--add-command-to-preset

#### Scenario: Unknown validation type errors
- **WHEN** validation type is not recognized
- **THEN** the system signals an error rather than updating incorrectly

### Requirement: Preset file validation before update
The expansion UI SHALL validate that preset.md exists and is writable before attempting updates.

#### Scenario: Missing preset errors cleanly
- **WHEN** attempting to add to scope but preset.md does not exist
- **THEN** the system signals a user-error with explanatory message

#### Scenario: Context directory resolution
- **WHEN** locating preset.md for update
- **THEN** the system tries transient scope context-dir, then jf/gptel--branch-dir, then buffer-file-name directory

#### Scenario: YAML parsing errors handled
- **WHEN** preset.md frontmatter has invalid YAML
- **THEN** the system reports the parsing error rather than corrupting the file

### Requirement: Manual preset editing option
The expansion UI SHALL provide an option to open preset.md for manual editing instead of automatic updates.

#### Scenario: Edit preset opens file
- **WHEN** user selects "Edit preset manually"
- **THEN** the system opens preset.md in an Emacs buffer

#### Scenario: Edit preserves transient state
- **WHEN** editing preset manually
- **THEN** the transient menu closes but the violation context is available if needed

#### Scenario: Edit does not invoke callback
- **WHEN** user chooses manual editing
- **THEN** the expansion callback is not invoked (user will handle the request manually)

### Requirement: Structured error information for LLM
Expansion denials SHALL return structured information that helps the LLM understand what happened and adjust its behavior.

#### Scenario: Denial includes user_denied flag
- **WHEN** user denies an expansion request
- **THEN** the response includes :user_denied true to distinguish from system errors

#### Scenario: Success includes patterns_added
- **WHEN** user approves permanent addition
- **THEN** the response includes :patterns_added array showing what was added to the preset

#### Scenario: Success includes allowed_once flag
- **WHEN** user approves temporary permission
- **THEN** the response includes :allowed_once true indicating the permission is temporary

#### Scenario: Message explains the outcome
- **WHEN** any expansion choice is made
- **THEN** the response includes :message string explaining what happened

### Requirement: Resource format preservation
The expansion UI SHALL preserve resource format conventions when adding to presets to ensure validation compatibility.

#### Scenario: Filepath expanded to absolute
- **WHEN** adding a file path to preset
- **THEN** the system uses expand-file-name to convert relative paths to absolute

#### Scenario: Org-roam format parsed correctly
- **WHEN** adding org-roam patterns from "subdirectory:gptel/**" format
- **THEN** the system extracts "gptel/**" and adds to org_roam_patterns.subdirectory

#### Scenario: Command base name extracted
- **WHEN** adding shell command "git status -s" to allowlist
- **THEN** the system extracts "git" as the base command name

#### Scenario: Resource format matches validation
- **WHEN** allow-once resource is converted to permanent pattern
- **THEN** the added pattern matches the format that validation expects
