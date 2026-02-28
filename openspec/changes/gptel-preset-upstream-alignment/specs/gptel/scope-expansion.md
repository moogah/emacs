## MODIFIED Requirements

### Requirement: Add to scope updates scope document
When user approves permanent addition, the system SHALL update `scope.yml` in the session's branch directory to include the new pattern.

#### Scenario: Path added to appropriate section
- **WHEN** adding a path-based resource to scope
- **THEN** the system determines read vs write based on tool operation and adds to correct paths subsection in `scope.yml`

#### Scenario: Pattern added to org-roam section
- **WHEN** adding an org-roam resource to scope
- **THEN** the system parses the resource format (subdirectory:X or tags:Y) and adds to correct org_roam_patterns subsection in `scope.yml`

#### Scenario: Command added to allowlist
- **WHEN** adding a shell command to scope
- **THEN** the system extracts the base command name and adds to shell_commands.allow array in `scope.yml`

#### Scenario: Duplicate patterns avoided
- **WHEN** adding a pattern that already exists in scope.yml
- **THEN** the system does not duplicate the entry

#### Scenario: YAML structure preserved
- **WHEN** updating scope.yml
- **THEN** the system preserves existing YAML structure and only modifies the relevant section

#### Scenario: Simpler serialization than preset.md
- **WHEN** writing to scope.yml
- **THEN** the system writes plain YAML (no frontmatter delimiters to parse around)
- **AND** serialization is simpler than the previous preset.md approach

### Requirement: Transient menu three-choice UI
The expansion UI SHALL present exactly three choices via a transient menu: Deny, Add to scope, and Allow once.

#### Scenario: Add to scope updates scope.yml
- **WHEN** user selects "Add to scope (permanent)"
- **THEN** the system updates `scope.yml` to include the resource in appropriate patterns

#### Scenario: Edit scope option available
- **WHEN** the expansion menu is displayed
- **THEN** a secondary option allows editing `scope.yml` manually

### Requirement: Scope file validation before update
The expansion UI SHALL validate that `scope.yml` exists and is writable before attempting updates.

#### Scenario: Missing scope.yml errors cleanly
- **WHEN** attempting to add to scope but `scope.yml` does not exist
- **THEN** the system signals a user-error with explanatory message

#### Scenario: Context directory resolution
- **WHEN** locating `scope.yml` for update
- **THEN** the system tries transient scope context-dir, then `jf/gptel--branch-dir`, then buffer-file-name directory

#### Scenario: YAML parsing errors handled
- **WHEN** `scope.yml` has invalid YAML
- **THEN** the system reports the parsing error rather than corrupting the file

### Requirement: Manual scope editing option
The expansion UI SHALL provide an option to open `scope.yml` for manual editing instead of automatic updates.

#### Scenario: Edit scope opens file
- **WHEN** user selects "Edit scope manually"
- **THEN** the system opens `scope.yml` in an Emacs buffer

#### Scenario: Edit does not invoke callback
- **WHEN** user chooses manual editing
- **THEN** the expansion callback is not invoked (user will handle the request manually)

### Requirement: Validation type routing for updates
When updating scope, the system SHALL route to appropriate updater functions based on validation type.

#### Scenario: Path validation routes to path updater
- **WHEN** adding a path-based resource (validation-type: path)
- **THEN** the system calls the path updater function targeting `scope.yml`

#### Scenario: Pattern validation routes to pattern updater
- **WHEN** adding an org-roam resource (validation-type: pattern)
- **THEN** the system calls the pattern updater function targeting `scope.yml`

#### Scenario: Command validation routes to command updater
- **WHEN** adding a shell command (validation-type: command)
- **THEN** the system calls the command updater function targeting `scope.yml`
