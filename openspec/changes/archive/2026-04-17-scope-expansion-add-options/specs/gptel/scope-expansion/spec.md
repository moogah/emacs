## MODIFIED Requirements

### Requirement: Transient menu three-choice UI

Expansion UI SHALL present five choices via transient menu: Deny, Add exact to scope, Add `/**` to scope, Add custom pattern to scope, Allow once.

#### Scenario: Menu shows violation details
- **WHEN** expansion UI displayed
- **THEN** shows tool name, resource identifier, denial reason

#### Scenario: Deny choice rejects permanently
- **WHEN** user selects "Deny (reject tool call)"
- **THEN** tool fails with `:success nil`, `:user_denied true`

#### Scenario: Add to scope updates scope.yml
- **WHEN** user selects "Add exact to scope (permanent)"
- **THEN** system updates scope.yml to include the denied resource pattern verbatim

#### Scenario: Allow once grants temporary permission
- **WHEN** user selects "Allow once (temporary)"
- **THEN** system adds (tool-name . resource) to buffer-local allow-once list

#### Scenario: Edit scope option available
- **WHEN** expansion menu displayed
- **THEN** secondary option allows editing scope.yml manually

#### Scenario: Menu quits without action
- **WHEN** user selects "Cancel" or quits menu
- **THEN** tool call treated as denied

## ADDED Requirements

### Requirement: Wildcard path expansion option

Expansion UI SHALL offer an option to add a `/**` wildcard scoped to the parent directory of the denied file. This option is only applicable when the denied resource is a file path (not a directory).

#### Scenario: Wildcard option only appears for file resources
- **WHEN** expansion UI displayed
- **AND** denied resource is a file path (not a directory)
- **THEN** "Add parent `/**` to scope" option is available in the menu

#### Scenario: Wildcard option not shown for directory resources
- **WHEN** expansion UI displayed
- **AND** denied resource is a directory path
- **THEN** "Add parent `/**` to scope" option is NOT available in the menu

#### Scenario: Wildcard menu label shows derived pattern
- **WHEN** denied resource is a file path (e.g., `~/foo/bar.txt`)
- **AND** expansion UI is displayed
- **THEN** wildcard option label shows the derived pattern explicitly (e.g., `"Add ~/foo/** to scope"`)

#### Scenario: Wildcard option derives parent directory from denied file
- **WHEN** denied resource is a file path (e.g., `~/foo/bar.txt`)
- **AND** user selects `"Add ~/foo/** to scope"`
- **THEN** system extracts the parent directory of the file (`~/foo`)
- **AND** writes pattern `~/foo/**` to scope.yml in the appropriate section
- **AND** invokes callback with `:success t`, `:patterns_added` containing `~/foo/**`

#### Scenario: Wildcard route follows existing validation-type routing
- **WHEN** user selects `"Add ~/foo/** to scope"`
- **THEN** routing uses validation-type from transient scope (same as exact add)
- **AND** path updater receives `<parent-dir>/**` instead of original resource

### Requirement: Custom pattern expansion option

Expansion UI SHALL offer an option to add a user-edited custom pattern to scope.yml, pre-seeded with the denied resource string.

#### Scenario: Custom option prompts with pre-populated value
- **WHEN** user selects "Add custom pattern to scope"
- **THEN** system prompts user with minibuffer input pre-populated with the denied resource string
- **AND** user may edit, accept, or cancel the prompt

#### Scenario: Accepted custom pattern written to scope.yml
- **WHEN** user edits the pre-populated string and confirms
- **THEN** system writes the edited string as the pattern to scope.yml
- **AND** routing uses validation-type from transient scope (same as exact add)
- **AND** invokes callback with `:success t`, `:patterns_added` containing the custom pattern

#### Scenario: Cancelled custom prompt denies
- **WHEN** user cancels the custom pattern prompt (C-g or empty input)
- **THEN** system treats the action as denied
- **AND** invokes callback with `:success nil`, `:user_denied true`

#### Scenario: Custom pattern not validated before writing
- **WHEN** user confirms a custom pattern
- **THEN** system writes the pattern as-is without structural validation
- **AND** user is responsible for correctness of the custom string
