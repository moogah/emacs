# Scope Profiles - Delta Spec

## MODIFIED Requirements

### Requirement: Profile file format

Scope profiles SHALL be plain YAML files with no frontmatter delimiters, containing paths, org_roam_patterns, and bash_tools sections, where bash_tools includes only a deny list without categories.

#### Scenario: Plain YAML, no frontmatter
- **WHEN** loading a scope profile
- **THEN** the system parses as YAML only (no markdown frontmatter extraction)
- **AND** distinguishes profiles from preset files (which have frontmatter)

#### Scenario: Partial profiles are valid
- **WHEN** a profile defines only `paths` (no org-roam or bash sections)
- **THEN** the profile is valid
- **AND** missing sections treated as empty (deny-by-default)

#### Scenario: Profile includes bash_tools section without categories
- **WHEN** a scope profile defines bash command permissions
- **THEN** it includes a `bash_tools` top-level key with only a `deny` subsection
- **AND** no `categories` subsection is present

#### Scenario: Bash tools deny list defined
- **WHEN** bash_tools is present in a profile
- **THEN** deny is an array of command names that are always rejected
- **AND** deny list contains only edge cases (sudo, commands with parser limitations)

#### Scenario: Bash tools section is optional
- **WHEN** a scope profile omits bash_tools
- **THEN** sessions created from that profile have no bash command access (deny by default)

#### Scenario: Bash tools config written to session scope.yml
- **WHEN** creating a session from a profile with bash_tools
- **THEN** the system writes the bash_tools section to the session's scope.yml
- **AND** bash_tools undergoes same YAML parsing and normalization as other scope config

### Requirement: Default bash-enabled profiles

The system SHOULD provide default scope profiles that demonstrate minimal bash_tools deny lists without categories.

#### Scenario: bash-enabled profile provided without categories
- **WHEN** the configuration is installed
- **THEN** a `bash-enabled.yml` profile SHALL exist with only bash_tools.deny section
- **AND** profile demonstrates minimal deny list (sudo, dd, chmod)

#### Scenario: restricted profile has no bash
- **WHEN** using the `restricted.yml` profile
- **THEN** bash_tools section is omitted (no bash command access)

#### Scenario: system-explorer profile has minimal deny list
- **WHEN** using the `system-explorer.yml` profile
- **THEN** bash_tools section includes deny list only
- **AND** deny list blocks destructive commands (rm, chmod, sudo)

## REMOVED Requirements

### Requirement: Bash tools categories structure

**Reason**: Category-based command validation (read_only, safe_write, dangerous) removed in favor of operation-first validation. Commands no longer require explicit allowlisting.

**Migration**: Remove `categories` section from bash_tools in all scope profiles. Keep only `deny` list for edge cases. Commands with no file operations are now allowed by default. Commands with file operations are validated against paths configuration.

### Requirement: Default bash-enabled profiles demonstrate comprehensive categorization

**Reason**: Categories no longer part of schema, so demonstration profiles cannot show categorization.

**Migration**: Update example profiles to show minimal deny lists instead of comprehensive command categorization.
