# Purpose

Define the enhanced scope.yml v4 schema with operation-specific path scoping, cloud authentication configuration, and security settings. Extends the v3 schema with new sections while maintaining YAML structure compatibility.

# Requirements

## Operation-specific path sections
The scope.yml v4 schema SHALL include separate path sections for each file operation type.

#### Scenario: paths.read section
- **WHEN** scope.yml contains paths.read
- **THEN** section is array of glob patterns for read-allowed paths

#### Scenario: paths.write section
- **WHEN** scope.yml contains paths.write
- **THEN** section is array of glob patterns for write-allowed paths

#### Scenario: paths.execute section
- **WHEN** scope.yml contains paths.execute
- **THEN** section is array of glob patterns for execute-allowed paths

#### Scenario: paths.modify section
- **WHEN** scope.yml contains paths.modify
- **THEN** section is array of glob patterns for modify-allowed paths

#### Scenario: paths.deny section unchanged
- **WHEN** scope.yml contains paths.deny
- **THEN** section is array of glob patterns that override all operation types

## Write scope includes read capability
The v4 schema SHALL maintain the principle that write scope includes read capability for backward compatibility.

#### Scenario: Write path allows read operations
- **WHEN** path matches paths.write pattern
- **THEN** both :read and :write operations validate against this path

#### Scenario: Read path does not allow write operations
- **WHEN** path matches paths.read pattern only
- **THEN** :read operations allowed but :write operations denied

## Cloud authentication configuration section
The scope.yml v4 schema SHALL include cloud section for authentication policy.

#### Scenario: cloud.auth_detection field
- **WHEN** scope.yml contains cloud.auth_detection
- **THEN** value is one of: "allow", "warn", "deny"

#### Scenario: cloud.allowed_providers field
- **WHEN** scope.yml contains cloud.allowed_providers
- **THEN** value is array of provider strings: ["aws", "gcp", "azure"]

#### Scenario: Missing cloud section defaults
- **WHEN** scope.yml has no cloud section
- **THEN** system defaults to auth_detection: "warn" and allowed_providers: []

## Security configuration section
The scope.yml v4 schema SHALL include security section for parse and coverage enforcement.

#### Scenario: security.enforce_parse_complete field
- **WHEN** scope.yml contains security.enforce_parse_complete
- **THEN** value is boolean (true/false)

#### Scenario: security.max_coverage_threshold field
- **WHEN** scope.yml contains security.max_coverage_threshold
- **THEN** value is float between 0.0 and 1.0 (e.g., 0.8 for 80%)

#### Scenario: Missing security section defaults
- **WHEN** scope.yml has no security section
- **THEN** system defaults to enforce_parse_complete: true and max_coverage_threshold: 0.8

## Bash tools section unchanged
The v4 schema SHALL maintain the existing bash_tools section structure from v3.

#### Scenario: bash_tools.categories structure
- **WHEN** scope.yml contains bash_tools
- **THEN** categories.read_only, categories.safe_write, categories.dangerous arrays present

#### Scenario: bash_tools.deny structure
- **WHEN** scope.yml contains bash_tools
- **THEN** deny array present with denied command names

## Complete v4 schema example
The v4 schema SHALL support complete scope documents with all sections.

#### Scenario: Full v4 scope document structure
- **WHEN** scope.yml is valid v4 format
- **THEN** document contains:
  - paths section with read, write, execute, modify, deny arrays
  - bash_tools section with categories and deny arrays
  - cloud section with auth_detection and allowed_providers
  - security section with enforce_parse_complete and max_coverage_threshold

## YAML parsing and normalization
The system SHALL parse v4 scope.yml using YAML parser with snake_case to kebab-case normalization.

#### Scenario: Snake case keys in YAML
- **WHEN** scope.yml uses snake_case keys (paths.read, auth_detection)
- **THEN** system parses correctly using YAML snake_case convention

#### Scenario: Kebab case normalization in Elisp
- **WHEN** YAML is loaded into Elisp
- **THEN** keys normalized to kebab-case (:paths {:read [...] :write [...] :execute [...] :modify [...]})

#### Scenario: Nested structure normalization
- **WHEN** cloud.auth_detection parsed from YAML
- **THEN** Elisp structure is {:cloud {:auth-detection "warn"}}

## Backward compatibility explicitly not supported
The v4 schema SHALL NOT support v3 documents without manual migration.

#### Scenario: v3 document with only read/write paths
- **WHEN** scope.yml has paths.read and paths.write but no execute/modify
- **AND** security.enforce_parse_complete is missing
- **THEN** system treats missing execute/modify as [] (no paths allowed)
- **AND** system uses default enforce_parse_complete: true

#### Scenario: No automatic migration from v3 to v4
- **WHEN** loading v3 scope document
- **THEN** system does not automatically add execute/modify sections
- **AND** commands requiring execute/modify permissions will be denied

## Validation on schema load
The system SHALL validate scope.yml structure when loading v4 documents.

#### Scenario: Invalid auth_detection value
- **WHEN** cloud.auth_detection is not "allow", "warn", or "deny"
- **THEN** system returns schema validation error

#### Scenario: Invalid coverage threshold
- **WHEN** security.max_coverage_threshold is not between 0.0 and 1.0
- **THEN** system returns schema validation error

#### Scenario: Invalid paths structure
- **WHEN** paths.read is not an array
- **THEN** system returns schema validation error

#### Scenario: Valid v4 document loads successfully
- **WHEN** all sections conform to v4 schema
- **THEN** document loads without validation errors
