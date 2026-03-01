## ADDED Requirements

### Requirement: Sandbox rules definition
The system SHALL support defining sandbox rules as a list of plists containing glob patterns and allowed operations.

#### Scenario: Define rule with single pattern
- **WHEN** creating sandbox rule with pattern "/workspace/**"
- **THEN** rule accepts patterns list and operations list

#### Scenario: Define rule with multiple patterns
- **WHEN** creating sandbox rule with multiple glob patterns
- **THEN** all patterns share same operation permissions

#### Scenario: Define operation-specific permissions
- **WHEN** defining rule with operations list containing `:read` and `:write`
- **THEN** only those operation types are allowed for matching paths

### Requirement: Command security validation
The system SHALL validate a bash command string against sandbox rules, returning allowed/denied status with detailed violations.

#### Scenario: Allow command matching rules
- **WHEN** checking "cat /workspace/file.txt" against rule allowing read on "/workspace/**"
- **THEN** validation returns `:allowed t` with no violations

#### Scenario: Deny command violating rules
- **WHEN** checking "rm /workspace/file.txt" against rule allowing only `:read` on "/workspace/**"
- **THEN** validation returns `:allowed nil` with violation for `:delete` operation

#### Scenario: Multiple violations
- **WHEN** checking command with multiple file operations that violate rules
- **THEN** validation returns all violations in list

### Requirement: Glob pattern matching
The system SHALL match file paths against glob patterns supporting wildcards (*, **, ?, []) without filesystem access.

#### Scenario: Single-level wildcard match
- **WHEN** matching "/workspace/file.txt" against "/workspace/*.txt"
- **THEN** path matches pattern

#### Scenario: Recursive wildcard match
- **WHEN** matching "/workspace/src/foo.el" against "/workspace/**/*.el"
- **THEN** path matches pattern

#### Scenario: No match for different directory
- **WHEN** matching "/etc/passwd" against "/workspace/**"
- **THEN** path does not match pattern

#### Scenario: Character class match
- **WHEN** matching "/workspace/file1.txt" against "/workspace/file[0-9].txt"
- **THEN** path matches pattern

### Requirement: Path segment matching
The system SHALL match paths by splitting into segments and matching each segment against pattern segments.

#### Scenario: Match with double-star consuming zero segments
- **WHEN** matching "/workspace/file.txt" against "/workspace/**/file.txt"
- **THEN** path matches (** matches zero segments)

#### Scenario: Match with double-star consuming multiple segments
- **WHEN** matching "/workspace/a/b/c/file.txt" against "/workspace/**/file.txt"
- **THEN** path matches (** matches "a/b/c")

#### Scenario: Single star does not cross directory boundaries
- **WHEN** matching "/workspace/src/foo.el" against "/workspace/*/foo.el"
- **THEN** path matches (one directory level)

#### Scenario: Single star rejects multiple levels
- **WHEN** matching "/workspace/a/b/foo.el" against "/workspace/*/foo.el"
- **THEN** path does not match (multiple directory levels)

### Requirement: Operation-specific permission checking
The system SHALL validate each file operation type (read/write/delete/modify) separately against allowed operations in matching rule.

#### Scenario: Read allowed but write denied
- **WHEN** rule allows `:read` only and command writes file
- **THEN** write operation is denied

#### Scenario: Write allowed but delete denied
- **WHEN** rule allows `:read :write` and command deletes file
- **THEN** delete operation is denied

#### Scenario: All operations allowed
- **WHEN** rule allows `:read :write :delete :modify :create`
- **THEN** any operation type is allowed

### Requirement: Rule matching priority
The system SHALL evaluate rules in order and use the first rule that matches the file path.

#### Scenario: First matching rule applies
- **WHEN** multiple rules match same path
- **THEN** first rule in list determines allowed operations

#### Scenario: More specific rule before general
- **WHEN** rules list has "/workspace/temp/**" before "/workspace/**"
- **THEN** files in temp directory match temp rule first

### Requirement: Violation reporting
The system SHALL report violations with file path, operation type, matched rule (if any), and reason for denial.

#### Scenario: Violation with matched rule
- **WHEN** operation denied by matched rule
- **THEN** violation includes matched rule and operation not in allowed list

#### Scenario: Violation with no matched rule
- **WHEN** file path matches no rules
- **THEN** violation includes reason "No rule matches this file path"

### Requirement: Unhandled operation reporting
The system SHALL separately report operations that cannot be fully analyzed (low confidence, unresolved variables).

#### Scenario: Low confidence operation
- **WHEN** extracted operation has confidence `:low` or `:unknown`
- **THEN** operation appears in `:unhandled` list, not violations

#### Scenario: Command with unhandled operations fails safely
- **WHEN** any operations are unhandled
- **THEN** overall `:allowed` status is nil (fail secure)

### Requirement: Security check result format
The system SHALL return security check results as plist with `:allowed`, `:command`, `:operations`, `:violations`, and `:unhandled` fields.

#### Scenario: Result structure for allowed command
- **WHEN** command passes all checks
- **THEN** result has `:allowed t`, `:operations` list, `:violations` empty, `:unhandled` empty

#### Scenario: Result structure for denied command
- **WHEN** command has violations
- **THEN** result has `:allowed nil`, `:operations` list, `:violations` list with details

### Requirement: Glob pattern to regex conversion
The system SHALL convert glob patterns to regex patterns for segment matching, handling *, ?, and [] correctly.

#### Scenario: Asterisk to regex
- **WHEN** converting glob "*" to regex
- **THEN** result is ".*" (match any characters)

#### Scenario: Question mark to regex
- **WHEN** converting glob "?" to regex
- **THEN** result is "." (match single character)

#### Scenario: Character class preserved
- **WHEN** converting glob "[abc]" to regex
- **THEN** result preserves "[abc]"

#### Scenario: Escape special regex characters
- **WHEN** converting glob with regex special characters like "." or "+"
- **THEN** characters are escaped in resulting regex
