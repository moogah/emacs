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

### Requirement: Reject cd commands
The system SHALL reject all `cd` commands as a security policy decision to avoid working directory tracking complexity.

#### Scenario: Reject simple cd command
- **WHEN** checking "cd /tmp"
- **THEN** validation returns `:allowed nil` with violation reason "cd commands are prohibited"

#### Scenario: Reject cd in chain
- **WHEN** checking "cd /tmp && rm file.txt"
- **THEN** validation rejects entire chain due to cd command

#### Scenario: Reject cd with options
- **WHEN** checking "cd -L /path" or "cd -P /path"
- **THEN** validation rejects command regardless of flags

#### Scenario: Provide guidance in violation
- **WHEN** cd command is rejected
- **THEN** violation message includes "use absolute paths instead"

### Requirement: Variable resolution in security context
The system SHALL support variable resolution during security checking using provided variable context.

#### Scenario: Resolve variables before path matching
- **WHEN** checking "cat $WORKSPACE/file.txt" with context WORKSPACE="/workspace"
- **THEN** validation resolves to "cat /workspace/file.txt" before pattern matching

#### Scenario: Reject unresolved variables
- **WHEN** checking command with unresolved variable "$UNKNOWN/file.txt"
- **THEN** validation returns `:allowed nil` with violation reason "unresolved variable"

#### Scenario: Partial resolution rejection
- **WHEN** checking "$WORKSPACE/$FILE" with only WORKSPACE resolved
- **THEN** validation rejects due to unresolved $FILE variable

### Requirement: Indirect operation handling
The system SHALL support stricter policies for indirect operations (operations from nested commands).

#### Scenario: Apply policy to indirect operations
- **WHEN** checking "bash -c 'rm file.txt'" with indirect operations
- **THEN** validation marks operation as `:indirect t` for policy evaluation

#### Scenario: Reject all indirect operations (optional strict mode)
- **WHEN** security policy is `:reject-indirect t`
- **THEN** any command with nested command injection is rejected

#### Scenario: Allow indirect with same rules
- **WHEN** security policy treats indirect operations normally
- **THEN** nested command operations validated against same allowlist rules

### Requirement: Variable context parameter
The system SHALL accept optional variable context parameter for resolving variables during validation.

#### Scenario: Provide variable context to checker
- **WHEN** calling security checker with variable context {"DIR": "/workspace"}
- **THEN** commands using $DIR are validated against resolved paths

#### Scenario: Missing variable context
- **WHEN** calling security checker without variable context
- **THEN** any variables in commands are treated as unresolved and rejected

### Requirement: Enhanced violation reporting
The system SHALL include additional context in violations for variables, indirect operations, and cd commands.

#### Scenario: Variable violation details
- **WHEN** unresolved variable causes rejection
- **THEN** violation includes `:unresolved-vars` list and suggestion to declare variables

#### Scenario: Indirect operation violation details
- **WHEN** indirect operation causes rejection
- **THEN** violation includes `:indirect t` and `:nested-command` string

#### Scenario: cd command violation details
- **WHEN** cd command causes rejection
- **THEN** violation includes guidance: "use absolute paths or configure runtime working directory"
