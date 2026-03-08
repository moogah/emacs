# Bash Sandbox Security

## Purpose

Validate bash commands against sandbox rules to prevent unauthorized file system access, ensuring commands only operate on allowed paths with permitted operations.

## Responsibilities

- Define sandbox rules with glob patterns and allowed operations
- Validate bash commands against sandbox rules
- Match file paths against glob patterns supporting wildcards (*, **, ?, [])
- Check operation-specific permissions (read/write/delete/modify separately)
- Report violations with file path, operation type, matched rule, and reason
- Handle unresolved variables and low-confidence operations safely (fail secure)
- Reject cd commands as security policy decision
- Support variable resolution during security checking

## Key Invariants

- Commands are denied (fail secure) if any operations are unhandled or have unresolved variables
- First matching rule in list determines allowed operations (rule order matters)
- More specific rules should appear before general rules in rule list
- cd commands are always rejected to avoid working directory tracking complexity
- `:all` wildcard for :operations is equivalent to explicit list of all operation types

## Requirements

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

### Requirement: All operations wildcard
The system SHALL support :all as a wildcard value for :operations to allow all operation types.

#### Scenario: Rule with :all allows any operation
- **WHEN** rule has :operations :all
- **THEN** any operation type (:read, :write, :delete, :modify, :create) is allowed

#### Scenario: :all is equivalent to explicit list
- **WHEN** comparing rule with :operations :all versus :operations (:read :write :delete :modify :create)
- **THEN** both rules allow identical operations

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

### Requirement: Variable context parameter
The system SHALL accept optional variable context parameter for resolving variables during validation.

#### Scenario: Provide variable context to checker
- **WHEN** calling security checker with variable context {"DIR": "/workspace"}
- **THEN** commands using $DIR are validated against resolved paths

#### Scenario: Missing variable context
- **WHEN** calling security checker without variable context
- **THEN** any variables in commands are treated as unresolved and rejected

## Rule Definition Structure

```elisp
;; Example sandbox rules
(defconst example-sandbox-rules
  '(;; Allow read/write in workspace
    (:patterns ("/workspace/**")
     :operations (:read :write :create :modify))

    ;; Allow all operations in temp directory
    (:patterns ("/workspace/temp/**")
     :operations :all)

    ;; Allow read-only in system directories
    (:patterns ("/usr/**" "/etc/**")
     :operations (:read))))
```

## Integration Points

- **Filesystem Plugin**: Uses extracted file operations for validation
- **Core Parser**: Uses parsed command structure
- **Variable Resolution**: Uses variable context for path resolution
- **gptel Scope System**: Consumer for validating bash commands before execution

## Example Usage

```elisp
;; Check command against rules
(jf/bash-check-security "cat /workspace/file.txt"
                        example-sandbox-rules
                        nil)  ; No variable context
;; => (:allowed t
;;     :command "cat /workspace/file.txt"
;;     :operations ((file "/workspace/file.txt" operation :read ...))
;;     :violations nil
;;     :unhandled nil)

;; Check violating command
(jf/bash-check-security "rm /etc/passwd"
                        example-sandbox-rules
                        nil)
;; => (:allowed nil
;;     :command "rm /etc/passwd"
;;     :operations ((file "/etc/passwd" operation :delete ...))
;;     :violations ((:file "/etc/passwd"
;;                   :operation :delete
;;                   :reason "Operation :delete not allowed by rule"
;;                   :matched-rule (:patterns ("/etc/**") :operations (:read))))
;;     :unhandled nil)
```
