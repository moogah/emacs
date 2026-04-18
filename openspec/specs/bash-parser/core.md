# Bash Parser Core

## Purpose

Parse bash command strings into structured representations with complete token inventory for semantic analysis and coverage tracking.

## Responsibilities

- Parse bash commands into structured format with command name, flags, arguments, redirections, pipes
- Track token inventory with unique IDs, types, values, and positions for each parsed element
- Track parse completeness flag indicating whether syntax was fully understood
- Support command substitution, subshell, and pipeline parsing
- Track which pipeline segment each token belongs to for multi-command analysis
- Link flag arguments to their associated flags for semantic understanding
- Handle nested structures (command substitutions, subshells) and track whether they were parsed

## Key Invariants

- Token IDs are unique within a parsed command
- Token positions (:start, :end) enable exact substring extraction from original command
- Tokens maintain parse order (ordered by position in command string)
- Parse completeness flag (:parse-complete) is true only when entire syntax is understood
- Parse errors (:parse-errors) are populated only when :parse-complete is false

## Requirements

### Requirement: Parse bash commands into structured format
The parser SHALL parse bash command strings into structured plists containing command name, flags, arguments, redirections, and pipes.

#### Scenario: Simple command parsing
- **WHEN** parsing "cat file.txt"
- **THEN** result includes :command-name "cat", :positional-args ("file.txt")

#### Scenario: Command with flags
- **WHEN** parsing "ls -la /tmp"
- **THEN** result includes :command-name "ls", :flags (("-la" . nil)), :positional-args ("/tmp")

#### Scenario: Pipeline parsing
- **WHEN** parsing "cat file.txt | grep foo"
- **THEN** result includes pipeline structure with multiple command segments

### Requirement: Token inventory in parsed output
The parser SHALL include granular token inventory in parsed command structure for coverage tracking.

#### Scenario: Parse result includes tokens
- **WHEN** jf/bash-parse completes successfully
- **THEN** result includes :tokens list with token plists

#### Scenario: Each token has required fields
- **WHEN** tokens are generated
- **THEN** each token includes :id, :type, :value, :start, :end fields

#### Scenario: Tokens cover entire command
- **WHEN** parsing complete command
- **THEN** token inventory represents all parsed syntactic elements

### Requirement: Token types
The system SHALL classify tokens into syntactic categories for coverage analysis.

#### Scenario: Command name token
- **WHEN** parsing command name
- **THEN** token type is :command-name

#### Scenario: Positional argument token
- **WHEN** parsing positional argument
- **THEN** token type is :positional-arg

#### Scenario: Flag token
- **WHEN** parsing flag (e.g., --flag or -f)
- **THEN** token type is :flag

#### Scenario: Flag argument token
- **WHEN** parsing argument following a flag
- **THEN** token type is :flag-arg with :associated-flag reference

#### Scenario: Command substitution token
- **WHEN** parsing $(command) or `command`
- **THEN** token type is :command-substitution

#### Scenario: Separator token
- **WHEN** parsing separator like "--"
- **THEN** token type is :separator

#### Scenario: Redirection token
- **WHEN** parsing redirection operator
- **THEN** token type is :redirection

#### Scenario: Pipe token
- **WHEN** parsing pipe operator
- **THEN** token type is :pipe

### Requirement: Token unique identifiers
The system SHALL assign unique integer IDs to each token for tracking and claiming by extraction layers.

#### Scenario: Token has unique ID
- **WHEN** parser creates tokens
- **THEN** each token has unique integer :id field

#### Scenario: Token ID sequence
- **WHEN** parsing command with multiple tokens
- **THEN** token IDs are sequential integers starting from 0

### Requirement: Token position tracking
The system SHALL track character positions for each token to enable source mapping and visualization.

#### Scenario: Token start position
- **WHEN** token is created
- **THEN** :start field indicates first character position (0-indexed)

#### Scenario: Token end position
- **WHEN** token is created
- **THEN** :end field indicates position after last character (exclusive)

#### Scenario: Position enables substring extraction
- **WHEN** using :start and :end positions
- **THEN** substring of original command yields token :value

### Requirement: Parse completeness flag
The parser SHALL include :parse-complete flag indicating whether syntax was fully understood.

#### Scenario: Successful parse
- **WHEN** parser successfully parses entire command syntax
- **THEN** :parse-complete is t

#### Scenario: Syntax error
- **WHEN** parser encounters unmatched quotes or invalid syntax
- **THEN** :parse-complete is nil and :parse-errors contains error messages

#### Scenario: Partial parse
- **WHEN** parser handles prefix but cannot parse remainder
- **THEN** :parse-complete is nil

### Requirement: Parse error tracking
The parser SHALL include :parse-errors list when parse-complete is false.

#### Scenario: Parse errors empty on success
- **WHEN** parse completes successfully
- **THEN** :parse-errors is nil or empty list

#### Scenario: Parse errors populated on failure
- **WHEN** parse fails with syntax error
- **THEN** :parse-errors contains descriptive error messages

#### Scenario: Multiple parse errors
- **WHEN** parse encounters multiple syntax issues
- **THEN** :parse-errors lists all detected errors

### Requirement: Pipeline segment tracking
The system SHALL track which pipeline segment each token belongs to for multi-command analysis.

#### Scenario: First command segment
- **WHEN** token is in first command of pipeline
- **THEN** token has :pipe-segment 0

#### Scenario: Second command segment
- **WHEN** token is in second command of pipeline
- **THEN** token has :pipe-segment 1

#### Scenario: Command after separator
- **WHEN** token follows "--" separator
- **THEN** token has updated :pipe-segment value

### Requirement: Flag association
The system SHALL link flag arguments to their associated flags for semantic understanding.

#### Scenario: Flag argument linked to flag
- **WHEN** parsing "--log-group-name /aws/lambda/logs"
- **THEN** flag-arg token has :associated-flag pointing to flag token ID

#### Scenario: Multiple flag arguments
- **WHEN** flag accepts multiple arguments
- **THEN** each flag-arg token links to same flag token ID

### Requirement: Nested structure tracking
The system SHALL track whether nested structures (command substitutions, subshells) were parsed.

#### Scenario: Parsed command substitution
- **WHEN** command substitution contents are parsed
- **THEN** token has :parsed-inner t

#### Scenario: Unparsed command substitution
- **WHEN** command substitution contents are not parsed (e.g., non-shell language)
- **THEN** token has :parsed-inner nil

## Integration Points

- **Orchestrator**: Consumes parsed commands with token inventory and :parse-complete flag
- **Coverage System**: Uses token inventory and :parse-complete to calculate semantic coverage
- **Command Handlers**: Receive parsed commands and claim tokens to indicate understanding

## Example Usage

```elisp
;; Parse simple command
(jf/bash-parse "cat /workspace/file.txt")
;; => (:command-name "cat"
;;     :positional-args ("/workspace/file.txt")
;;     :tokens [(:id 0 :type :command-name :value "cat" :start 0 :end 3)
;;              (:id 1 :type :positional-arg :value "/workspace/file.txt" :start 4 :end 24)]
;;     :parse-complete t
;;     :parse-errors nil)

;; Parse command with syntax error
(jf/bash-parse "cat 'unclosed")
;; => (:command-name "cat"
;;     :tokens [(:id 0 :type :command-name :value "cat" :start 0 :end 3)]
;;     :parse-complete nil
;;     :parse-errors ("Unclosed quote at position 4"))
```
