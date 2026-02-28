## CONTEXT: Preset Alignment

This spec describes bash tools in the context of the preset-alignment architecture (see `openspec/changes/gptel-preset-upstream-alignment`).

**Configuration storage:**
- Scope profile templates (`config/gptel/scope-profiles/*.yml`) define default bash_tools config
- Session `scope.yml` files contain per-session bash_tools (mutable)
- Presets may define inline bash_tools (extracted during registration to scope defaults)

**Source of truth for enforcement:** `scope.yml` in the session's branch directory.

**Related specs:** See delta specs for `scope-profiles` and `preset-registration` for details on how bash_tools configuration flows from profiles to sessions.

---

## ADDED Requirements

### Requirement: Command categorization by operation type
The bash tools system SHALL categorize commands into read_only, safe_write, and dangerous categories based on their operational impact.

#### Scenario: Read-only command categorized
- **WHEN** a command only reads data (ls, grep, find, cat, head, tail, wc, file, git log, git show, git diff)
- **THEN** the system categorizes it as read_only requiring paths.read access

#### Scenario: Safe write command categorized
- **WHEN** a command creates or modifies safely (mkdir, touch, echo, git add, git commit)
- **THEN** the system categorizes it as safe_write requiring paths.write access

#### Scenario: Dangerous command categorized
- **WHEN** a command is explicitly listed in bash_tools.categories.dangerous
- **THEN** the system categorizes it as dangerous, always denies it, and requires explicit user approval via request_scope_expansion

#### Scenario: Denied command rejected immediately
- **WHEN** a command appears in bash_tools.deny list
- **THEN** the system rejects it without checking category or directory scope

#### Scenario: Unknown command denied by default
- **WHEN** a command is not in any category allow list
- **THEN** the system denies access with "command_not_allowed" error

### Requirement: Directory scope validation per category
The bash tools system SHALL validate that the working directory matches the command category's path scope requirement.

#### Scenario: Read-only command in read path
- **WHEN** a read_only command executes with directory matching paths.read patterns
- **THEN** the system allows execution

#### Scenario: Read-only command in write path
- **WHEN** a read_only command executes with directory matching paths.write patterns
- **THEN** the system allows execution (write scope includes read capability)

#### Scenario: Safe write command in write path
- **WHEN** a safe_write command executes with directory matching paths.write patterns
- **THEN** the system allows execution

#### Scenario: Safe write command in read-only path
- **WHEN** a safe_write command executes with directory matching only paths.read patterns
- **THEN** the system denies with "directory_not_in_scope" error including required_scope and allowed_patterns

#### Scenario: Directory matches deny pattern
- **WHEN** a directory matches paths.deny patterns
- **THEN** the system denies regardless of category (deny takes precedence)

#### Scenario: Directory outside all patterns
- **WHEN** a directory does not match any paths.read or paths.write patterns
- **THEN** the system denies with "directory_not_in_scope" error

### Requirement: Explicit directory required for all commands
The bash tools system SHALL require an explicit directory argument for every command execution.

#### Scenario: Command with explicit directory
- **WHEN** run_bash_command is called with both command and directory arguments
- **THEN** the system resolves the directory to absolute path and validates it

#### Scenario: Relative directory path resolved
- **WHEN** a relative directory path is provided
- **THEN** the system expands it to an absolute path using expand-file-name

#### Scenario: Symlink directory resolved
- **WHEN** a directory contains symlinks
- **THEN** the system resolves to real path using file-truename before validation

### Requirement: Shell composition features allowed
The bash tools system SHALL allow shell composition features (pipes, redirects, command substitution) while validating only the base command.

#### Scenario: Piped commands validated by base command
- **WHEN** command is "ls | grep foo"
- **THEN** the system extracts base command "ls" for categorization

#### Scenario: Redirected output allowed
- **WHEN** command is "cat file.txt > output.txt"
- **THEN** the system extracts base command "cat" and allows if categorized correctly

#### Scenario: Command substitution allowed
- **WHEN** command is "echo $(pwd)"
- **THEN** the system extracts base command "echo" and allows if categorized correctly

#### Scenario: Complex pipeline validated by first command
- **WHEN** command is "find . -name '*.el' | xargs grep -l 'defun' | head -10"
- **THEN** the system extracts base command "find" for categorization

### Requirement: Base command extraction from complex commands
The bash tools system SHALL parse command strings to extract the base command for categorization.

#### Scenario: Simple command extraction
- **WHEN** parsing "ls -la"
- **THEN** the system extracts "ls" as base command

#### Scenario: Pipeline command extraction
- **WHEN** parsing "ls | grep foo"
- **THEN** the system extracts "ls" as base command (first word before pipe)

#### Scenario: Redirect command extraction
- **WHEN** parsing "cat file > out"
- **THEN** the system extracts "cat" as base command (first word before redirect)

#### Scenario: Command with arguments extraction
- **WHEN** parsing "grep -rn 'pattern' ."
- **THEN** the system extracts "grep" as base command (first word)

#### Scenario: Leading/trailing whitespace handled
- **WHEN** parsing "  ls -la  "
- **THEN** the system trims and extracts "ls" as base command

### Requirement: Complete validation pipeline
The bash tools system SHALL execute a multi-step validation pipeline for each command.

#### Scenario: Validation pipeline for command execution
- **WHEN** run_bash_command is called with command and directory
- **THEN** the system executes these steps in order:
  1. Parse command to extract base command (first token before pipes/redirects)
  2. Categorize base command (check deny → read_only → safe_write → dangerous)
  3. Resolve directory to absolute path using expand-file-name
  4. Resolve directory symlinks to real path using file-truename
  5. Validate directory against category's path scope requirement
  6. Execute command with timeout and output truncation

#### Scenario: Directory resolution with symlinks
- **WHEN** validating a directory path containing symlinks
- **THEN** the system resolves symlinks to the real path before pattern matching
- **AND** pattern matching occurs against the fully resolved path

#### Scenario: Relative directory resolution
- **WHEN** a relative directory path is provided
- **THEN** the system expands it to absolute path relative to buffer's default-directory
- **AND** then resolves any symlinks to real path

### Requirement: Safe command execution with timeouts
The bash tools system SHALL execute commands with timeout protection to prevent runaway processes.

#### Scenario: Command completes within timeout
- **WHEN** a command executes and completes within 30 seconds
- **THEN** the system returns success with output and exit code

#### Scenario: Command exceeds timeout
- **WHEN** a command runs longer than 30 seconds
- **THEN** the system terminates it and returns error "timeout" with explanatory message

#### Scenario: Command execution captures output
- **WHEN** a command executes successfully
- **THEN** the system captures stdout/stderr in :output field

#### Scenario: Command execution captures exit code
- **WHEN** a command executes
- **THEN** the system captures the exit code in :exit_code field

#### Scenario: Non-zero exit code reported
- **WHEN** a command exits with non-zero status
- **THEN** the system returns :success nil and includes exit_code in result

### Requirement: Output truncation for large results
The bash tools system SHALL limit output size to prevent overwhelming the LLM context.

#### Scenario: Output within limit returned fully
- **WHEN** command output is less than maximum characters
- **THEN** the system returns complete output

#### Scenario: Output exceeding limit truncated
- **WHEN** command output exceeds maximum characters
- **THEN** the system truncates output and appends truncation notice with character count

#### Scenario: Truncation notice suggests filtering
- **WHEN** output is truncated
- **THEN** the notice suggests using filters like 'head', 'grep', or 'tail' to narrow results

### Requirement: Absolute path warning in command arguments
The bash tools system SHALL warn when command arguments contain absolute paths that bypass directory scope validation.

#### Scenario: Command with relative paths
- **WHEN** command is "grep -r 'TODO' ./src"
- **THEN** the system executes without warnings

#### Scenario: Command with absolute path argument
- **WHEN** command is "grep -r 'TODO' /Users/jefffarr/other-project"
- **THEN** the system includes :warnings field noting absolute path bypasses scope validation

#### Scenario: Warning suggests relative paths
- **WHEN** an absolute path warning is issued
- **THEN** the warning message suggests using relative paths for proper scope validation

### Requirement: Structured error responses with expansion guidance
The bash tools system SHALL return structured errors that guide the LLM to request scope expansion when needed.

#### Scenario: Command not allowed error structure
- **WHEN** a command is not in any allow list
- **THEN** the system returns :allowed nil with :reason "command_not_allowed", tool, command, and message fields

#### Scenario: Command denied error structure
- **WHEN** a command is in deny list
- **THEN** the system returns :allowed nil with :reason "denied", tool, command, and security warning

#### Scenario: Directory not in scope error structure
- **WHEN** directory does not match category's path requirement
- **THEN** the system returns :allowed nil with :reason "directory_not_in_scope", directory, required_scope, allowed_patterns, and message

#### Scenario: Error messages suggest expansion
- **WHEN** any scope violation occurs
- **THEN** the error message suggests using request_scope_expansion tool

### Requirement: Integration with scope expansion flow
The bash tools system SHALL integrate with the existing scope expansion mechanism for permission requests.

#### Scenario: LLM requests command expansion
- **WHEN** request_scope_expansion is called for run_bash_command with a denied command
- **THEN** the system infers validation type and presents transient menu to user

#### Scenario: User approves command permanently
- **WHEN** user selects "Add to scope" in expansion menu
- **THEN** the system adds command to appropriate category in scope.yml

#### Scenario: User approves command once
- **WHEN** user selects "Allow once" in expansion menu
- **THEN** the system adds to allow-once list for current turn

#### Scenario: LLM requests directory expansion
- **WHEN** request_scope_expansion is called with directory pattern
- **THEN** the system adds pattern to appropriate paths section (read or write)

### Requirement: Configuration loading from scope document
The bash tools system SHALL load bash command configuration from `scope.yml` located in the session's branch directory.

#### Scenario: Bash tools configuration loaded from scope.yml
- **WHEN** a bash command executes
- **THEN** the system loads bash_tools section from scope.yml in buffer's branch directory
- **AND** uses plain YAML parsing (no frontmatter extraction)
- **AND** normalizes YAML keys from snake_case to kebab-case

#### Scenario: Missing bash tools section handled
- **WHEN** scope.yml exists but has no bash_tools section
- **THEN** the system uses empty allow lists (deny all commands by default)

#### Scenario: Missing scope.yml errors
- **WHEN** scope.yml does not exist in the branch directory
- **THEN** the system returns error "no_scope_config"

#### Scenario: Category structure parsed
- **WHEN** bash_tools configuration is loaded
- **THEN** the system parses bash_tools.categories.read_only, bash_tools.categories.safe_write, and bash_tools.categories.dangerous lists from YAML (using snake_case keys)
- **AND** normalizes keys to kebab-case during parsing: {:bash-tools {:categories {:read-only [...] :safe-write [...]} :deny [...]}}
- **NOTE:** YAML uses snake_case (bash_tools, read_only), Elisp uses kebab-case (:bash-tools, :read-only)

#### Scenario: Deny list parsed
- **WHEN** bash_tools configuration is loaded
- **THEN** the system parses bash_tools.deny list for globally denied commands
