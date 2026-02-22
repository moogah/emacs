---
description: >
  Example preset demonstrating bash_tools configuration for scoped command execution.
  Shows command categorization, directory scope validation, and security features.
backend: Claude
model: claude-sonnet-4-5-20250929
temperature: 0.5

# Path scope configuration
# Commands execute within these boundaries, validated by category
paths:
  read:
    - "/Users/jefffarr/emacs/**"
    - "/Users/jefffarr/projects/**"
  write:
    - "/tmp/**"
    - "/Users/jefffarr/emacs/scratch/**"
  deny:
    - "**/.git/**"
    - "**/runtime/**"
    - "**/.env"
    - "**/node_modules/**"

# Bash tools configuration
# Commands are categorized by operational impact and bound to path scope requirements
bash_tools:
  # Read-only commands: only read data, no modifications
  # Path requirement: paths.read OR paths.write (write scope includes read capability)
  # Use for: exploration, searching, viewing
  read_only:
    commands:
      - "ls"           # List directory contents
      - "grep"         # Search file contents
      - "find"         # Find files by pattern
      - "cat"          # Display file contents
      - "head"         # Display file start
      - "tail"         # Display file end
      - "wc"           # Count lines/words/chars
      - "file"         # Identify file type
      - "stat"         # File status/metadata
      - "git log"      # View commit history
      - "git show"     # View commit details
      - "git diff"     # View changes
      - "git status"   # View working tree status

  # Safe write commands: create or modify files/directories safely
  # Path requirement: paths.write only
  # Use for: creating directories, touching files, git operations
  safe_write:
    commands:
      - "mkdir"        # Create directories
      - "touch"        # Create empty files
      - "echo"         # Write text (with redirection)
      - "git add"      # Stage changes
      - "git commit"   # Commit changes

  # Dangerous commands: require explicit user approval via scope expansion
  # Empty by default - user must approve each addition
  # Use for: commands that could cause data loss or system changes
  dangerous:
    commands: []

  # Denied commands: never allowed, even with scope expansion
  # These are destructive operations that should never be automated
  deny:
    - "rm"           # Remove files/directories
    - "mv"           # Move/rename (can overwrite)
    - "cp"           # Copy (can overwrite)
    - "chmod"        # Change permissions
    - "chown"        # Change ownership
    - "sudo"         # Elevated privileges
    - "dd"           # Low-level disk operations
    - "mkfs"         # Format filesystems

---

# Bash Tools Example Preset

This preset demonstrates the **bash_tools** configuration for scoped shell command execution.

## Configuration Philosophy

### Command Categorization

Commands are grouped by **operational impact** rather than technical function:

1. **read_only** - Commands that only read data
   - Examples: `ls`, `grep`, `find`, `cat`, `git log`
   - Can execute in any readable directory (paths.read or paths.write)
   - No risk of data modification

2. **safe_write** - Commands that create or modify safely
   - Examples: `mkdir`, `touch`, `echo`, `git add`
   - Require write-enabled directory (paths.write)
   - Create new content but don't destroy existing data

3. **dangerous** - Commands requiring explicit approval
   - Empty by default
   - User must approve via scope expansion for each addition
   - For commands with potential for unintended consequences

4. **deny** - Commands never allowed
   - Examples: `rm`, `mv`, `sudo`, `dd`
   - Destructive operations that should never be automated
   - Rejected immediately without scope expansion option

### Path Scope Binding

Each category binds to path scope requirements:

| Category    | Requires paths.read | Requires paths.write | Notes                           |
|-------------|---------------------|----------------------|---------------------------------|
| read_only   | ✓ (or write)        |                      | Write scope includes read       |
| safe_write  |                     | ✓                    | More restrictive                |
| dangerous   | ✓                   | ✓                    | Plus explicit approval          |
| deny        | ✗                   | ✗                    | Never allowed                   |

**Key insight:** Write paths imply read access. If a directory is in `paths.write`, read_only commands can execute there. This matches the intuition that "write permission includes read permission."

### Shell Composition

The system supports standard shell composition features:

- **Pipes**: `grep pattern file.txt | head -10`
- **Redirects**: `ls -la > listing.txt`
- **Command substitution**: `echo $(pwd)`

**Validation strategy:** Only the **base command** (first command before pipes/redirects) is categorized and validated. If `grep` is in `read_only` and the directory is in `paths.read`, then `grep | head | tail` is allowed.

**Rationale:** Validating the base command provides the primary security boundary. If `grep` is safe in a scoped directory, composing it with `head` doesn't increase risk. Parsing complex pipelines is error-prone and provides marginal security benefit.

### Security Features

1. **Timeouts** - Commands automatically terminate after 30 seconds
   - Prevents runaway processes (e.g., `find /` or infinite loops)
   - LLM receives timeout error and can retry with more specific filters

2. **Output Truncation** - Command output limited to prevent context overflow
   - Long output truncated with notice suggesting filters
   - LLM learns to use `head`, `grep`, `tail` to narrow results

3. **Absolute Path Warnings** - Warning when command arguments contain absolute paths
   - Example: `grep pattern /other/project` triggers warning
   - Absolute paths in arguments bypass directory scope validation
   - Warning suggests using relative paths

4. **Explicit Directory Required** - Every command must specify working directory
   - No implicit fallbacks or context inference
   - Forces clear intent about where commands execute

### Error Messages and Scope Expansion

When validation fails, the system provides structured errors guiding the LLM to request scope expansion:

**Command not allowed:**
```json
{
  "success": false,
  "error": "command_not_allowed",
  "tool": "run_bash_command",
  "command": "tree",
  "message": "Command 'tree' is not in allowed command lists. Use request_scope_expansion to ask user for approval."
}
```

**Directory not in scope:**
```json
{
  "success": false,
  "error": "directory_not_in_scope",
  "directory": "/Users/jefffarr/other-project",
  "required_scope": "read",
  "allowed_patterns": ["/Users/jefffarr/emacs/**", "/Users/jefffarr/projects/**"],
  "message": "Directory not in read scope. Use request_scope_expansion to request access."
}
```

The LLM can then call `request_scope_expansion` to request user approval:
```
request_scope_expansion(
  tool_name="run_bash_command",
  patterns=["tree"],
  justification="Need to visualize directory structure for debugging."
)
```

User sees transient menu with options:
- **Deny** - Reject request
- **Add to scope** - Permanently add to preset.md
- **Allow once** - Allow for current turn only

## Tool: run_bash_command

The `run_bash_command` tool executes shell commands in a specified directory with scope validation.

### Function Signature

```
run_bash_command(command, directory)
```

**Parameters:**
- `command` (string, required): Shell command to execute, including any arguments, pipes, or redirects
- `directory` (string, required): Absolute path to working directory where command executes

**Returns:**
- `success` (boolean): Whether command executed successfully
- `stdout` (string): Command output (truncated if too long)
- `stderr` (string): Command error output
- `exit_code` (integer): Process exit code
- `error` (string): Error type if validation failed

### Command Categorization

Commands are validated based on their **operational impact**:

1. **read_only** - Commands that only read data
   - Examples: `ls`, `grep`, `find`, `cat`, `git log`, `git diff`
   - Scope requirement: Directory must be in `paths.read` OR `paths.write`
   - No data modification risk

2. **safe_write** - Commands that create or modify files safely
   - Examples: `mkdir`, `touch`, `echo`, `git add`, `git commit`
   - Scope requirement: Directory must be in `paths.write`
   - Create new content but don't destroy existing data

3. **dangerous** - Commands requiring explicit user approval
   - Empty by default (user must approve each addition via scope expansion)
   - Scope requirement: Directory must be in both `paths.read` AND `paths.write`, plus user approval
   - For commands with potential for unintended consequences

4. **deny** - Commands never allowed
   - Examples: `rm`, `mv`, `cp`, `chmod`, `sudo`
   - No scope can make these commands available
   - Rejected immediately without scope expansion option

**Validation:** Only the **base command** (first word before arguments, pipes, or redirects) is categorized. If `grep` is allowed, then `grep pattern file.txt | head -10` is allowed.

### Scope Expansion Flow

When a command is denied, the tool returns a structured error guiding you to request approval:

**Step 1: Command denied**
```json
{
  "success": false,
  "error": "command_not_allowed",
  "command": "tree",
  "message": "Command 'tree' is not in allowed command lists. Use request_scope_expansion to ask user for approval."
}
```

**Step 2: Request approval**
```
request_scope_expansion(
  tool_name="run_bash_command",
  patterns=["tree"],
  justification="Need to visualize directory structure for debugging."
)
```

**Step 3: User decides**
- **Deny**: Request rejected, command remains unavailable
- **Add to scope**: Command added to preset configuration permanently
- **Allow once**: Command allowed for current turn only

**Step 4: Retry command**
After approval, the original command succeeds:
```
run_bash_command(
  command="tree -L 2",
  directory="/Users/jefffarr/projects/myapp"
)
```

### Directory Scope Validation

Every command validates that its working directory is in scope for its category:

**Directory not in scope error:**
```json
{
  "success": false,
  "error": "directory_not_in_scope",
  "directory": "/Users/jefffarr/other-project",
  "required_scope": "read",
  "allowed_patterns": ["/Users/jefffarr/emacs/**", "/Users/jefffarr/projects/**"],
  "message": "Directory not in read scope. Use request_scope_expansion to request access."
}
```

You can then request directory access:
```
request_scope_expansion(
  tool_name="run_bash_command",
  patterns=["/Users/jefffarr/other-project/**"],
  justification="Need to access other-project for cross-repository analysis.",
  scope_type="read"
)
```

### Common Patterns

**Codebase exploration (read_only):**
```
run_bash_command("ls -la", "/Users/jefffarr/projects/myapp")
run_bash_command("find . -name '*.py' -type f", "/Users/jefffarr/projects/myapp")
run_bash_command("grep -rn 'TODO' src/", "/Users/jefffarr/projects/myapp")
run_bash_command("git log --oneline -10", "/Users/jefffarr/projects/myapp")
```

**File creation (safe_write):**
```
run_bash_command("mkdir -p scratch/experiment", "/Users/jefffarr/emacs")
run_bash_command("touch notes.txt", "/tmp")
run_bash_command("echo 'content' > file.txt", "/tmp")
```

**Git operations:**
```
run_bash_command("git status", "/Users/jefffarr/emacs")          # read_only
run_bash_command("git add config/file.el", "/Users/jefffarr/emacs")  # safe_write
run_bash_command("git commit -m 'message'", "/Users/jefffarr/emacs") # safe_write
```

**Shell composition:**
```
run_bash_command("grep pattern . | head -20", "/Users/jefffarr/projects/myapp")
run_bash_command("find . -name '*.org' | wc -l", "/Users/jefffarr/emacs")
run_bash_command("ls -la > listing.txt", "/tmp")
```

## Usage Examples

### Exploring a codebase

```elisp
;; List files in project
run_bash_command(
  command="ls -la",
  directory="/Users/jefffarr/projects/myapp"
)

;; Search for TODOs
run_bash_command(
  command="grep -rn 'TODO' src/",
  directory="/Users/jefffarr/projects/myapp"
)

;; Find all Python files
run_bash_command(
  command="find . -name '*.py' -type f",
  directory="/Users/jefffarr/projects/myapp"
)

;; View recent commits
run_bash_command(
  command="git log --oneline -10",
  directory="/Users/jefffarr/projects/myapp"
)
```

### Safe write operations

```elisp
;; Create directory structure
run_bash_command(
  command="mkdir -p scratch/experiment",
  directory="/Users/jefffarr/emacs"
)

;; Stage changes
run_bash_command(
  command="git add config/gptel/presets/new-preset.md",
  directory="/Users/jefffarr/emacs"
)

;; Commit changes
run_bash_command(
  command="git commit -m 'Add new preset'",
  directory="/Users/jefffarr/emacs"
)
```

### Shell composition

```elisp
;; Pipeline: find + filter + limit
run_bash_command(
  command="find . -name '*.org' | grep gptel | head -5",
  directory="/Users/jefffarr/emacs"
)

;; Output redirection
run_bash_command(
  command="ls -la > /tmp/listing.txt",
  directory="/Users/jefffarr/emacs"
)

;; Command substitution
run_bash_command(
  command="echo Current directory: $(pwd)",
  directory="/Users/jefffarr/emacs"
)
```

### Requesting scope expansion

```elisp
;; Command not in allow list triggers error
run_bash_command(
  command="tree -L 2",
  directory="/Users/jefffarr/emacs"
)
;; Returns: command_not_allowed error

;; LLM requests approval
request_scope_expansion(
  tool_name="run_bash_command",
  patterns=["tree"],
  justification="Need to visualize directory structure to understand module organization."
)

;; After user approval, command succeeds
run_bash_command(
  command="tree -L 2",
  directory="/Users/jefffarr/emacs"
)
```

## Customization

### Adding custom commands

To add application-specific commands:

1. **Determine category** - Is it read-only or safe write?
2. **Add to appropriate list** - Edit preset.md
3. **Test in session** - Verify it works as expected

Example: Adding `jq` for JSON processing
```yaml
bash_tools:
  read_only:
    commands: [..., "jq"]  # JSON query tool reads but doesn't modify
```

### Adjusting path scope

To allow commands in additional directories:

```yaml
paths:
  read:
    - "/Users/jefffarr/emacs/**"
    - "/Users/jefffarr/dotfiles/**"  # NEW: Allow reading dotfiles
  write:
    - "/tmp/**"
    - "/Users/jefffarr/emacs/experiments/**"  # NEW: Allow experiments directory
```

### Promoting dangerous commands

If a command is needed frequently, consider promoting from dangerous to safe_write:

```yaml
bash_tools:
  safe_write:
    commands: [..., "npm install"]  # Promote if needed often
  dangerous:
    commands: []  # Remove from dangerous
```

**Caution:** Only promote commands you're comfortable running automatically in scoped directories.

## Best Practices

1. **Start restrictive** - Begin with minimal command lists, expand as needed
2. **Use scope expansion** - Let the LLM request commands rather than pre-approving everything
3. **Review deny list** - Periodically check that destructive commands are blocked
4. **Test in isolation** - Use `/tmp/**` for write operations during development
5. **Audit paths.write** - Ensure write paths don't include sensitive directories
6. **Prefer relative paths** - Encourage LLM to use relative paths in command arguments
7. **Monitor command usage** - Track which commands are requested most, adjust configuration
8. **Document custom commands** - Add comments explaining why non-standard commands are allowed

## Security Considerations

- **Directory scope is primary boundary** - Even read_only commands can leak data if directories aren't scoped properly
- **Base command only** - Validation applies to first command, not entire pipeline
- **Absolute paths bypass scope** - Command arguments with absolute paths aren't validated
- **Deny takes precedence** - paths.deny blocks access even if command is allowed
- **No stdin support** - Interactive commands aren't supported
- **Output truncation** - Extremely long output is truncated to protect context window
- **Timeout protection** - Commands killed after 30 seconds to prevent DoS

## Troubleshooting

### Command denied unexpectedly

**Symptom:** Command should be allowed but gets "command_not_allowed" error

**Check:**
1. Is base command in any category list? (not full command with arguments)
2. Spelling matches exactly? (case-sensitive)
3. Git commands use full form? (`git log` not just `git`)

### Directory scope error

**Symptom:** "directory_not_in_scope" error

**Check:**
1. Does directory match paths.read or paths.write pattern?
2. Is directory in paths.deny?
3. For safe_write commands, is directory in paths.write (not just paths.read)?
4. Are glob patterns correct? (`**` for recursive, `*` for single level)

### Timeout errors

**Symptom:** Command killed after 30 seconds

**Solution:**
- Use more specific filters: `grep -r pattern . --include='*.el'`
- Limit scope: `find . -maxdepth 3 -name '*.org'`
- Use `head` to limit output: `git log | head -50`

### Truncated output

**Symptom:** Output ends with truncation notice

**Solution:**
- Add filters: `grep`, `head`, `tail` to narrow results
- Use more specific patterns: `find . -name 'exact-name.txt'`
- Process in chunks: Multiple commands with different filters
