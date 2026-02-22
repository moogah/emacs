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

Example preset demonstrating the `run_bash_command` tool with category-based command authorization and directory scope validation.

## Quick Start

Commands are categorized by operational impact:
- **read_only**: Safe exploration (ls, grep, find, git log)
- **safe_write**: Non-destructive creation (mkdir, touch, git add)
- **dangerous**: Requires approval (empty by default)
- **deny**: Never allowed (rm, mv, sudo)

Directory scope validation:
- read_only: requires `paths.read` OR `paths.write`
- safe_write: requires `paths.write`

## Example Usage

```bash
# Read-only exploration
run_bash_command("ls -la", "/Users/jefffarr/emacs")
run_bash_command("grep -r TODO . | head -20", "/path/to/project")

# Safe write operations
run_bash_command("mkdir scratch", "/tmp")
run_bash_command("git add config/file.el", "/Users/jefffarr/emacs")

# Request scope expansion for new commands
request_scope_expansion(
  tool_name="run_bash_command",
  patterns=["tree"],
  justification="Need to visualize directory structure"
)
```

See `config/gptel/tools/scope-shell-tools.org` for complete documentation.
