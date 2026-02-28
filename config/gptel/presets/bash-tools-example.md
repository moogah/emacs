---
description: >
  Example preset demonstrating bash_tools configuration for scoped command execution.
  Uses the bash-enabled scope profile with extended command set and directory scope validation.
backend: Claude
model: claude-sonnet-4-5-20250929
temperature: 0.5
scope_profile: bash-enabled
tools:
  - run_bash_command
---

# Bash Tools Example Preset

Example preset demonstrating the `run_bash_command` tool with category-based command authorization and directory scope validation.

This preset uses the **bash-enabled** scope profile (see `config/gptel/scope-profiles/bash-enabled.yml` for the complete configuration).

## Architecture

Presets reference scope profiles via `scope_profile: bash-enabled` in their frontmatter. The profile defines:
- **Path scope** - Where commands can execute (read: `/**`, write: `${project_root}/**`)
- **Bash tools** - Which commands are allowed and how they're categorized

## Command Categories

Commands are categorized by operational impact:
- **read_only**: Safe exploration (ls, grep, find, git log)
- **safe_write**: Non-destructive creation (mkdir, touch, git add)
- **dangerous**: Requires approval (empty by default)
- **deny**: Never allowed (rm, mv, sudo)

Directory scope validation:
- read_only: requires `paths.read` OR `paths.write`
- safe_write: requires `paths.write`

## Command Validation Strategy

**Only the base command (first token before pipes/redirects) is categorized and validated.**

This means:
- Listing `grep` allows `grep pattern file | head | tail`
- Listing `ls` allows `ls -la | grep "\.el$" | wc -l`
- Listing `brew` allows `brew list` and `brew install` (constrained by directory scope)

Security relies on three layers:
1. **Base command categorization** - Command must be in an allow list
2. **Directory scope enforcement** - Working directory must match category's path requirements
3. **Read vs write path validation** - Category determines required path access level

**Important:** Pipes, redirects, and command substitution are allowed if the base command is categorized. The working directory determines what files can be accessed, not the command arguments.

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

See `config/gptel/tools/scope-shell-tools.org` for implementation details and complete documentation.
