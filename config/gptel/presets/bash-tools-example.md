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

**All commands in pipelines and chains are categorized and validated independently.**

This means:
- `cat file.txt | grep pattern` requires both `cat` and `grep` in allow lists
- `ls -la | grep "\.el$" | wc -l` requires `ls`, `grep`, and `wc` in allow lists
- `ls | xargs rm` is **blocked** because `rm` is in the deny list (even if `ls` is allowed)

Security relies on four layers:
1. **Pipeline command validation** - ALL commands in pipelines/chains must be in allow lists
2. **Command categorization** - Each command must be in read_only, safe_write, or dangerous categories
3. **File path validation** - File arguments are validated against operation-specific scope (paths.read, paths.write, paths.execute, paths.modify)
4. **Deny list enforcement** - Commands in the deny list are blocked regardless of pipeline position

**Important:** Pipeline bypass has been closed. Commands like `ls | xargs rm` are properly rejected because all pipeline commands are validated, not just the first one.

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
