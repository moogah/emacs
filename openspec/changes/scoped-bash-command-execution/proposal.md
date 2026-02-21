## Why

Current `run_approved_command` validates shell commands by name only, without controlling where they execute. This allows commands to access any directory on the filesystem, bypassing the path-based security model used by file and org-roam tools. LLM agents need broad shell capability to explore codebases and perform development tasks, but this capability must be constrained by the same scope system that protects file operations.

## What Changes

- **Add `run_bash_command` tool** with explicit directory argument and category-based validation
- **Categorize commands** into read_only (ls, grep, find, cat), safe_write (mkdir, touch, echo), and dangerous (empty by default)
- **Validate working directory** against category's path scope requirement (read_only uses paths.read, safe_write uses paths.write)
- **Allow shell composition** (pipes, redirects, command substitution) while validating base command only
- **Add security features**: 30-second timeout protection, output truncation with suggestions, warnings for absolute paths in arguments
- **Parse base command** from complex command strings to categorize correctly
- **Deny by default**: Commands not in allow lists are rejected
- **BREAKING: Replace `run_approved_command`** with new tool that enforces directory scoping

## Capabilities

### New Capabilities
- `bash-tools`: Shell command execution with category-based command validation and directory scope enforcement. Provides controlled access to arbitrary shell commands while respecting path-based security boundaries.

### Modified Capabilities
- `scope`: Extends scope validation system to support bash tool category (command + directory validation). Adds bash validation type to tool categorization and directory-per-category validation logic.

## Impact

**Code changes:**
- `config/gptel/scope/scope-bash-tools.{org,el}` - Major rewrite to replace `run_approved_command` with `run_bash_command`
- `config/gptel/scope/scope-core.el` - Add bash validation type to tool categories and implement `jf/gptel-scope--validate-bash-tool`

**Configuration changes:**
- Preset.md files require new `bash_tools` section with category-based command lists
- Existing presets with `shell_commands` need migration to new structure

**Tool interface:**
- Breaking: `run_approved_command` removed
- New: `run_bash_command` requires explicit directory argument

**Dependencies:**
- Leverages existing `paths.read`, `paths.write`, and `paths.deny` from scope system
- Integrates with existing scope expansion flow via `request_scope_expansion`
