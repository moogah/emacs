## Why

Current `run_approved_command` validates shell commands by name only, without controlling where they execute. This allows commands to access any directory on the filesystem, bypassing the path-based security model used by file and org-roam tools. LLM agents need broad shell capability to explore codebases and perform development tasks, but this capability must be constrained by the same scope system that protects file operations.

## Context: Preset Alignment Architecture

This change integrates with the preset-alignment architecture (see `openspec/changes/gptel-preset-upstream-alignment`). Key architectural points:

**Immutable Presets:** Presets are registered at init in `gptel--known-presets`, defined in `config/gptel/presets/*.md`, and reference scope profiles via `scope_profile: "coding"`. They are never modified after registration.

**Mutable Scope:** Per-session `scope.yml` files live in branch directories, created from scope profile templates, and modified by scope expansion when users approve new permissions.

**Bash Tools Integration:** The `bash_tools` configuration section is part of the scope system (not preset files). It lives in scope profile templates (`config/gptel/scope-profiles/*.yml`) and per-session `scope.yml` documents. This change extends the scope profile schema and scope validation system to support bash command execution.

## What Changes

- **Add `run_bash_command` tool** with explicit directory argument and category-based validation
- **Extend scope profile schema** to include `bash_tools` section with command categorization (read_only, safe_write, dangerous) and deny lists
- **Categorize commands** into read_only (ls, grep, find, cat), safe_write (mkdir, touch, echo), and dangerous (empty by default)
- **Validate working directory** against category's path scope requirement (read_only uses paths.read, safe_write uses paths.write)
- **Load bash_tools config from `scope.yml`** in the session's branch directory (part of mutable scope system)
- **Allow shell composition** (pipes, redirects, command substitution) while validating base command only
- **Add security features**: 30-second timeout protection, output truncation with suggestions, warnings for absolute paths in arguments
- **Parse base command** from complex command strings to categorize correctly
- **Deny by default**: Commands not in allow lists are rejected
- **Update preset registration** to extract `bash_tools` from scope profiles and store in scope defaults
- **Update scope expansion** to write approved commands to `scope.yml` bash_tools section
- **BREAKING: Replace `run_approved_command`** with new tool that enforces directory scoping

## Capabilities

### New Capabilities
- `bash-tools`: Shell command execution with category-based command validation and directory scope enforcement. Provides controlled access to arbitrary shell commands while respecting path-based security boundaries.

### Modified Capabilities
- `scope-profiles`: Extends scope profile schema to include `bash_tools` section for command categorization and deny lists.
- `preset-registration`: Extends scope key extraction to recognize and extract `bash_tools` from scope profiles.
- `scope`: Extends scope validation system to support bash tool category (command + directory validation). Adds bash validation type to tool categorization and directory-per-category validation logic. Loads bash_tools config from `scope.yml`.
- `scope-expansion`: Extends scope expansion to write approved bash commands to `scope.yml` bash_tools section.

## Impact

**Code changes:**
- `config/gptel/tools/scope-shell-tools.{org,el}` - Major rewrite to replace `run_approved_command` with `run_bash_command` (relocated from `scope/` to `tools/` directory)
- `config/gptel/scope/scope-core.{org,el}` - Add bash validation type to tool categories and implement `jf/gptel-scope--validate-bash-tool`
- `config/gptel/scope/scope-expansion.{org,el}` - Add bash command expansion to write approved commands to `scope.yml`
- `config/gptel/preset-registration.{org,el}` - Extend scope key extraction to recognize `bash_tools`
- `config/gptel/scope-profiles.{org,el}` - Extend scope profile schema to include `bash_tools` section

**Configuration changes:**
- Scope profile templates (`config/gptel/scope-profiles/*.yml`) include new `bash_tools` section with category-based command lists
- Per-session `scope.yml` files created from profiles will contain bash_tools configuration
- `bash_tools` configuration is defined in scope profiles and flows to `scope.yml` (source of truth for enforcement)

**Tool interface:**
- Breaking: `run_approved_command` removed
- New: `run_bash_command` requires explicit directory argument

**Dependencies:**
- Leverages existing `paths.read`, `paths.write`, and `paths.deny` from scope system
- Integrates with existing scope expansion flow via `request_scope_expansion`
- Depends on preset-alignment architecture (scope profiles, preset registration, `scope.yml` loading)
