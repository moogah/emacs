# Backwards Compatibility: preset.md bash_tools Migration

## Overview

The bash_tools configuration has been migrated from inline preset.md files to the scope.yml system. This document describes the migration strategy and backwards compatibility approach.

**TL;DR:** Both inline bash_tools (in preset.md frontmatter) and scope profile references (scope.yml) are fully supported. Inline bash_tools are NOT deprecated and will continue to work. The scope profile approach is recommended for shared configurations.

## Migration Path

### Old Format (Inline in Presets)

Previously, presets could define bash_tools inline:

```yaml
---
description: Example preset
backend: Claude
model: claude-sonnet-4-5
bash_tools:
  read_only:
    commands: ["ls", "grep", "find"]
  safe_write:
    commands: ["mkdir", "touch"]
  dangerous:
    commands: []
  deny:
    - "rm"
    - "sudo"
---
```

### New Format (Scope Profiles)

Bash_tools configuration now lives in scope profile templates:

**Preset file** (`config/gptel/presets/example.md`):
```yaml
---
description: Example preset
backend: Claude
model: claude-sonnet-4-5
scope_profile: "coding"  # Reference to profile
---
```

**Scope profile** (`config/gptel/scope-profiles/coding.yml`):
```yaml
paths:
  read:
    - "${project_root}/**"
  write:
    - "${project_root}/**"
  deny:
    - "**/.git/**"

bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep", "find", "cat", "head", "tail", "wc", "file", "git log", "git show", "git diff"]
    safe_write:
      commands: ["mkdir", "touch", "echo", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "mv"
    - "sudo"
    - "chmod"
```

## Configuration Approaches

There are two ways to configure bash_tools for presets:

### Approach 1: Inline in Preset (preset.md frontmatter)

**Status:** ✅ Fully supported, NOT deprecated

Bash_tools can be defined directly in preset frontmatter:

**File:** `config/gptel/presets/my-preset.md`
```yaml
---
description: My coding assistant
backend: Claude
model: claude-sonnet-4-5
bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep", "git status"]
    safe_write:
      commands: ["mkdir", "git add"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "sudo"
---
```

**Use when:**
- Preset has unique bash_tools configuration not shared with other presets
- You want configuration self-contained in the preset file
- Quick prototyping or one-off presets

### Approach 2: Scope Profile Reference (scope.yml)

**Status:** ✅ Recommended for shared configurations

Reference a reusable scope profile:

**Preset file:** `config/gptel/presets/my-preset.md`
```yaml
---
description: My coding assistant
backend: Claude
model: claude-sonnet-4-5
scope_profile: "coding"
---
```

**Profile file:** `config/gptel/scope-profiles/coding.yml`
```yaml
bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep", "git status", "git log", "git diff"]
    safe_write:
      commands: ["mkdir", "touch", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "sudo"
    - "mv"
```

**Use when:**
- Multiple presets share the same bash_tools configuration
- You want to update bash_tools for multiple presets in one place
- Configuration is complex and benefits from dedicated file
- Working with profiles that also define paths, org_roam_patterns

### Which Should I Use?

**Choose inline** if:
- Configuration is preset-specific
- You want everything in one file
- Preset is simple or experimental

**Choose scope profile** if:
- Multiple presets share similar permissions
- You want centralized configuration management
- You're defining paths and org_roam_patterns too

**Both approaches work identically** - the choice is organizational preference, not functionality.

## Backwards Compatibility Strategy

### Inline bash_tools Still Supported

Presets can still define bash_tools inline for backwards compatibility:

```yaml
---
description: Example preset with inline bash_tools
backend: Claude
model: claude-sonnet-4-5
bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep"]
    safe_write:
      commands: []
    dangerous:
      commands: []
  deny:
    - "rm"
---
```

**How it works:**
1. During preset registration, `jf/gptel-preset--extract-scope-keys` recognizes `:bash-tools` key
2. Inline bash_tools extracted to `jf/gptel-preset--scope-defaults` alist
3. On session creation, `jf/gptel-scope-profile--resolve` returns inline config directly
4. Session's `scope.yml` populated with inline bash_tools configuration

### What If Both Are Present?

If a preset has BOTH inline bash_tools AND scope_profile reference:

```yaml
---
description: Conflicting preset
scope_profile: "coding"
bash_tools:
  categories:
    read_only:
      commands: ["ls"]
---
```

**Behavior:** Inline bash_tools takes precedence. The scope_profile reference is ignored.

**Recommendation:** Don't do this - it's confusing. Use one approach or the other, not both.

### Key Changes to Be Aware Of

#### 1. Structure Change: Flat to Nested

**Old structure** (flat):
```yaml
bash_tools:
  read_only:
    commands: ["ls"]
  safe_write:
    commands: ["mkdir"]
```

**New structure** (nested under `categories`):
```yaml
bash_tools:
  categories:
    read_only:
      commands: ["ls"]
    safe_write:
      commands: ["mkdir"]
  deny:
    - "rm"
```

**Migration:** The preset registration system normalizes both formats to the new nested structure. Old inline presets will work but should be updated to use the new structure.

#### 2. Naming: shell_commands vs bash_tools

**Deprecated:** `shell_commands` section with `allow`/`deny` lists
**Preferred:** `bash_tools` section with category-based structure

The `shell_commands` section was the legacy approach (simple allow/deny lists without directory scoping). It should be migrated to `bash_tools` for full functionality.

#### 3. Git Command Specificity

**Old approach:** List `"git"` as a single command
**New approach:** List specific git subcommands

```yaml
# Old (base command validation cannot distinguish git subcommands)
read_only:
  commands: ["git"]

# New (explicit subcommands for clarity)
read_only:
  commands: ["git status", "git log", "git diff", "git branch"]
safe_write:
  commands: ["git add", "git commit"]
```

**Why:** While base command validation allows any subcommand of listed base commands, being explicit about which git subcommands are safe improves documentation and intent clarity.

## Migration Checklist

For existing presets with inline bash_tools:

- [ ] Update structure to use `categories` nesting
- [ ] Add explicit `deny` list for dangerous commands
- [ ] List specific git subcommands instead of bare "git"
- [ ] Consider extracting to scope profile if shared across presets
- [ ] Test session creation to ensure scope.yml generated correctly

For new presets:

- [ ] Use `scope_profile` reference instead of inline configuration
- [ ] Create scope profile YAML file if needed
- [ ] Use category-based structure with explicit deny lists
- [ ] List specific git subcommands for clarity

## Examples

### Example 1: Simple Migration

**Before:**
```yaml
---
description: Coding assistant
backend: Claude
bash_tools:
  read_only:
    commands: ["ls", "grep", "git"]
  deny:
    - "rm"
---
```

**After (inline, backwards compatible):**
```yaml
---
description: Coding assistant
backend: Claude
bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep", "git status", "git log", "git diff"]
    safe_write:
      commands: []
    dangerous:
      commands: []
  deny:
    - "rm"
    - "mv"
    - "sudo"
---
```

**After (extracted to profile):**
```yaml
---
description: Coding assistant
backend: Claude
scope_profile: "coding"
---
```

### Example 2: Read-Only Exploration Preset

**Before:**
```yaml
bash_tools:
  read_only:
    commands: ["ls", "cat", "grep", "find", "git"]
```

**After:**
```yaml
bash_tools:
  categories:
    read_only:
      commands:
        - "ls"
        - "cat"
        - "grep"
        - "find"
        - "git status"
        - "git log"
        - "git show"
        - "git diff"
        - "git branch"
    safe_write:
      commands: []
    dangerous:
      commands: []
  deny:
    - "rm"
    - "mv"
    - "chmod"
    - "sudo"
```

## Testing Migration

After migrating, verify:

1. **Preset registration:**
   ```elisp
   ;; Check extracted bash_tools in scope defaults
   jf/gptel-preset--scope-defaults
   ```

2. **Session creation:**
   ```bash
   # Create test session and inspect generated scope.yml
   cat sessions/<session-name>/scope.yml
   ```

3. **Tool execution:**
   ```
   # In gptel session, test bash commands
   run_bash_command("ls -la", "/tmp")
   run_bash_command("git status", "/path/to/repo")
   ```

## Breaking Changes

### None

The migration maintains full backwards compatibility:
- Inline bash_tools in presets continue to work
- Old flat structure is normalized to new nested structure
- Existing sessions with scope.yml files remain valid

### Recommended Updates

While not breaking, these updates are recommended:

1. Migrate to scope profiles for reusability
2. Update to nested `categories` structure
3. List specific git subcommands
4. Add comprehensive deny lists
5. Remove legacy `shell_commands` sections

## Deprecation Status

**Inline bash_tools are NOT deprecated.** Both approaches are fully supported:

| Feature                           | Status            | Support Timeline    |
|-----------------------------------|-------------------|---------------------|
| Inline bash_tools (preset.md)    | ✅ Supported      | Indefinite          |
| Scope profile reference           | ✅ Supported      | Indefinite          |
| Old flat structure (read_only)    | ⚠️ Auto-normalized| Works but update    |
| shell_commands section            | ⚠️ Legacy         | Use bash_tools      |

**No breaking changes planned.** The system is designed to support both approaches indefinitely.

### When to Migrate

You should consider updating your configuration if:
- You're using the old flat structure (without `categories` nesting)
- You're using `shell_commands` instead of `bash_tools`
- Multiple presets duplicate the same bash_tools configuration

You do NOT need to migrate if:
- Your inline bash_tools are working
- Configuration is preset-specific
- You prefer keeping everything in preset.md

## Questions?

See:
- `config/gptel/scope/scope-profiles.org` - Profile system documentation
- `config/gptel/tools/scope-shell-tools.org` - Bash tools implementation
- `openspec/changes/scoped-bash-command-execution/design.md` - Original design document
