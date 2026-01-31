# Scope Management Refactoring: v2.0 → v3.0

## Summary

Migrated gptel scope configuration from per-tool patterns in `scope-plan.yml` to global path lists in `preset.md` frontmatter, with specialized sections for non-path tools.

## Architecture Changes

### v2.0 (Old)
- **Configuration**: `scope-plan.yml` with per-tool patterns
- **Format**: Each tool had independent `patterns` and `deny_patterns` lists
- **Size**: ~100+ lines of YAML per session
- **Validation**: 70+ line `pcase` dispatcher routing to 11 separate validators

### v3.0 (New)
- **Configuration**: `preset.md` YAML frontmatter (unified with backend/model config)
- **Format**: Global `paths` lists + specialized sections for org-roam and shell
- **Size**: ~30 lines of YAML per session
- **Validation**: Category-based dispatch to 3 strategy validators

## File Structure

### preset.md (New Format)
```yaml
---
backend: Claude
model: claude-opus-4-5-20251101
tools: [read_file, write_file_in_scope, ...]

paths:
  read: ["/**"]
  write: ["/tmp/**"]
  deny: ["**/.git/**", "**/runtime/**", "**/.env", "**/node_modules/**"]

org_roam_patterns:
  subdirectory: ["gptel/**"]
  tags: ["gptel"]
  node_ids: ["*"]

shell_commands:
  allow: ["ls", "grep", "git"]
  deny: ["rm -rf", "sudo"]
---
```

### scope-plan.yml (Simplified)
```yaml
version: "3.0"
session_id: "..."
created: "..."
updated: "..."
preset: "programming-assistant"
activity_org_file: "..."  # optional
```

**Metadata only** - no tools section.

## Implementation

### Tool Categorization
```elisp
(defconst jf/gptel-scope--tool-categories
  '(("read_file" . (:validation path :operation read))
    ("write_file_in_scope" . (:validation path :operation write))
    ("create_roam_node_in_scope" . (:validation pattern :operation write))
    ("run_approved_command" . (:validation command :operation write))
    ("PersistentAgent" . (:validation meta :operation delegate))))
```

### Validation Strategies

**Path-based** (filesystem, projectile, treesitter):
- Validates against `paths.read`, `paths.write`, `paths.deny`
- Deny patterns have highest priority
- First arg is always filepath

**Pattern-based** (org-roam):
- Validates against `org_roam_patterns.subdirectory`, `.tags`, `.node_ids`
- Format: `"subdirectory:path"`, `"tag:name"`
- Variable arg positions per tool

**Command-based** (shell):
- Validates against `shell_commands.allow`, `.deny`
- Deny uses substring match, allow uses exact command name match
- First arg is command string

**Meta** (PersistentAgent, inspect_scope_plan):
- Always allowed, no validation

### Dispatcher
```elisp
(defun jf/gptel-scope--check-tool-permission (config tool-name args)
  ;; 1. Check allow-once (temporary permissions)
  ;; 2. Lookup tool category
  ;; 3. Route to validator based on validation type
  ;; 4. Return (:allowed t/nil :reason STRING :resource STRING))
```

### Expansion UI

**Transient menu** with 3 choices when validation fails:
1. **Deny** - Reject tool call, return error to LLM
2. **Add to scope** - Update `preset.md` permanently
3. **Allow once** - Grant temporary permission for current LLM turn

**Allow-once mechanism**:
- Buffer-local list: `(tool-name . resource)` pairs
- Single-use consumption
- Auto-cleared via `gptel-post-response-functions` hook

## Key Functions

### Core
- `jf/gptel-scope--load-config()` - Parse preset.md frontmatter
- `jf/gptel-scope--check-tool-permission()` - Main validator dispatcher
- `jf/gptel-scope--validate-path-tool()` - Path-based validation
- `jf/gptel-scope--validate-pattern-tool()` - Org-roam validation
- `jf/gptel-scope--validate-command-tool()` - Shell command validation

### Expansion
- `jf/gptel-scope--prompt-expansion()` - Show transient menu
- `jf/gptel-scope--add-path-to-preset()` - Update preset.md with paths
- `jf/gptel-scope--add-pattern-to-preset()` - Update org-roam patterns
- `jf/gptel-scope--add-command-to-preset()` - Update shell allowlist

### Allow-Once
- `jf/gptel-scope--check-allow-once()` - Validate and consume temp permission
- `jf/gptel-scope--add-to-allow-once-list()` - Grant temp permission
- `jf/gptel-scope--clear-allow-once()` - Cleanup hook

### PersistentAgent
- `jf/gptel-scope--update-preset-paths()` - Update preset.md frontmatter
- Agent creation generates preset.md with paths + minimal scope-plan.yml

## Code Metrics

- **Net reduction**: ~300 lines
- **Commits**: 4
- **Files modified**: 7 (scope-core, scope-expansion, persistent-agent, commands, 11 presets)
- **Functions removed**: 12 (old validators, YAML generator, template functions)
- **Functions added**: 15 (new validators, expansion UI, allow-once)

## Benefits

1. **Unified configuration** - Single file for all settings
2. **Simpler architecture** - 3 validators vs 11, category dispatch vs explicit routing
3. **More accurate** - Separate sections for non-path tools (org-roam, shell)
4. **Better UX** - Expansion UI with temporary permissions
5. **Smaller files** - 30 lines vs 100+ for scope config

## Migration Notes

**No backward compatibility** - Hard cut to v3.0 format.

**Existing sessions**: Will fail with error directing users to update `preset.md` manually.

**Optional migration**: Provide `jf/gptel-scope-migrate-session` command if needed.

## Testing Checklist

- [ ] Create session with project-aware scope
- [ ] Test path-based validation (read/write operations)
- [ ] Test org-roam validation (node creation with subdirs/tags)
- [ ] Test shell command validation (allow/deny lists)
- [ ] Test PersistentAgent with path inheritance
- [ ] Test allow-once mechanism
- [ ] Verify preset.md updates persist correctly
- [ ] Verify scope-plan.yml contains only metadata

## Integration Point

**Expansion UI** requires gptel-level change to invoke `jf/gptel-scope--prompt-expansion()` when validation fails, rather than returning error directly to LLM. Currently validators return error plists - these need to be caught and routed to the UI.

## Files Changed

- `config/gptel/scope/scope-core.{org,el}` - Core validation, allow-once
- `config/gptel/scope/scope-expansion.{org,el}` - Expansion UI (new)
- `config/gptel/scope/scope-commands.{org,el}` - Removed old generators
- `config/gptel/tools/persistent-agent.{org,el}` - v3.0 migration
- `config/gptel/sessions/commands.{org,el}` - Session creation fix
- `config/gptel/presets/*.md` - Added paths sections (11 files)
