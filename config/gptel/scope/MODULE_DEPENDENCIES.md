# GPTEL Scope Module Dependencies and Architecture

## Overview

The GPTEL scope system consists of 3 primary modules with a clear dependency hierarchy:

```
scope-core.el           (Foundation layer)
    ↓
scope-expansion.el      (UI layer)
    ↓
scope-shell-tools.el    (Tool implementation layer)
```

## Module Responsibilities

### scope-core.el (Foundation)

**Location:** `config/gptel/scope/scope-core.el`

**Purpose:** Core validation infrastructure and tool categorization.

**Provides:**
- `gptel-make-scoped-tool` - Generic macro for creating scope-aware tools
- `jf/gptel-scope--tool-categories` - Constant mapping tools to validation strategies
- Validation dispatchers for all validation types (path, pattern, command, bash)
- Scope configuration loading and YAML parsing
- Pattern matching utilities (glob → regex conversion)
- Allow-once mechanism for temporary permissions
- Key normalization (snake_case ↔ kebab-case)

**Dependencies:**
- `cl-lib` (standard library)
- `yaml` (Emacs built-in YAML parser)
- `gptel-session-constants` (jf/gptel-session--scope-file constant)
- `gptel-session-logging` (logging utilities)

**No dependencies on:** scope-expansion, scope-shell-tools

### scope-expansion.el (UI Layer)

**Location:** `config/gptel/scope/scope-expansion.el`

**Purpose:** Interactive UI for handling scope violations.

**Provides:**
- `jf/gptel-scope-expansion-menu` - Transient menu with 3 choices (deny/add/allow-once)
- `jf/gptel-scope--prompt-expansion` - Entry point for showing expansion UI
- Scope updaters for each validation type:
  - `jf/gptel-scope--add-path-to-scope` (filesystem)
  - `jf/gptel-scope--add-pattern-to-scope` (org-roam)
  - `jf/gptel-scope--add-bash-to-scope` (bash_tools)
- YAML writer (`jf/gptel-scope--write-yaml-plist`)
- Key conversion helpers (kebab-case → snake_case for YAML output)

**Dependencies:**
- `transient` (for menu UI)
- `jf-gptel-scope-core` (provides validator and tool categories)
- `yaml` (for parsing existing scope.yml)

**Depends on scope-core for:**
- `jf/gptel-scope--tool-categories` - To determine operation type (read/write)
- `jf/gptel-scope--add-to-allow-once-list` - For temporary permissions
- `jf/gptel-scope--normalize-plist-keys` - For key normalization

**Relationship to bash_tools:**
- Knows about bash_tools schema (categories: read-only, safe-write, dangerous)
- Uses `jf/gptel-scope--tool-categories` to determine read vs write operation
- Writes nested bash_tools.categories structure to YAML

### scope-shell-tools.el (Tool Layer)

**Location:** `config/gptel/tools/scope-shell-tools.el`

**Purpose:** Implements bash execution and scope expansion request tools.

**Provides:**
- `run_bash_command` tool - Execute shell commands with category-based validation
- `request_scope_expansion` tool - LLM requests user approval via transient menu
- Bash helper functions (parsing, categorization, directory validation, execution)
- `jf/gptel-scope--infer-validation-type` - Maps tool names to validation types

**Dependencies:**
- `cl-lib` (standard library)
- `jf-gptel-scope-core` (provides `gptel-make-scoped-tool` macro)
- `jf-gptel-scope-expansion` (provides `jf/gptel-scope--prompt-expansion`)

**Uses from scope-core:**
- `gptel-make-scoped-tool` - Macro for creating run_bash_command tool
- `jf/gptel-scope--matches-any-pattern` - For directory validation
- `jf/gptel-scope--tool-categories` - For inferring validation types

**Uses from scope-expansion:**
- `jf/gptel-scope--prompt-expansion` - Triggers transient menu for user approval

## Load Order (config/gptel/gptel.el)

```elisp
;; Line 128: Load scope-profiles first (template system)
(jf/load-module (expand-file-name "config/gptel/scope-profiles.el" jf/emacs-dir))

;; Line 131: Load scope-core (foundation)
(jf/load-module (expand-file-name "config/gptel/scope/scope-core.el" jf/emacs-dir))

;; Lines 133-147: Load non-scoped tools
;; (filesystem, projectile, ggtags, treesitter, org-roam, sql, meta, community, transient)

;; Line 157: Load scope-commands
(jf/load-module (expand-file-name "config/gptel/scope/scope-commands.el" jf/emacs-dir))

;; Line 160: Load scope-expansion (UI layer)
(jf/load-module (expand-file-name "config/gptel/scope/scope-expansion.el" jf/emacs-dir))

;; Line 163: Load scope-shell-tools (tool implementations)
(jf/load-module (expand-file-name "config/gptel/tools/scope-shell-tools.el" jf/emacs-dir))

;; Lines 166+: Load persistent-agent, session commands, scoped filesystem/org-roam tools
```

**Critical ordering constraints:**
1. `scope-core` BEFORE `scope-expansion` (line 131 < 160)
   - expansion requires `jf/gptel-scope--tool-categories`
2. `scope-core` BEFORE `scope-shell-tools` (line 131 < 163)
   - shell-tools requires `gptel-make-scoped-tool` macro
3. `scope-expansion` BEFORE `scope-shell-tools` (line 160 < 163)
   - shell-tools requires `jf/gptel-scope--prompt-expansion`

## Dependency Verification

✓ **No circular dependencies**
  - All dependencies flow downward in load order
  - scope-core has no dependencies on expansion or shell-tools
  - scope-expansion depends only on scope-core
  - scope-shell-tools depends on both (loaded last)

✓ **All dependencies satisfied**
  - scope-core loaded before expansion/shell-tools
  - scope-expansion loaded before shell-tools
  - Constants and logging loaded before scope-core

✓ **Load order correct**
  - gptel.el lines 131, 160, 163 maintain proper ordering

## Knowledge Coupling Analysis

### Low-Risk Coupling

**scope-expansion knows about bash_tools schema**
- Location: `scope-expansion.el` lines 336-391 (`jf/gptel-scope--add-bash-to-scope`)
- Reason: YAML updaters centralized in UI module for consistency
- Tradeoff: Single place for YAML writing vs strict separation of concerns
- Impact: Low - schema is stable, coupling is one-way (expansion → core categories)

**Rationale for current design:**
1. All YAML writing centralized in scope-expansion.el (paths, org_roam_patterns, bash_tools)
2. Keeps YAML format handling in one module (easier to maintain consistency)
3. scope-core remains pure validator (no file I/O)
4. scope-shell-tools remains pure tool implementation (no YAML handling)

### Alternative Considered

Move bash_tools YAML writing to scope-shell-tools.el:
```elisp
;; In scope-expansion.el
('bash
 (jf/gptel-bash--add-to-scope scope-file resource tool))

;; Define jf/gptel-bash--add-to-scope in scope-shell-tools.el
```

**Rejected because:**
- Creates additional coupling (expansion → shell-tools)
- Spreads YAML writing across multiple modules
- No clear benefit (bash schema is stable)
- Current design works well in practice

## Integration Points

### scope-expansion → scope-core

**Uses:**
- `jf/gptel-scope--tool-categories` (lines 154, 294)
  - To determine operation type (read vs write) for routing
- `jf/gptel-scope--add-to-allow-once-list` (line 122)
  - To grant temporary permissions
- `jf/gptel-scope--normalize-plist-keys` (line 158, 193, 242)
  - To normalize YAML keys from snake_case

### scope-shell-tools → scope-core

**Uses:**
- `gptel-make-scoped-tool` (line 253)
  - Macro for creating run_bash_command with validation
- `jf/gptel-scope--matches-any-pattern` (lines 150, 158)
  - For directory validation against path patterns
- `jf/gptel-scope--tool-categories` (line 320)
  - For inferring validation type in request_scope_expansion

### scope-shell-tools → scope-expansion

**Uses:**
- `jf/gptel-scope--prompt-expansion` (line 312)
  - To trigger transient menu for scope expansion approval

## Validation Type Flow

The 'bash validation type flows through the system:

1. **Tool categorization** (scope-core.el)
   ```elisp
   ("run_bash_command" . (:validation bash :operation write))
   ```

2. **Validation dispatch** (scope-core.el line 779)
   ```elisp
   ('bash (jf/gptel-scope--validate-bash-tool tool-name args config))
   ```

3. **Expansion UI routing** (scope-expansion.el line 126)
   ```elisp
   ('bash (jf/gptel-scope--add-bash-to-scope scope-file resource tool))
   ```

4. **Type inference** (scope-shell-tools.el line 509)
   ```elisp
   (defun jf/gptel-scope--infer-validation-type (tool-name)
     (let ((category (cdr (assoc tool-name jf/gptel-scope--tool-categories))))
       (plist-get category :validation)))
   ```

## Status Summary

✓ Module load order correct (gptel.org lines 131, 160, 163)
✓ All dependencies satisfied
✓ No circular dependencies
✓ bash validation type handled correctly throughout
✓ Integration points verified
✓ Knowledge coupling is acceptable (centralized YAML writing)

## Related Files

- `config/gptel/gptel.org` - Module loading orchestration
- `config/gptel/scope/scope-core.org` - Foundation layer source
- `config/gptel/scope/scope-expansion.org` - UI layer source
- `config/gptel/tools/scope-shell-tools.org` - Tool layer source
- `config/gptel/scope-profiles.el` - Template system (loaded before core)

## Maintenance Guidelines

When modifying the scope system:

1. **Adding new validation types:**
   - Update `jf/gptel-scope--tool-categories` in scope-core.el
   - Add validator function in scope-core.el
   - Add YAML updater in scope-expansion.el
   - Update validation dispatch in scope-core.el

2. **Adding new scope-aware tools:**
   - Use `gptel-make-scoped-tool` macro from scope-core
   - Add tool category to `jf/gptel-scope--tool-categories`
   - No changes to expansion or core needed (automatic routing)

3. **Changing load order:**
   - Maintain: core → expansion → shell-tools
   - Always load scope-core after constants/logging
   - Document reason in gptel.org comments

4. **Refactoring YAML handling:**
   - Keep all YAML writing in scope-expansion.el (centralized)
   - Keep all YAML reading in scope-core.el (validators)
   - Avoid spreading YAML logic across multiple modules
