# Project Worldview

This document describes how specs are organized and which areas of the codebase are under active development vs stable. This guides AI assistants in making appropriate decisions about breaking changes, migration plans, and backward compatibility.

## Spec Organization

### Directory Structure

```
openspec/
├── specs/                    # Source of truth (current system behavior)
│   ├── core/                 # Core Emacs configuration specs
│   ├── major-modes/          # Major mode configuration specs
│   ├── bash-parser/          # Bash parser subsystem specs
│   │   ├── architecture.md   # Component structure, interfaces, boundaries
│   │   ├── core.md           # Parser core and token system
│   │   ├── plugins.md        # Plugin registry and protocol
│   │   ├── coverage.md       # Coverage calculation
│   │   ├── filesystem.md     # Filesystem operations plugin
│   │   ├── cloud-auth.md     # Cloud authentication plugin
│   │   ├── semantics.md      # Semantic analysis concepts
│   │   └── security.md       # Security considerations
│   └── gptel/                # gptel subsystem specs (see below)
│       ├── architecture.md   # Subsystem boundaries (ALWAYS LOAD)
│       ├── core.md           # Package integration, auth
│       ├── sessions.md       # Session subsystem (8 modules)
│       ├── scope.md          # Scope validation subsystem (7 modules)
│       ├── skills.md         # Skills subsystem (3 modules)
│       └── tools.md          # Tools subsystem (10 modules)
└── changes/                  # Change-specific artifacts
    └── <change-name>/
        ├── proposal.md
        ├── specs/            # Delta specs (what's changing)
        ├── architecture.md
        └── design.md
```

### Spec Philosophy

**Specs are behavioral contracts, not technical documentation.**

Specs describe:
- **WHAT** a module does (behavior and intent)
- **WHY** it exists (responsibilities)
- **HOW** it integrates with other modules (contracts)
- **Key invariants** and constraints

Specs do NOT duplicate:
- Implementation details → see `.org` files
- API documentation → see `.org` files
- Code structure → see `.org` files
- Usage examples → see `.org` files

The literate `.org` files are already comprehensive technical documentation. Specs provide the behavioral context for changes.

### Module-to-Spec Mapping

**Most modules get a single spec file:**
- `config/core/completion.el` → `openspec/specs/core/completion.md`
- `config/major-modes/org-mode.el` → `openspec/specs/major-modes/org-mode.md`

**Exception: gptel** - Architecturally complex (31 modules, 4 subsystems) gets its own directory with subsystem-level specs.

**Exception: bash-parser** - Architecturally complex (plugin-based semantic analysis) gets its own directory with component-level specs.

### Delta Specs and Context Control

Changes use delta specs to control what context AI loads. The change's `specs/` directory mirrors the main `specs/` structure.

**Example: Isolated subsystem change**
```
openspec/changes/add-session-export/
└── specs/
    └── gptel/
        └── sessions.md    # Only sessions context loaded
```

Claude loads:
- `openspec/specs/gptel/architecture.md` (subsystem boundaries - ALWAYS)
- `openspec/specs/gptel/sessions.md` (existing sessions spec)
- `openspec/changes/.../specs/gptel/sessions.md` (your delta)
- **NOT** loaded: `gptel/{core,scope,skills,tools}.md`

**Example: Cross-subsystem change**
```
openspec/changes/sessions-scope-integration/
└── specs/
    └── gptel/
        ├── architecture.md  # Explicit subsystem boundaries
        ├── sessions.md
        └── scope.md
```

Claude loads only the relevant subsystems plus architecture boundaries.

**Benefits:**
- **Cognitive load**: Claude only sees relevant subsystems
- **Searchability**: Focused, navigable specs
- **Change isolation**: Delta specs naturally scope work
- **Context control**: Create delta specs only for what you're modifying

### gptel Architecture Exception

**Why gptel has its own directory**: 31 modules across 4 subsystems (sessions, scope, skills, tools) with complex integration points. A single spec would be unmanageable and would force loading irrelevant context for focused changes.

**CRITICAL: Always load architecture.md**: When working on any gptel change, `openspec/specs/gptel/architecture.md` defines subsystem boundaries and integration points. This is required context for understanding where changes fit.

**Subsystem isolation**: Each subsystem can evolve independently with delta specs scoped to just that subsystem.

### bash-parser Architecture Exception

**Why bash-parser has its own directory**: Plugin-based semantic analysis architecture with multiple layers (core, analysis, semantic plugins). Token system, coverage calculation, and plugin protocol are distinct concerns that benefit from separate specs.

**Component-level specs**: Each major component (core parser, plugin registry, coverage, filesystem plugin, cloud-auth plugin) has its own spec file.

## Development Status

### Active Development (Breaking Changes OK)

These areas are under active development. Breaking changes are acceptable without migration plans or backward compatibility concerns.

**gptel (all subsystems)**
- Status: **ACTIVE** - All subsystems under active development
- Breaking changes: OK
- Migration plans: Not required
- Backward compatibility: Not required
- Subsystems: core, sessions, scope, skills, tools

**bash-parser (all components)**
- Status: **ACTIVE** - Plugin architecture being refined
- Breaking changes: OK
- Migration plans: Not required
- Backward compatibility: Not required

### Stable (Consider Backward Compatibility)

These areas are stable. Changes should consider backward compatibility and provide migration plans when making breaking changes.

**Core Emacs configuration**
- Status: **STABLE**
- Breaking changes: Avoid if possible, provide migration if necessary
- Areas: `config/core/*`, module system, testing infrastructure

**Major mode configurations**
- Status: **STABLE**
- Breaking changes: Avoid if possible
- Areas: `config/major-modes/*`

**Language mode configurations**
- Status: **STABLE**
- Breaking changes: Avoid if possible
- Areas: `config/language-modes/*`

### Guidance for AI Assistants

**When working on ACTIVE areas:**
- ✅ Make breaking changes freely to improve design
- ✅ Focus on getting the architecture right
- ✅ Iterate rapidly without backward compatibility concerns
- ❌ Don't add migration complexity prematurely

**When working on STABLE areas:**
- ✅ Preserve existing behavior when possible
- ✅ Provide clear migration paths for breaking changes
- ✅ Document breaking changes prominently
- ✅ Consider deprecation warnings before removal
- ❌ Don't break things without user-facing migration guidance

**Migration plan template (for STABLE areas):**
```markdown
## Migration from vX to vY

### Breaking Changes
- [Change 1]: What changed and why
- [Change 2]: What changed and why

### Migration Steps
1. [Step 1]: How to update configuration
2. [Step 2]: How to update code

### Backward Compatibility
- [Compatibility note]: What still works, what doesn't
```

## Tech Stack Context

**Key technologies** (for understanding specs):
- **Emacs Lisp**: Primary language (Common Lisp via `cl-lib`)
- **tree-sitter**: Bash AST parsing
- **org-mode**: Literate programming (`.org` → `.el` tangling)
- **Buttercup/ERT**: Test frameworks
- **straight.el**: Package management

**Architecture principles** (for understanding specs):
- **Literate programming**: Edit `.org` files, generate `.el` files
- **Modular loading**: `jf/load-module` with dependency order
- **Plugin architecture**: bash-parser uses dynamic plugin registration
- **Scope validation**: Operation-first validation via semantic analysis

## Bootstrapping Strategy

Specs are created on-demand as we touch modules:

1. **Create specs when making changes** - If you're modifying a module, create its spec first
2. **gptel and bash-parser priority** - These architectural outliers need complete specs
3. **Fill in organically** - Other modules get specs as we work on them

**Current spec coverage:**
- ✅ gptel: All subsystems specified (core, sessions, scope, skills, tools)
- ✅ bash-parser: All components specified (core, plugins, coverage, filesystem, cloud-auth)
- ⚠️ core: Partial (create on-demand)
- ⚠️ major-modes: Partial (create on-demand)
- ⚠️ language-modes: Minimal (create on-demand)
