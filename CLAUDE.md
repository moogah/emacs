# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Repository Overview

Isolated, modular Emacs configuration using **literate programming**. Runs independently via git worktrees with three-tier architecture (development, testing, production).

## Core Concepts

### Literate Programming

**Workflow:** Edit `.org` files → tangle to `.el` → Emacs loads `.el`

```bash
# Always edit .org files, never .el directly
./bin/tangle-org.sh config/major-modes/org.org  # tangles AND validates
```

**Required headers:**
```org
#+title: Module Name
#+property: header-args:emacs-lisp :tangle module-name.el
#+auto_tangle: y
```

**Key rules:**
- Edit `.org` files only - `.el` files are generated and overwritten
- Commit both `.org` and `.el` files together
- Keep babel blocks small (one function per block for easy debugging)
- `./bin/tangle-org.sh` auto-validates with `check-parens`

### Launch Methods

Launch via macOS GUI (Emacs.app) or `./bin/emacs-isolated.sh` - both isolate to `runtime/` directory. Init files (`early-init.el`, `init.el`, `init.org`) MUST be at repository root.

### Path Resolution

- `jf/emacs-dir` - Repository root
- Module paths use `config/` prefix: `"core/defaults"` → `config/core/defaults.el`
- `jf/resolve-module-path` handles both `"core/defaults"` and `"transient"` formats

### Git Worktree Architecture

Three tiers: `~/emacs/` (development), `~/emacs-<feature>/` (testing), `~/.emacs.d/` (production).

Each worktree has independent `runtime/` directory. Use absolute paths for siblings:
```bash
git worktree add ~/emacs-feature-name -b feature-name        # Good
./bin/init-worktree-runtime.sh ~/emacs-feature-name         # Copy packages (fast)
cd ~/emacs-feature-name && git submodule update --init      # Init submodules
```

### Module System

Modules defined in `jf/enabled-modules` list in `init.org`, loaded with error handling.

**Loading order critical:**
- `transient` before `language-modes` and `major-modes/magit`
- `major-modes/magit` before `major-modes/org`

**Key functions:**
- `jf/load-module` - Auto-loads during init with error handling
- `jf/reload-module` - Interactive reload for testing (M-x)
- `jf/module-debug` - Set to `t` for verbose output

**Adding modules:** Add to `jf/enabled-modules`, create `.org` file, test with `jf/reload-module`.

### Packages (straight.el)

Uses straight.el (not package.el). Storage in `runtime/straight/`.

```elisp
(use-package package-name
  :straight (package-name :type git :host github :repo "user/repo")
  :config (setq package-setting value))
```

### GPTEL Architecture

Located in `config/gptel/` (not `major-modes/`), organized by subsystem:

```
gptel/
├── gptel.org/el         - Main loader
├── sessions/            - 8 modules (constants, logging, filesystem, registry, metadata, subagent, commands, activities-integration)
├── skills/              - 3 modules (skills-core, skills-roam, skills-transient)
├── tools/               - 10 modules (filesystem, projectile, ggtags, treesitter, org-roam, meta, community, persistent-agent, sql, transient)
└── agents/              - 5 agent definitions (.md files)
```

**Load order enforced in gptel.org:**
1. Skills system (core, roam, transient)
2. gptel-agent package + tool definitions
3. Session modules in dependency order (constants → logging → filesystem → registry → metadata → subagent → commands)
4. Activities integration (if activities package loaded)

**All paths use `config/` prefix:**
```elisp
(jf/load-module (expand-file-name "config/gptel/skills/skills-core.el" jf/emacs-dir))
```

## Common Commands

```bash
# Tangle and validate
./bin/tangle-org.sh config/core/defaults.org

# Test configuration
./bin/emacs-isolated.sh
./bin/emacs-isolated.sh -nw  # Terminal mode

# Worktree workflow
git worktree add ~/emacs-feature-name -b feature-name
./bin/init-worktree-runtime.sh ~/emacs-feature-name  # Copy packages (746MB)
./bin/invalidate-runtime.sh                           # Force rebuild
git worktree lock ~/.emacs.d --reason "Production"   # Protect production

# Release management
git tag -a v0.3.0-rc1 -m "Description"
cd ~/.emacs.d && git checkout v0.3.0-rc1
```

## Development Workflow

**Iteration cycle:** Edit `.org` → tangle/validate → stage → user tests → feedback → iterate → commit milestone.

Commits should be granular (working module/component), not entire features. 

```bash
git commit -m "Brief milestone description

Detailed explanation and what was tested.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

### OpenSpec Workflow

For non-trivial changes, use **OpenSpec** to plan before implementing:

**When to use:**
- New features or significant modifications
- Changes requiring architectural decisions
- Multi-file refactoring
- Unclear requirements needing exploration

**Invoke skills:**
- `/opsx:explore` - Investigate and clarify requirements before planning
- `/opsx:new` - Start structured change (proposal → design → tasks → implementation)
- `/opsx:continue` - Progress to next artifact in workflow
- `/opsx:ff` - Fast-forward through all artifacts to reach implementation
- `/opsx:apply` - Implement tasks from change
- `/opsx:verify` - Validate implementation matches artifacts
- `/opsx:archive` - Archive completed change

**Skip OpenSpec for:** Single-file edits, bug fixes, documentation updates, trivial changes.

#### Spec Philosophy

**Specs are behavioral contracts, not technical documentation.**

OpenSpec specs (`openspec/specs/`) describe:
- What a module does (behavior and intent)
- Why it exists (responsibilities)
- How it integrates with other modules (contracts)
- Key invariants and constraints

OpenSpec specs do NOT duplicate:
- Implementation details → see `.org` files
- API documentation → see `.org` files
- Code structure → see `.org` files
- Usage examples → see `.org` files

The literate `.org` files are already comprehensive technical documentation. Specs provide the behavioral context for changes.

#### Spec Organization

Most modules get a single spec file:

```
openspec/specs/
├── core/
│   ├── module-system.md
│   ├── completion.md
│   └── evil.md
└── major-modes/
    ├── org-mode.md
    └── magit.md
```

**Exception: gptel** is architecturally complex (31 modules, 4 subsystems) and gets its own directory:

```
openspec/specs/gptel/
├── architecture.md    - Subsystem boundaries, integration points
├── core.md           - Package integration, auth, main config
├── sessions.md       - Session subsystem (8 modules)
├── scope.md          - Scope control subsystem (7 modules)
├── skills.md         - Skills subsystem (3 modules)
└── tools.md          - Tools subsystem (10 modules)
```

#### Change Workflow with Delta Specs

Changes use delta specs to control context and scope. The change's `specs/` directory mirrors the main `specs/` structure.

**Isolated subsystem change:**
```
openspec/changes/add-session-export/
└── specs/
    └── gptel/
        └── sessions.md    ← Only sessions context loaded
```

Claude loads:
- `openspec/specs/gptel/architecture.md` (subsystem boundaries)
- `openspec/specs/gptel/sessions.md` (existing session spec)
- `openspec/changes/.../specs/gptel/sessions.md` (your delta)
- NOT loaded: `gptel/{core,scope,skills,tools}.md`

**Cross-subsystem integration:**
```
openspec/changes/sessions-scope-integration/
└── specs/
    └── gptel/
        ├── architecture.md
        ├── sessions.md
        └── scope.md
```

Claude loads relevant subsystems only.

**Non-gptel changes:**
```
openspec/changes/evil-completion-binding/
└── specs/
    └── core/
        ├── evil.md
        └── completion.md
```

All gptel specs remain invisible unless explicitly referenced.

**Benefits:**
- Cognitive load: Claude only sees relevant subsystems
- Searchability: Focused, navigable specs
- Change isolation: Delta specs naturally scope work
- Context control: Create delta specs only for what you're modifying

#### Bootstrapping Strategy

1. **Create specs on-demand** - When making a change, create the spec
2. **Start with gptel** - It's the architectural outlier (priority: `architecture.md` first)
3. **Fill in organically** - Spec modules as you touch them

### Beads Issue Tracking

Use **Beads** for tracking implementation work:

**When to use:**
- Converting OpenSpec changes to trackable issues: `/openspec-to-beads`
- Working on existing bead: `/bead-implementation` (handles discovery and session protocol)

**Database location:** `.beads/beads.db` (initialized with `bd init`)

## Key Locations

**Root:** `early-init.el`, `init.el`, `init.org` (MUST be at root)
**Config:** `config/core/`, `config/gptel/`, `config/major-modes/`, `config/language-modes/`, `config/local/`
**Runtime:** `runtime/straight/`, `runtime/cache/`, `runtime/state/` (gitignored)

**Machine roles:** `~/.machine-role` → `config/local/<role>.el` (apploi-mac, personal-mac, personal-mac-air)

## Writing and Debugging Elisp

**Validate after each function** using `./bin/tangle-org.sh file.org` - never skip validation for complex nested forms (cl-loop, multiple let*, lambdas, 3+ nesting levels).

### Debugging Paren Errors

**Bisection (preferred):** Disable file-level tangling, enable per-subtree with `:header-args:emacs-lisp: :tangle file.el` in PROPERTIES drawer. Run `./bin/tangle-org.sh` after each subtree.

**Error patterns:**
- `Wrong type argument: sequencep, function-name` = missing closing paren
- `scan-error` = extra closing paren earlier in file

**Git-based debugging:**
```bash
git log --oneline -- path/to/file.el
git diff <working-commit> HEAD -- path/to/file.el
```

**Common Lisp gotchas:** Always `(require 'cl-lib)`. Use `cl-coerce`, `cl-return`, `cl-loop` (not CL versions).
