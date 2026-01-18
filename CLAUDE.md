# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an **isolated, modular Emacs configuration** using literate programming. It runs completely independent of system Emacs installations via git worktrees and a three-tier architecture (development, testing, production).

## Critical Architecture Concepts

### Literate Programming Workflow

**Source of truth:** `.org` files contain both documentation and tangled code.

**Required workflow for all changes:**
1. Edit `.org` file (never edit `.el` directly)
2. Tangle: `./bin/tangle-org.sh path/to/file.org`
3. Validate: `emacs --batch --eval "(progn (find-file \"path/to/file.el\") (check-parens))"`
4. Commit **both** `.org` and `.el` files

**Why both files in git:** Preserves history, allows diffs, ensures generated code matches source.

### Dual-Launch Support

The repository supports two launch methods that both work seamlessly:

1. **From Applications** (macOS GUI): Emacs finds `early-init.el` and `init.el` at repository root automatically
2. **Via script**: `./bin/emacs-isolated.sh` explicitly sets `EMACS_USER_DIRECTORY`

**Key mechanism (early-init.el):**
- Priority 1: Use `EMACS_USER_DIRECTORY` env var if set by script
- Priority 2: Use local `runtime/` directory if it exists
- Result: Both methods isolate to `runtime/` directory

**Critical:** Init files MUST be at repository root (not in `config/`) for automatic discovery.

### Directory Paths Resolution

**jf/emacs-dir:** Points to repository root (where init.el lives)
```elisp
(defvar jf/emacs-dir
  (file-name-directory (or load-file-name buffer-file-name)))
```

**Module paths:** All module paths include `config/` prefix:
```elisp
(expand-file-name "config/core/defaults.el" jf/emacs-dir)
```

**Critical invariant:** Module loading function (`jf/resolve-module-path`) handles both formats:
- With subdirectory: `"core/defaults"` → `config/core/defaults.el`
- Without subdirectory: `"transient"` → `config/transient.el`

### Three-Tier Git Worktree Architecture

```
~/emacs/                      - Development (main branch, potentially unstable)
~/emacs-<feature-name>/       - Testing (feature branches, multiple allowed)
~/.emacs.d/                   - Production (release tags, used by host OS)
```

**Multiple testing worktrees supported:** Each feature branch gets its own meaningfully-named directory:
```
~/emacs-gptel-session-tools/  - Feature: gptel session tools
~/emacs-performance-tuning/   - Feature: performance improvements
~/emacs-new-theme/            - Feature: theme customization
```

**Each worktree has independent `runtime/` directory** containing:
- `packages/` - straight.el installed packages
- `cache/` - Emacs cache files
- `state/` - Session state
- `snippets/` - Git submodule
- `templates/` - Git submodule

**Creating worktrees:** Always use absolute paths with meaningful names to create siblings (not subdirectories):
```bash
git worktree add ~/emacs-gptel-session-tools -b gptel-session-tools    # Good
git worktree add emacs-testing -b feature-name                          # Bad (creates subdirectory)
```

### Module System

**Module loading order matters.** Defined in `init.org` via `jf/enabled-modules`:

Critical dependencies:
- `transient` MUST load before `language-modes` (docker.el requires it)
- `transient` MUST load before `major-modes/magit`
- `major-modes/magit` MUST load before `major-modes/org` (org loads orgit)

**Module structure:**
```elisp
(defvar jf/enabled-modules
  '(("category/module-name" "Description")
    ...))
```

**Module resolution:** Paths are relative to `config/`:
- `"core/defaults"` → `config/core/defaults.el`
- `"gptel/gptel"` → `config/gptel/gptel.el`

### GPTEL Architecture

Located in `config/gptel/` (not `major-modes/`), organized by subsystem:

```
gptel/
├── gptel.org/el         - Main loader
├── sessions/            - 6 modules (registry, metadata, tracing, hooks, browser, branching)
├── skills/              - 3 modules (skills-core, skills-roam, skills-transient)
├── tools/               - 7 modules (filesystem, projectile, ggtags, treesitter, org-roam, meta, community)
└── agents/              - 5 agent definitions (.md files)
```

**Load order enforced in gptel.org:**
1. Skills system (core, roam, transient)
2. gptel-agent package + tool definitions
3. Session modules in dependency order (registry → metadata → tracing → hooks → browser/branching)

**All paths use `config/` prefix:**
```elisp
(jf/load-module (expand-file-name "config/gptel/skills/skills-core.el" jf/emacs-dir))
```

## Common Development Commands

### Literate Programming

```bash
# Tangle single file
./bin/tangle-org.sh config/core/defaults.org

# Tangle and validate
./bin/tangle-org.sh config/core/defaults.org && \
  emacs --batch --eval "(progn (find-file \"config/core/defaults.el\") (check-parens))"

# Tangle init file (affects module loading)
./bin/tangle-org.sh init.org
```

### Testing Configuration

```bash
# Launch isolated instance
./bin/emacs-isolated.sh

# Launch with debug enabled (shows module loading)
# Edit init.org: (setq jf/module-debug t)
./bin/tangle-org.sh init.org && ./bin/emacs-isolated.sh

# Test in terminal mode
./bin/emacs-isolated.sh -nw
```

### Worktree Management

```bash
# Create testing worktree with meaningful name
git worktree add ~/emacs-gptel-session-tools -b gptel-session-tools

# Test in worktree
cd ~/emacs-gptel-session-tools
./bin/emacs-isolated.sh

# List all worktrees (shows lock status)
git worktree list

# Clean up worktree when done
git worktree remove ~/emacs-gptel-session-tools
git branch -d gptel-session-tools

# Update production worktree to new release
cd ~/.emacs.d
git checkout v0.3.0
```

#### Protecting Worktrees

**Lock production worktree** to prevent accidental deletion or modification:
```bash
# Lock with descriptive reason
git worktree lock ~/.emacs.d --reason "Production Emacs config - do not delete"

# Verify lock status (shows "locked" indicator)
git worktree list

# Unlock if needed
git worktree unlock ~/.emacs.d
```

**Why lock:** Locked worktrees require `--force --force` to remove, preventing accidental deletion. The production worktree at `~/.emacs.d` should always be locked since it's used by the host OS.

**Magit limitation:** Magit doesn't currently support lock/unlock commands. Use git command line for locking operations.

### Release Management

```bash
# Create release candidate
git tag -a v0.3.0-rc1 -m "Description"

# Update production to RC for testing
cd ~/.emacs.d
git checkout v0.3.0-rc1

# Promote RC to stable after testing
cd ~/emacs
git tag -a v0.3.0 -m "Stable release"
cd ~/.emacs.d
git checkout v0.3.0
```

### Syntax Validation

```bash
# Validate single file
emacs --batch --eval "(progn (find-file \"config/path/to/file.el\") (check-parens))"

# Validate multiple files
for f in config/core/*.el; do
  emacs --batch --eval "(progn (find-file \"$f\") (check-parens))"
done
```

## Critical File Locations

### Repository Root
- `early-init.el` - Pre-UI initialization, sets up isolation (MUST be at root)
- `init.el` - Main entry point, loads modules (MUST be at root)
- `init.org` - Source of truth for init.el (MUST be at root)

### Configuration
- `config/core/` - Core functionality (17+ modules)
- `config/gptel/` - LLM/AI integration (40 files total)
- `config/major-modes/` - Major mode configs (magit, org, org-roam, dirvish)
- `config/language-modes/` - Programming languages
- `config/look-and-feel/` - UI theming
- `config/local/` - Machine-specific configs (not in git, plain `.el` files)

### Runtime (gitignored)
- `runtime/packages/` - straight.el packages
- `runtime/straight/` - straight.el internal state
- `runtime/cache/`, `runtime/state/`, `runtime/data/`

## Machine Roles

Uses `~/.machine-role` file for stable identification. Config loaded from `config/local/<role>.el` (plain `.el`, no `.org` source).

Available roles: `apploi-mac`, `personal-mac`, `personal-mac-air`

## Common Pitfalls

### Path Resolution Issues

**Problem:** Module can't be found
**Cause:** Missing `config/` prefix in path or init files not at root
**Fix:** Ensure all `jf/load-module` calls use paths that resolve correctly via `jf/resolve-module-path`

### Transient Version Conflicts

**Problem:** `void-function transient--set-layout`
**Cause:** Emacs 30.2 has old built-in transient v0.6.0
**Fix:** Module order MUST have `transient` before `language-modes` and `major-modes/magit`

### Dual-Launch Breaks

**Problem:** Applications launch can't find init files
**Cause:** Init files moved to `config/` subdirectory
**Fix:** `early-init.el`, `init.el`, `init.org` MUST be at repository root

### Module Path Typos

**Problem:** Module loading fails with wrong path
**Cause:** Forgot to update paths when moving modules
**Fix:** Search and replace ALL references, including in `.org` source files, then retangle

## When Making Structural Changes

1. **Moving modules:** Update ALL references in `gptel.org`, `init.org`, parent modules
2. **Adding new subsystem:** Create `.org` file with tangle header, add to `jf/enabled-modules`
3. **Changing load order:** Edit `jf/enabled-modules` list in `init.org`, consider dependency chain
4. **Reorganizing directories:** Update paths in source `.org` files, not `.el` files
5. **Always:** Tangle → Validate → Test → Commit both `.org` and `.el`

## Secrets Management

Sensitive values go in `~/.emacs-secrets.el` (outside repository), loaded automatically by init.el if present.

Template: `.emacs-secrets.el.example` in repository root.

## Validation After Changes

**Minimal validation:**
```bash
./bin/tangle-org.sh path/to/file.org
emacs --batch --eval "(progn (find-file \"path/to/file.el\") (check-parens))"
```

**Full validation:**
```bash
# Tangle all changed files
./bin/tangle-org.sh init.org
./bin/tangle-org.sh config/gptel/gptel.org
# ... others

# Launch and check *Messages* buffer for errors
./bin/emacs-isolated.sh
```

**Release validation:**
```bash
# Test in fresh worktree with meaningful name
git worktree add ~/emacs-test-v0.3.0 -b test-v0.3.0
cd ~/emacs-test-v0.3.0
./bin/emacs-isolated.sh
# Verify no errors in *Messages* buffer
```
