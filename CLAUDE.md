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

#### Runtime Sharing for Worktrees

**Problem:** By default, each new worktree requires straight.el to clone 142 package repos (746MB) and build 140 packages (175MB) from scratch on first launch. This takes significant time and bandwidth.

**Solution:** Copy the `runtime/straight/` directory from an existing worktree (usually `~/emacs`) to new worktrees, reusing cloned repos and built packages.

**Quick Start:**
```bash
# Create new worktree
git worktree add ~/emacs-feature-name -b feature-name

# Copy runtime from main worktree (fast local copy vs slow download)
./bin/init-worktree-runtime.sh ~/emacs-feature-name

# Initialize submodules
cd ~/emacs-feature-name
git submodule update --init

# Launch Emacs (packages already installed!)
./bin/emacs-isolated.sh
```

**Alternative - Automatic initialization:**
```bash
# First launch automatically copies runtime if needed
cd ~/emacs-feature-name
./bin/emacs-isolated.sh --init-runtime
```

**What gets copied:**
- `runtime/straight/repos/` (746MB) - Git clones of all packages
- `runtime/straight/build/` (175MB) - Built/compiled packages
- `runtime/straight/build-cache.el` (529KB) - Build cache metadata

**What does NOT get copied (worktree-specific state):**
- `custom.el` - User customizations
- `org-roam.db` - Org-roam database
- `history`, `bookmarks`, `recentf`, `projectile-bookmarks.eld` - Session files
- `cache/`, `state/`, `data/`, `persist/`, `backups/` - Runtime state
- `snippets/`, `templates/` - Git submodules (use `git submodule update --init`)

**Invalidating Runtime (Force Rebuild):**
```bash
# Remove all packages from current worktree (forces full rebuild)
./bin/invalidate-runtime.sh

# Remove specific package (forces rebuild of just that package)
./bin/invalidate-runtime.sh magit

# Remove packages from specific worktree
./bin/invalidate-runtime.sh --worktree ~/emacs-feature-name

# Skip confirmation prompt
./bin/invalidate-runtime.sh --force
```

**Copy from alternate source:**
```bash
# Copy from production instead of main worktree
./bin/init-worktree-runtime.sh ~/emacs-feature-name --source ~/.emacs.d
```

**When to invalidate runtime:**
- Testing fresh package installation
- Package versions need updating (shared packages may mask version differences)
- Build artifacts appear corrupted
- Switching between branches with different package requirements

**Trade-offs:**
- **Pro:** Dramatically faster worktree creation (local copy vs internet download)
- **Pro:** Reduces bandwidth usage
- **Pro:** Identical package versions across worktrees (unless explicitly invalidated)
- **Con:** Requires manual step after `git worktree add` (or use `--init-runtime`)
- **Con:** Shared packages may mask version differences between branches
- **Con:** Each worktree still uses disk space (but that's already true)

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

## Development Workflow with Git

This section describes the iterative development workflow for making changes to the Emacs configuration. The workflow emphasizes **granular milestone commits** and a clear separation of responsibilities between Claude Code and the user.

### Starting from Clean State

**Always begin from a clean git state:**

```bash
# Check current status
git status

# Start from clean main branch
git checkout main
git pull

# Create feature branch
git checkout -b feature-name

# OR create dedicated worktree (recommended for larger features)
git worktree add ~/emacs-feature-name -b feature-name
cd ~/emacs-feature-name
./bin/init-worktree-runtime.sh ~/emacs-feature-name
git submodule update --init
```

**Why start clean:** Ensures changes are isolated and easily trackable. See [Worktree Management](#worktree-management) for details on worktree setup.

### Implementation and Validation Cycle

**Claude Code's responsibilities:**
1. Edit `.org` source files (never `.el` directly)
2. Tangle modified files: `./bin/tangle-org.sh path/to/file.org`
3. Run syntax validation: `emacs --batch --eval "(progn (find-file \"path/to/file.el\") (check-parens))"`
4. Stage files if validation passes (see next section)

**Important:** This cycle repeats for each iteration based on user feedback.

### Staging Changes

**CRITICAL: Claude stages changes but DOES NOT commit.**

After validation passes:
```bash
# Stage both .org and .el files
git add config/path/to/file.org config/path/to/file.el

# Stage multiple related files
git add init.org init.el config/gptel/gptel.org config/gptel/gptel.el
```

**Why stage without committing:**
- User needs to test changes in a running Emacs instance
- Allows for iterative refinement before milestone
- User controls when a development milestone is reached

### User Testing and Iteration

**User's responsibilities:**
1. Launch Emacs with staged changes: `./bin/emacs-isolated.sh`
2. Test functionality works as expected
3. Check `*Messages*` buffer for errors or warnings
4. Provide feedback to Claude Code for refinements

**Iteration cycle:**
- If issues found: User describes problem → Claude adjusts → repeat from Implementation Cycle
- If working: Continue testing until confident milestone is reached

### Committing Development Milestones

**When user determines a milestone is reached:**

```bash
# User creates commit (not Claude!)
git commit -m "Brief description of milestone

More detailed explanation of what was implemented.
What was tested and verified.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

**Milestone commit guidelines:**
- Commit when a module/component works in isolation
- Include clear description of what was tested
- Note any limitations or next steps in commit body
- Always co-author with Claude Code

**Important:** User creates commits, not Claude. Claude only stages files.

### Milestone Granularity

**Development milestones should be relatively granular** - not necessarily a complete feature, but a module or component tested to perform as expected in isolation.

**Good milestone examples:**
```
✓ "Add metadata tracking to gptel sessions"
  - Core data structure implemented and working
  - Tested: metadata correctly stored and retrieved

✓ "Implement session registry lookup functions"
  - Registry functions working with sample data
  - Tested: lookup by ID, list all sessions

✓ "Wire up session UI to display metadata"
  - UI renders metadata correctly
  - Tested: UI updates when session changes
```

**Too coarse (complete feature):**
```
✗ "Complete gptel session management feature"
✗ "Finish entire refactoring of module system"
✗ "Implement full authentication system"
```

**Too fine (not worth separate commit):**
```
✗ "Fix typo in comment"
✗ "Rename single variable"
✗ "Adjust indentation"
```

### Example Development Cycle

**Complete workflow from start to first milestone:**

```bash
# 1. Start clean
cd ~/emacs
git status  # Should be clean
git checkout -b add-session-metadata

# 2. User requests: "Add metadata tracking to gptel sessions"

# 3. Claude implements changes to .org files
# 4. Claude tangles:
./bin/tangle-org.sh config/gptel/sessions/metadata.org

# 5. Claude validates:
emacs --batch --eval "(progn (find-file \"config/gptel/sessions/metadata.el\") (check-parens))"

# 6. Validation passes - Claude stages files:
git add config/gptel/sessions/metadata.org config/gptel/sessions/metadata.el

# 7. User tests:
./bin/emacs-isolated.sh
# User opens gptel session, creates metadata, verifies storage
# User checks *Messages* buffer for errors

# 8. Issue found: "Metadata not persisting across sessions"

# 9. User provides feedback, Claude adjusts, repeats steps 3-7

# 10. Testing passes - milestone reached!

# 11. User commits:
git commit -m "Add metadata tracking to gptel sessions

Implements core data structure for session metadata.
Functions: jf/gptel-session-set-metadata, jf/gptel-session-get-metadata
Tested: metadata correctly stored, retrieved, and persists across sessions

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

# 12. Continue to next milestone (e.g., "Display metadata in session browser")
```

### Integration with Existing Workflows

**This workflow integrates with:**
- [Literate Programming Workflow](#literate-programming-workflow) - Always edit `.org` then tangle
- [Worktree Management](#worktree-management) - Use worktrees for feature isolation
- [Validation After Changes](#validation-after-changes) - Run appropriate validation commands
- [Release Management](#release-management) - Milestones accumulate into releases

**Key workflow principle:** Small, tested, working increments accumulate into complete features.

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
