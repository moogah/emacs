# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an **isolated, modular Emacs configuration** using literate programming. It runs completely independent of system Emacs installations via git worktrees and a three-tier architecture (development, testing, production).

## Critical Architecture Concepts

### Literate Programming Workflow

**Source of truth:** `.org` files contain both documentation and tangled code.

**Core principle:** `.org` files are source of truth → tangle to `.el` files → Emacs loads `.el` files

**Required workflow for all changes:**
1. Edit `.org` file (never edit `.el` directly)
2. Tangle and validate: `./bin/tangle-org.sh path/to/file.org`
   - Automatically tangles to `.el` file
   - Automatically validates with `check-parens`
   - Exits with error if validation fails
3. Commit **both** `.org` and `.el` files

**Why both files in git:** Preserves history, allows diffs, ensures generated code matches source.

**CRITICAL:** DO NOT edit `.el` files directly - they get overwritten when tangled.

#### Tangling Methods

**Auto-Tangle (Primary Method)**
- **Trigger**: Save the `.org` file
- **Requirement**: `#+auto_tangle: y` header in file
- **Result**: Automatically generates `.el` file

**Manual Tangle in Emacs**
- **When**: Auto-tangle fails or disabled
- **Command**: `C-c C-v t` (org-babel-tangle)

**CLI Tangling (bin/tangle-org.sh)**
```bash
# Single file - tangles AND validates automatically
./bin/tangle-org.sh config/major-modes/org.org

# All org files - each is validated after tangling
find config/ -name "*.org" -exec ./bin/tangle-org.sh {} \;
```

**Features**:
- Auto-finds Emacs binary (uses `/Applications/Emacs.app/Contents/MacOS/Emacs` on macOS)
- Validates tangled `.el` files with `check-parens` automatically
- Fails with error if validation detects syntax errors
- Batch mode with clear error messages

#### Manual Validation

**Only needed if you're debugging or testing `.el` files directly (rare):**
```bash
# Validate single file - use full Emacs path
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(progn (find-file \"file.el\") (check-parens))"

# Byte-compile check (more thorough, slower)
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(progn (find-file \"file.el\") (byte-compile-file \"file.el\"))"
```

**IMPORTANT:** Always use `/Applications/Emacs.app/Contents/MacOS/Emacs` (not `emacs` or `/usr/local/bin/emacs`)

#### Required File Headers

```org
#+title: Module Name
#+property: header-args:emacs-lisp :tangle module-name.el
#+auto_tangle: y
```

#### Keeping Babel Blocks Focused

**Principle:** Keep org-babel blocks small and focused - typically one function per block.

**Why this matters:**
- **Easier debugging**: When validation fails, you can immediately identify which specific function has the error
- **Better bisection**: The org-babel tangle bisection strategy is more effective with smaller blocks
- **Incremental validation**: You can validate after writing each function without re-tangling large amounts of code
- **Maintainability**: Changes to one function don't require re-tangling unrelated code

**Guidelines:**

**Good - Single function per block:**
```org
*** Record Answer
#+begin_src emacs-lisp
(defun jf/gptel-ask--record-answer (question-id question-text answer key)
  "Record user's answer for QUESTION-ID."
  (setq jf/gptel-ask--answers
        (cons (list question-id question-text answer key)
              (assoc-delete-all question-id jf/gptel-ask--answers))))
#+end_src

*** Check All Answered
#+begin_src emacs-lisp
(defun jf/gptel-ask--all-answered-p (questions)
  "Check if all QUESTIONS have been answered."
  (= (length jf/gptel-ask--answers)
     (length questions)))
#+end_src
```

**Acceptable - Small, logically related items grouped:**
```org
*** State Variables
#+begin_src emacs-lisp
(defvar jf/gptel-ask--active-callback nil
  "Global callback for currently active question flow.")

(defvar jf/gptel-ask--transitioning nil
  "Flag indicating transition between questions.")
#+end_src
```

**Bad - Multiple unrelated functions in one block:**
```org
*** Question Handling
#+begin_src emacs-lisp
(defun jf/gptel-ask--record-answer (...)
  ...)

(defun jf/gptel-ask--all-answered-p (...)
  ...)

(defun jf/gptel-ask--clear-answers (...)
  ...)

(defun jf/gptel-ask--current-question-index (...)
  ...)

(defun jf/gptel-ask--build-suffixes (...)
  ...)

(defun jf/gptel-ask--build-choice-suffixes (...)
  ...)
#+end_src
```

**When to group items:**
- Multiple `defvar` declarations that are closely related
- Short helper macros or constants used by a single function
- Very simple related functions (< 5 lines each)

**Refactoring large blocks:**

When you encounter large blocks with many functions:
1. Add subsection headers (*** or ****) for each function or logical group
2. Move each function to its own `#+begin_src` block
3. Keep the same tangle target
4. Validate after refactoring: `./bin/tangle-org.sh file.org`
5. Verify the tangled `.el` file is identical (no functional changes)

**Benefits for debugging:**

With focused blocks, the org-babel tangle bisection strategy becomes more powerful:
- Disable tangling at file level
- Enable tangling per subtree or per block
- Validate after each addition
- Quickly isolate the exact function with a syntax error

See [Org-Babel Tangle Bisection Strategy](#org-babel-tangle-bisection-strategy) for details on using this debugging technique.

#### After Tangling

1. **Restart Emacs** or use `jf/reload-module` to load changes
2. **Test changes** ensure they work
3. **Commit both files** - keep .org and .el in sync

#### Common Literate Programming Mistakes

| Mistake | Why Bad | Fix |
|---------|---------|-----|
| Editing `.el` files directly | Changes overwritten on next tangle | Edit `.org` file instead |
| Committing only `.org` | `.el` out of sync, breaks config | Commit both `.org` and `.el` |
| Forgetting to tangle | Changes in `.org` not applied | Save (auto-tangle) or `./bin/tangle-org.sh` |
| Missing `#+auto_tangle: y` | Manual tangling required | Add header to `.org` file |
| Property line not activated | Tangling fails silently | Press `C-c C-c` on `#+PROPERTY` line |
| Syntax errors in org blocks | Breaks config on load | `./bin/tangle-org.sh` catches these |
| Large babel blocks with many functions | Hard to debug syntax errors | Keep blocks focused - one function per block |

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

**Modular Loading:** This Emacs config uses `jf/enabled-modules` list and `jf/load-module()` function for explicit module control with error handling.

**Core principle:** Define modules in list → loader finds files → loads with error handling → config continues even if module fails

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

#### Key Module System Variables and Functions

| Name | Type | Purpose |
|------|------|---------|
| `jf/enabled-modules` | Variable | List of modules to load in init.org |
| `jf/load-module` | Function | Loads module with error handling (automatic during init) |
| `jf/reload-module` | Function | Interactive reload for testing (`M-x jf/reload-module`) |
| `jf/resolve-module-path` | Function | Converts module path to file path |
| `jf/emacs-dir` | Variable | Root emacs directory |
| `jf/machine-name` | Variable | Current hostname |
| `jf/module-debug` | Variable | Set to `t` for verbose loading output |

#### Adding New Modules

1. Add to `jf/enabled-modules` in init.org:
```elisp
(setq jf/enabled-modules
  '("core/completion"
    "major-modes/org-roam"
    "your-new-module"))  ; Add here
```

2. Create module file following literate programming structure
3. Test with `M-x jf/reload-module` before full restart

#### Module Loading Details

**jf/load-module**: Loads module with error handling
- Continues loading even if module fails
- Error details go to `*Messages*` buffer
- Machine-specific configs auto-loaded from `config/local/{hostname}.el`

**jf/reload-module**: Interactive reload for testing
- Use for iterative development
- No need to restart Emacs

**jf/resolve-module-path**: Converts module path to file path
- Handles category directories automatically
- Resolves relative to `jf/emacs-dir`

#### Package Management (straight.el)

**Configuration:**
- **Package manager**: straight.el (not package.el)
- **Default behavior**: `straight-use-package-by-default t`
- **Storage**: `runtime/straight/`

**Package Template:**
```elisp
(use-package package-name
  :straight (package-name :type git :host github :repo "user/repo")
  :bind (("C-c k" . package-function))
  :hook (mode . package-mode)
  :config
  (setq package-setting value))
```

**Common Package Issues:**

| Issue | Solution |
|-------|----------|
| Package won't install | Check `*Messages*` for errors, verify repo access |
| Stale package | `M-x straight-rebuild-package` |
| Corrupted cache | Delete straight directories, restart |
| Wrong version | Check straight recipe, rebuild package |

### GPTEL Architecture

Located in `config/gptel/` (not `major-modes/`), organized by subsystem:

```
gptel/
├── gptel.org/el         - Main loader
├── sessions/            - 7 modules (registry, metadata, tracing, hooks, browser, branching, transient)
├── skills/              - 3 modules (skills-core, skills-roam, skills-transient)
├── tools/               - 7 modules (filesystem, projectile, ggtags, treesitter, org-roam, meta, community)
└── agents/              - 5 agent definitions (.md files)
```

**Load order enforced in gptel.org:**
1. Skills system (core, roam, transient)
2. gptel-agent package + tool definitions
3. Session modules in dependency order (registry → metadata → tracing → hooks → browser → branching → transient)

**All paths use `config/` prefix:**
```elisp
(jf/load-module (expand-file-name "config/gptel/skills/skills-core.el" jf/emacs-dir))
```

#### Session Browser Module Details

**sessions/browser.el** - Core browsing functionality
- `jf/gptel-browse-sessions` - Open sessions directory in dired
- `jf/gptel-open-session` - Select specific session with completing-read
- `jf/gptel-view-context-at-point` - View context.md files
- `jf/gptel-view-tools-at-point` - View tools.md files
- `jf/gptel-session-tree-mode` - Minor mode with keybindings (auto-enabled in session directories)

**sessions/transient.el** - Transient menu (press `?` in session browser)
- Organized command groups: Browse, View, Actions
- Context-aware info display (current session, node type, session count)
- Discoverable interface for all session operations

**sessions/branching.el** - Resume and branch operations
- `jf/gptel-resume-from-context` - Load context into gptel buffer
- `jf/gptel-branch-from-point` - Copy node to create alternate path
- `jf/gptel-send-from-context` - Send edited context to API

## Common Development Commands

### Literate Programming

```bash
# Tangle single file (auto-validates)
./bin/tangle-org.sh config/core/defaults.org

# Tangle all org files
find config/ -name "*.org" -exec ./bin/tangle-org.sh {} \;
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
2. Tangle: `./bin/tangle-org.sh path/to/file.org` (auto-validates)
3. Stage files if validation passes

**This cycle repeats for each iteration based on user feedback.**

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
# 4. Claude tangles (auto-validates):
./bin/tangle-org.sh config/gptel/sessions/metadata.org

# 5. Claude stages files:
git add config/gptel/sessions/metadata.org config/gptel/sessions/metadata.el

# 6. User tests:
./bin/emacs-isolated.sh
# User opens gptel session, creates metadata, verifies storage
# User checks *Messages* buffer for errors

# 7. Issue found: "Metadata not persisting across sessions"

# 8. User provides feedback, Claude adjusts, repeats steps 3-6

# 9. Testing passes - milestone reached!

# 10. User commits:
git commit -m "Add metadata tracking to gptel sessions

Implements core data structure for session metadata.
Functions: jf/gptel-session-set-metadata, jf/gptel-session-get-metadata
Tested: metadata correctly stored, retrieved, and persists across sessions

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

# 11. Continue to next milestone (e.g., "Display metadata in session browser")
```

### Integration with Existing Workflows

**This workflow integrates with:**
- [Literate Programming Workflow](#literate-programming-workflow) - Always edit `.org` then tangle
- [Worktree Management](#worktree-management) - Use worktrees for feature isolation
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

### Module Loading Issues

**Problem:** Module fails to load or causes errors
**Solutions:**
1. Check variable: `C-h v jf/machine-name` to verify hostname
2. Enable debug: `(setq jf/module-debug t)` in init.org
3. Check messages: `*Messages*` buffer shows load errors
4. Verify path: Module file must exist at resolved path
5. Check list: Module must be in `jf/enabled-modules`

### Common Mistakes Summary

| Mistake | Why Bad | Fix |
|---------|---------|-----|
| Module not in list | Won't load | Add to `jf/enabled-modules` |
| Wrong file path | Load fails | Check `jf/resolve-module-path` output |
| No error checking | Silent failures | Check `*Messages*` buffer |
| Restart for every change | Slow workflow | Use `jf/reload-module` for testing |
| Using package.el | Wrong package manager | Use straight.el with use-package |
| Skipping machine configs | Settings not applied | Machine configs auto-load from `config/local/{hostname}.el` |

## When Making Structural Changes

1. **Moving modules:** Update ALL references in `gptel.org`, `init.org`, parent modules
2. **Adding new subsystem:** Create `.org` file with tangle header, add to `jf/enabled-modules`
3. **Changing load order:** Edit `jf/enabled-modules` list in `init.org`, consider dependency chain
4. **Reorganizing directories:** Update paths in source `.org` files, not `.el` files
5. **Always:** Tangle (auto-validates) → Test → Commit both `.org` and `.el`

## Secrets Management

Sensitive values go in `~/.emacs-secrets.el` (outside repository), loaded automatically by init.el if present.

Template: `.emacs-secrets.el.example` in repository root.

## Writing and Validating Elisp

This section provides proactive guidance for writing syntactically valid Emacs Lisp code, with emphasis on incremental validation to catch errors early. LLMs frequently produce parenthesis errors in complex nested elisp - these practices help prevent those issues through automated validation.

### When to Use Extra Care

- Before writing any elisp function >20 lines
- When writing complex nested forms (cl-loop, multiple let*, lambdas)
- When modifying existing elisp in literate org files
- After generating elisp code with an LLM
- When working with deeply nested expressions (3+ levels)

### Incremental Validation Workflow

The key principle: **validate after each function, before moving to the next**.

1. **Write in small chunks** - One function at a time, not entire files
2. **Validate immediately** - Run validation after each function
3. **Use automated tools** - Never rely on visual inspection or manual counting
4. **Test in isolation** - Extract to temp file if needed for testing
5. **Fix before continuing** - Don't accumulate errors

### Validation Commands

**Use `./bin/tangle-org.sh file.org` - validation is automatic.** See [Literate Programming Workflow](#literate-programming-workflow) for details.

For manual validation or debugging, see the "Manual Validation" section in [Literate Programming Workflow](#literate-programming-workflow).

### Complexity Thresholds

**Low Risk (2 nesting levels)**
```elisp
(let ((x 1))
  (+ x 2))
```

**Action**: Validate after 5-10 functions

**Medium Risk (3 nesting levels)**
```elisp
(let ((x (foo)))
  (when x
    (bar x)))
```

**Action**: Validate after each 2-3 functions

**High Risk (4+ nesting levels)**
```elisp
(cl-loop for i from 1 below (length path)
         for target-id = (aref path i)
         do
         (let ((children (plist-get node :children)))
           (when node
             (let* ((file (plist-get node :file))
                    (content (when (file-exists-p full-path)
                              (with-temp-buffer
                                (insert-file-contents full-path)
                                (buffer-string)))))
               (push (list :type type :content content) context)))))
```

**Action**: Validate after EACH high-risk function
**Consider**: Breaking into smaller helper functions

### Integration with Literate Programming

After writing each elisp block in your `.org` file, tangle immediately:
```bash
./bin/tangle-org.sh file.org  # Automatic validation included
```

This validates syntax early, preventing error accumulation.

### Example Workflow

**Task:** "Add a function to load context from a tree path"

1. Write function in org-mode code block
2. `./bin/tangle-org.sh file.org` (automatic validation)
3. Fix errors in .org file if needed, repeat step 2
4. Continue to next function

### Common LLM Failure Patterns

LLMs frequently produce paren errors in:
- cl-loop with nested let* and lambdas
- Functions >50 lines
- Complex backquote/unquote expressions
- Multiple nested when/if/cond forms

**Prevention Strategy:**
- Write these in 10-20 line chunks
- Validate after each chunk
- Use helper functions to reduce nesting

### Tips for Writing Elisp

1. **Never skip validation** - "It looks right" is not reliable for elisp
2. **Start simple** - Write trivial version first, then add complexity
3. **Test incrementally** - Don't write 100+ lines before first validation
4. **Use git** - Commit working code frequently so you have rollback points
5. **Break down complexity** - Helper functions are cheaper than debugging

## Debugging Elisp

Systematic debugging strategies for Emacs Lisp development, focusing on common error patterns like missing parentheses and Common Lisp compatibility issues.

### Org-Babel Tangle Bisection Strategy

**Best approach for debugging syntax errors in .org files with elisp code blocks.**

When you encounter "Unmatched bracket or quote" or similar syntax errors in tangled .el files, use this binary search approach to quickly isolate the problem.

#### Step 1: Disable All Tangling

Set file-level default to not tangle:
```org
#+property: header-args:emacs-lisp :tangle no
#+auto_tangle: nil
```

Tangle and verify empty/minimal output:
```bash
./bin/tangle-org.sh file.org
# Should tangle 0 blocks or only explicitly enabled ones
```

#### Step 2: Enable Subtrees Incrementally

Use PROPERTIES drawers at subtree level to enable tangling. Start with foundational code:

```org
* Helper Functions
:PROPERTIES:
:header-args:emacs-lisp: :tangle filename.el
:END:

** function-1
#+begin_src emacs-lisp
(defun function-1 () ...)
#+end_src

** function-2
#+begin_src emacs-lisp
(defun function-2 () ...)
#+end_src
```

All code blocks under "Helper Functions" will now tangle.

#### Step 3: Validate After Each Addition

After enabling each subtree:
```bash
./bin/tangle-org.sh file.org  # Auto-validates
```

If it exits successfully, continue to next subtree. If it fails, the error is in the subtree you just enabled.

#### Step 4: Narrow Down Within Subtree

Once you've identified the problematic subtree, you can either:
- **Binary search within subtree**: Add PROPERTIES drawer to sub-sections
- **Move to individual blocks**: Add `:tangle filename.el` to specific code blocks

Example of narrowing to individual blocks:
```org
* Problematic Section
:PROPERTIES:
:header-args:emacs-lisp: :tangle no
:END:

** tool-1
#+begin_src emacs-lisp :tangle filename.el
(gptel-make-tool ...)
#+end_src

** tool-2
#+begin_src emacs-lisp
(gptel-make-tool ...)  ; Not tangling yet
#+end_src
```

#### Why This Works Better Than Manual Inspection

- **Binary search efficiency**: O(log n) instead of O(n) for checking each block
- **Immediate validation**: Catch errors right after introducing them
- **No manual counting**: Eliminates human error in paren matching
- **Maintainable**: Easy to track which sections are enabled
- **Reproducible**: Clear state at each validation step

#### PROPERTIES Drawer Syntax

Key formats to remember:
```org
* Subtree
:PROPERTIES:
:header-args:emacs-lisp: :tangle no          # Disable tangling
:header-args:emacs-lisp: :tangle file.el     # Enable tangling
:header-args: :tangle file.el                # Works for all languages
:END:
```

**Inheritance:** Settings apply to current subtree and all children.
**Precedence:** Block-level > Subtree PROPERTIES > File-level #+PROPERTY

### Recognizing Common Error Patterns

#### Symbol appearing as value = missing closing parenthesis

```
ERROR: Wrong type argument: sequencep, some-function-name
DEBUG output: candidates=some-function-name
DEBUG output: candidates type=symbol
```

This pattern means a function definition is missing a closing `)`, causing Emacs to read past the function boundary and interpret the next `defun` name as the return value.

#### Containing expression ends prematurely = extra closing parenthesis

```
ERROR: (scan-error Containing expression ends prematurely 33145 33146)
```

This means there are more closing parens than opening parens somewhere earlier in the file. The position shown is where the scan stops, but the actual error is usually in a previous function.

### Debugging Strategy for Paren Errors

When you suspect missing parentheses but can't spot them:

1. **Org-babel tangle bisection** (for .org files) - Use the binary search method described above. This is the most efficient way to isolate syntax errors in literate elisp files.

2. **Isolate with org-babel** (for testing individual functions):
   - Create a test block with `:tangle no`
   - Copy the suspicious function
   - Add inline comments marking nesting levels
   - Evaluate with `C-c C-c` to test in isolation

3. **Use Emacs built-in checks**:
   ```elisp
   M-x check-parens  ; in the buffer
   ; or batch mode:
   /Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(progn (find-file \"file.el\") (check-parens))"
   ```

4. **Manual paren counting** (last resort for non-org files):
```elisp
(defun example ()
  (let (var1)                    ; open 1
    (dolist (item items)         ; open 2
      (when condition            ; open 3
        (something)))            ; close 3,2 - MISSING close for 1!
  (return result)))              ; this closes defun but not let!
```

**Tips for manual counting:**
- Add inline comments marking nesting depth
- Work from the innermost form outward
- Use editor commands like `show-paren-mode` and `forward-sexp`
- Consider using automated validation instead

### Common Lisp vs Emacs Lisp Gotchas

**Always add `(require 'cl-lib)` at the top of your elisp files.**

Common incompatibilities:

| Common Lisp | Emacs Lisp | Note |
|-------------|------------|------|
| `coerce` | `cl-coerce` | Type conversion |
| `dotimes` with `(return)` | `cl-dotimes` with `(cl-return)` | Early loop exit |
| `return` | `cl-return` | Must be in `cl-` block form |
| `loop` | `cl-loop` | Full loop macro |

**Quick fix checklist:**
- [ ] Added `(require 'cl-lib)` at top?
- [ ] Changed `coerce` to `cl-coerce`?
- [ ] Using `cl-dotimes` with `cl-return`?
- [ ] Or using manual flag instead of `cl-return`?

**Alternative to cl-return:**
```elisp
;; Instead of:
(dotimes (i n)
  (when condition
    (cl-return)))  ; ERROR: no catch for tag

;; Use manual flag:
(let ((found nil))
  (cl-dotimes (i n)
    (unless found
      (when condition
        (setq found t)))))
```

### Git-Based Debugging Strategy

When a file was recently working, use git to isolate what changed rather than examining the entire file.

**Find Last Working Commit**
```bash
git log --oneline -- path/to/file.el
```

Look for commits where the file loaded successfully (before the error appeared).

**Test Specific Commit**
```bash
git show <commit-hash>:path/to/file.el > /tmp/working.el
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval \
  "(progn (find-file \"/tmp/working.el\") (check-parens))"
```

If this passes, you know the error was introduced after this commit.

**Diff Against Working Version**
```bash
git diff <working-commit> HEAD -- path/to/file.el
```

This shows exactly what changed. The paren error is likely in one of the modified functions.

**Example Workflow:**
```
1. File worked at commit 6f7aa22
2. File fails at current HEAD
3. Run: git diff 6f7aa22 HEAD -- file.el
4. See ~700 lines added in session auto-save functions
5. Extract one of the new functions to test in isolation
6. Run check-parens on extracted function
7. Find the function with the paren error
```

This approach is much faster than examining the entire file, especially when hundreds of lines were added.

### Automated Validation Tools

**flyparens** - Minor mode that checks for unbalanced parens on the fly:
```elisp
(use-package flyparens
  :straight t
  :hook (emacs-lisp-mode . flyparens-mode))
```

Highlights the first mismatched paren as you type, whether at point or not.

**Flycheck** - Already checks elisp with byte compiler automatically:
```elisp
;; Usually already configured
(use-package flycheck
  :straight t
  :hook (emacs-lisp-mode . flycheck-mode))
```

**Save Hooks** - Add check-parens to save hook so you can't save invalid code:
```elisp
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (add-hook 'before-save-hook #'check-parens nil t)))
```

Warning: This blocks saving until you fix the error.

**Forward-Sexp Scanning** - Programmatically find exact position of paren mismatches:
```bash
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "
(condition-case err
    (with-temp-buffer
      (insert-file-contents \"file.el\")
      (goto-char (point-min))
      (condition-case scan-err
          (while (not (eobp)) (forward-sexp 1))
        (scan-error
         (message \"Scan error at position %d: %s\" (point) scan-err))))
  (error (message \"Error: %s\" err)))
"
```

Shows the exact character position where scanning fails.

### LLM Elisp Limitations

LLMs frequently produce parenthesis errors in these patterns:

**High-Risk Constructs:**
- **Deeply nested forms** (3+ levels): cl-loop with nested let* and lambdas
- **Long functions** (>50 lines): Easy to lose track of nesting
- **Complex backquote expressions**: Multiple levels of `,` and `,@`
- **Multiple nested conditionals**: when/if/cond with let forms inside

**Common Failure Example:**
```elisp
(cl-loop for i from 1 below (length path)
         do
         (let ((children ...))
           (when node
             (let* ((file ...)
                    (content ...))
               (push ... context))))))  ; Extra paren here!
```

The extra paren typically appears at the end of the most deeply nested form.

**Mitigation Strategies:**
1. **Write incrementally** - One function at a time, validate after each
2. **Break down complexity** - Extract helper functions to reduce nesting
3. **Never manually count parens** - Use automated tools (check-parens, forward-sexp)
4. **Test in isolation** - Extract complex functions to temp files for testing
5. **Use git frequently** - Commit working code so you can diff against last-good version
