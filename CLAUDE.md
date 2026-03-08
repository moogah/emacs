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

### Testing Infrastructure

**Frameworks:** Support for both **Buttercup** (preferred) and **ERT** (legacy).

**Framework Selection:**
- **Buttercup** (preferred for new tests): BDD-style framework with `describe`/`it`/`expect` syntax, built-in setup/teardown, and spy system
- **ERT** (maintain existing): Built-in testing framework - no forced migration required
- Both frameworks supported side-by-side

**Architecture:** Makefile provides single source of truth for Emacs invocation. No script-to-script dependencies.

```
Makefile (core)          - Emacs detection, environment setup, low-level targets
  ↓
run-tests.sh (CLI)       - User-friendly interface, argument parsing, snapshots, framework selection
emacs-isolated.sh (GUI)  - Interactive launches (independent)
```

**Test organization:**
- **Buttercup tests**: `*-spec.el` suffix (preferred for new tests)
- **ERT tests**: `*-test.el` suffix (existing tests)
- Framework: `config/core/testing.el` provides discovery and runner functions for both
- Location: `config/*/test/` or co-located with modules

**Test naming conventions:**
```elisp
;; Buttercup (preferred)
(describe "Module name"
  (it "does something"
    (expect result :to-equal expected)))

;; ERT (legacy)
(ert-deftest test-module-something ()
  (should (equal result expected)))
```

**Running tests:**
```bash
# Via make (direct)
make test                                # All ERT tests
make test-buttercup                      # All Buttercup tests
make test-bash-parser                    # Module shortcut (ERT)
make test-directory DIR=config/gptel     # Custom directory (ERT)
make test-buttercup-directory DIR=config/gptel  # Custom directory (Buttercup)

# Via run-tests.sh (user-friendly CLI with auto-detection)
./bin/run-tests.sh                       # All tests (both frameworks)
./bin/run-tests.sh -f buttercup          # Only Buttercup tests
./bin/run-tests.sh -f ert                # Only ERT tests
./bin/run-tests.sh -d config/gptel       # Directory-scoped (auto-detects framework)
./bin/run-tests.sh -p '^test-glob-'      # Pattern-scoped (ERT)
./bin/run-tests.sh -d config/foo -s      # With snapshot

# Interactive (via transient menu)
C-c t    # Open test menu
# Then choose ERT or Buttercup options
```

**Test discovery functions** (in `config/core/testing.el`):
- **ERT**: `jf/test-run-all-batch`, `jf/test-run-directory-batch`, `jf/test-run-pattern-batch`
- **Buttercup**: `jf/test-run-all-buttercup-batch`, `jf/test-run-buttercup-directory-batch`
- Load functions: `jf/test-load-all-test-files`, `jf/test-load-all-buttercup-test-files`

**When to use Buttercup:**
- New test suites (preferred going forward)
- Tests requiring significant setup/teardown (before-each, after-each)
- Behavioral/integration tests with shared fixtures
- Tests needing function mocking or call verification (spies)
- Hierarchical test organization (nested describe blocks)

**When to use ERT:**
- Maintaining existing test suites (no forced migration)
- Simple unit tests with minimal setup
- Quick assertion-based tests

**Snapshot testing:**
- Capture test output to git-tracked files for regression tracking
- Default location: `DIR/test-results.txt` for directory-scoped tests
- Compare changes: `git diff config/foo/test-results.txt`
- Use for: Monitoring test progress, catching regressions in CI

**Key principles:**
- Makefile owns Emacs invocation (single source of truth)
- Scripts are thin wrappers (no coupling)
- Tests co-located with modules (easy navigation)
- Automatic discovery (no manual test registration)
- Dual-framework support (run independently or together)

**Bash-parser test organization:** `config/experiments/bash-parser/test/`
- `behavioral/` - User-facing scenarios from specs (WHAT) - 129 tests
- `unit/{core,semantic,analysis}/` - Module tests by architecture layer (HOW) - 240 tests
- `integration/` - Multi-module interactions - 51 tests
- `construct/` - Bash construct-specific tests - 94 tests
- `corpus/{data,runners}/` - Test data and corpus-driven tests - 34 + 135 tests

**Running bash-parser tests:**
```bash
# All tests
./bin/run-tests.sh -d config/experiments/bash-parser

# Specific category
./bin/run-tests.sh -d config/experiments/bash-parser/test/behavioral
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

# Test configuration (interactive)
./bin/emacs-isolated.sh              # GUI mode
./bin/emacs-isolated.sh -nw          # Terminal mode
./bin/emacs-isolated.sh myfile.txt   # Open file

# Run tests (Makefile-based)
make test                                    # All tests (auto-discovery)
make test-bash-parser                        # Module-specific tests
make test-directory DIR=config/gptel         # Custom directory
make test-pattern PATTERN='^test-glob-'      # Pattern matching
./bin/run-tests.sh -d config/foo --snapshot  # With CLI and snapshot

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

**Default schema:** `spec-driven-beads` (proposal → specs → architecture → design → beads)

**When to use:**
- New features or significant modifications
- Changes requiring architectural decisions
- Multi-file refactoring
- Unclear requirements needing exploration

**Workflow artifacts:**
1. **proposal.md** - WHY (problem, motivation, capabilities, impact)
2. **specs/** - WHAT (behavioral requirements with scenarios)
3. **architecture.md** - HOW to structure and test (components, interfaces, testing approach)
4. **design.md** - HOW to implement (technical decisions, implementation approach)
5. **Beads** - Implementation tracking (generated from design and specs)

**Invoke skills:**
- `/opsx:explore` - Investigate and clarify requirements before planning
- `/opsx:new` - Start structured change with spec-driven-beads schema
- `/opsx:continue` - Progress to next artifact in workflow
- `/opsx:ff` - Fast-forward through all artifacts to reach implementation
- `/opsx:create-beads` - Generate Beads issues from design and specs
- `/opsx:apply` - Implement Beads (uses `/bead-implementation` workflow)
- `/opsx:verify` - Validate implementation matches artifacts
- `/opsx:archive` - Archive completed change

**Skip OpenSpec for:** Single-file edits, bug fixes, documentation updates, trivial changes.

#### Architecture Artifact

The **architecture.md** artifact bridges requirements (specs) and implementation (design):

**Purpose:**
- Define system structure (components, interfaces, boundaries)
- Establish testing approach BEFORE writing code
- Create testability contract for implementation

**Testing Approach section (critical):**
- **Test Framework**: Which framework and why (ERT for Emacs, Jest for JS, pytest for Python)
- **Test Organization**: Where test files live (test/, tests/, co-located)
- **Naming Conventions**: Test file and function naming patterns
- **Running Tests**: Commands to run tests (all tests, specific tests)
- **Test Patterns**: Mocking/stubbing approach, test data setup, helpers
- **Scenario Mapping**: How spec scenarios map to test cases

**Workflow:** Claude will use `AskUserQuestion` during architecture creation to gather your testing preferences. These decisions are documented and guide both test creation and implementation.

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

**Root:** `early-init.el`, `init.el`, `init.org` (MUST be at root), `Makefile` (test infrastructure)
**Config:** `config/core/`, `config/gptel/`, `config/major-modes/`, `config/language-modes/`, `config/local/`
**Tests:** `config/*/test/` or `config/*-test.el` (co-located with modules)
**Runtime:** `runtime/straight/`, `runtime/cache/`, `runtime/state/` (gitignored)
**Bin:** `bin/run-tests.sh` (test CLI), `bin/emacs-isolated.sh` (GUI launcher), `bin/tangle-org.sh` (literate programming)

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
