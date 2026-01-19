# Isolated Emacs Configuration

A modular, literate Emacs configuration that runs completely isolated from system Emacs installations. This configuration uses its own runtime directory for packages, cache, and state, ensuring it never interferes with `~/.emacs.d` or other Emacs instances on your machine.

## Quick Start

1. Clone this repository:
   ```bash
   git clone <repository-url> ~/emacs
   cd ~/emacs
   ```

2. Initialize submodules (snippets and templates):
   ```bash
   git submodule update --init --recursive
   ```

3. Create your machine role identifier:
   ```bash
   echo "personal-mac" > ~/.machine-role
   ```
   Available roles: `apploi-mac`, `personal-mac`, `personal-mac-air`

4. Launch isolated Emacs:
   ```bash
   ./bin/emacs-isolated.sh
   ```

On first launch, packages will be installed to `runtime/packages/` via straight.el.

## Using Worktrees for Testing

One of the key benefits of this isolated configuration is the ability to use **git worktrees** for testing configuration changes without affecting your main setup. Each worktree gets its own `runtime/` directory with independent packages, cache, and state.

### Creating a Test Worktree

**Best Practice:** Create worktrees as sibling directories (not subdirectories) to avoid confusion:

```bash
cd ~/emacs
git worktree add ~/emacs-testing -b testing-new-features
```

This creates:
```
~/emacs/          # Main configuration (main branch)
~/emacs-testing/  # Test worktree (testing-new-features branch)
```

**Avoid:** Using relative paths, which create subdirectories:
```bash
# DON'T DO THIS - creates ~/emacs/emacs-testing/
git worktree add emacs-testing -b testing-new-features
```

### Using the Worktree

Each worktree runs completely independently:

```bash
cd ~/emacs-testing
./bin/emacs-isolated.sh
```

**What happens:**
- The worktree uses its own `~/emacs-testing/runtime/` directory
- Packages are installed fresh (or use straight.el's cache)
- No conflicts with your main configuration
- Changes to the testing branch don't affect main
- You can run both instances simultaneously

### Testing New Features

1. **Create worktree** for your experiment
2. **Make changes** to config files in the worktree
3. **Test thoroughly** in the isolated environment
4. **Merge back** to main when stable:
   ```bash
   git checkout main
   git merge testing-new-features
   ```
5. **Clean up** worktree when done:
   ```bash
   cd ~/emacs
   git worktree remove ~/emacs-testing
   git branch -d testing-new-features
   ```

   Or with path:
   ```bash
   git worktree remove /Users/jefffarr/emacs-testing
   ```

### Worktree Tips

- Each worktree's `runtime/` is gitignored (never committed)
- The `jf/emacs-dir` variable dynamically resolves to the worktree's root
- Config files (`.org` and `.el`) are version controlled
- Worktrees share the same `.git` repository (efficient storage)
- You can have multiple worktrees active simultaneously

## Release Management for Production Use

This configuration uses a **three-tier architecture** similar to how operating systems manage stable vs development versions:

### Architecture

```
~/emacs/          - Development (main branch)
                    Active development, potentially unstable

~/emacs-testing/  - Testing (feature branches)
                    Experiments and new features

~/emacs-stable/   - Production (release tags)
                    Vetted releases for daily use
                    Host OS points here for primary Emacs
```

### Current Release Structure

**Release Candidates** - Tested features, needs production validation
- `v0.2.0-rc1` - GPTEL reorganization, worktree support (current)

**Stable Releases** - Thoroughly tested, production-ready
- `v0.1.0-beta` - Initial working configuration

**Development** - Active work on main branch

### Setting Up Production Worktree

The `emacs-stable` worktree is already configured at `v0.2.0-rc1`:

```bash
# Already created:
# git worktree add ~/emacs-stable -b stable/v0.2.0-rc1 v0.2.0-rc1

# Verify:
cd ~/emacs-stable
git log --oneline --decorate -1
```

Configure your host OS to use this worktree:
- macOS: Point app to `~/emacs-stable/bin/emacs-isolated.sh`
- Terminal alias: `alias emacs='~/emacs-stable/bin/emacs-isolated.sh'`

### Updating Production to New Release

When a new stable release is ready:

```bash
cd ~/emacs-stable

# Switch to new release tag
git fetch origin
git checkout v0.3.0  # or v0.2.0 when RC is promoted

# Or update to newer RC
git checkout v0.3.0-rc1
```

### Creating New Releases

**From main branch:**

1. **Tag Release Candidate** after testing new features:
   ```bash
   cd ~/emacs
   git tag -a v0.3.0-rc1 -m "Release candidate description"
   ```

2. **Test in Production** - Switch stable worktree to RC:
   ```bash
   cd ~/emacs-stable
   git checkout v0.3.0-rc1
   ```

3. **Promote to Stable** after thorough testing:
   ```bash
   cd ~/emacs
   git tag -a v0.3.0 -m "Stable release description"
   ```

4. **Update Production** to stable:
   ```bash
   cd ~/emacs-stable
   git checkout v0.3.0
   ```

### Release Workflow

```
Development Cycle:
main → rc1 → test → rc2 → test → stable → production

Example:
v0.3.0-rc1  Test in emacs-stable for 1 week
v0.3.0-rc2  Fix issues, test another week
v0.3.0      Promote to stable, update production
```

### Benefits

✅ **Isolation** - Development never breaks production
✅ **Testing** - RC releases validated before stable
✅ **Rollback** - Easy to revert to previous release
✅ **Confidence** - Production uses only vetted code
✅ **Flexibility** - Experiment freely in dev/testing

## Directory Structure

```
emacs/
├── bin/                    # Helper scripts
│   ├── emacs-isolated.sh   # Launch wrapper (sets EMACS_USER_DIRECTORY)
│   ├── tangle-org.sh       # Tangle .org files to .el
│   └── get-machine-role.sh # Read machine role from ~/.machine-role
├── config/                 # Configuration files
│   ├── init.org/.el        # Main entry point (source/tangled)
│   ├── early-init.el       # Early initialization (pre-UI)
│   ├── core/               # Core functionality (17+ modules)
│   ├── look-and-feel/      # UI theming and appearance
│   ├── language-modes/     # Programming language support
│   ├── major-modes/        # Major mode configurations
│   ├── experiments/        # Experimental/disabled modules
│   └── local/              # Machine-specific configs (*.el)
└── runtime/                # Isolated runtime (not in ~/.emacs.d)
    ├── packages/           # straight.el packages
    ├── cache/              # Emacs cache files
    ├── data/               # Persistent data
    ├── state/              # Transient state
    ├── snippets/           # Git submodule (yasnippets)
    ├── templates/          # Git submodule (file templates)
    └── custom.el           # Emacs customizations
```

## Literate Programming Workflow

This configuration uses **literate programming** with Org mode. Configuration is written in `.org` files and tangled to `.el` files.

### Source of Truth: `.org` Files

All configuration lives in `.org` files with these properties:
```org
#+title: Module Name
#+property: header-args:emacs-lisp :tangle module-name.el
#+auto_tangle: y
```

### Edit → Tangle → Validate → Commit

1. **Edit** the `.org` source file:
   ```bash
   emacs config/core/defaults.org
   ```

2. **Tangle** to generate the `.el` file:
   ```bash
   ./bin/tangle-org.sh config/core/defaults.org
   ```

3. **Validate** syntax (critical for catching parenthesis errors):
   ```bash
   emacs --batch --eval "(progn (find-file \"config/core/defaults.el\") (check-parens))"
   ```

4. **Commit** both `.org` and `.el` files:
   ```bash
   git add config/core/defaults.org config/core/defaults.el
   git commit -m "Update defaults configuration"
   ```

### Why This Workflow?

- `.org` files are human-readable with documentation
- Tangling generates clean `.el` files
- Validation catches syntax errors immediately
- Both files in git preserve history and allow diffs

## Usage

### Launching Emacs

Always use the wrapper script to ensure isolation:
```bash
./bin/emacs-isolated.sh              # GUI mode
./bin/emacs-isolated.sh file.txt     # Open file
./bin/emacs-isolated.sh -nw          # Terminal mode
```

**Never** run `emacs` directly - it will use system Emacs configuration instead.

### Adding a New Module

1. Create `config/category/module-name.org`:
   ```org
   #+title: Module Name
   #+property: header-args:emacs-lisp :tangle module-name.el
   #+auto_tangle: y

   * Introduction
   Description of what this module does.

   * Configuration
   #+begin_src emacs-lisp
   ;; -*- lexical-binding: t; -*-
   (use-package package-name
     :straight t
     :config
     (setq option value))
   #+end_src
   ```

2. Tangle to `.el`:
   ```bash
   ./bin/tangle-org.sh config/category/module-name.org
   ```

3. Add to `config/init.org` in the `jf/enabled-modules` list:
   ```elisp
   (defvar jf/enabled-modules
     '(
       ;; ... existing modules ...
       ("category/module-name" "Description of module")
       ))
   ```

4. Tangle init.org and restart Emacs:
   ```bash
   ./bin/tangle-org.sh config/init.org
   ./bin/emacs-isolated.sh
   ```

### Machine-Specific Configuration

Create a file in `config/local/` matching your machine role:

```bash
# For machine role "personal-mac"
emacs config/local/personal-mac.el
```

This file is loaded after all modules and can override any settings:
```elisp
;; -*- lexical-binding: t; -*-

;; Override org directory for this machine
(setq jf/org-directory (expand-file-name "~/Documents/org"))

;; Machine-specific keybindings
(global-set-key (kbd "C-c m") 'my-custom-function)
```

**Note:** Local configs are plain `.el` files (no `.org` source).

## Machine Roles

The machine role system uses `~/.machine-role` for stable identification (survives hostname changes).

### Available Roles

- `apploi-mac` - Work MacBook Air
- `personal-mac` - Personal MacBook Pro
- `personal-mac-air` - Personal MacBook Air

### Setting Your Role

```bash
echo "your-role-name" > ~/.machine-role
```

The corresponding config file `config/local/your-role-name.el` will be loaded automatically.

## Secrets Management

Sensitive credentials should **never** be committed to this repository.

### Setup

1. Copy the example file:
   ```bash
   cp .emacs-secrets.el.example ~/.emacs-secrets.el
   ```

2. Edit with your credentials:
   ```bash
   emacs ~/.emacs-secrets.el
   ```
   ```elisp
   ;; PostgreSQL credentials
   (setq jf/postgres-server "my-server.example.com")
   (setq jf/postgres-database "my_database")
   (setq jf/postgres-user "myusername")

   ;; GPG email for encryption
   (setq jf/gpg-email "me@example.com")
   ```

3. Secrets are automatically loaded by `config/init.el` if the file exists.

### Using Secrets in Config

Reference secrets variables in your configuration:
```elisp
(setq sql-postgres-login-params
      `((user :default ,(or jf/postgres-user "defaultuser"))
        (database :default ,(or jf/postgres-database "defaultdb"))
        (server :default ,(or jf/postgres-server "localhost"))))
```

## Customization

### Path Configuration

Key paths are configurable via variables defined in `config/core/defaults.el`:

- `jf/org-directory` - Base directory for org files (default: `~/org`)
- `user-emacs-directory` - Runtime directory (set by `bin/emacs-isolated.sh`)

Override in machine-specific config:
```elisp
;; In config/local/your-machine.el
(setq jf/org-directory (expand-file-name "~/Documents/org"))
```

### Disabling Modules

Comment out modules in `config/init.org`:
```elisp
(defvar jf/enabled-modules
  '(
    ;; ("module/to-disable" "Description")  ; Disabled
    ("core/defaults" "Basic Emacs behavior")  ; Enabled
    ))
```

Then retangle and restart:
```bash
./bin/tangle-org.sh config/init.org
./bin/emacs-isolated.sh
```

## Isolation Verification

To verify complete isolation from system Emacs:

1. Check `user-emacs-directory` points to runtime:
   ```elisp
   M-: user-emacs-directory RET
   ;; Should show: /path/to/this/repo/runtime/
   ```

2. Verify packages are in runtime:
   ```bash
   ls runtime/packages/
   # Should show straight.el and installed packages
   ```

3. Confirm no leakage to ~/.emacs.d:
   ```bash
   ls -la ~/.emacs.d 2>/dev/null
   # Should either not exist or be unchanged by this config
   ```

4. Test system Emacs is unaffected:
   ```bash
   /Applications/Emacs.app/Contents/MacOS/Emacs
   # Should launch with system configuration
   ```

## Troubleshooting

### Module Loading Errors

Check the `*Messages*` buffer after startup for errors:
```
M-x switch-to-buffer RET *Messages* RET
```

Enable debug mode in `config/init.org`:
```elisp
(setq jf/module-debug t)
```

### Syntax Errors in Elisp

Always validate after tangling:
```bash
./bin/tangle-org.sh config/path/to/file.org
emacs --batch --eval "(progn (find-file \"config/path/to/file.el\") (check-parens))"
```

For complex debugging, see the `writing-elisp` skill in `~/.claude/skills/`.

### Package Installation Issues

Delete and reinstall packages:
```bash
rm -rf runtime/packages/
./bin/emacs-isolated.sh  # Will reinstall on startup
```

## Contributing

When contributing changes:

1. Edit `.org` files (never `.el` directly)
2. Tangle and validate each file
3. Test by launching isolated Emacs
4. Commit both `.org` and `.el` files
5. Include clear commit messages

## GPTEL Session Browser

This configuration includes a **filesystem-based session browser** for gptel (LLM) conversations.

### Features

✅ **Filesystem-Native Storage** - Sessions stored as directory trees
✅ **Complete History** - Each node contains full conversation context
✅ **Easy Branching** - Try different conversation paths
✅ **Standard Tools** - Browse with dired/dirvish
✅ **Plain Markdown** - All content is human-readable
✅ **Git-Friendly** - Version control your conversations

### Quick Start

```elisp
;; Start a conversation
SPC l   (jf/gptel-launcher)

;; Browse saved sessions
M-x jf/gptel-browse-sessions

;; View conversation history
Navigate to any msg-N or response-N directory
Press C-c C-v

;; Create a branch
Put cursor on a node directory
Press C-c C-b to copy
Edit the context.md file
Press C-c C-r to resume
```

### Directory Structure

```
~/gptel-sessions/
└── SESSION-ID/
    ├── current -> msg-1/.../context.md
    ├── metadata.json
    └── msg-1/
        ├── context.md           # Complete conversation to this point
        └── response-1/
            ├── context.md
            └── msg-2/
                ├── context.md   # Continues down the tree
                └── msg-2-alt1/  # Branch (alternate path)
```

### Key Concepts

**Each directory is a conversation node** - Navigate the tree structure to explore different conversation paths

**context.md contains complete history** - Self-contained, can be fed back to any LLM

**Branches are copies** - Create alternative conversation paths by copying node directories

**Three Operations**:
1. **View** (`C-c C-v`) - Browse conversation history (read-only)
2. **Resume** (`C-c C-r`) - Load context into gptel buffer to continue
3. **Branch** (`C-c C-b`) - Copy node to create alternate path

### Documentation

See **[config/gptel/sessions/USAGE.md](config/gptel/sessions/USAGE.md)** for:
- Detailed workflows with flowcharts
- Step-by-step instructions
- Complete keybinding reference
- Troubleshooting guide
- Advanced usage examples

### Architecture

The session browser consists of three modules:

```
filesystem.el  → Core directory/file operations
registry.el    → Session state tracking
hooks.el       → Auto-save integration
    ↓
browser.el     → Navigation (dired/dirvish integration)
branching.el   → Resume and branch operations
```

All modules use **literate programming** - source in `.org` files, tangled to `.el`.

## License

This configuration is personal and unlicensed. Feel free to fork and adapt for your own use.
