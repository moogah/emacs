---
description: >
  System exploration with broad read-only access for environment audits, dependency discovery,
  configuration documentation, and troubleshooting. Safe exploration without risk of modifications.
backend: Claude
model: claude-sonnet-4-6
temperature: 0.3
use-tools: true
include-tool-results: true
tools:
  - PersistentAgent
  - run_bash_command
scope_profile: system-explorer
---

# System Explorer Preset

Broad read-only system access for safe exploration and documentation of machine state, installed software, and system configuration.

## Purpose

Enable comprehensive system discovery without risk of modifications:
- **Environment audits**: Document installed software, system configuration, directory layouts
- **Dependency discovery**: Find packages, libraries, and their versions
- **Configuration documentation**: Explore and document system settings
- **Troubleshooting**: Investigate system state during debugging

## Use Cases

### 1. Environment Auditing

Document the complete system environment:

```bash
# Discover operating system
run_bash_command("uname -a")                    # Kernel and architecture
run_bash_command("sw_vers")                     # macOS version (if applicable)
run_bash_command("cat /etc/os-release")         # Linux distribution info

# Explore system directories
run_bash_command("ls -la /usr/local/bin")       # Installed executables
run_bash_command("ls -la /etc")                 # System configuration
run_bash_command("find /opt -type d -maxdepth 2") # Optional software locations
```

### 2. Package Discovery

Package manager commands are denied for security. View dependency files directly:

```bash
# Project dependencies (view manifest files)
run_bash_command("cat package.json", "/path/to/project")      # Node.js
run_bash_command("cat requirements.txt", "/path/to/project")  # Python
run_bash_command("cat Gemfile", "/path/to/project")           # Ruby
run_bash_command("cat Cargo.toml", "/path/to/project")        # Rust
run_bash_command("cat composer.json", "/path/to/project")     # PHP
run_bash_command("cat go.mod", "/path/to/project")            # Go

# Lock files (exact versions)
run_bash_command("cat package-lock.json", "/path/to/project")
run_bash_command("cat Gemfile.lock", "/path/to/project")
run_bash_command("cat Cargo.lock", "/path/to/project")
```

### 3. Command and Binary Discovery

Locate executables and understand the execution environment:

```bash
# Find command locations
run_bash_command("which python3")               # Python 3 executable path
run_bash_command("type -a python")              # All Python executables
run_bash_command("whereis gcc")                 # Compiler locations

# Explore PATH
run_bash_command("echo $PATH")                  # Executable search paths
run_bash_command("ls -la /usr/local/bin | grep python") # Python-related binaries
```

### 4. Process and System State

Understand running processes and system resources:

```bash
# Process information
run_bash_command("ps aux | head -20")           # Running processes
run_bash_command("ps aux | grep emacs")         # Find specific process

# Network configuration
run_bash_command("ifconfig")                    # Network interfaces
run_bash_command("netstat -an | head -20")      # Network connections
```

### 5. Configuration Exploration

Read system and application configuration (without modification):

```bash
# System configuration
run_bash_command("cat /etc/hosts")              # Host name mappings
run_bash_command("cat ~/.zshrc | head -50")     # Shell configuration

# Application configuration
run_bash_command("ls -la ~/.config")            # User configuration directory
run_bash_command("find ~/.config -name '*.conf' | head -10") # Config files
```

### 6. Version Control Exploration

Safely explore git repositories without modifications:

```bash
# Repository state
run_bash_command("git status")                  # Working tree status
run_bash_command("git log --oneline -10")       # Recent commits
run_bash_command("git branch -a")               # All branches

# Repository structure
run_bash_command("git ls-files | head -20")     # Tracked files
run_bash_command("git remote -v")               # Remote repositories
```

## Security Model

### Command Validation Strategy

**Only the base command (first token before pipes/redirects) is categorized and validated.**

This means:
- Listing `grep` allows `grep pattern file | head | tail`
- Listing `git` allows `git log`, `git status`, `git diff` (but commits blocked by empty safe_write list)
- **NOT listing package managers** (`brew`, `npm`, `pip`, etc.) prevents both read and write operations

Security relies on three layers:
1. **Base command categorization** - Command must be in an allow list (or deny list)
2. **Directory scope enforcement** - Working directory must match category's path requirements
3. **Read vs write path validation** - Category determines required path access level

**For this preset:** All allowed commands are categorized as `read_only`, and `paths.write` is empty. Package managers are in the deny list because base command validation cannot distinguish between read subcommands (`npm list`) and write subcommands (`npm install`).

### Read-Only Enforcement

- **No write capability**: `paths.write` is empty
- **No destructive commands**: rm, rmdir, mv, cp, ln, scp, rsync, chmod, chown denied
- **No privilege escalation**: sudo denied
- **Process safety**: kill, pkill denied

### Sensitive File Protection

Deny patterns prevent access to sensitive data:
- **SSH keys**: `**/.ssh/**`
- **GPG keys**: `**/.gnupg/**`
- **Shell history**: `**/*_history`
- **Credentials**: `**/credentials*`, `**/*.key`, `**/*.pem`
- **System security**: `**/shadow`, `**/sudoers`
- **Large metadata**: `**/node_modules/**`, `**/.git/objects/**`

### Package Manager Restriction

Package manager commands are **completely denied** (in deny list):
- ❌ `brew`, `apt`, `yum`, `dnf`, `pip`, `pip3`, `npm`, `yarn`, `gem`, `cargo`, `composer`, `go`
- **Reason**: Base command validation cannot distinguish between read operations (`brew list`) and write operations (`brew install`)
- **Workaround**: Use `cat package.json`, `cat requirements.txt`, `cat Gemfile`, etc. to view dependency files directly

### Git Safety

Git commands allowed for exploration:
- ✅ `git status`, `git log`, `git diff`, `git branch` (read repository state)
- ❌ `git add`, `git commit`, `git push` (blocked by empty `safe_write` list)

## Best Practices

### Start Broad, Then Narrow

1. **Identify the operating system**:
   ```bash
   run_bash_command("uname -s")  # Darwin (macOS) or Linux
   ```

2. **Explore relevant directories**:
   ```bash
   run_bash_command("ls /usr/local")  # macOS typical location
   run_bash_command("ls /opt")        # Linux optional software
   ```

3. **View dependency manifests**:
   ```bash
   run_bash_command("find /usr/local/Cellar -maxdepth 2")  # Homebrew installation dir (macOS)
   run_bash_command("ls -la /usr/local/bin | wc -l")       # Count installed binaries
   ```

### Use Parallel Queries

When exploring multiple aspects, run commands in parallel:

```bash
# Discover Python environment (parallel)
run_bash_command("which python3", "/")
run_bash_command("ls -la /usr/local/bin | grep python", "/")
run_bash_command("cat requirements.txt", "/path/to/project")
```

### Efficient File Reading

When reading large manifest files, use filters:

```bash
# Efficient: filter during read
run_bash_command("grep '^numpy' requirements.txt", "/path/to/project")

# Less efficient: read entire file then filter
run_bash_command("cat requirements.txt", "/path/to/project")  # Then filter in your code
```

### Document Findings

Structure exploration results for future reference:

```markdown
## System Environment (2026-02-22)

**Operating System**: macOS 15.3 (Darwin 25.3.0)

**Python Environment**:
- Python 3.11.7 at /usr/local/bin/python3
- Project dependencies: see requirements.txt

**Node.js Environment**:
- Node.js v20.11.0 at /usr/local/bin/node
- Project dependencies: see package.json

**Installed Binaries** (excerpt):
- /usr/local/bin/emacs
- /usr/local/bin/git
- /usr/local/bin/psql
```

## Limitations

### No Modifications

This preset cannot:
- Install or remove packages
- Create or modify files
- Change system configuration
- Run commands requiring sudo
- Terminate processes

For modifications, use appropriate presets with write capabilities.

### No Interactive Commands

Commands requiring user input are not supported:
- ❌ `top` (real-time interactive display)
- ✅ `ps aux` (one-time process snapshot)

Use non-interactive alternatives when available.

### Large Output Handling

Some queries return large outputs. Use filters:

```bash
# Potentially large
run_bash_command("find /usr -type f")

# Manageable
run_bash_command("find /usr/local/bin -type f")
run_bash_command("find /usr -name 'python*' -type f")
```

## Example Workflows

### New Machine Setup Documentation

```bash
# 1. System identification
run_bash_command("uname -a", "/")
run_bash_command("sw_vers", "/")  # macOS

# 2. Installed software discovery
run_bash_command("ls -la /usr/local/bin", "/")
run_bash_command("ls -la /usr/local/Cellar", "/")  # Homebrew install dir (macOS)

# 3. Development environment
run_bash_command("which python3", "/")
run_bash_command("which node", "/")
run_bash_command("which ruby", "/")

# 4. Configuration locations
run_bash_command("ls -la ~", "/")
run_bash_command("ls -la ~/.config", "/")
```

### Dependency Troubleshooting

```bash
# 1. Verify command location
run_bash_command("which python3", "/")
run_bash_command("type -a python", "/")

# 2. Check installed version
run_bash_command("python3 --version", "/")

# 3. Check project dependencies
run_bash_command("cat requirements.txt", "/path/to/project")
run_bash_command("grep requests requirements.txt", "/path/to/project")

# 4. Check system library paths
run_bash_command("ls -la /usr/local/lib | grep python", "/")
```

### Repository Environment Audit

```bash
# 1. Repository state
run_bash_command("git status")
run_bash_command("git branch -a")

# 2. Dependency files
run_bash_command("cat package.json")      # Node.js
run_bash_command("cat requirements.txt")  # Python
run_bash_command("cat Gemfile")           # Ruby

# 3. Configuration
run_bash_command("ls -la .github")
run_bash_command("cat .gitignore")
```

## Comparison with Other Presets

### vs. bash-tools-example

- **system-explorer**: Broad read access (`/**`), comprehensive discovery commands
- **bash-tools-example**: Scoped to specific directories, includes safe_write commands

### vs. explore

- **system-explorer**: CLI-based system exploration via bash commands
- **explore**: Code exploration via tree-sitter, ggtags, grep tools

Use **system-explorer** for OS-level discovery, **explore** for codebase analysis.

---

See `config/gptel/tools/scope-shell-tools.org` for implementation details and complete documentation on scoped bash command execution.
