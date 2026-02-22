---
description: >
  System exploration with broad read-only access for environment audits, dependency discovery,
  configuration documentation, and troubleshooting. Safe exploration without risk of modifications.
backend: Claude
model: claude-sonnet-4-5-20250929
temperature: 0.3

# Path scope configuration
# Broad read access for system exploration, no write capability
paths:
  read:
    - "/**"
  write: []
  deny:
    - "**/.ssh/**"
    - "**/.gnupg/**"
    - "**/*_history"
    - "**/credentials*"
    - "**/*.key"
    - "**/*.pem"
    - "**/shadow"
    - "**/sudoers"
    - "**/node_modules/**"
    - "**/.git/objects/**"

# Bash tools configuration
# Comprehensive read-only command set for system discovery
bash_tools:
  # Read-only commands: comprehensive system exploration capabilities
  # OS info, file exploration, command discovery, package managers, process/network inspection
  read_only:
    commands:
      # Operating system information
      - "uname"          # System information
      - "sw_vers"        # macOS version details
      - "lsb_release"    # Linux distribution info

      # File system exploration
      - "ls"             # List directory contents
      - "cat"            # Display file contents
      - "grep"           # Search file contents
      - "find"           # Find files by pattern
      - "file"           # Identify file type
      - "stat"           # File status/metadata
      - "head"           # Display file start
      - "tail"           # Display file end
      - "wc"             # Count lines/words/chars

      # Command discovery
      - "which"          # Locate command executables
      - "whereis"        # Locate binary/source/manual
      - "type"           # Display command type/location

      # Package managers (read-only queries)
      - "brew"           # Homebrew (macOS)
      - "apt"            # APT (Debian/Ubuntu)
      - "yum"            # YUM (RHEL/CentOS)
      - "dnf"            # DNF (Fedora)
      - "pip"            # Python 2 packages
      - "pip3"           # Python 3 packages
      - "npm"            # Node.js packages
      - "gem"            # Ruby packages
      - "cargo"          # Rust packages

      # Process and network inspection
      - "ps"             # Process status
      - "top"            # Real-time process viewer
      - "ifconfig"       # Network interface configuration
      - "netstat"        # Network statistics

      # Version control (read-only)
      - "git"            # Git operations (read-only commands only)

  # Safe write commands: none - this is a read-only exploration preset
  safe_write:
    commands: []

  # Dangerous commands: none - exploration should never require these
  dangerous:
    commands: []

  # Denied commands: destructive operations never allowed
  deny:
    - "rm"             # Remove files/directories
    - "mv"             # Move/rename (can overwrite)
    - "cp"             # Copy (can overwrite)
    - "chmod"          # Change permissions
    - "chown"          # Change ownership
    - "sudo"           # Elevated privileges
    - "dd"             # Low-level disk operations
    - "mkfs"           # Format filesystems
    - "kill"           # Terminate processes
    - "pkill"          # Kill processes by name

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

Query installed packages across all package managers:

```bash
# Homebrew (macOS)
run_bash_command("brew list")                   # All installed formulae
run_bash_command("brew list --cask")            # GUI applications
run_bash_command("brew info python@3.11")       # Package details

# Python packages
run_bash_command("pip3 list")                   # Python packages
run_bash_command("pip3 show numpy")             # Package metadata

# Node.js packages
run_bash_command("npm list -g --depth=0")       # Global packages
run_bash_command("npm list --depth=0")          # Local packages (if in project)

# Linux package managers
run_bash_command("apt list --installed")        # Debian/Ubuntu packages
run_bash_command("yum list installed")          # RHEL/CentOS packages
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

### Read-Only Enforcement

- **No write capability**: `paths.write` is empty
- **No destructive commands**: rm, mv, cp, chmod, chown denied
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

### Safe Package Manager Usage

Package manager commands allowed for queries only:
- ✅ `brew list`, `pip3 list`, `npm list` (read package state)
- ✅ `brew info`, `pip3 show`, `npm view` (read package details)
- ❌ `brew install`, `pip3 install`, `npm install` (blocked by read-only scope)

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

3. **Query appropriate package managers**:
   ```bash
   run_bash_command("brew list")      # macOS
   run_bash_command("apt list --installed")  # Debian/Ubuntu
   ```

### Use Parallel Queries

When exploring multiple aspects, run commands in parallel:

```bash
# Discover Python environment (parallel)
run_bash_command("which python3")
run_bash_command("python3 --version")
run_bash_command("pip3 list | grep numpy")
```

### Respect Rate Limits

Package manager queries can be slow. Use filters:

```bash
# Efficient: filter during query
run_bash_command("brew list | grep python")

# Less efficient: retrieve all then filter
run_bash_command("brew list")  # Then filter in your code
```

### Document Findings

Structure exploration results for future reference:

```markdown
## System Environment (2026-02-22)

**Operating System**: macOS 15.3 (Darwin 25.3.0)

**Python Environment**:
- Python 3.11.7 at /usr/local/bin/python3
- pip 24.0
- Key packages: numpy 1.26.3, pandas 2.1.4

**Node.js Environment**:
- Node.js v20.11.0
- npm 10.2.4
- Global packages: typescript@5.3.3, eslint@8.56.0

**Homebrew Packages** (excerpt):
- emacs 29.2
- git 2.43.0
- postgresql@15 15.5
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
run_bash_command("uname -a")
run_bash_command("sw_vers")  # macOS

# 2. Installed software discovery
run_bash_command("brew list")
run_bash_command("brew list --cask")

# 3. Development environment
run_bash_command("which python3")
run_bash_command("which node")
run_bash_command("which ruby")

# 4. Configuration locations
run_bash_command("ls -la ~")
run_bash_command("ls -la ~/.config")
```

### Dependency Troubleshooting

```bash
# 1. Verify command location
run_bash_command("which python3")
run_bash_command("type -a python")

# 2. Check installed version
run_bash_command("python3 --version")

# 3. Verify package installation
run_bash_command("pip3 show requests")
run_bash_command("pip3 list | grep requests")

# 4. Check system library paths
run_bash_command("ls -la /usr/local/lib | grep python")
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

See `config/gptel/tools/scope-shell-tools.org` for complete documentation on scoped bash command execution.
