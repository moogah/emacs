# Command-Based Semantic Handlers

## Problem

The bash-parser semantic analysis system currently uses **category-based grouping** where plugins are organized by semantic domain (filesystem, cloud-auth, security). This creates several architectural problems:

### 1. Artificial Grouping

Commands don't naturally belong to a single category. For example:
- `aws` performs cloud authentication AND filesystem operations AND network calls
- `curl` performs network operations AND writes local files
- `git` performs filesystem operations AND network calls AND credential management

Currently, we have to choose one primary domain per command or create complex cross-cutting logic.

### 2. Monolithic Database

File operations are defined in a large, monolithic database (`jf/bash-command-file-semantics`) mapping commands to operation specs. This creates:
- **Poor locality**: All file operation knowledge scattered across one large structure
- **Tight coupling**: Adding `aws` filesystem support means editing the core database
- **Limited extensibility**: Can't easily plug in new command handlers

### 3. Plugin Asymmetry

The plugin system exists but is underutilized:
- Filesystem operations use a monolithic database, not a plugin
- Cloud auth is implemented as a plugin
- This inconsistency makes the architecture hard to understand and extend

### 4. Cross-Domain Commands

Commands like `aws s3 cp` need to contribute to multiple semantic domains:
```bash
aws s3 cp local.txt s3://bucket/file.txt --profile prod
# ↓
# Filesystem: reads local.txt
# Cloud Auth: uses profile "prod"
# Network: connects to amazonaws.com
```

Currently, there's no clean way to express this multi-domain nature.

## Motivation

We want to **make commands first-class entities** in the semantic analysis system. Each command should:

1. Be defined in its own file (locality)
2. Contribute to multiple semantic domains (multi-domain)
3. Be pluggable via auto-discovery (extensibility)
4. Have clear, testable interfaces (maintainability)

This architectural shift will:
- **Simplify adding new commands**: Drop a file in `commands/`, it's auto-loaded
- **Enable multi-domain semantics**: `aws.el` registers handlers for `:filesystem`, `:authentication`, `:network`
- **Improve testability**: Test `commands/aws.el` in isolation with its own test file
- **Better documentation**: Each command file is self-documenting

## Capabilities

### Command-Based Handler Architecture

```
bash-parser/
├── commands/
│   ├── index.el              # Auto-discovers and loads all command files
│   ├── cat.el                # cat command (filesystem only)
│   ├── grep.el               # grep command (filesystem only)
│   ├── aws.el                # AWS CLI (multi-domain!)
│   │                         #   - :authentication handler
│   │                         #   - :filesystem handler
│   │                         #   - :network handler
│   ├── curl.el               # curl (multi-domain)
│   │                         #   - :network handler
│   │                         #   - :filesystem handler
│   ├── git.el                # git (multi-domain)
│   └── ...                   # One file per command/family
└── semantics/
    └── bash-parser-semantics.el  # Handler registry (not database)
```

### Handler Registration API

Commands register handlers for each domain they contribute to:

```elisp
;; In commands/aws.el
(jf/bash-register-command-handler
  :command "aws"
  :domain :authentication
  :handler #'jf/bash-command-aws--cloud-auth-handler)

(jf/bash-register-command-handler
  :command "aws"
  :domain :filesystem
  :handler #'jf/bash-command-aws--filesystem-handler)

(jf/bash-register-command-handler
  :command "aws"
  :domain :network
  :handler #'jf/bash-command-aws--network-handler)
```

### Handler Interface

Each handler receives `parsed-command` and returns a standard result plist:

```elisp
(:domain DOMAIN-KEYWORD            ; e.g., :filesystem, :authentication
 :operations [...]                 ; Domain-specific operations
 :claimed-token-ids [...]          ; Tokens this handler processed
 :metadata {...})                  ; Additional context
```

### Auto-Discovery

The index file discovers all command handlers:

```elisp
;; In commands/index.el
(defun jf/bash-commands--discover-and-load ()
  "Discover all .el files in commands/ and load them.
  Each file registers its own handlers."
  ...)
```

Adding a new command is as simple as:
1. Create `commands/terraform.el`
2. Define handlers
3. Register handlers
4. Done - auto-loaded on next require

### Preserved Interface

The public API (`jf/bash-extract-semantics`) remains **unchanged**:

```elisp
;; INPUT: parsed-command from jf/bash-parse
;; OUTPUT: Same structure as before
(:domains ((domain . operations)...)
 :coverage {...}
 :parse-complete t/nil
 :plugin-results [...])
```

This ensures the **gptel scope validation system works without modification**.

## Impact

### What Changes

**Implementation:**
- `bash-parser/commands/` directory structure (new)
- `bash-parser/semantics/bash-parser-semantics.el` becomes a registry
- Individual command files replace monolithic database
- Handler registration API (new)
- Auto-discovery system (new)

**Testing:**
- One test file per command: `test/commands/test-aws.el`
- Test handlers in isolation
- Test multi-domain interactions

### What Stays the Same

**Public Interface:**
- `jf/bash-parse` - unchanged
- `jf/bash-extract-semantics` - same signature, same return structure
- Plugin infrastructure (`bash-parser-plugins.el`) - enhanced but compatible
- Coverage tracking - unchanged
- Token claiming - unchanged

**Clients:**
- `config/gptel/tools/scope-shell-tools.el` - no changes needed
- Scope validation system - works as-is
- All existing tests for scope validation - should pass

### What Goes Away

- `jf/bash-command-file-semantics` monolithic database
- Category-based semantic extraction functions
- Asymmetry between plugins and database

### Migration Strategy

**No migration or backwards compatibility**. This is a clean architectural refactor with:

1. **Interface preservation**: Same public API for scope validation
2. **Test preservation**: Existing scope validation tests must pass
3. **Clean break**: No support for old database format

## Success Criteria

1. **Interface compatibility**: Scope validation system works without changes
2. **Multi-domain support**: `aws` command contributes to `:filesystem`, `:authentication`, `:network`
3. **Auto-discovery**: Adding `terraform.el` automatically enables Terraform support
4. **Test isolation**: Can test `aws.el` handlers independently
5. **Documentation**: Each command file clearly shows what domains it affects

## Non-Goals

- Backwards compatibility with old database format
- Migration utilities for existing commands
- Support for both old and new systems simultaneously

## Questions

1. **Naming conventions**: `commands/cat.el` vs `commands/bash-command-cat.el`?
2. **Command families**: Git has many subcommands - one file or split into `git/*.el`?
3. **Universal plugins**: Keep domain-based plugins for universal concerns (redirections, variables)?
4. **Handler composition**: Should handlers be composable/chainable?
5. **Lazy loading**: Load all commands upfront or on-demand?
