## Context

The current `run_approved_command` tool validates shell commands by name only, without controlling the working directory. This creates a security gap where approved commands (like `ls` or `grep`) can access any directory on the filesystem, bypassing the path-based security model used by file operations (`read_file`, `write_file_in_scope`, `edit_file_in_scope`) and org-roam tools.

The scope system already has infrastructure for path-based validation (`paths.read`, `paths.write`, `paths.deny`) and pattern matching (glob patterns with wildcard support). LLM agents need broad shell capability to explore codebases and perform development tasks, but this must be constrained by the same security boundaries that protect file operations.

**Current architecture:**
- `scope-core.el` - Central validation dispatcher with tool categorization system
- `scope-filesystem-tools.el` - File operations with path validation
- `scope-shell-tools.el` - Shell commands with command-name-only validation (current)
- `scope-expansion.el` - Permission request flow via transient menu

**Constraints:**
- Must maintain literate programming structure (.org files tangle to .el)
- Must integrate with existing scope expansion flow
- Must use existing `gptel-make-scoped-tool` macro for consistency
- Breaking change is acceptable since `run_approved_command` was never production-tested

## Alignment with Preset System

This feature integrates with the preset-alignment architecture (see `openspec/changes/gptel-preset-upstream-alignment`).

### Preset-Alignment Architecture Summary

**Immutable Presets:**
- Registered at init in `gptel--known-presets`
- Defined in `config/gptel/presets/*.md`
- Reference scope profiles via `scope_profile: "coding"`
- Never modified after registration

**Mutable Scope:**
- Per-session `scope.yml` in branch directories
- Created from scope profile templates
- Modified by scope expansion when user approves new permissions

**Config Flow:**
1. **Init:** Parse presets → extract scope config → register in `gptel--known-presets`
2. **Session creation:** Load scope profile → write `scope.yml` with bash_tools
3. **Tool execution:** Read `scope.yml` → validate bash command + directory
4. **Scope expansion:** Update `scope.yml` with approved commands

### Bash Tools Integration Points

**Config Storage:**
- Scope profile templates: `config/gptel/scope-profiles/*.yml` include `bash_tools` section
- Session scope: `scope.yml` contains per-session `bash_tools` config (mutable)
- Scope defaults: `bash_tools` configuration from scope profiles provides defaults during session creation

**Module Updates:**
- `preset-registration.el`: Extend scope key extraction to recognize `bash_tools`
- `scope-profiles.el`: Include `bash_tools` in profile schema and session scope creation
- `scope-core.el`: Load `bash_tools` from `scope.yml`, validate bash tools
- `scope-expansion.el`: Write approved bash commands to `scope.yml`
- `scope-bash-tools.el`: Implement tool, relocated from `scope/` to `tools/` directory

**Core Validation Logic:**
The categorization, directory scoping, and shell composition validation remain as originally designed (see Decision sections below). The integration uses scope.yml as the source of truth, with bash_tools configuration flowing from scope profiles to per-session scope.yml files.

## Goals / Non-Goals

**Goals:**
- Add directory-scoped validation to shell command execution
- Categorize commands by operational impact (read_only, safe_write, dangerous)
- Validate both command and working directory before execution
- Leverage existing path scope infrastructure (`paths.read`, `paths.write`)
- Maintain LLM flexibility to choose appropriate shell tools
- Provide clear error messages guiding LLM to scope expansion
- Add security features: timeouts, output truncation, path warnings

**Non-Goals:**
- Validating paths within command arguments (future enhancement)
- Supporting interactive commands (stdin-requiring tools)
- Parsing complex shell scripts (only single commands with pipes/redirects)
- Backward compatibility with `run_approved_command` (breaking change accepted)
- Command output formatting/parsing (return raw output)

## Decisions

### Decision 1: Category-based command validation with path scope binding

**Choice:** Commands are grouped into categories (read_only, safe_write, dangerous), and each category binds to a path scope requirement.

**Alternatives considered:**
- **Per-path command allowlists** - Each path pattern would specify allowed commands. Rejected: Too granular, hard to maintain, doesn't scale.
- **Global command allowlist with separate path validation** - Commands approved globally, paths validated separately. Rejected: Doesn't express the semantic relationship between command type and required access level.
- **Command-level path scope annotation** - Each command annotated with required scope. Rejected: More verbose configuration, harder to understand groupings.

**Rationale:** Category-based grouping makes the security model semantic and intuitive. Read-only commands naturally require read paths, write commands require write paths. Configuration is concise and easy to extend. Categories can evolve independently (add new read_only commands without touching write commands).

**Configuration structure:**
```yaml
bash_tools:
  read_only:
    commands: ["ls", "grep", "find", "cat", "head", "tail", "wc", "file", "git log", "git show", "git diff"]
  safe_write:
    commands: ["mkdir", "touch", "echo", "git add", "git commit"]
  dangerous:
    commands: []  # Empty by default
  deny:
    - "rm"
    - "mv"
    - "chmod"
    - "sudo"
```

**Validation logic:**
1. Extract base command from command string
2. Categorize: Check deny list → read_only → safe_write → dangerous → unknown (deny)
3. Resolve directory to absolute path (expand-file-name + file-truename)
4. Validate directory against category's path requirement:
   - read_only: must match paths.read OR paths.write (write implies read)
   - safe_write: must match paths.write
   - dangerous: requires explicit expansion
5. Check paths.deny (deny takes precedence)
6. Execute if all checks pass

### Decision 2: Allow shell composition, validate base command only

**Choice:** Allow pipes, redirects, command substitution (`|`, `>`, `<`, `$(...)`) but validate only the first command in the pipeline.

**Alternatives considered:**
- **Block all composition** - Only allow simple commands like "ls -la". Rejected: Too restrictive, prevents useful patterns like "grep | head" or "find | xargs".
- **Validate all commands in pipeline** - Parse full pipeline and validate each command. Rejected: Complex to parse reliably, diminishes returns (if first command is safe and directory is scoped, pipeline is likely safe).
- **Configurable composition policy** - Let preset control whether pipes/redirects are allowed. Rejected: Added complexity for unclear benefit.

**Rationale:** Validating the base command provides the primary security boundary. If `grep` is allowed in a read-scoped directory, `grep | head` is no riskier. Parsing complex pipelines is error-prone and provides marginal security benefit. This decision favors LLM flexibility over theoretical completeness.

**Implementation:** Parse command string with `split-string` on shell metacharacters (`[ |><;&]`) and extract first word. Handle edge cases: leading/trailing whitespace, quoted strings.

### Decision 3: Explicit directory argument required

**Choice:** `run_bash_command` requires explicit directory argument for every invocation.

**Alternatives considered:**
- **Infer from buffer context** - Use `default-directory` or `jf/gptel--branch-dir`. Rejected: Implicit behavior is harder to reason about, LLM must be clear about intent.
- **Optional directory with fallback** - Allow omitting directory, default to buffer context. Rejected: Introduces ambiguity, harder to validate.

**Rationale:** Explicit is better than implicit for security-sensitive operations. Forces LLM to think about where commands execute. Makes validation logic simpler (no fallback chain). Tool signature is clearer.

**Trade-off:** Slightly more verbose tool calls, but improved clarity and safety.

### Decision 4: Replace run_approved_command rather than extend

**Choice:** Remove `run_approved_command`, replace with `run_bash_command`.

**Alternatives considered:**
- **Keep both tools** - Old tool for backward compat, new tool for directory scoping. Rejected: Confusing to have two similar tools, LLM might use wrong one.
- **Extend run_approved_command** - Add optional directory argument. Rejected: Awkward to retrofit, name doesn't reflect new capability.

**Rationale:** Clean break is better than legacy baggage. Tool was never production-tested (per user confirmation). New name signals new capability. Simpler mental model for LLM.

**Migration:** No migration needed (tool unused in practice). Update agent definitions to use new tool.

### Decision 5: Security features (timeout, truncation, warnings)

**Choice:**
- 30-second timeout using `with-timeout`
- Output truncation at configurable limit (suggest 10,000 chars)
- Warning when command arguments contain absolute paths

**Alternatives considered:**
- **No timeout** - Trust commands to complete. Rejected: Risk of runaway processes (e.g., `find /`).
- **Configurable timeout** - Let preset control timeout. Rejected: Added complexity, 30s is reasonable default.
- **Stream output** - Return output in chunks as it arrives. Rejected: Requires async tool pattern, complicates implementation.
- **Block absolute paths** - Reject commands with absolute path arguments. Rejected: Too restrictive, warning is sufficient.

**Rationale:** Defensive programming prevents DoS attacks and resource exhaustion. Truncation preserves LLM context budget. Warnings educate LLM without blocking legitimate use cases.

### Decision 6: Leverage existing scope-core infrastructure

**Choice:** Register bash validation type in `jf/gptel-scope--tool-categories`, implement `jf/gptel-scope--validate-bash-tool` validator.

**Rationale:** Consistent with existing validation patterns (path, pattern, command validators). Integrates with allow-once flow, scope expansion, error formatting. Reuses glob matching, path resolution, error formatting logic.

**Implementation:**
```elisp
;; In scope-core.el tool categories
("run_bash_command" . (:validation bash :operation write))

;; New validator in scope-core.el
(defun jf/gptel-scope--validate-bash-tool (tool-name args config)
  "Validate bash tool: categorize command, validate directory."
  ...)

;; Dispatcher update (scope-core.el)
(pcase validation-type
  ('path (jf/gptel-scope--validate-path-tool ...))
  ('pattern (jf/gptel-scope--validate-pattern-tool ...))
  ('command (jf/gptel-scope--validate-command-tool ...))
  ('bash (jf/gptel-scope--validate-bash-tool ...))  ; NEW
  ...)
```

## Architecture

### File Structure

**Modified files:**
- `config/gptel/scope/scope-bash-tools.{org,el}` - Replace `run_approved_command` with `run_bash_command`, add command parsing and categorization
- `config/gptel/scope/scope-core.el` - Add bash validation type to tool categories, implement bash validator

**New functions in scope-bash-tools.el:**
```elisp
(defun jf/gptel-bash--parse-command (cmd-string)
  "Extract base command from CMD-STRING.")

(defun jf/gptel-bash--categorize-command (command config)
  "Categorize COMMAND as read_only, safe_write, dangerous, or denied.")

(defun jf/gptel-bash--validate-directory-for-category (directory category config)
  "Validate DIRECTORY matches CATEGORY's path scope requirement.")

(defun jf/gptel-bash--execute-command (command directory)
  "Execute COMMAND in DIRECTORY with timeout and output truncation.")

(defun jf/gptel-bash--check-absolute-paths (command)
  "Check if COMMAND contains absolute paths, return warning if so.")

(gptel-make-scoped-tool "run_bash_command" ...)
```

**New validator in scope-core.el:**
```elisp
(defun jf/gptel-scope--validate-bash-tool (tool-name args config)
  "Validate bash tool: parse command, categorize, validate directory.

  ARGS format: (command directory)

  Returns (:allowed t) or (:allowed nil :reason ... :allowed-patterns ...).")
```

### Data Flow

```
run_bash_command(command, directory)
  ↓
gptel-make-scoped-tool macro
  ↓
Load config from scope.yml (in session's branch directory)
  ↓
jf/gptel-scope--validate-bash-tool
  ↓
┌─────────────────────────────────┐
│ 1. Parse command                │
│    "grep foo | head" → "grep"   │
└────────────┬────────────────────┘
             ↓
┌─────────────────────────────────┐
│ 2. Categorize command           │
│    Check: deny → read_only →    │
│           safe_write → dangerous│
│    Result: "read_only"          │
└────────────┬────────────────────┘
             ↓
┌─────────────────────────────────┐
│ 3. Resolve directory            │
│    expand-file-name             │
│    + file-truename              │
└────────────┬────────────────────┘
             ↓
┌─────────────────────────────────┐
│ 4. Validate directory           │
│    read_only: paths.read OR     │
│               paths.write       │
│    safe_write: paths.write      │
│    Check paths.deny             │
└────────────┬────────────────────┘
             ↓
   Allowed? ─────No────→ Return error
             │
            Yes
             ↓
┌─────────────────────────────────┐
│ 5. Execute command              │
│    with-timeout (30s)           │
│    Truncate output              │
│    Check for absolute paths     │
└─────────────────────────────────┘
```

### Configuration Schema

**Scope profile template structure** (`config/gptel/scope-profiles/coding.yml`):
```yaml
paths:
  read:
    - "/**"
  write:
    - "${project_root}/**"
  deny:
    - "**/.git/**"
    - "**/runtime/**"
    - "**/.env"
    - "**/node_modules/**"

org_roam_patterns:
  subdirectory: ["gptel/**"]

shell_commands:
  allow: ["ls", "find", "grep", "git", "rg"]
  deny: ["rm -rf", "sudo"]

bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep", "find", "tree", "cat", "head", "tail", "wc", "file", "git log", "git show", "git diff"]
    safe_write:
      commands: ["mkdir", "touch", "echo", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "mv"
    - "chmod"
    - "sudo"
    - "chown"
```

**Session scope.yml structure** (in branch directory, e.g., `sessions/main/scope.yml`):
```yaml
# Same structure as profile, but with variables expanded
paths:
  read:
    - "/**"
  write:
    - "/Users/jefffarr/emacs/**"  # ${project_root} expanded
  deny:
    - "**/.git/**"
    - "**/runtime/**"

org_roam_patterns:
  subdirectory: ["gptel/**"]

shell_commands:
  allow: ["ls", "find", "grep", "git", "rg"]
  deny: ["rm -rf", "sudo"]

bash_tools:
  categories:
    read_only:
      commands: ["ls", "grep", "find", "tree", "cat", "head", "tail", "wc", "file", "git log", "git show", "git diff"]
    safe_write:
      commands: ["mkdir", "touch", "echo", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "mv"
    - "chmod"
    - "sudo"
```

**Loading logic:**
- Load plain YAML from `scope.yml` using `yaml-parse-string` (no frontmatter extraction needed)
- Extract `bash_tools` section with `:categories` and `:deny` subsections
- Handle missing `bash_tools` section gracefully (empty allow lists = deny all commands by default)
- Normalize YAML keys from snake_case to kebab-case (`:bash_tools` → `:bash-tools`)
- Convert vectors to lists using `jf/gptel-scope--vectorp-to-list`

### Integration with Scope Expansion

When validation fails, LLM receives structured error:
```elisp
(:success nil
 :error "command_not_allowed"  ; or "directory_not_in_scope"
 :tool "run_bash_command"
 :command "rm foo.txt"
 :directory "/Users/jefffarr/emacs"
 :required_scope "write"  ; for directory errors
 :allowed_patterns ["/tmp/**"]  ; for directory errors
 :message "Command 'rm' is not in allowed command lists. Use request_scope_expansion to ask user for approval.")
```

LLM calls:
```elisp
(request_scope_expansion
  :tool_name "run_bash_command"
  :patterns ["rm"]  ; command to add, or directory pattern
  :justification "Need to remove temporary build artifacts.")
```

Scope expansion flow:
1. Infer validation type from tool name (bash → command + path validation)
2. Show transient menu to user
3. User chooses: Deny / Add to scope / Allow once
4. If "Add to scope", update `scope.yml` bash_tools section or paths section
5. If "Allow once", add to buffer-local allow-once list

**Note:** The expansion system writes to the session's `scope.yml` file, not the immutable preset. This preserves the ability to expand scope dynamically while keeping preset definitions unchanged.

## Risks / Trade-offs

**Risk: Command parsing edge cases**
- Commands with complex quoting or escaping might not parse correctly
- **Mitigation:** Start with simple parsing (split on shell metacharacters), iterate based on real usage. Document limitations in tool description.

**Risk: Write scope implies read capability**
- read_only commands allowed in paths.write creates asymmetry
- **Mitigation:** This is intentional (write paths are higher privilege). Document clearly in spec. Consider if this causes confusion in practice.

**Risk: Base command validation only**
- Piped commands could bypass validation (e.g., "cat | bash evil.sh")
- **Mitigation:** User controls what commands enter allow lists. If `bash` is not in safe_write, pipe example fails. Defense-in-depth via command allow lists.

**Risk: Absolute paths in arguments bypass directory scope**
- Command "grep pattern /other/path" runs in scoped directory but accesses unscoped path
- **Mitigation:** Issue warning in response. Document limitation. Future enhancement could parse and validate all path arguments.

**Trade-off: Timeout at 30 seconds**
- Some legitimate commands might need longer (e.g., recursive grep in large codebase)
- **Mitigation:** 30s is conservative default. If issues arise, make configurable. LLM can use more specific filters (grep with --include) to speed up commands.

**Trade-off: Output truncation**
- Long command output gets cut off, potentially losing important information
- **Mitigation:** Truncation notice suggests using filters (head, grep, tail). LLM learns to narrow queries. Prevents context window overflow.

**Risk: Breaking change affects existing sessions**
- Removing `run_approved_command` breaks any in-flight sessions using it
- **Mitigation:** User confirmed tool was never production-tested. Update agent definitions before deploying. No migration path needed.

## Implementation Plan

**Step 1: Extend scope profile schema**
- Add `bash_tools` section to existing scope profile templates in `config/gptel/scope-profiles/`
- Create new `bash-enabled.yml` profile with comprehensive bash command examples
- Update `scope-profiles.el` to handle bash_tools during profile loading and scope.yml creation

**Step 2: Update preset registration**
- Extend `jf/gptel-preset--extract-scope-keys` in `preset-registration.el` to recognize `:bash-tools`
- Add `:bash-tools` to scope extraction logic
- Store extracted bash_tools in `jf/gptel-preset--scope-defaults`

**Step 3: Implement bash validation in scope-core**
- Add `jf/gptel-scope--validate-bash-tool` to scope-core.el
- Register bash validation type in tool categories
- Update config loading to read bash_tools from `scope.yml` (using plain YAML parsing)
- Tangle and validate scope-core.org

**Step 4: Implement bash tool**
- Relocate `scope-bash-tools.{org,el}` from `config/gptel/scope/` to `config/gptel/tools/`
- Replace `run_approved_command` tool definition with `run_bash_command`
- Implement command parsing, categorization, execution helpers
- Use `gptel-make-scoped-tool` macro for consistency
- Tangle and validate scope-bash-tools.org

**Step 5: Update scope expansion**
- Extend scope-expansion.el to write approved bash commands to `scope.yml` bash_tools section
- Add bash command expansion routing
- Test YAML serialization of bash_tools updates

**Step 6: Update loader**
- Update `config/gptel/gptel.org` to load scope-bash-tools from `tools/` directory
- Verify module load order (preset-registration → scope-profiles → scope-core → tools → sessions)

**Step 7: Create example presets**
- Create `bash-tools-example.md` preset with `scope_profile: bash-enabled`
- Create `system-explorer.md` preset for read-only system exploration
- Update agent definitions to use `run_bash_command`

**Step 8: Integration testing**
- Create test session with bash-enabled profile
- Verify bash_tools appears in scope.yml
- Test categorization logic
- Test scope expansion flow
- Verify timeout and truncation
- Test error messages

## Open Questions

1. **Should dangerous category ever be used?** Or is it effectively "requires manual approval every time"?
   - Lean toward: dangerous = always requires expansion, never auto-allowed

2. **What's the right output truncation limit?** 10,000 chars? Configurable?
   - Start with 10,000, make configurable if needed

3. **Should we warn or block when detecting command substitution?** `$(...)` or `` `...` ``
   - Lean toward: allow with warning, same as absolute paths

4. **Should timeout be configurable per command or global?**
   - Start global (30s), make per-command if use cases emerge

5. **Should we provide a tool to list allowed commands?** Helper for LLM to discover capabilities.
   - Lean toward: yes, add `inspect_scope_plan` enhancement to show bash_tools section
