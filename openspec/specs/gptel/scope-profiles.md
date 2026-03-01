# Scope Profiles

## Purpose

Scope profiles provide reusable permission templates for gptel sessions. Instead of duplicating scope configuration across preset files, profiles define standard permission sets (coding, research, restricted) that can be referenced by name and customized via variable expansion.

This capability separates:
- **Immutable templates** (scope profiles in `config/gptel/scope-profiles/`)
- **Mutable session config** (scope.yml in session branches)
- **Preset definitions** (registered via gptel-make-preset)

## Key Concepts

### Scope Profiles vs Session Scope

**Scope Profile Templates** (immutable, reusable):
- Plain YAML files in `config/gptel/scope-profiles/` (e.g., `coding.yml`)
- Define default permissions for categories of work
- Include variable placeholders like `${project_root}`
- Referenced by preset name (e.g., `:scope-profile "coding"`)
- Shared across multiple presets

**Session Scope Configuration** (mutable, instance-specific):
- Written to `scope.yml` in each session branch directory
- Created by expanding profile variables with runtime values
- Modified by scope expansion during session lifetime
- Source of truth for tool enforcement
- Never modifies the registered preset or profile template

**Example flow**:
1. Preset "executor" has `:scope-profile "coding"` (extracted during registration)
2. Session created → loads `coding.yml` → expands `${project_root}` → writes `scope.yml`
3. User adds path via scope expansion → updates session's `scope.yml`
4. Profile template `coding.yml` remains unchanged

### Resolution Priority

When creating a session, scope is resolved in this order:

1. **Named profile reference** - Preset has `:scope-profile "coding"` → load `config/gptel/scope-profiles/coding.yml`
2. **Inline scope defaults** - Preset has `:paths`, `:bash-tools` etc. directly → use those values
3. **Fallback** - No scope configuration → create empty scope.yml (deny-by-default)

### Variable Expansion

Scope profiles support `${project_root}` substitution:

- **When**: During session creation, after profile loaded, before scope.yml written
- **Placeholder**: Literal string `${project_root}` in YAML files
- **Expansion**: First selected projectile project or explicit parameter
- **Unresolvable**: Patterns with placeholder are removed, warning logged
- **Currently supported**: Only `${project_root}` (other variables reserved for future)

### Deep Merge for Multi-Project Sessions

When session has both preset scope AND explicit worktree paths (e.g., from activities integration):

**Merge strategy** (schema-agnostic):
- **Nested plists** (e.g., `:paths` with `:read`, `:write`, `:deny`): recursively merge
- **Lists**: concatenate and deduplicate (base items first, then override additions)
- **Scalars**: override value wins
- **nil in override**: treated as absence (base value retained)

This enables activities integration to augment preset scope with additional worktree paths.

## Scope Configuration Sections

### Paths - File Access Control

```yaml
paths:
  read: ["/**"]
  write: ["${project_root}/**"]
  deny: ["**/.git/**", "**/runtime/**", "**/.env", "**/node_modules/**"]
```

- `read` - Directories/patterns accessible for reading
- `write` - Directories/patterns accessible for creation/modification (write includes read)
- `deny` - Patterns that override both read and write (deny wins)

### Org-Roam Patterns - Knowledge Base Filtering

```yaml
org_roam_patterns:
  subdirectory: ["gptel/**"]
  tags: ["gptel", "research"]
  node_ids: ["*"]
```

Controls which org-roam nodes are accessible to tools.

### Bash Tools - Category-Based Command Execution

```yaml
bash_tools:
  categories:
    read_only:
      commands: ["ls", "cat", "grep", "git log"]
    safe_write:
      commands: ["mkdir", "touch", "git add", "git commit"]
    dangerous:
      commands: []
  deny:
    - "rm"
    - "sudo"
    - "chmod"
```

**Category semantics**:

| Category | Path Requirement | Meaning |
|----------|------------------|---------|
| `read_only` | `paths.read` (or write) | Read-only commands; write scope includes read |
| `safe_write` | `paths.write` | Non-destructive creation commands |
| `dangerous` | Both read AND write | Requires explicit user approval |
| `deny` | Never allowed | Blocked even with scope expansion |

## Requirements

### Requirement: Profile file format

Scope profiles SHALL be plain YAML files with no frontmatter delimiters.

**Implementation**: `config/gptel/scope-profiles.org` - `jf/gptel-scope-profile--load` parses with `yaml-parse-string` only

#### Scenario: Plain YAML, no frontmatter
- **WHEN** loading a scope profile
- **THEN** the system parses as YAML only (no markdown frontmatter extraction)
- **AND** distinguishes profiles from preset files (which have frontmatter)

#### Scenario: Partial profiles are valid
- **WHEN** a profile defines only `paths` (no org-roam or bash sections)
- **THEN** the profile is valid
- **AND** missing sections treated as empty (deny-by-default)

### Requirement: Profile directory and naming

Scope profiles SHALL be stored in `config/gptel/scope-profiles/` with `.yml` extension. Profile name matches filename without extension.

**Implementation**: Variable `jf/gptel--scope-profiles-directory` (defaults to `config/gptel/scope-profiles/`)

#### Scenario: Profile resolved by name
- **WHEN** preset references `:scope-profile "coding"`
- **THEN** system loads `config/gptel/scope-profiles/coding.yml`

#### Scenario: Missing profile handled gracefully
- **WHEN** preset references non-existent profile
- **THEN** system logs warning
- **AND** creates empty scope.yml (deny-by-default)

#### Scenario: Default profiles provided
- **WHEN** gptel configuration is installed
- **THEN** these profiles exist: `coding.yml` (broad read, project write), `research.yml` (read-only), `restricted.yml` (minimal), `bash-enabled.yml` (extended tools), `system-explorer.yml` (system inspection)

### Requirement: Variable expansion with ${project_root}

The system SHALL expand `${project_root}` to actual project directory during session creation, before writing scope.yml.

**Implementation**: `jf/gptel-scope-profile--expand-variables` recursively processes plists

**Expansion rules**:
- Placeholder replaced with project root path
- Project root from parameter (typically first projectile project)
- Patterns with unresolvable `${project_root}` removed, warning logged
- YAML arrays (vectors) converted to lists before expansion

#### Scenario: Projectile project detected
- **WHEN** creating session and `projectile-project-root` returns `/path/to/project`
- **THEN** `${project_root}/**` becomes `/path/to/project/**` in scope.yml

#### Scenario: No project detected
- **WHEN** creating session without project root
- **THEN** patterns containing `${project_root}` removed from scope.yml
- **AND** warning logged

### Requirement: Resolution priority (named vs inline)

When resolving scope for a preset, named profile references SHALL take precedence over inline scope defaults.

**Implementation**: `jf/gptel-scope-profile--resolve` checks `:scope-profile` key first

**Resolution process**:
1. Lookup preset in `jf/gptel-preset--scope-defaults` alist
2. If `:scope-profile` key present → load profile from file
3. Else if plist has `:paths`, `:bash-tools`, etc. → treat as inline config
4. Else → no scope configuration

#### Scenario: Named profile takes precedence
- **WHEN** preset has both `:scope-profile "coding"` AND inline `:paths`
- **THEN** named profile is used
- **AND** inline defaults ignored

#### Scenario: Inline defaults used when no profile
- **WHEN** preset has `:paths` but no `:scope-profile`
- **THEN** inline defaults used directly

#### Scenario: Empty scope fallback
- **WHEN** preset has no scope configuration
- **THEN** empty scope.yml created (deny-by-default)

### Requirement: Mutable scope.yml in session branches

Each session branch SHALL have a `scope.yml` file created at session creation time. This is the mutable copy used for enforcement.

**Implementation**: `jf/gptel-scope-profile--write-scope-yml` writes to branch directory

**File format**: YAML with sections matching profile structure (paths, org_roam_patterns, bash_tools)

#### Scenario: scope.yml created at session creation
- **WHEN** creating session with preset "executor"
- **THEN** scope.yml written to branch directory
- **AND** populated from preset's scope profile or inline defaults

#### Scenario: scope.yml is mutable
- **WHEN** scope expansion adds path to session
- **THEN** scope.yml updated
- **AND** registered preset unchanged
- **AND** profile template unchanged

#### Scenario: scope.yml is enforcement source
- **WHEN** tool executes and needs scope validation
- **THEN** scope system reads from session's scope.yml
- **AND** NOT from gptel--known-presets or profile files

### Requirement: Deep merge for multi-worktree sessions

When session has both preset scope AND explicit worktree paths, the system SHALL deep-merge them with worktree paths augmenting preset scope.

**Implementation**: `jf/gptel-scope-profile--deep-merge` (schema-agnostic recursive merge)

**Merge algorithm**:
- **Nested plists**: recursively merge
- **Lists**: concatenate and deduplicate
- **Scalars**: override wins
- **nil**: treated as absence (doesn't clear base)

#### Scenario: Worktree paths augment preset scope
- **WHEN** creating session with preset having `paths: read: ["/**"]`
- **AND** providing worktree-paths `/project-a`, `/project-b`
- **THEN** scope.yml has both preset paths AND worktree paths
- **AND** worktree paths added to read/write lists

#### Scenario: Explicit worktree overrides profile reference
- **WHEN** preset has `:scope-profile "coding"`
- **AND** worktree-paths provided (activities integration)
- **THEN** worktree paths take precedence
- **AND** profile not consulted

### Requirement: Key normalization (snake_case ↔ kebab-case)

Scope profiles use YAML snake_case keys. System SHALL convert to kebab-case keywords for Elisp, then back to snake_case when writing scope.yml.

**Implementation**:
- Load: `jf/gptel-scope-profile--normalize-keys` (snake_case → kebab-case)
- Write: `jf/gptel-scope-profile--kebab-to-snake` (reverse conversion)

**Normalization rules**:
- Recursive (nested plists processed)
- Idempotent (already-normalized keys pass through)

#### Scenario: YAML keys normalized on load
- **WHEN** loading profile with `org_roam_patterns` and `bash_tools`
- **THEN** converted to `:org-roam-patterns` and `:bash-tools` (keywords)

#### Scenario: Kebab-case converted back on write
- **WHEN** writing scope.yml
- **THEN** `:org-roam-patterns` becomes `org_roam_patterns` in YAML output

### Requirement: Integration with preset registration

Scope profile names and inline configs SHALL be extracted during preset registration and stored in `jf/gptel-preset--scope-defaults` alist.

**Implementation**: `jf/gptel-preset--extract-scope` (in preset-registration.org) separates scope keys from preset plist

**Extraction process**:
- During preset parsing
- Scope keys (`:paths`, `:org-roam-patterns`, `:bash-tools`, `:scope-profile`) removed from preset
- Stored in `jf/gptel-preset--scope-defaults` alist
- Preset registered without scope keys

#### Scenario: Scope defaults stored by preset name
- **WHEN** registering preset "executor" with `:scope-profile "coding"`
- **THEN** entry added: `(executor . (:scope-profile "coding"))`
- **AND** `:scope-profile` removed from registered preset

#### Scenario: Scope defaults used during session creation
- **WHEN** creating session with preset "executor"
- **THEN** system looks up `(alist-get 'executor jf/gptel-preset--scope-defaults)`
- **AND** resolves scope from that configuration

## Integration Points

### With Preset Registration
- Scope keys extracted during registration (preset-registration.org)
- Stored in `jf/gptel-preset--scope-defaults` alist
- Presets registered without scope configuration (gptel--known-presets remains clean)

### With Session Creation
- Called during branch creation (sessions/commands.org)
- Function: `jf/gptel-scope-profile--create-for-session`
- Parameters: preset-name, target-dir, project-root (optional), worktree-paths (optional)

### With Scope Enforcement
- Session's scope.yml is read by scope-core (scope validation)
- Profile templates never consulted during enforcement
- Mutable session scope allows runtime expansion

## File Organization

```
config/gptel/
├─ scope-profiles.org          # This module
├─ scope-profiles/             # Profile templates
│  ├─ coding.yml
│  ├─ research.yml
│  ├─ restricted.yml
│  ├─ bash-enabled.yml
│  └─ system-explorer.yml
├─ preset-registration.org     # Extracts scope config
└─ sessions/commands.org       # Calls profile creation
```

## Summary

Scope profiles solve the boilerplate problem: multiple presets can share permission sets via named profiles. The system maintains clear separation between immutable templates (profiles), mutable session config (scope.yml), and registered presets (gptel--known-presets). Variable expansion and deep merge enable flexible, context-aware scope initialization while keeping profiles reusable.
