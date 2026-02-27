## ADDED Requirements

### Requirement: Scope profile file format

Scope profiles SHALL be plain YAML files (no frontmatter delimiters) containing scope configuration sections: `paths`, `org_roam_patterns`, and `shell_commands`.

```yaml
paths:
  read: ["/**"]
  write: ["${project_root}/**"]
  deny: ["**/.git/**", "**/runtime/**", "**/.env", "**/node_modules/**"]
org_roam_patterns:
  subdirectory: ["gptel/**"]
  tags: ["gptel"]
  node_ids: ["*"]
shell_commands:
  allow: ["ls", "find", "grep", "git", "rg"]
  deny: ["rm -rf", "sudo"]
```

#### Scenario: Profile contains all scope sections
- **WHEN** a scope profile defines filesystem, org-roam, and shell permissions
- **THEN** it includes `paths`, `org_roam_patterns`, and `shell_commands` top-level keys

#### Scenario: Partial profiles are valid
- **WHEN** a scope profile only defines `paths` (no org-roam or shell sections)
- **THEN** the profile is valid
- **AND** missing sections are treated as empty (deny-by-default)

#### Scenario: Variable expansion in profiles
- **WHEN** a profile path pattern contains `${project_root}`
- **THEN** the variable is resolved at session creation time to the project directory
- **AND** the resolved path is written to the session's `scope.yml`

### Requirement: Scope profile directory

Scope profiles SHALL be stored in `config/gptel/scope-profiles/` and referenced by name (filename without extension).

#### Scenario: Profile located by name
- **WHEN** a preset references `scope_profile: "coding"`
- **THEN** the system resolves this to `config/gptel/scope-profiles/coding.yml`

#### Scenario: Missing profile handled gracefully
- **WHEN** a preset references a scope profile that does not exist
- **THEN** the system logs a warning
- **AND** creates the session's `scope.yml` with empty sections (deny-by-default)

#### Scenario: Default profiles provided
- **WHEN** the configuration is installed
- **THEN** at least these profiles SHALL exist: `coding.yml` (broad read, project write), `research.yml` (broad read, no write), `restricted.yml` (minimal permissions)

### Requirement: Session scope document (scope.yml)

Each session branch directory SHALL contain a `scope.yml` file that holds the mutable scope configuration for that session. The format is identical to scope profile files.

#### Scenario: scope.yml created at session creation
- **WHEN** creating a new session with preset "executor"
- **THEN** the system creates `scope.yml` in the branch directory
- **AND** populates it from the preset's scope profile (or scope defaults)

#### Scenario: scope.yml is mutable
- **WHEN** scope expansion adds a path to the session
- **THEN** `scope.yml` is updated with the new pattern
- **AND** the registered preset in `gptel--known-presets` is NOT modified

#### Scenario: scope.yml is source of truth for enforcement
- **WHEN** a tool executes and needs scope validation
- **THEN** the scope system reads from `scope.yml` in the buffer's branch directory
- **AND** does NOT read from preset.md or gptel--known-presets

### Requirement: Scope profile resolution at session creation

The system SHALL resolve which scope profile to use when creating a session, following a priority order.

Resolution order:
1. Explicit scope profile from preset's `scope_profile` key in `jf/gptel-preset--scope-defaults`
2. Inline scope defaults from preset's extracted `:paths`, `:org_roam_patterns`, `:shell_commands`
3. Fallback to empty scope (deny-by-default)

#### Scenario: Preset with named scope profile
- **WHEN** creating a session with preset "executor" which has `scope_profile: "coding"`
- **THEN** the system reads `config/gptel/scope-profiles/coding.yml`
- **AND** writes its contents (with variable expansion) to the session's `scope.yml`

#### Scenario: Preset with inline scope defaults
- **WHEN** creating a session with preset "executor" which has `:paths` in scope defaults but no `scope_profile`
- **THEN** the system uses the inline scope defaults from `jf/gptel-preset--scope-defaults`
- **AND** writes them to the session's `scope.yml`

#### Scenario: Preset with both profile and inline defaults
- **WHEN** a preset specifies both `scope_profile: "coding"` and inline `:paths`
- **THEN** the named profile takes precedence
- **AND** inline defaults are ignored (profile is the authoritative source)

#### Scenario: Preset with no scope configuration
- **WHEN** creating a session with a preset that has no scope defaults and no scope profile
- **THEN** the system creates `scope.yml` with empty sections
- **AND** all tool operations are denied by default until scope is expanded

### Requirement: Project root resolution for variable expansion

The system SHALL resolve `${project_root}` in scope profile templates to the actual project directory at session creation time.

#### Scenario: Projectile project detected
- **WHEN** creating a session and `projectile-project-root` returns a directory
- **THEN** `${project_root}` is replaced with that directory path

#### Scenario: No project detected
- **WHEN** creating a session and no project root can be determined
- **THEN** `${project_root}` patterns are removed from scope.yml
- **AND** the system logs a warning that project root could not be resolved

### Requirement: Multi-project scope from activities integration

When creating a session via activities integration, multiple worktree paths may be provided. The system SHALL support writing explicit paths to `scope.yml` that bypass scope profile template expansion.

#### Scenario: Activities session with multiple worktree paths
- **WHEN** creating a session via activities integration with worktree paths `["/path/to/project-a", "/path/to/project-b"]`
- **THEN** the system writes `scope.yml` with each worktree path as a separate `read` entry (with `/**` glob suffix)
- **AND** `write` entries for each path
- **AND** standard `deny` entries (`.git`, `runtime`, `.env`, `node_modules`)
- **AND** does NOT use `${project_root}` variable expansion (paths are already resolved)

#### Scenario: Activities session with single worktree path
- **WHEN** creating a session via activities integration with one worktree path
- **THEN** the behavior is identical to multi-project — explicit path written to `scope.yml`

#### Scenario: Activities session with no worktree paths
- **WHEN** creating a session via activities integration with no worktree paths
- **THEN** scope profile resolution falls back to the preset's scope defaults or deny-by-default
- **AND** follows the standard resolution priority (named profile > inline defaults > empty)

#### Scenario: Explicit paths take precedence over scope profiles
- **WHEN** a session is created with explicit worktree paths AND the preset has a `scope_profile`
- **THEN** the explicit paths are used (activities-provided paths override the profile)
- **AND** the scope profile is NOT consulted
