# Scope Profiles

## Purpose

Scope profiles provide reusable permission templates for gptel sessions. Rather than duplicating scope configuration across preset files, profiles define standard permission sets (coding, research, restricted, bash-enabled, system-explorer) that can be referenced by name from any preset and customized at session-creation time via variable expansion.

The profile module lives at `config/gptel/scope-profiles.org` and is distinct from the `scope-*` modules under `config/gptel/scope/`. Profiles feed `scope.yml`; the `scope-validation` module then enforces that file (via `scope-yaml`) — profile templates themselves are never consulted during enforcement.

## Scope Profiles vs Session Scope

**Scope Profile Templates** (immutable, reusable):
- Plain YAML files in `config/gptel/scope-profiles/` (e.g., `coding.yml`)
- Define default permissions for categories of work
- May include variable placeholders like `${project_root}`
- Referenced by preset via `:scope-profile "<name>"`
- Shared freely across presets

**Session Scope Configuration** (mutable, per-session):
- Written as `scope.yml` into each session branch directory
- Produced by resolving a profile, expanding variables, and optionally deep-merging worktree paths
- Modified by scope expansion during the session lifetime
- Sole source of truth for tool enforcement
- Changes never propagate back to the preset or the profile template

**Example flow**:
1. Preset `executor` declares `scope_profile: coding` in its YAML frontmatter; `preset-registration` extracts this into `jf/gptel-preset--scope-defaults`.
2. Session creation resolves profile `coding` → loads `coding.yml` → expands `${project_root}` → writes `scope.yml`.
3. User triggers scope expansion → updates the session's `scope.yml`.
4. `coding.yml` and the registered preset remain unchanged.

## Resolution Priority

For a given preset, scope is resolved in this order:

1. **Named profile reference** — `:scope-profile "coding"` → load `config/gptel/scope-profiles/coding.yml`
2. **Inline scope defaults** — preset plist with `:paths`/`:cloud`/`:security` directly → use as-is
3. **Empty fallback** — no scope configuration → write minimal `scope.yml` (deny-by-default)

## Variable Expansion

Profiles support `${project_root}` substitution only (other variables are reserved for future use):

- Expansion runs after profile load and before `scope.yml` is written
- The project root is supplied by session creation (typically the first selected projectile project)
- Patterns whose `${project_root}` cannot be resolved are removed from the output, and a warning is logged
- Non-string values pass through untouched; YAML arrays parsed as vectors are coerced to lists before expansion

## Deep Merge for Multi-Worktree Sessions

When a session has both a resolved profile and explicit worktree paths (the activities integration case), the two are combined via a schema-agnostic deep merge:

- **Nested plists** — recursively merged key-by-key
- **Lists of scalars** — concatenated and deduplicated (base entries first, then override additions)
- **Scalars** — override wins
- **nil in override** — treated as absence; the base value is retained

This lets an activity supplying multiple worktree roots augment a profile's `:paths.read` and `:paths.write` without discarding the profile's `:cloud` and `:security` choices.

## Scope Configuration Sections

Profiles produce the same plist shape that `scope-validation` consumes (see `interfaces.org` §Scope Config Shape for canonical semantics). Snake_case in YAML on disk; kebab-case keywords in elisp.

### Paths — File Access Control

```yaml
paths:
  read:    ["/**"]
  write:   ["${project_root}/**"]
  modify:  []
  execute: []
  deny:    ["**/.git/**", "**/runtime/**", "**/.env", "**/node_modules/**"]
```

- `read` — read-like operations (includes what `write` covers, since write implies read)
- `write` — create / write / append / delete
- `modify` — in-place edits; `write` also satisfies `modify`
- `execute` — execute operations (no implication from any other permission)
- `deny` — overrides every allow pattern

### Cloud — Authentication Detection

```yaml
cloud:
  auth_detection: "warn"        # "allow" | "warn" | "deny"
  allowed_providers: []
```

### Security — Parsing Strictness

```yaml
security:
  enforce_parse_complete: true
  max_coverage_threshold: 1
```

For the full validator semantics of these sections (permission hierarchy, deny precedence, bash parse enforcement), see `openspec/specs/gptel/scope.md`.

## Requirements

### Requirement: Profile file format (plain YAML)

Scope profiles SHALL be plain YAML files with no markdown frontmatter delimiters. Key normalization from YAML snake_case to elisp kebab-case is delegated to the `scope-yaml` module; the profile loader does not implement its own normalization.

**Implementation**: `config/gptel/scope-profiles.org` (`jf/gptel-scope-profile--load`) calls `yaml-parse-string` and then `jf/gptel-scope-yaml--normalize-keys`.

#### Scenario: Plain YAML, no frontmatter

- **WHEN** loading a scope profile file
- **THEN** the system parses it as YAML only
- **AND** does NOT attempt to strip `---` frontmatter delimiters (those belong to preset files)

#### Scenario: Partial profiles are valid

- **WHEN** a profile defines only `paths` and omits `cloud` and `security`
- **THEN** the profile loads successfully
- **AND** missing sections are treated as absent (deny-by-default at enforcement time)

#### Scenario: Key normalization delegated to scope-yaml

- **WHEN** a profile contains `auth_detection` or `max_coverage_threshold`
- **THEN** the loader passes parsed output through `jf/gptel-scope-yaml--normalize-keys`
- **AND** the result uses kebab-case keywords (`:auth-detection`, `:max-coverage-threshold`)

### Requirement: Profile directory and naming

Scope profiles SHALL reside in `config/gptel/scope-profiles/` with a `.yml` extension. The profile name referenced by a preset matches the filename sans extension.

**Implementation**: `jf/gptel--scope-profiles-directory` in `config/gptel/scope-profiles.org`.

#### Scenario: Profile resolved by name

- **WHEN** a preset declares `:scope-profile "coding"`
- **THEN** the system loads `config/gptel/scope-profiles/coding.yml`

#### Scenario: Missing profile handled gracefully

- **WHEN** a preset references a profile file that does not exist
- **THEN** the loader logs a warning
- **AND** session creation writes an empty (deny-by-default) `scope.yml`

#### Scenario: Default profiles provided

- **WHEN** the gptel configuration is installed
- **THEN** these profiles exist: `coding.yml` (broad read, project write), `research.yml` (read-only), `restricted.yml` (minimal), `bash-enabled.yml` (extended bash read, project write), `system-explorer.yml` (read-only system inspection with expanded deny list)

### Requirement: Variable expansion with ${project_root}

The system SHALL expand `${project_root}` in string values during session creation, before `scope.yml` is written. Only `${project_root}` is supported.

**Implementation**: `jf/gptel-scope-profile--expand-variables` recursively walks the resolved plist.

#### Scenario: Projectile project detected

- **WHEN** session creation supplies `/path/to/project` as the project root
- **THEN** `${project_root}/**` in the profile becomes `/path/to/project/**` in the written `scope.yml`

#### Scenario: No project available

- **WHEN** session creation has no project root
- **THEN** entries containing `${project_root}` are removed from the output
- **AND** a warning is logged

#### Scenario: YAML arrays coerced to lists

- **WHEN** `yaml-parse-string` returns a vector for a list-valued section
- **THEN** the expander converts the vector to a list before iterating

### Requirement: Resolution priority (named profile vs inline vs empty)

When resolving scope for a preset, a named profile reference SHALL take precedence over inline scope defaults; absence of both SHALL yield an empty `scope.yml`.

**Implementation**: `jf/gptel-scope-profile--resolve` in `config/gptel/scope-profiles.org` inspects `jf/gptel-preset--scope-defaults`.

#### Scenario: Named profile takes precedence

- **WHEN** a preset has both `:scope-profile "coding"` and inline `:paths`
- **THEN** `coding.yml` is loaded
- **AND** the inline `:paths` are ignored

#### Scenario: Inline defaults used when no profile reference

- **WHEN** a preset has `:paths`/`:cloud`/`:security` but no `:scope-profile`
- **THEN** the inline plist is used directly as the resolved scope

#### Scenario: Empty scope fallback

- **WHEN** a preset has no scope configuration at all
- **THEN** session creation writes a minimal `scope.yml` that denies by default

### Requirement: Mutable scope.yml in session branches

Each session branch SHALL receive a `scope.yml` at creation time. This file is the single source of truth for enforcement; the profile template and the registered preset are never consulted by validators.

**Implementation**: `jf/gptel-scope-profile--write-scope-yml` in `config/gptel/scope-profiles.org`.

#### Scenario: scope.yml created at session creation

- **WHEN** a session is created with preset `executor`
- **THEN** `scope.yml` is written into the branch directory
- **AND** its contents derive from the preset's resolved scope configuration

#### Scenario: scope.yml is mutable

- **WHEN** scope expansion adds a path during a session
- **THEN** the session's `scope.yml` is updated in place
- **AND** the registered preset and the profile template remain unchanged

#### Scenario: scope.yml is the enforcement source

- **WHEN** a tool invocation requires path validation
- **THEN** `scope-validation` reads the session's `scope.yml` (via `scope-yaml`)
- **AND** does NOT read from `gptel--known-presets` or from profile files

### Requirement: Deep merge for multi-worktree sessions

When session creation receives both a resolved profile and explicit worktree paths, the system SHALL deep-merge them such that worktree paths augment (not replace) the profile's configuration.

**Implementation**: `jf/gptel-scope-profile--deep-merge` in `config/gptel/scope-profiles.org` (schema-agnostic recursive merge).

#### Scenario: Worktree paths augment profile scope

- **WHEN** the resolved profile has `paths.read: ["/**"]`
- **AND** worktree paths `/project-a`, `/project-b` are supplied
- **THEN** the written `scope.yml` contains both the profile's entries and the worktree paths in `paths.read`/`paths.write`

#### Scenario: Activities session with explicit worktree paths

- **WHEN** activities integration supplies already-resolved worktree paths
- **THEN** those paths are written as-is (no `${project_root}` expansion needed)
- **AND** standard deny entries from the profile are preserved
- **AND** `:cloud` and `:security` from the profile are preserved

#### Scenario: Activities session with no worktree paths

- **WHEN** activities integration supplies no worktree paths
- **THEN** resolution falls back to the standard priority (named profile > inline > empty)

### Requirement: Integration with preset registration

Scope keys SHALL be extracted from preset frontmatter during registration and stored in `jf/gptel-preset--scope-defaults`, keyed by preset name symbol, so that `gptel--known-presets` contains no scope data.

**Implementation**: `jf/gptel-preset--extract-scope` in `config/gptel/preset-registration.org`. Extracted keys: `:paths`, `:org-roam-patterns`, `:shell-commands`, `:bash-tools`, `:scope-profile`.

#### Scenario: Scope defaults stored by preset name

- **WHEN** registering preset `executor` whose frontmatter contains `scope_profile: coding`
- **THEN** `jf/gptel-preset--scope-defaults` gains entry `(executor . (:scope-profile "coding"))`
- **AND** the plist passed to `gptel-make-preset` contains no scope keys

#### Scenario: Scope defaults consulted during session creation

- **WHEN** a session is created for preset `executor`
- **THEN** `jf/gptel-scope-profile--resolve` looks up `executor` in `jf/gptel-preset--scope-defaults`
- **AND** feeds the result into the resolution priority above

## Integration Points

### With preset registration (`config/gptel/preset-registration.org`)

- Scope keys are stripped from preset plists and stored in `jf/gptel-preset--scope-defaults`
- Upstream `gptel--known-presets` never sees scope data

### With session creation (`config/gptel/sessions/commands.org`)

- Entry point: `jf/gptel-scope-profile--create-for-session`
- Parameters: preset name, target directory, optional project root, optional worktree paths
- Called during branch directory creation; responsible for the final `scope.yml`

### With scope enforcement (`config/gptel/scope/`)

- `scope-validation` reads the session's `scope.yml` via `scope-yaml`
- Key normalization and schema defaulting live in `scope-yaml` (not in the profile module)
- Profiles are never consulted at validation time

## File Organization

```
config/gptel/
├─ scope-profiles.org          # This module (profile loader, resolver, expander, merger)
├─ scope-profiles/             # Profile templates
│  ├─ coding.yml
│  ├─ research.yml
│  ├─ restricted.yml
│  ├─ bash-enabled.yml
│  └─ system-explorer.yml
├─ preset-registration.org     # Extracts scope keys into jf/gptel-preset--scope-defaults
├─ sessions/commands.org       # Calls jf/gptel-scope-profile--create-for-session
└─ scope/
   ├─ scope-yaml.org           # Key normalization + schema defaults (delegated target)
   └─ scope-validation.org     # Reads scope.yml; enforces at tool-call time
```
