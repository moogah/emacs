# Scope Profiles

## Purpose

Scope profiles provide reusable permission templates for gptel sessions. Rather than duplicating scope configuration across preset files, profiles define standard permission sets (coding, research, restricted, bash-enabled, system-explorer) that can be referenced by name from any preset and customized at session-creation time via variable expansion.

The profile module lives at `config/gptel/scope-profiles.org` and is distinct from the `scope-*` modules under `config/gptel/scope/`. Profiles feed the `:PROPERTIES:` drawer of `session.org`; the `scope-validation` module then enforces that drawer — profile templates themselves are never consulted during enforcement.

## Scope Profiles vs Session Scope

**Scope Profile Templates** (immutable, reusable):
- Plain YAML files in `config/gptel/scope-profiles/` (e.g., `coding.yml`)
- Define default permissions for categories of work
- May include variable placeholders like `${project_root}`
- Referenced by preset via `:scope-profile "<name>"`
- Shared freely across presets

**Session Scope Configuration** (mutable, per-session):
- Written into the session's `session.org` `:PROPERTIES:` drawer at creation time
- Produced by resolving a profile, expanding variables, and optionally deep-merging worktree paths
- Modified by scope expansion during the session lifetime (drawer mutation in place)
- Sole source of truth for tool enforcement
- Changes never propagate back to the preset or the profile template

**Example flow**:
1. Preset `executor` declares `scope_profile: coding` in its YAML frontmatter; `preset-registration` extracts this into `jf/gptel-preset--scope-defaults`.
2. Session creation resolves profile `coding` → loads `coding.yml` → expands `${project_root}` → emits a `:PROPERTIES:` drawer block prepended to the new `session.org`.
3. User triggers scope expansion → updates the session's drawer in place.
4. `coding.yml` and the registered preset remain unchanged.

## Resolution Priority

For a given preset, scope is resolved in this order:

1. **Named profile reference** — `:scope-profile "coding"` → load `config/gptel/scope-profiles/coding.yml`
2. **Inline scope defaults** — preset plist with `:paths`/`:cloud` directly → use as-is
3. **Empty fallback** — no scope configuration → emit a minimal drawer (deny-by-default)

## Variable Expansion

Profiles support `${project_root}` substitution only (other variables are reserved for future use):

- Expansion runs after profile load and before the drawer block is emitted
- The project root is supplied by session creation (typically the first selected projectile project)
- Patterns whose `${project_root}` cannot be resolved are removed from the output, and a warning is logged
- Non-string values pass through untouched; YAML arrays parsed as vectors are coerced to lists before expansion

## Deep Merge for Multi-Worktree Sessions

When a session has both a resolved profile and explicit worktree paths (the activities integration case), the two are combined via a schema-agnostic deep merge:

- **Nested plists** — recursively merged key-by-key
- **Lists of scalars** — concatenated and deduplicated (base entries first, then override additions)
- **Scalars** — override wins
- **nil in override** — treated as absence; the base value is retained

This lets an activity supplying multiple worktree roots augment a profile's `:paths.read` and `:paths.write` without discarding the profile's `:cloud` choices.

## Scope Configuration Sections

Profiles produce the same plist shape that `scope-validation` consumes (see `interfaces.org` §Scope Config Shape for canonical semantics). Snake_case in YAML on disk; kebab-case keywords in elisp. The shape carries `:paths` and `:cloud` only — `enforce-parse-complete` and the coverage threshold are module-level constants in `scope-validation.el`, not per-profile knobs.

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

For the full validator semantics of these sections (permission hierarchy, deny precedence, bash parse enforcement), see `openspec/specs/gptel/scope.md`.

## Requirements

### Requirement: Profile file format (plain YAML)

Scope profiles SHALL be plain YAML files with no markdown frontmatter delimiters. Key normalization from YAML snake_case to elisp kebab-case lives in the profile loader (the previous `scope-yaml` module is retired with the move to drawer-resident scope).

**Implementation**: `config/gptel/scope-profiles.org` (`jf/gptel-scope-profile--load`) calls `yaml-parse-string` and then performs key normalization inline.

#### Scenario: Plain YAML, no frontmatter

- **WHEN** loading a scope profile file
- **THEN** the system parses it as YAML only
- **AND** does NOT attempt to strip `---` frontmatter delimiters (those belong to preset files)

#### Scenario: Partial profiles are valid

- **WHEN** a profile defines only `paths` and omits `cloud`
- **THEN** the profile loads successfully
- **AND** missing sections are treated as absent (deny-by-default at enforcement time)

#### Scenario: Key normalization

- **WHEN** a profile contains `auth_detection` or `allowed_providers`
- **THEN** the loader normalizes keys
- **AND** the result uses kebab-case keywords (`:auth-detection`, `:allowed-providers`)

### Requirement: Profile directory and naming

Scope profiles SHALL reside in `config/gptel/scope-profiles/` with a `.yml` extension. The profile name referenced by a preset matches the filename sans extension.

**Implementation**: `jf/gptel--scope-profiles-directory` in `config/gptel/scope-profiles.org`.

#### Scenario: Profile resolved by name

- **WHEN** a preset declares `:scope-profile "coding"`
- **THEN** the system loads `config/gptel/scope-profiles/coding.yml`

#### Scenario: Missing profile handled gracefully

- **WHEN** a preset references a profile file that does not exist
- **THEN** the loader logs a warning
- **AND** session creation emits an empty (deny-by-default) drawer block

#### Scenario: Default profiles provided

- **WHEN** the gptel configuration is installed
- **THEN** these profiles exist: `coding.yml` (broad read, project write), `research.yml` (read-only), `restricted.yml` (minimal), `bash-enabled.yml` (extended bash read, project write), `system-explorer.yml` (read-only system inspection with expanded deny list)

### Requirement: Variable expansion with ${project_root}

The system SHALL expand `${project_root}` in string values during session creation, before the drawer block is emitted. Only `${project_root}` is supported.

**Implementation**: `jf/gptel-scope-profile--expand-variables` recursively walks the resolved plist.

#### Scenario: Projectile project detected

- **WHEN** session creation supplies `/path/to/project` as the project root
- **THEN** `${project_root}/**` in the profile becomes `/path/to/project/**` in the emitted drawer line

#### Scenario: No project available

- **WHEN** session creation has no project root
- **THEN** entries containing `${project_root}` are removed from the output
- **AND** a warning is logged

#### Scenario: YAML arrays coerced to lists

- **WHEN** `yaml-parse-string` returns a vector for a list-valued section
- **THEN** the expander converts the vector to a list before iterating

### Requirement: Resolution priority (named profile vs inline vs empty)

When resolving scope for a preset, a named profile reference SHALL take precedence over inline scope defaults; absence of both SHALL yield an empty drawer block.

**Implementation**: `jf/gptel-scope-profile--resolve` in `config/gptel/scope-profiles.org` inspects `jf/gptel-preset--scope-defaults`.

#### Scenario: Named profile takes precedence

- **WHEN** a preset has both `:scope-profile "coding"` and inline `:paths`
- **THEN** `coding.yml` is loaded
- **AND** the inline `:paths` are ignored

#### Scenario: Inline defaults used when no profile reference

- **WHEN** a preset has `:paths`/`:cloud` but no `:scope-profile`
- **THEN** the inline plist is used directly as the resolved scope

#### Scenario: Empty scope fallback

- **WHEN** a preset has no scope configuration at all
- **THEN** session creation emits a minimal drawer that denies by default

### Requirement: Mutable scope drawer in session.org

Each session SHALL receive a populated `:PROPERTIES:` drawer in its `session.org` at creation time. The drawer is the single source of truth for scope enforcement and for chat-mode configuration; the profile template, the registered preset, and the upstream `gptel-org-set-properties` writer are never consulted by validators or by the chat-mode save hook.

The scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, ...) are written by this module. The chat-mode configuration keys (`:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:`, optional `:GPTEL_PARENT_SESSION_ID:`) are also written by this module's renderer at session creation, drawn from the resolved preset spec — see Requirement: Integration with session creation. The drawer DOES NOT include `:GPTEL_SYSTEM:` (system prompt is intentionally excluded; it is read from the preset file at mode activation — see `gptel/chat-mode` Requirement: Configuration drawer save on buffer save).

**Implementation**: `jf/gptel-scope-profile--apply-to-drawer` and `jf/gptel-scope-profile--render-drawer-text` in `config/gptel/scope-profiles.org`. The renderer accepts the resolved preset spec (in addition to the existing scope plist) and emits the chat-mode snapshot keys alongside the scope keys.

#### Scenario: Drawer populated at session creation with full snapshot
- **WHEN** a session is created with preset `executor`
- **THEN** the new `session.org` is written with a `:PROPERTIES:` drawer at `point-min` containing `:GPTEL_PRESET: executor`, the preset's snapshot keys (`:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` etc., when non-nil in the preset spec), and the preset's resolved scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, ...)
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** followed by the chat-mode initial content (`#+begin_user\n\n#+end_user\n`)
- **AND** no `scope.yml` is written

#### Scenario: Drawer is mutable
- **WHEN** scope expansion adds a path during a session
- **THEN** the chat buffer's drawer is updated in place via `org-entry-put` / multi-value helpers
- **AND** the buffer is saved
- **AND** the registered preset and the profile template remain unchanged

#### Scenario: Drawer is the enforcement source
- **WHEN** a tool invocation requires path validation
- **THEN** `scope-validation` reads the chat buffer's drawer (or the file's drawer if no buffer is open)
- **AND** does NOT read from `gptel--known-presets` or from profile files
- **AND** does NOT read any `scope.yml` (no such file exists)

### Requirement: Integration with session creation

The profile module SHALL expose a single creation entrypoint that, given a preset name (and the module resolves the preset spec internally), target session.org path, optional project root, optional worktree paths, and optional parent-session-id, returns the drawer text (as a string) ready to embed in a freshly created `session.org`.

The returned drawer text carries:

1. `:GPTEL_PRESET: <name>`
2. `:GPTEL_PARENT_SESSION_ID: <id>` when supplied (agent sessions)
3. The chat-mode snapshot keys derived from the resolved preset spec — `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:` — each emitted only when the preset declares a non-nil value for the corresponding key
4. The scope keys `:GPTEL_SCOPE_*:` resolved from the preset's scope profile, project root, and worktree paths
5. NO `:GPTEL_SYSTEM:` line — the system prompt is intentionally excluded

**Implementation**: `jf/gptel-scope-profile--create-for-session` and `jf/gptel-scope-profile--render-drawer-text` in `config/gptel/scope-profiles.org`. The renderer's signature gains an explicit preset-spec argument (or resolves the spec internally from the preset name) so it can emit the chat-mode snapshot keys.

#### Scenario: Returns drawer text for embedding in initial session.org
- **WHEN** session creation calls `--create-for-session` with a preset and target directory before opening the buffer
- **THEN** the function resolves the profile, resolves the preset spec, expands variables, and returns a string of the form `:PROPERTIES:\n:GPTEL_PRESET: ...\n:GPTEL_MODEL: ...\n:GPTEL_TOOLS: ...\n[other snapshot keys]\n:GPTEL_SCOPE_READ: ...\n[other scope keys]\n:END:\n` ready to prepend to the chat-mode initial content
- **AND** the returned text contains no `:GPTEL_SYSTEM:` line
- **AND** session creation writes the resulting `session.org` in one shot

#### Scenario: Applies snapshot to an existing session.org buffer
- **WHEN** `--create-for-session` is called against an already-open chat buffer (e.g. an agent or branched session being initialized in a buffer)
- **THEN** the function uses `org-entry-put` and multi-value helpers to write each snapshot key (`:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, ...) and each `:GPTEL_SCOPE_*` key into the buffer's `:PROPERTIES:` drawer at `point-min`
- **AND** the function does NOT write `:GPTEL_SYSTEM:` even if the preset declares `:system`
- **AND** existing drawer keys not part of the snapshot or scope set (`:GPTEL_PARENT_SESSION_ID:`, custom keys) are preserved

#### Scenario: Sparse preset produces sparse snapshot
- **WHEN** the preset declares only `:model` and `:tools` (no `:temperature`, no `:max-tokens`, no `:num-messages-to-send`, no `:backend`)
- **AND** `--create-for-session` is invoked
- **THEN** the returned drawer text contains `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:` from the preset (plus `:GPTEL_SCOPE_*:` if the preset has a scope profile)
- **AND** the returned text does NOT contain `:GPTEL_BACKEND:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, or `:GPTEL_NUM_MESSAGES_TO_SEND:` lines

### Requirement: Deep merge for multi-worktree sessions

When session creation receives both a resolved profile and explicit worktree paths, the system SHALL deep-merge them such that worktree paths augment (not replace) the profile's configuration.

**Implementation**: `jf/gptel-scope-profile--deep-merge` in `config/gptel/scope-profiles.org` (schema-agnostic recursive merge).

#### Scenario: Worktree paths augment profile scope

- **WHEN** the resolved profile has `paths.read: ["/**"]`
- **AND** worktree paths `/project-a`, `/project-b` are supplied
- **THEN** the emitted drawer contains both the profile's entries and the worktree paths in `:GPTEL_SCOPE_READ:`/`:GPTEL_SCOPE_WRITE:`

#### Scenario: Activities session with explicit worktree paths

- **WHEN** activities integration supplies already-resolved worktree paths
- **THEN** those paths are written as-is (no `${project_root}` expansion needed)
- **AND** standard deny entries from the profile are preserved
- **AND** `:cloud` settings from the profile are preserved

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
- Parameters: preset name, target session.org path, optional project root, optional worktree paths
- Called during branch directory creation; responsible for the drawer block embedded in the new `session.org`

### With scope enforcement (`config/gptel/scope/`)

- `scope-validation` reads the session's `:PROPERTIES:` drawer (buffer-first, file fallback)
- Key normalization and schema defaulting live in the profile loader and the validator's drawer reader (the previous `scope-yaml` module is retired)
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
   └─ scope-validation.org     # Reads the :PROPERTIES: drawer; enforces at tool-call time
```
