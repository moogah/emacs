# Scope (Delta Spec)

This delta moves scope configuration from a `scope.yml` sidecar into the file-level `:PROPERTIES:` drawer of `session.org`, removes the `:security` configuration block (replacing it with module-level constants), and switches the validator's config loader to a buffer-first drawer reader.

## ADDED Requirements

### Requirement: Scope drawer encoding

Scope configuration SHALL be encoded as multi-value org properties in the file-level `:PROPERTIES:` drawer at `point-min` of `session.org`. Each list-shaped scope key uses Org's `+`-suffix multi-value convention so that one pattern occupies one drawer line. Scalar keys use single properties.

**Implementation**: `config/gptel/scope/scope-validation.org` (drawer reader); `config/gptel/scope/interfaces.org` (drawer key vocabulary)

The drawer key vocabulary:

| Drawer key                       | Type     | Maps to plist               |
|----------------------------------|----------|-----------------------------|
| `:GPTEL_SCOPE_READ:` / `+:`      | list     | `(:paths (:read ...))`      |
| `:GPTEL_SCOPE_WRITE:` / `+:`     | list     | `(:paths (:write ...))`     |
| `:GPTEL_SCOPE_MODIFY:` / `+:`    | list     | `(:paths (:modify ...))`    |
| `:GPTEL_SCOPE_EXECUTE:` / `+:`   | list     | `(:paths (:execute ...))`   |
| `:GPTEL_SCOPE_DENY:` / `+:`      | list     | `(:paths (:deny ...))`      |
| `:GPTEL_SCOPE_CLOUD_AUTH:`       | scalar   | `(:cloud (:auth-detection ...))` (`"allow"` / `"warn"` / `"deny"`) |
| `:GPTEL_SCOPE_CLOUD_PROVIDERS:` / `+:` | list | `(:cloud (:allowed-providers ...))` |

#### Scenario: Multi-value `+` form is the canonical encoding for list keys
- **WHEN** the writer emits more than one path under `:GPTEL_SCOPE_READ`
- **THEN** the first occurrence uses `:GPTEL_SCOPE_READ:` and subsequent occurrences use `:GPTEL_SCOPE_READ+:`
- **AND** the reader uses `org-entry-get-multivalued-property` to recover the full list

#### Scenario: Missing list key reads as empty list
- **WHEN** the drawer omits `:GPTEL_SCOPE_MODIFY:` entirely
- **THEN** the loaded plist carries `(:paths (:modify nil ...))` (empty list)
- **AND** validation behaves as if the user had written zero modify-allowed patterns

#### Scenario: Missing cloud-auth scalar defaults to `"warn"`
- **WHEN** the drawer omits `:GPTEL_SCOPE_CLOUD_AUTH:`
- **THEN** the loaded plist carries `(:cloud (:auth-detection "warn" ...))`
- **AND** the value matches the previous `scope-yaml` schema default

#### Scenario: Invalid cloud-auth value rejected on load
- **WHEN** `:GPTEL_SCOPE_CLOUD_AUTH:` is anything other than `"allow"`, `"warn"`, or `"deny"`
- **THEN** the loader signals a schema error (same contract as the previous YAML loader)

## MODIFIED Requirements

### Requirement: Scope configuration loading

The scope system SHALL load scope configuration fresh on every tool invocation from the session's `session.org` `:PROPERTIES:` drawer. Configuration is never cached; the buffer (or, when no buffer is open, the file) is the source of truth.

**Implementation**: `config/gptel/scope/scope-validation.org` (loader)

#### Scenario: Config resolved from the chat buffer when one is loaded
- **WHEN** a scoped tool is invoked from a chat buffer
- **THEN** the loader reads the `:PROPERTIES:` drawer from that buffer's contents (not from disk)
- **AND** the loader returns the same plist shape that previously came from `scope.yml`

#### Scenario: Config falls back to the file when no buffer is loaded
- **WHEN** the loader is invoked outside a chat buffer (e.g. a programmatic call from `request_scope_expansion`)
- **THEN** the loader resolves the session's `session.org` path via buffer-local `jf/gptel--branch-dir` or the current buffer's file directory
- **AND** parses the drawer headlessly via `with-temp-buffer` + `insert-file-contents` + the drawer reader

#### Scenario: Missing or absent drawer denies with `no_scope_config`
- **WHEN** the resolved `session.org` either does not exist or contains no `:PROPERTIES:` drawer with any `:GPTEL_SCOPE_*` key
- **THEN** the authorization entrypoint invokes the on-deny thunk with `:error "no_scope_config"`
- **AND** the response bypasses the expansion UI (missing config is not a scope violation)

#### Scenario: Configuration read fresh every call
- **WHEN** the same tool is called twice in a session after the drawer is edited
- **THEN** each call re-reads the drawer; there is no in-memory cache
- **AND** unsaved buffer edits are visible to the second call (buffer-first read)

### Requirement: Scope configuration shape

The loaded scope configuration SHALL produce a plist with `:paths` and `:cloud` sections only. Missing list entries default to nil (empty); the missing `:cloud` `:auth-detection` defaults to `"warn"`. There is no `:security` section in the configuration shape — `enforce-parse-complete` and `max-coverage-threshold` are module-level constants in `scope-validation.el`.

**Implementation**: `config/gptel/scope/scope-validation.org` (drawer reader, defaults, and security constants)

The canonical shape:

```
(:paths (:read    (PATTERN ...)
         :write   (PATTERN ...)
         :modify  (PATTERN ...)
         :execute (PATTERN ...)
         :deny    (PATTERN ...))
 :cloud (:auth-detection     "allow"|"warn"|"deny"
         :allowed-providers  (PROVIDER ...)))
```

#### Scenario: Missing list keys default to nil
- **WHEN** the drawer omits `:GPTEL_SCOPE_DENY:` entirely
- **THEN** the merged config carries `(:paths (... :deny nil))`

#### Scenario: Missing cloud-auth defaults to `"warn"`
- **WHEN** the drawer omits `:GPTEL_SCOPE_CLOUD_AUTH:`
- **THEN** the merged config carries `(:cloud (:auth-detection "warn" ...))`

#### Scenario: Invalid auth-detection rejected on load
- **WHEN** `:GPTEL_SCOPE_CLOUD_AUTH:` is anything other than `"allow"`, `"warn"`, or `"deny"`
- **THEN** the loader signals a schema error

#### Scenario: No `:security` key in the loaded plist
- **WHEN** the loader runs against any drawer
- **THEN** the returned plist has exactly the keys `:paths` and `:cloud`
- **AND** validators reading parse-complete or coverage-threshold consult module-level constants, not the plist

### Requirement: Parse completeness gate

Stage 1 SHALL refuse to validate commands when `bash-parser` reports `:parse-complete nil`. Enforcement is unconditional; there is no per-session override.

**Implementation**: `config/gptel/scope/scope-validation.org` (Stage 1)

#### Scenario: Incomplete parse rejected
- **WHEN** the command has a syntax error (e.g. unclosed quote)
- **AND** `bash-parser` reports `:parse-complete nil`
- **THEN** Stage 1 returns `:error "parse_incomplete"` with `:parse-errors` and `:command`
- **AND** Stages 2, 3, and 4 do not run

#### Scenario: Complete parse proceeds silently
- **WHEN** `:parse-complete` is `t`
- **THEN** Stage 1 returns nil and the pipeline proceeds

### Requirement: Coverage threshold warning

The coverage check SHALL be non-blocking: it emits an elisp `warn` when `bash-parser`'s semantic plugin coverage ratio is below `1.0` (the constant `jf/gptel-scope--coverage-threshold` in `scope-validation.el`) and otherwise returns nil. The threshold is fixed at module load and is not configurable per session.

**Implementation**: `config/gptel/scope/scope-validation.org`

#### Scenario: Below-threshold coverage warns
- **WHEN** `:coverage-ratio` is below `1.0`
- **THEN** the validator emits a `warn` and the pipeline still succeeds

#### Scenario: At-or-above threshold silent
- **WHEN** `:coverage-ratio` is `1.0`
- **THEN** no warning is emitted

## REMOVED Requirements

### Requirement: scope-yaml boundary module

**Reason**: The `scope-yaml` module existed solely to translate `scope.yml` on disk into and out of the validator's plist shape. With scope state moving into the org property drawer, the YAML boundary is replaced by `org-entry-get-multivalued-property` (read) and `org-entry-put` + multi-value helpers (write). All four prior responsibilities of `scope-yaml` (parse, key normalization, schema defaulting, kebab/snake conversion) are subsumed: parsing becomes a drawer read; key normalization is a fixed table of `:GPTEL_SCOPE_*` ↔ plist mappings; schema defaulting is a single defaults plist in `scope-validation.el`; kebab/snake conversion is unnecessary because drawer keys are uppercase ASCII by construction.

**Migration**: Delete `config/gptel/scope/scope-yaml.org` and `scope-yaml.el`. Remove the `(require 'jf-gptel-scope-yaml)` lines from `scope-profiles.el` and `scope-expansion.el`. Replace `jf/gptel-scope-yaml--load-schema` callers with the drawer reader (`jf/gptel-scope--load-from-buffer` or `--load-from-file`). Replace `jf/gptel-scope-yaml--parse-file` callers in `scope-expansion.el` with the drawer reader. Replace `jf/gptel-scope-yaml--kebab-to-snake` callers (none remain after the YAML writer is removed). Remove the loader entry in `gptel.el` (`jf/load-module .../scope/scope-yaml.el`).

### Requirement: scope.yml on-disk persistence

**Reason**: The on-disk `scope.yml` is replaced by drawer-resident scope. It is no longer written by session creation, mutated by the expansion UI, or consulted by the validator. Keeping the file would be a source of stale state.

**Migration**: Existing sessions with `scope.yml` are not migrated; users are expected to recreate sessions (per the project's "old sessions are dead" cutover policy). New session creation does not write `scope.yml`. The expansion UI no longer touches `scope.yml`. The validator no longer reads `scope.yml`. References to `scope.yml` in other specs are removed in this change's other delta specs.

### Requirement: `:security` configuration section

**Reason**: The two `:security` knobs (`enforce-parse-complete`, `max-coverage-threshold`) were never set differently from their defaults in any session, and their per-session configurability was the main reason the `scope-yaml` schema needed to be parsed and merged. Hard-coding them removes a config surface area that existed only to be ignored.

**Migration**: Replace any reference to `(plist-get config :security)` in `scope-validation.el` with the constants `jf/gptel-scope--enforce-parse-complete` (always `t`) and `jf/gptel-scope--coverage-threshold` (always `1.0`). Remove the `:security` plist key from the canonical scope-config-shape contract in `interfaces.org`. Remove `validate-security-config` and the `:security` branch of `merge-schema-defaults` (both deleted with the `scope-yaml` module). The "warn when not enforced" branch of Stage 1 (parse completeness) is also removed because the constant is always `t`.
