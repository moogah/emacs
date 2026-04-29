## Why

Now that gptel sessions are org-mode native (chat-mode buffers in `session.org`), the per-session `scope.yml` sidecar is the only piece of session state still living outside the buffer. It duplicates the storage pattern already established for the preset (`:GPTEL_PRESET:` in the file's `:PROPERTIES:` drawer at point-min) and forces a separate YAML loader, schema, and file writer. Folding the scope configuration into the same property drawer collapses storage into a single org-native source of truth, lets scope mutations land in the buffer's undo history, and removes the entire `scope-yaml` boundary module along with all `scope.yml` I/O. The `security` section — `enforce_parse_complete` and `max_coverage_threshold` — becomes hard-coded constants because the per-session knobs were never used in practice and their presence forces a schema we no longer need.

## What Changes

- **BREAKING** `scope.yml` removed as the per-session scope storage medium. Existing sessions with only a `scope.yml` are not migrated and will fail validation; the user has confirmed old sessions are expected to be recreated.
- Scope state moves into the file-level `:PROPERTIES:` drawer of `session.org` using multi-value `+` properties: `:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_MODIFY:`, `:GPTEL_SCOPE_EXECUTE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, `:GPTEL_SCOPE_CLOUD_PROVIDERS:`.
- Scope is read **buffer-first**: validators consult the live chat buffer's drawer (matching what the user just typed); the file is only consulted when no buffer is open.
- Scope mutations from the expansion UI write to the buffer drawer via `org-entry-put` / multi-value helpers and trigger `save-buffer`, with care to avoid disrupting active streaming or user editing.
- **BREAKING** `:security` is removed from the scope plist shape entirely. `enforce-parse-complete` becomes the constant `t` and `max-coverage-threshold` becomes the constant `1.0`, both referenced directly from `scope-validation.el`. There is no per-session override.
- `config/gptel/scope/scope-yaml.{org,el}` is deleted.
- `jf/gptel-scope-profile--write-scope-yml` is removed; replaced by `--apply-to-drawer` (or equivalent) that writes resolved scope into the target session's drawer.
- `persistent-agent.el` pre-populates the child agent's `session.org` with a scope-bearing drawer at file-creation time; the call to `--write-scope-yml` and the parent-scope-read code paths around it go away.
- Scope expansion writer in `scope-expansion.el` switches from sidecar YAML mutation to buffer-drawer mutation.
- Profile templates (`config/gptel/scope-profiles/*.yml`) remain plain YAML on disk — they are reusable, immutable templates, not session state. Only the *output sink* changes.

## Capabilities

### New Capabilities

_(none — this change is a storage-medium and schema reduction for existing capabilities, not a new behavior.)_

### Modified Capabilities

- `gptel/scope`: Configuration source changes from `scope.yml` to the session.org property drawer; loader becomes buffer-first; the `:security` plist key is removed and `enforce-parse-complete` / `max-coverage-threshold` become constants in the validator module.
- `gptel/scope-profiles`: Profile resolution still loads YAML templates and expands variables, but the terminal write step becomes a drawer writer, not a YAML file writer. The "mutable scope.yml in session branches" requirement is replaced with "mutable scope drawer in session.org".
- `gptel/scope-expansion`: The "add to scope" choice writes to the chat buffer's property drawer (via `org-entry-put` / multi-value helpers) and saves the buffer, instead of appending to a sidecar YAML file.
- `gptel/sessions-persistence`: The directory layout no longer includes `scope.yml`; scope is part of `session.org`.
- `gptel/persistent-agent`: Child agent creation writes scope into the agent's `session.org` initial content (drawer pre-populated) rather than producing a `scope.yml` sidecar.

## Impact

**Code removed**:
- `config/gptel/scope/scope-yaml.org` and `scope-yaml.el` (the entire YAML boundary module)
- YAML writer / reader code paths in `scope-profiles.el`, `scope-validation.el`, `scope-expansion.el`, `persistent-agent.el`
- `:security`-related defaults, validators, and merge logic in the scope schema
- `scope.yml` references in `interfaces.el` (canonical config-shape contract) and in tests

**Code refactored**:
- `config/gptel/scope/scope-validation.org` — config loader switches from `(scope-yaml-load-schema scope-file)` to a buffer-first drawer reader
- `config/gptel/scope-profiles.org` — terminal `--write-scope-yml` step becomes `--apply-to-drawer`
- `config/gptel/scope/scope-expansion.org` — writer switches from YAML append to `org-entry-put` + multi-value
- `config/gptel/tools/persistent-agent.el` — child agent creation embeds scope drawer in initial `session.org` content
- `config/gptel/scope/interfaces.org` — scope config shape spec drops `:security`
- All scope tests under `config/gptel/scope/test/yaml/`, `config/gptel/test/session-creation-spec.el`, and `config/gptel/tools/test/persistent-agent/creation-spec.el` migrate from "fixture a `scope.yml` on disk" to "fixture a chat buffer with a property drawer"

**Code unchanged**:
- Scope profile templates in `config/gptel/scope-profiles/*.yml`
- Validator pipeline stages, error codes, glob matching, path validation hierarchy
- `gptel-make-scoped-tool` macro and the authorize-tool-call dispatcher contract
- Filesystem tools (`read_file_in_scope`, `write_file_in_scope`, `edit_file_in_scope`)
- `bash-parser` semantic plugin system

**User impact**:
- Scope is visible inline in the session's org drawer; folded by default via standard org folding.
- Scope mutations are part of the chat buffer's undo history (`C-_` reverts an accidental "add to scope").
- No migration: existing sessions with `scope.yml` only will not validate; the user has confirmed this is acceptable since old sessions are dead.
- The per-session escape hatches `enforce_parse_complete: false` and `max_coverage_threshold: 0.8` no longer exist; both behaviors are now fixed (`true` and `1.0` respectively). The coverage warning will fire on essentially every bash command whose semantic plugins don't cover 100%, which the user explicitly confirmed.

**Dependencies**:
- Builds on `gptel-org-mode-sessions` (merged, in testing), which established session.org as the chat file format and the `:GPTEL_PRESET:` drawer pattern.
- Coordinates with `scope-rearch-followups` (in flight): both touch `scope-expansion.org`, `scope-profiles.org`, and `preset-registration.org`. That change should land first or be merged into this one's tasks.
