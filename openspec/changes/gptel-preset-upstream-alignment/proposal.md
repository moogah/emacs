## Why

The gptel upstream has added preset persistence features (commits 07715ec, 11ddace) that track applied presets by name, enable differential save/restore, and solve the Org property size limit for system messages. Our preset management code evolved independently using `gptel-agent` as a file parser and building custom load/apply/serialize functions. These two implementations now overlap significantly, and our extensions should align with upstream's model — immutable registered presets applied by name — to reduce maintenance burden, leverage upstream's save/restore, and keep our code mergeable upstream. This also means separating scope configuration (mutable, per-session) from preset definitions (immutable, registered at init).

## What Changes

### Removals

- **BREAKING**: Drop `gptel-agent` package dependency entirely. Our `PersistentAgent` tool is the preferred sub-agent mechanism and does not use `gptel-agent` APIs. The only functional dependency was `gptel-agent-read-file` (YAML frontmatter parser), which we replace with our own.
- **BREAKING**: Remove `preset.md` from session directories. Sessions no longer copy preset files — they reference presets by name via `gptel--preset` in Local Variables. Upstream's save/restore handles the rest.
- Remove `jf/gptel--load-preset-from-file`, `jf/gptel--apply-session-preset`, `jf/gptel--normalize-preset-for-serialization` — replaced by upstream's `gptel--apply-preset` and differential save.
- Remove system message save-prevention advice — upstream's differential save omits system messages that match the preset.
- Remove vestigial scope code: `scope-manager.org` (v1.0), v2.0 templates in `scope-commands.org`.
- Remove stale `(require 'gptel-agent)` from `persistent-agent.org` and `sql-tools.org`.

### New Capabilities

- **Preset registration pipeline**: Parse preset `.md` files at init, fix YAML coercion issues (`:model` string-to-symbol, `:confirm-tool-calls` "nil"-to-nil, `:include-tool-results` :false-to-nil), strip scope keys, and register via `gptel-make-preset`. All presets become visible in gptel's transient menu, `@preset` inline cookies, and `gptel-with-preset`.
- **Scope profiles**: Standalone scope configuration files (`config/gptel/scope-profiles/*.yml`) that serve as templates for per-session `scope.yml` documents. Presets reference a scope profile by name. Scope is mutable per-session; presets are immutable.

### Modifications

- Sessions reference presets by name (stored in Local Variables as `gptel--preset` symbol). Session creation applies the preset via upstream's `gptel--apply-preset` with a buffer-local setter.
- Session resume delegates entirely to upstream's `gptel--restore-state` (applies preset, overlays overrides). Our open hook only needs to load `scope.yml` for scope enforcement.
- Scope enforcement reads from `scope.yml` in the session directory instead of `preset.md` YAML frontmatter.
- Scope expansion writes to `scope.yml` instead of `preset.md`.
- Branch creation copies `scope.yml` (mutable scope) and `metadata.yml` but no longer copies `preset.md`.
- PersistentAgent reads preset definitions from `gptel--known-presets` and applies them via `gptel-with-preset` (already does this), with agent directories containing `scope.yml` and `metadata.yml` instead of `preset.md`.
- Skills `@mention` expansion (currently dead code) adapts to iterate `gptel--known-presets` instead of `gptel-agent--agents`.

## Capabilities

### New Capabilities

- `preset-registration`: Preset file parsing, YAML coercion, scope extraction, and registration into gptel's `gptel--known-presets` via `gptel-make-preset`. Runs at init time. Handles the bridge between file-based preset definitions and gptel's in-memory preset registry.
- `scope-profiles`: Scope profile template files and per-session scope documents. Defines the `scope.yml` format, profile templates, default selection from presets, and session-level scope lifecycle (creation from profile, mutation via expansion, loading for enforcement).

### Modified Capabilities

- `gptel/sessions-persistence`: Session create no longer copies `preset.md` — stores preset name only. Session open delegates restore to upstream `gptel--restore-state`. Session save relies on upstream differential save. Session directory structure loses `preset.md`, gains `scope.yml`.
- `gptel/sessions-branching`: Branch creation copies `scope.yml` instead of `preset.md`. No longer copies preset file.
- `gptel/persistent-agent`: Agent configuration comes from `gptel--known-presets` lookup (not file parsing). Agent directories contain `scope.yml` and `metadata.yml` instead of `preset.md`. Zero-inheritance principle unchanged but implementation path changes.
- `gptel/scope`: Scope config loading reads from `scope.yml` instead of `preset.md` YAML frontmatter. Validation logic unchanged.
- `gptel/scope-expansion`: "Add to scope" writes to `scope.yml` instead of `preset.md`. YAML serialization targets a simpler document (scope-only, no frontmatter parsing needed).
- `gptel/scope-presets`: **Replaced by `scope-profiles`**. The combined preset+scope document concept is retired. This spec becomes obsolete and is superseded by the new `scope-profiles` spec plus the preset files themselves (which remain in `config/gptel/presets/` but contain only model/backend/tools/system-message, no scope).

## Impact

### Code affected
- `config/gptel/gptel.org` — remove `use-package gptel-agent` block, add preset registration, update default tools
- `config/gptel/sessions/commands.org` — remove preset file loading/applying/serializing functions, simplify session creation
- `config/gptel/sessions/constants.org` — new scope profile directory path
- `config/gptel/scope/scope-core.org` — load config from `scope.yml` instead of `preset.md`
- `config/gptel/scope/scope-commands.org` — remove v2.0 templates, update tool parsing
- `config/gptel/scope/scope-expansion.org` — write to `scope.yml` instead of `preset.md`
- `config/gptel/scope/scope-manager.org` — delete entirely (v1.0 dead code)
- `config/gptel/tools/persistent-agent.org` — remove `gptel-agent` require, update agent init to use `gptel--known-presets`
- `config/gptel/tools/sql-tools.org` — remove unnecessary `gptel-agent` require
- `config/gptel/presets/*.md` — remove scope sections (`:paths`, `:org_roam_patterns`, `:shell_commands`), add `scope_profile:` reference, remove "Agent" tool references

### Dependencies
- Drops: `gptel-agent` package (including `gptel-agent-tools.el`)
- Adds: Direct dependency on `yaml.el` (already installed as transitive dep of gptel-agent)
- Requires: gptel upstream with preset persistence (commits 07715ec, 11ddace)

### Existing sessions
- Existing session directories with `preset.md` will need migration or graceful fallback during transition
