## MODIFIED Requirements

### Requirement: Package dependencies

The gptel loader (`config/gptel/gptel.org`) SHALL remove the `gptel-agent` package dependency and add `yaml.el` as an explicit dependency.

#### Scenario: gptel-agent package removed
- **WHEN** gptel initializes
- **THEN** there is no `use-package gptel-agent` block
- **AND** no `(require 'gptel-agent)` calls anywhere in gptel config modules
- **AND** `gptel-agent-read-file` is not called
- **AND** `gptel-agent--agents` is not referenced

#### Scenario: yaml.el available for preset parsing
- **WHEN** gptel initializes
- **THEN** `(require 'yaml)` is called before preset registration
- **AND** `yaml-parse-string` is available for frontmatter parsing

### Requirement: Preset registration pipeline

The gptel loader SHALL register all preset files as upstream presets during initialization, in the `:config` block after gptel and yaml.el are loaded.

The registration pipeline SHALL execute in this order:
1. `(require 'yaml)` — ensure parser is available
2. `jf/gptel-preset-register-all` — scan presets directory, parse, coerce, strip scope keys, register via `gptel-make-preset`
3. Skills `@mention` expansion — iterate `gptel--known-presets` and expand `@skill` tokens in `:system` strings (replaces advice on `gptel-agent-update`)
4. Session hooks — `find-file-hook` for session detection/restore

#### Scenario: Presets registered before session hooks
- **WHEN** a user opens Emacs and a session.md file is auto-discovered
- **THEN** preset registration has already completed
- **AND** `gptel--restore-state` can find the preset by name in `gptel--known-presets`

#### Scenario: Skills expansion iterates registered presets
- **WHEN** skills expansion runs after preset registration
- **THEN** it iterates `gptel--known-presets` (not `gptel-agent--agents`)
- **AND** expands `@skill` tokens in each preset's `:system` string
- **AND** updates the registered preset in-place

#### Scenario: Re-registration is idempotent
- **WHEN** `jf/gptel-preset-register-all` is called multiple times (e.g., via `jf/reload-module`)
- **THEN** existing entries in `gptel--known-presets` are updated, not duplicated

### Requirement: Default tools configuration

The gptel loader SHALL NOT set a global default for `gptel-tools`. Presets define their own tools.

#### Scenario: No global default tools
- **WHEN** gptel initializes
- **THEN** `gptel-tools` is NOT set via `setq-default`
- **AND** non-session gptel buffers (ad-hoc `M-x gptel`) start with no tools (upstream default)

#### Scenario: Preset tools applied per-buffer
- **WHEN** a session opens and a preset is applied
- **THEN** tools are set buffer-locally from the preset's `:tools` list
- **AND** do not affect other gptel buffers

## REMOVED Requirements

### Requirement: gptel-agent package integration
**Reason**: The only functional dependency on `gptel-agent` was its YAML frontmatter parser (`gptel-agent-read-file`), which is replaced by `jf/gptel-preset--parse-file`. The `Agent` interactive tool is replaced by `PersistentAgent`. The two hardcoded `gptel-make-preset` calls in `gptel-agent-update` are superseded by the registration pipeline.
**Migration**: Remove `use-package gptel-agent` block, remove stale `(require 'gptel-agent)` in `persistent-agent.org` and `sql-tools.org`, replace skills expansion advice target.

### Requirement: Default Agent tool
**Reason**: `(setq-default gptel-tools (list (gptel-get-tool "Agent")))` depended on gptel-agent providing the "Agent" tool. With gptel-agent removed, there is no "Agent" tool. Presets define their own tools, and toolless is the correct upstream default for ad-hoc buffers.
**Migration**: Remove the `setq-default` line. Preset files that listed "Agent" in their tools should be updated to "PersistentAgent".
