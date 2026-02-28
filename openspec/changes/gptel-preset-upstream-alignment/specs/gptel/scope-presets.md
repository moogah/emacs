## REMOVED Requirements

### Requirement: Preset YAML frontmatter structure
**Reason**: The combined preset+scope document (`preset.md` in session directories) is retired. Preset definitions remain in `config/gptel/presets/*.md` for authoring but are registered into `gptel--known-presets` at init time and referenced by name. Scope configuration moves to standalone `scope.yml` files.
**Migration**: Scope sections (`:paths`, `:org_roam_patterns`, `:shell_commands`) are extracted from preset files during registration and stored in `jf/gptel-preset--scope-defaults`. Session directories use `scope.yml` instead of embedding scope in `preset.md`. See `scope-profiles` spec for the new scope document format.

### Requirement: Path patterns use glob syntax
**Reason**: Path pattern format is unchanged, but the location has moved. Glob patterns now live in `scope.yml` (per-session) and scope profile templates, not in `preset.md` frontmatter.
**Migration**: Glob syntax is preserved in the new `scope-profiles` spec. No pattern format changes.

### Requirement: Tool declarations in preset
**Reason**: Tool declarations are now part of the registered preset in `gptel--known-presets`, managed by upstream's `gptel-make-preset`. The preset file still contains the `tools:` array in YAML frontmatter, but it is parsed at init time and registered — not read from session directories at runtime.
**Migration**: Tool declarations remain in `config/gptel/presets/*.md` authoring files. The `preset-registration` spec covers how they are parsed and registered.

### Requirement: Org-roam pattern format
**Reason**: Org-roam patterns move from `preset.md` frontmatter to `scope.yml` / scope profiles. Format is unchanged.
**Migration**: See `scope-profiles` spec.

### Requirement: Shell command pattern format
**Reason**: Shell command patterns move from `preset.md` frontmatter to `scope.yml` / scope profiles. Format is unchanged.
**Migration**: See `scope-profiles` spec.

### Requirement: Preset file location convention
**Reason**: Preset files remain at `config/gptel/presets/<name>.md` for authoring, but they are no longer copied to session directories. Sessions reference presets by name via the `gptel--preset` Local Variable.
**Migration**: Remove `jf/gptel--copy-preset-template` from session creation. The `preset-registration` spec covers how preset files are scanned and registered at init time.

### Requirement: Preset metadata fields
**Reason**: Metadata fields (description, backend, model, temperature, etc.) are now registered via `gptel-make-preset` and managed by upstream's preset system. The authoring format in `.md` files is unchanged.
**Migration**: See `preset-registration` spec for how fields are parsed and registered.

### Requirement: Preset format evolution
**Reason**: The concept of "template functions vs. hand-crafted presets" is replaced by a single registration pipeline. All preset `.md` files are parsed identically.
**Migration**: Legacy template functions in `scope-commands.el` are removed.

### Requirement: Preset markdown body as instructions
**Reason**: The markdown body is still parsed as the `:system` message during preset registration. This behavior is preserved in the `preset-registration` spec.
**Migration**: No change to the authoring experience. Body text is extracted during parsing and stored as `:system` in the registered preset.

### Requirement: Deny-by-default security model
**Reason**: The deny-by-default model is preserved but now enforced via `scope.yml` documents and scope profiles, not via `preset.md` frontmatter.
**Migration**: See `scope-profiles` spec and `scope.md` delta spec.
