## ADDED Requirements

### Requirement: Preset file parsing

The system SHALL parse preset `.md` files from `config/gptel/presets/` by extracting YAML frontmatter (between `---` delimiters) and the markdown body (as `:system` message). The parser SHALL use `yaml-parse-string` with `:object-type 'plist`, `:object-key-type 'keyword`, `:sequence-type 'list`.

The parser SHALL NOT depend on the `gptel-agent` package.

#### Scenario: Parse valid preset file
- **WHEN** parsing `config/gptel/presets/executor.md` containing YAML frontmatter and a body
- **THEN** the system returns a plist with `:backend`, `:model`, `:tools`, `:temperature`, etc. from frontmatter
- **AND** includes `:system` key with the markdown body text after the closing `---`

#### Scenario: Parse file with no frontmatter
- **WHEN** parsing a `.md` file that lacks `---` delimiters
- **THEN** the system returns nil
- **AND** logs a warning with the file path

#### Scenario: Parse file that does not exist
- **WHEN** parsing a path that does not exist
- **THEN** the system returns nil
- **AND** does not signal an error

#### Scenario: YAML parsing error handled gracefully
- **WHEN** the YAML frontmatter contains invalid syntax
- **THEN** the system returns nil
- **AND** logs an error with the file path and parse error

### Requirement: YAML coercion fixes

The system SHALL fix known YAML-to-elisp coercion issues after parsing, before registration. Coercion SHALL handle:

- `:model` — string values SHALL be interned to symbols (e.g., `"claude-opus-4-5"` becomes `'claude-opus-4-5`)
- `:confirm-tool-calls` — string `"nil"` SHALL become `nil`, string `"auto"` SHALL become symbol `'auto`, string `"always"` SHALL become `t`
- `:include-tool-results` — YAML `:false` keyword SHALL become `nil`, YAML `t` passes through

#### Scenario: Model string converted to symbol
- **WHEN** a parsed plist contains `:model "claude-opus-4-5"`
- **THEN** after coercion the value is the symbol `claude-opus-4-5`

#### Scenario: Confirm-tool-calls nil string converted
- **WHEN** a parsed plist contains `:confirm-tool-calls "nil"`
- **THEN** after coercion the value is `nil` (not the string)

#### Scenario: Confirm-tool-calls auto string converted
- **WHEN** a parsed plist contains `:confirm-tool-calls "auto"`
- **THEN** after coercion the value is the symbol `auto`

#### Scenario: Include-tool-results false converted
- **WHEN** a parsed plist contains `:include-tool-results :false`
- **THEN** after coercion the value is `nil`

#### Scenario: Unknown keys pass through unchanged
- **WHEN** a parsed plist contains keys not in the coercion table
- **THEN** their values pass through without modification

### Requirement: Scope key extraction

The system SHALL extract scope-related keys from parsed preset plists before registration with `gptel-make-preset`. Scope keys are: `:paths`, `:org_roam_patterns`, `:shell_commands`, `:scope_profile`.

Extracted scope data SHALL be stored in `jf/gptel-preset--scope-defaults`, an alist mapping preset name symbols to scope plists.

#### Scenario: Scope keys stripped from preset plist
- **WHEN** a parsed plist contains `:paths`, `:org_roam_patterns`, and `:shell_commands`
- **THEN** these keys are removed from the plist before calling `gptel-make-preset`
- **AND** the registered preset contains only gptel-recognized keys

#### Scenario: Scope defaults stored by preset name
- **WHEN** parsing preset "executor" with `:paths (:read ("/**") :write ("/tmp/**"))`
- **THEN** `jf/gptel-preset--scope-defaults` contains entry `(executor . (:paths (:read ("/**") :write ("/tmp/**"))))`

#### Scenario: Scope profile reference extracted
- **WHEN** a preset contains `:scope_profile "coding"`
- **THEN** the scope defaults entry includes `:scope-profile "coding"`
- **AND** `:scope_profile` is removed from the preset plist

#### Scenario: Preset with no scope keys
- **WHEN** a preset contains no scope-related keys
- **THEN** no entry is added to `jf/gptel-preset--scope-defaults` for that preset
- **AND** the preset plist is passed to `gptel-make-preset` unchanged (except coercion fixes)

### Requirement: Preset registration via gptel-make-preset

The system SHALL register each parsed and coerced preset with upstream's `gptel-make-preset`, using the preset file's basename (sans extension) as the name symbol.

**Value formats passed to `gptel-make-preset`:**
- `:backend` — string name (e.g., `"Claude"`). Upstream's `gptel--apply-preset` resolves to a backend object at application time.
- `:model` — symbol (e.g., `'claude-opus-4-6`). Already coerced from string by the parser.
- `:tools` — list of tool name strings (e.g., `("PersistentAgent" "Read" "Write")`). Upstream resolves to tool objects via `gptel-get-tool` at application time.
- `:system` — string (the markdown body from the preset file).
- `:temperature`, `:description` — passed through as-is.

#### Scenario: All presets in directory registered
- **WHEN** `jf/gptel-preset-register-all` runs at init time
- **THEN** every `.md` file in `config/gptel/presets/` is parsed, coerced, scope-stripped, and registered
- **AND** each preset is accessible via `(gptel-get-preset 'executor)`, `(gptel-get-preset 'research)`, etc.

#### Scenario: Preset name derived from filename
- **WHEN** registering `config/gptel/presets/executor.md`
- **THEN** the preset name symbol is `executor` (file-name-sans-extension, interned)

#### Scenario: Presets visible in transient menu
- **WHEN** presets are registered via `gptel-make-preset`
- **THEN** they appear in gptel's transient preset menu (`@` key)
- **AND** their `:description` is shown as an annotation during completion
- **AND** can be selected by the user

#### Scenario: Presets usable inline via @preset-name syntax
- **WHEN** presets are registered in `gptel--known-presets`
- **THEN** typing `@executor` in a prompt automatically applies the `executor` preset before sending the request
- **AND** `@executor` is removed from the prompt text
- **AND** completion is available for preset names via `gptel-preset-capf`

#### Scenario: Presets usable with gptel-with-preset
- **WHEN** a preset `executor` is registered
- **THEN** `(gptel-with-preset 'executor ...)` correctly applies the preset's configuration

#### Scenario: Duplicate registration updates existing
- **WHEN** `jf/gptel-preset-register-all` is called again (e.g., manual refresh)
- **THEN** existing entries in `gptel--known-presets` are updated (not duplicated)

### Requirement: Registration timing and dependencies

Preset registration SHALL run during gptel initialization, after `yaml.el` and `gptel` are loaded, but before any session hooks fire.

#### Scenario: Registration before session open
- **WHEN** a user opens an existing session.md file
- **THEN** `gptel--restore-state` can find the preset by name in `gptel--known-presets`
- **AND** preset was registered before the find-file-hook ran

#### Scenario: yaml.el required before parsing
- **WHEN** `jf/gptel-preset-register-all` runs
- **THEN** `(require 'yaml)` has already been called
- **AND** `yaml-parse-string` is available

#### Scenario: Skills expansion runs after registration
- **WHEN** presets are registered
- **THEN** the skills `@mention` expansion iterates `gptel--known-presets` to expand `@skill` tokens in `:system` strings
- **AND** runs once after all presets are registered

### Requirement: Presets directory configuration

The system SHALL use a configurable directory path for preset file discovery.

#### Scenario: Default presets directory
- **WHEN** `jf/gptel-presets-directory` is not customized
- **THEN** it defaults to `config/gptel/presets/` relative to `jf/emacs-dir`

#### Scenario: Only .md files scanned
- **WHEN** scanning the presets directory
- **THEN** only files with `.md` extension are processed
- **AND** subdirectories, dotfiles, and non-markdown files are ignored

### Requirement: Upstream preset features — out of scope

Upstream's `gptel-make-preset` supports advanced features that this registration pipeline SHALL NOT use in the initial implementation. These are documented here for awareness:

- **`:parents`** — preset inheritance chains (apply parent presets before this one)
- **`:pre` / `:post`** — hook functions run before/after preset application
- **Value modifiers** — `(:append LIST)`, `(:prepend LIST)`, `(:eval FORM)`, `(:function FUNC)`, `(:merge PLIST)` for dynamic value composition
- **`:system` as symbol** — resolves against `gptel-directives` alist

These features may be adopted in future iterations (e.g., using `:parents` for preset composition, or `:post` for scope initialization). For now, presets are registered with flat plists containing concrete values only.
