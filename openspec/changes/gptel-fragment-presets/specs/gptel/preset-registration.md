## ADDED Requirements

### Requirement: Preset authored as Org source tangled to Elisp

A preset SHALL be authored as an `.org` source file that tangles to an `.el`
file, consistent with the repository's literate-programming workflow. The
tangled `.el` SHALL register the preset (via `gptel-make-preset`) at load time.
Editing the `.org` source and re-tangling SHALL be the supported way to change a
preset; the `.el` is a generated artifact. A preset's role content SHALL be
authored as a role fragment (a static `prompt-fragments` fragment) whose
rendered text is the preset's role contribution.

#### Scenario: Preset org tangles to a registering el
- **WHEN** a preset `.org` source is tangled
- **THEN** an `.el` artifact is produced that, when loaded, registers the preset via `gptel-make-preset`

#### Scenario: Role content authored as a fragment
- **WHEN** a preset declares role content
- **THEN** that content is authored as a static role fragment and its rendered text is the preset's role contribution

### Requirement: Preset configuration declared in an Elisp config block

Preset configuration (e.g. `:backend`, `:model`, `:tools`, `:temperature`,
`:description`, and scope/mode keys) SHALL be declared in an Elisp config block
within the preset `.org` source, using native Elisp value types. Because the
config is Elisp, the system SHALL NOT perform YAML parsing, snake_case→kebab-case
key normalization, or YAML-to-Elisp value coercion when registering presets.

#### Scenario: Config block provides native Elisp types
- **WHEN** a preset config block declares `:model` as a symbol and `:tools` as a list of strings
- **THEN** those values are used directly for registration with no coercion step

#### Scenario: No YAML processing on the registration path
- **WHEN** a preset is registered
- **THEN** no YAML parsing, key normalization, or YAML-type coercion is performed

## MODIFIED Requirements

### Requirement: Scope key extraction

The system SHALL extract scope-related keys from the preset configuration before
registration with `gptel-make-preset`. Scope keys are: `:paths`,
`:shell-commands`, `:bash-tools`, and `:scope-profile`. The keys SHALL be read
directly from the preset's Elisp config (no YAML normalization step precedes
extraction). Extracted scope data SHALL be stored in
`jf/gptel-preset--scope-defaults`, an alist mapping preset name symbols to scope
plists, and SHALL be removed from the plist passed to `gptel-make-preset`. The
legacy `:org-roam-patterns` key is not a scope key and SHALL NOT be extracted.

#### Scenario: Scope keys stored and stripped before registration
- **WHEN** a preset config carries `:paths`, `:shell-commands`, or `:scope-profile`
- **THEN** those keys are stored in `jf/gptel-preset--scope-defaults` under the preset name
- **AND** they are removed from the plist passed to `gptel-make-preset`

#### Scenario: Scope profile reference extracted
- **WHEN** a preset config declares `:scope-profile "coding"`
- **THEN** the scope defaults entry for that preset includes `:scope-profile "coding"`
- **AND** `:scope-profile` is removed from the registration plist

#### Scenario: Preset with no scope keys
- **WHEN** a preset config carries no scope-related keys
- **THEN** no entry is added to `jf/gptel-preset--scope-defaults` for that preset
- **AND** the registration plist is passed to `gptel-make-preset` unchanged

#### Scenario: Scope defaults used during session creation
- **WHEN** a session is created with a given preset
- **THEN** the scope subsystem looks up the preset in `jf/gptel-preset--scope-defaults` to initialize the session's scope

### Requirement: Preset registration via gptel-make-preset

The system SHALL register each preset with upstream's `gptel-make-preset`, using
the preset's basename (sans extension) as the name symbol. Registration SHALL be
driven by the tangled preset `.el` artifacts rather than by scanning a directory
of `.md` files and parsing YAML. The values passed to `gptel-make-preset` SHALL
be the native-Elisp config (scope keys excluded and stored separately), and
`:system` SHALL be the preset's rendered role text. Registration SHALL be
idempotent — re-loading a preset's `.el` updates the existing entry.

#### Scenario: Preset registered from its tangled el
- **WHEN** a preset's tangled `.el` is loaded at init
- **THEN** the preset is registered via `gptel-make-preset` and is accessible via `(gptel-get-preset '<name>)`

#### Scenario: Preset name derived from basename
- **WHEN** registering the preset whose source basename is `system-explorer`
- **THEN** the preset name symbol is `system-explorer` and it is callable via `(gptel-get-preset 'system-explorer)`

#### Scenario: Registration is idempotent
- **WHEN** a preset's `.el` is re-loaded after an edit and re-tangle
- **THEN** the existing `gptel--known-presets` entry is updated rather than duplicated

#### Scenario: Presets visible in transient menu and inline mentions
- **WHEN** a preset is registered via `gptel-make-preset`
- **THEN** it appears in gptel's transient preset menu with its `:description` annotation
- **AND** it is usable inline via `@<preset-name>` and via `gptel-with-preset`

## REMOVED Requirements

### Requirement: Preset file parsing

**Reason:** Presets are no longer `.md` files with YAML frontmatter and a verbatim
Markdown body. They are `.org` sources tangling to `.el`, with config in an Elisp
block and role content authored as a fragment. There is no frontmatter to parse.

**Migration:** Re-author each preset as an `.org` source (config block + role
fragment). The frontmatter `:system` body becomes the role fragment content; all
other frontmatter keys become entries in the Elisp config block.

### Requirement: YAML key normalization

**Reason:** Config is authored directly in Elisp with kebab-case keyword keys, so
there is no snake_case→kebab-case normalization step.

**Migration:** Write config keys as Elisp keywords (e.g. `:scope-profile`,
`:confirm-tool-calls`) directly in the config block.

### Requirement: YAML coercion fixes

**Reason:** Config values are authored as native Elisp types (symbols, lists, `t`/
`nil`), so there is no YAML-to-Elisp coercion step (no `:true`/`:false` mapping,
no string-to-symbol interning of `:model`).

**Migration:** Write `:model` as a symbol, booleans as `t`/`nil`, and
`:confirm-tool-calls` as `nil`/`'auto`/`t` directly in the config block.
