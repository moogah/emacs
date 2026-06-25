# Preset Registration

## Purpose

Provides the pipeline for registering gptel presets authored as `.org` sources
that tangle to `.el` artifacts under `config/gptel/presets/`. Each preset's
tangled `.el` registers the preset with upstream's `gptel-make-preset` at load
time, using a native-Elisp config block and a rendered role fragment as the
`:system` message. Because configuration is authored directly in Elisp, the
system performs no YAML parsing, key normalization, or type coercion.

This capability bridges the gap between authored preset sources and upstream's
preset API, handling scope separation. Scope configuration keys are extracted
from the native Elisp config and stored in a side table
(`jf/gptel-preset--scope-defaults`) for later use by the scope subsystem during
session initialization.

## Key Concepts

### Preset Sources

Each preset lives in its own subdirectory under `config/gptel/presets/` as an
`.org` source that tangles to a `preset.el` artifact:

```
config/gptel/presets/
├── workspace-assistant/
│   ├── preset.org   (authored source: config block + role fragment)
│   └── preset.el    (generated: registers via gptel-make-preset)
└── system-explorer/
    ├── preset.org
    └── preset.el
```

- **Config block**: An Elisp block in the `.org` source declaring preset
  configuration (`:backend`, `:model`, `:tools`, `:temperature`,
  `:description`, plus scope/mode keys such as `:scope-profile`) using native
  Elisp value types (symbols, lists, `t`/`nil`).
- **Role fragment**: A static `prompt-fragments` fragment authored in the
  `.org` source. Its rendered text becomes the preset's `:system` contribution.
- **Preset name**: The subdirectory basename becomes the preset name symbol
  (e.g. `workspace-assistant/` → symbol `workspace-assistant`).

Example: `config/gptel/presets/system-explorer/preset.org` carries an Elisp
config block such as

```elisp
(:backend "Claude"
 :model claude-sonnet-4-6
 :temperature 0.3
 :confirm-tool-calls 'auto
 :scope-profile "coding"
 :tools ("Read" "Bash"))
```

and a role fragment whose rendered text is the preset's `:system`.

### Pipeline Stages

Registration is driven by loading the tangled preset `.el` artifacts rather
than scanning a directory and parsing files. The stages are:

1. **Tangle** — Editing the `.org` source and re-tangling produces a `preset.el`
   that, when loaded, calls `gptel-make-preset`. The `.el` is a generated
   artifact; the `.org` source is the supported edit point.
2. **Render role** — The preset's role fragment is rendered to backend-appropriate
   text; that rendered text is supplied as `:system`.
3. **Extract scope** — Scope keys (`:paths`, `:shell-commands`, `:bash-tools`,
   `:scope-profile`) are read directly from the native Elisp config, stored in
   `jf/gptel-preset--scope-defaults` keyed by preset name, and removed from the
   plist passed to registration.
4. **Register** — `gptel-make-preset` is called with the native-Elisp config
   (scope keys excluded, `:system` set to the rendered role text). No YAML
   parsing, key normalization, or value coercion is performed.

### Scope Defaults Storage

Variable `jf/gptel-preset--scope-defaults` is an alist mapping preset name
symbols to scope configuration plists:

```elisp
((system-explorer . (:scope-profile "coding"))
 (workspace-assistant . (:paths (:read ("/**") :write ("/tmp/**"))
                         :scope-profile "coding")))
```

These defaults are used during session creation to initialize the session's
scope configuration when the preset is applied.

## Requirements

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

### Requirement: Registration timing and dependencies

Preset registration SHALL run during gptel initialization, after the fragment
renderer/composer (`prompt-fragments`) and `gptel` are loaded, but before any
session hooks fire. No YAML library is required on the registration path.

The initialization order is:
1. Load constants module (defines `jf/gptel-presets-directory`)
2. Load logging module (used by registration warnings)
3. Load the fragments module (renderer/composer) so role fragments can render
4. Load the preset registration module
5. Discover and load the tangled preset `.el` artifacts (registering each preset)
6. Run skills expansion across registered presets
7. Load scope-profiles and scope-core
8. Load session modules (which may open existing sessions) — later

#### Scenario: Registration before session open
- **WHEN** a user opens an existing session.md file (e.g., via find-file)
- **THEN** `gptel--restore-state` can find the preset by name in `gptel--known-presets`
- **AND** the preset was registered before the session activation hooks ran
- **AND** the session can apply the preset during restore

#### Scenario: Fragment renderer available before role rendering
- **WHEN** a preset's tangled `.el` is loaded and renders its role fragment for `:system`
- **THEN** the `prompt-fragments` renderer/composer has already been loaded
- **AND** the role fragment renders without an "unknown function" error

#### Scenario: Constants available for path resolution
- **WHEN** preset registration runs
- **THEN** `jf/gptel-presets-directory` is defined and accessible
- **AND** can be customized by the user before init completes

#### Scenario: Skills expansion runs after registration
- **WHEN** presets are registered
- **THEN** `@skill` tokens in registered preset `:system` strings are expanded once afterward, before user sessions open
- **AND** skill content is injected into preset system prompts

### Requirement: Module location

The preset registration pipeline SHALL be defined in
`config/gptel/presets/registration.org` (tangled to `registration.el`),
providing feature `gptel-preset-registration`. This module supersedes the former
top-level `config/gptel/preset-registration.org`; the feature name is preserved
so existing `(require 'gptel-preset-registration)` call sites continue to work.

The module provides:
- A main orchestration function that discovers and loads the tangled preset
  `.el` artifacts.
- A scope-extraction helper that reads native-Elisp scope keys and stores them
  in `jf/gptel-preset--scope-defaults`.
- The `jf/gptel-preset--scope-defaults` side-table variable (initialized to
  `nil`).

#### Scenario: Module loaded during gptel init
- **WHEN** `config/gptel/gptel.org` initializes
- **THEN** it loads `config/gptel/presets/registration.el` after the fragments module and `gptel` are available
- **AND** before session module loading

#### Scenario: Module feature declaration
- **WHEN** the module is tangled and loaded
- **THEN** `(require 'gptel-preset-registration)` succeeds
- **AND** feature `gptel-preset-registration` is registered

### Requirement: Presets directory configuration

The system SHALL use a configurable directory path for preset discovery.
Presets are discovered as per-directory tangled artifacts:
`<jf/gptel-presets-directory>/<name>/preset.el`.

**Implementation:** Variable `jf/gptel-presets-directory` in
`config/gptel/sessions/constants.org` (tangled to `constants.el`).

#### Scenario: Default presets directory
- **WHEN** `jf/gptel-presets-directory` is not customized
- **THEN** it defaults to `(expand-file-name "config/gptel/presets/" jf/emacs-dir)`
- **AND** registration uses this directory to discover per-preset subdirectories

#### Scenario: Per-preset artifacts discovered
- **WHEN** discovering presets
- **THEN** each `<name>/preset.el` artifact under the presets directory is loaded
- **AND** each loaded artifact registers its preset via `gptel-make-preset`

#### Scenario: Directory configuration is customizable
- **WHEN** a user customizes `jf/gptel-presets-directory` to a different path
- **THEN** preset discovery uses that path instead of the default
- **AND** custom presets in the alternate location are registered

### Requirement: Skills expansion integration

After preset registration, the system SHALL expand `@skill` mentions in preset
system prompts.

This function SHALL:
1. Check the skills system is loaded (detection and discovery functions are
   defined and the registry is bound)
2. Discover available skills
3. Iterate all registered presets in `gptel--known-presets`
4. For each preset's `:system` string, expand `@skill` mentions by replacing
   them with skill content
5. Update the preset's `:system` in place if any expansions occurred

#### Scenario: Skills expanded in preset system prompts
- **WHEN** a preset's `:system` contains text like `"See @agile-methodology for details..."` (where `@agile-methodology` is a registered skill)
- **THEN** after expansion, `:system` is updated to include the full skill content in place of the `@agile-methodology` mention

#### Scenario: Expansion happens after all presets registered
- **WHEN** preset registration completes
- **THEN** skills expansion runs immediately afterward
- **AND** operates on the complete `gptel--known-presets` registry

#### Scenario: Skills not available — expansion skipped
- **WHEN** the skills subsystem is not loaded (functions not defined or registry not bound)
- **THEN** skills expansion skips processing
- **AND** preset system prompts remain unchanged
- **AND** no errors are signaled

#### Scenario: No @mentions in preset — unchanged
- **WHEN** a preset's `:system` contains no `@mention` syntax
- **THEN** the preset remains unchanged after expansion

### Requirement: Upstream preset features — out of scope

Upstream's `gptel-make-preset` supports advanced features that this registration
pipeline SHALL NOT use in the current implementation. These are documented here
for awareness and possible future adoption:

- **`:parents`** — preset inheritance chains (apply parent presets before this one)
- **`:pre` / `:post`** — hook functions run before/after preset application
- **Value modifiers** — `(:append LIST)`, `(:prepend LIST)`, `(:eval FORM)`, `(:function FUNC)`, `(:merge PLIST)` for dynamic value composition
- **`:system` as symbol** — resolves against `gptel-directives` alist

These features may be adopted in future iterations. For now, presets are
registered with flat plists containing concrete values only.
