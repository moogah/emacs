# Preset Configuration

Behavioral specification for preset file parsing, resolution, and application with clear phase separation.

**Scope:** This spec covers the complete preset lifecycle from file I/O through resolution to buffer configuration. It defines the internal representation contract and data flow between phases.

## ADDED Requirements

### Requirement: Preset file parsing

The system SHALL parse preset files (`.md` or `.org`) into an intermediate data representation without performing object lookups.

Parsing extracts configuration from YAML frontmatter and markdown/org body into a pure-data plist with string/number/list values only. No backend structs, model symbols, or tool structs are resolved during parsing.

#### Scenario: Parse markdown preset with YAML frontmatter
- **WHEN** parsing a `.md` preset file with YAML frontmatter
- **THEN** the system extracts frontmatter fields (backend, model, temperature, tools, etc.) as strings/numbers/lists
- **AND** extracts markdown body as system message string
- **AND** returns plist: `(:backend "Claude" :model "claude-opus-4-5" :temperature 1.0 :tools ("tool1" "tool2") :system "message...")`

#### Scenario: Parse org preset with properties drawer
- **WHEN** parsing a `.org` preset file with PROPERTIES drawer
- **THEN** the system extracts properties as strings/numbers
- **AND** extracts org body as system message string
- **AND** returns plist with same structure as markdown format

#### Scenario: Tools as simple array
- **WHEN** preset YAML contains `tools: ["read_file", "write_file_in_scope"]`
- **THEN** the parsed plist contains `:tools ("read_file" "write_file_in_scope")`
- **AND** no tool struct resolution occurs during parsing

#### Scenario: Missing frontmatter returns nil
- **WHEN** preset file has no YAML frontmatter or PROPERTIES drawer
- **THEN** parsing returns nil
- **AND** logs error indicating missing configuration

#### Scenario: Invalid YAML returns nil
- **WHEN** YAML frontmatter cannot be parsed
- **THEN** parsing returns nil
- **AND** logs error with YAML parse failure details

### Requirement: Backend name resolution

The system SHALL resolve backend name strings to gptel-backend structs using gptel's backend registry.

#### Scenario: Resolve known backend name
- **WHEN** resolving backend name "Claude"
- **THEN** the system looks up name in `gptel--known-backends` alist
- **AND** returns the corresponding gptel-backend struct

#### Scenario: Unknown backend name signals error
- **WHEN** resolving backend name that doesn't exist in registry
- **THEN** the system signals an error with message "Unknown backend: <name>"
- **AND** does not return nil or fall back to default

#### Scenario: Backend already struct passes through
- **WHEN** resolving a value that is already a gptel-backend struct
- **THEN** the system returns it unchanged

### Requirement: Model name resolution

The system SHALL convert model name strings to symbols for gptel compatibility.

#### Scenario: Convert string to symbol
- **WHEN** resolving model name "claude-opus-4-5-20251101"
- **THEN** the system returns symbol `'claude-opus-4-5-20251101`

#### Scenario: Symbol passes through unchanged
- **WHEN** resolving a model that is already a symbol
- **THEN** the system returns it unchanged

### Requirement: Tool name resolution

The system SHALL resolve tool name strings to tool structs using gptel's tool registry.

#### Scenario: Resolve list of tool names
- **WHEN** resolving tool names `("read_file" "write_file_in_scope")`
- **THEN** the system calls `gptel-get-tool` for each name
- **AND** returns list of tool structs for tools that exist
- **AND** logs warning for tools that don't exist
- **AND** excludes missing tools from result list

#### Scenario: Empty tool list returns nil
- **WHEN** resolving an empty tools list
- **THEN** the system returns nil

#### Scenario: All tools missing returns nil
- **WHEN** all tool names in the list fail to resolve
- **THEN** the system returns nil
- **AND** logs warning for each missing tool

### Requirement: Preset resolution phase

The system SHALL convert parsed preset plist to gptel-compatible representation by resolving all names to objects.

Resolution takes the intermediate plist from parsing phase and performs lookups to create the final representation with backend structs, model symbols, and tool structs.

#### Scenario: Resolve complete preset configuration
- **WHEN** resolving parsed plist with backend, model, and tools
- **THEN** the system resolves backend name to struct via `jf/gptel-preset--resolve-backend`
- **AND** resolves model name to symbol via `jf/gptel-preset--resolve-model`
- **AND** resolves tool names to structs via `jf/gptel-preset--resolve-tools`
- **AND** returns plist: `(:backend <struct> :model 'symbol :tools (<struct> <struct>) ...)`

#### Scenario: Preserve non-lookup fields
- **WHEN** resolving preset with temperature, description, system message
- **THEN** these fields pass through unchanged in the resolved plist

#### Scenario: Partial resolution on error
- **WHEN** backend resolution fails but model succeeds
- **THEN** the system signals error and does not return partial result

### Requirement: Buffer configuration application

The system SHALL apply resolved preset configuration to current buffer via buffer-local variables.

Application sets buffer-local gptel variables (`gptel-backend`, `gptel-model`, `gptel-tools`, etc.) from the resolved preset plist. This phase depends on resolution phase completing successfully.

#### Scenario: Apply complete configuration
- **WHEN** applying resolved preset with backend, model, tools, temperature, system
- **THEN** the system sets `(setq-local gptel-backend <backend-struct>)`
- **AND** sets `(setq-local gptel-model 'model-symbol)`
- **AND** sets `(setq-local gptel-tools <tool-list>)`
- **AND** sets `(setq-local gptel-temperature <number>)`
- **AND** sets system message via `jf/gptel--set-session-system-message`

#### Scenario: Tools always set for isolation
- **WHEN** applying preset with empty or nil tools list
- **THEN** the system sets `(setq-local gptel-tools nil)` explicitly
- **AND** ensures buffer-local binding exists to prevent global tool leakage

#### Scenario: Optional fields omitted when nil
- **WHEN** resolved preset does not include temperature field
- **THEN** the system does not set `gptel-temperature`
- **AND** buffer uses existing or default value

### Requirement: Preset serialization for writing

The system SHALL convert resolved preset plist to serializable form for writing to files.

Serialization is the inverse of resolution: backend structs → names, model symbols → strings, tool structs → name strings. Used when creating preset files during session initialization.

#### Scenario: Serialize backend struct to name
- **WHEN** serializing preset with backend struct
- **THEN** the system extracts backend name via `(gptel-backend-name backend)`
- **AND** includes in serialized plist as `:backend "Claude"`

#### Scenario: Serialize model symbol to string
- **WHEN** serializing preset with model symbol `'claude-opus-4-5`
- **THEN** the system converts via `(symbol-name model)`
- **AND** includes in serialized plist as `:model "claude-opus-4-5"`

#### Scenario: Serialize tool structs to names
- **WHEN** serializing preset with tool structs
- **THEN** the system extracts names via `(gptel-tool-name tool)` for each
- **AND** includes in serialized plist as `:tools ("tool1" "tool2")`

#### Scenario: Handle new tool format gracefully
- **WHEN** tools are in plist format `(:tool1 (:allowed t) :tool2 (:allowed t))`
- **THEN** the system extracts tool names by iterating keywords
- **AND** includes in serialized plist as simple array `(:tools ("tool1" "tool2"))`

### Requirement: In-flight operation scope

The system SHALL only maintain resolved preset configuration during active operations, never caching in memory.

Configuration exists transiently during load-resolve-apply or parse-serialize-write operations. Once applied to buffer or written to file, the intermediate representations are discarded.

#### Scenario: No global preset cache
- **WHEN** loading and applying a preset
- **THEN** the resolved plist exists only in the call stack
- **AND** is discarded after buffer-local variables are set
- **AND** no global variable stores resolved presets

#### Scenario: File is source of truth
- **WHEN** buffer configuration needs to be retrieved
- **THEN** the system reads buffer-local variables directly
- **AND** does not read from preset file again (buffer-local state is authoritative after first save)

### Requirement: Phase separation enforcement

The system SHALL maintain strict boundaries between parsing, resolution, and application phases.

Each phase has a single responsibility and well-defined inputs/outputs. Phases are composable but never intermixed.

#### Scenario: Parsing phase has no dependencies
- **WHEN** parsing a preset file
- **THEN** the parser does not call `gptel-get-backend`, `gptel-get-tool`, or any gptel registry lookups
- **AND** only uses file I/O and YAML/org parsing libraries

#### Scenario: Resolution phase depends only on parsed data
- **WHEN** resolving a parsed preset plist
- **THEN** resolution functions accept plist parameter only
- **AND** do not perform file I/O or parsing
- **AND** call gptel registry functions for lookups

#### Scenario: Application phase depends only on resolved data
- **WHEN** applying preset configuration to buffer
- **THEN** application function accepts resolved plist parameter only
- **AND** does not perform parsing or resolution
- **AND** only sets buffer-local variables

### Requirement: Error handling with context

The system SHALL provide clear error messages indicating which phase failed and why.

#### Scenario: Parsing error includes file path
- **WHEN** YAML parsing fails
- **THEN** error message includes preset file path and parse error details

#### Scenario: Resolution error includes field name
- **WHEN** backend resolution fails
- **THEN** error message includes backend name and registry state

#### Scenario: Application error includes buffer context
- **WHEN** applying configuration fails
- **THEN** error message includes buffer name and failing variable
