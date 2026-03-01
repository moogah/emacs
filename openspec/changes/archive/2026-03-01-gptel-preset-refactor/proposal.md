## Why

Preset configuration parsing has evolved organically and now mixes file I/O, YAML parsing, type conversions, and gptel object lookups in a single function. This makes it difficult to reason about the data flow, test individual components, and maintain consistency across different preset operations (loading, applying, serializing). Clear separation of parsing, resolution, and application phases will improve maintainability and enable better error handling.

## What Changes

- **Separate parsing from resolution**: Create distinct phases for (1) reading/parsing YAML to pure data structures, (2) resolving names to gptel objects, and (3) applying configuration to buffers
- **Consolidate type conversions**: Extract backend/model/tool resolution into dedicated converter functions with consistent error handling
- **Standardize preset file format**: Establish tools array as canonical format (eliminate plist/alist variants)
- **Define internal representation contract**: Document the transient plist structure used during in-flight operations
- **Eliminate gptel-agent dependency** (optional): Parse YAML frontmatter directly to reduce coupling

## Capabilities

### New Capabilities
- `preset-configuration`: Preset file parsing, resolution, and application lifecycle with clear phase separation

### Modified Capabilities
- `gptel/sessions-persistence`: Update preset loading during session initialization to use new parsing architecture
- `gptel/persistent-agent`: Update agent preset loading and path inheritance to use new parsing architecture

## Impact

**Affected modules**:
- `config/gptel/sessions/commands.org` - Functions: `jf/gptel--load-preset-from-file`, `jf/gptel--apply-session-preset`, `jf/gptel--normalize-preset-for-serialization`
- `config/gptel/tools/persistent-agent.org` - Agent preset loading logic
- `config/gptel/scope/scope-core.org` - Preset config parsing for scope validation

**No breaking changes**: Internal refactoring only, no changes to preset.md file format or user-facing APIs. Existing preset files remain compatible.

**Dependencies**: Relies on gptel package APIs (`gptel--known-backends`, `gptel-get-tool`, buffer-local configuration variables)
