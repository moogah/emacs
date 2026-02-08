## Why

The gptel scope control system is fully implemented and operational, but lacks behavioral specifications. This creates a barrier to spec-driven development: we cannot write delta specs for future changes because there's no baseline spec describing current behavior. Bootstrapping these specs now enables the OpenSpec workflow for future scope system enhancements.

## What Changes

- Create comprehensive behavioral specs documenting the existing scope control system
- Document scope validation strategies (path-based, pattern-based, command-based)
- Document preset configuration format and tool permission model
- Document expansion UI interaction flow and allow-once mechanism
- Document integration points with gptel tool system

**No code changes** - this is purely documentation to establish the behavioral baseline.

## Capabilities

### New Capabilities

- `gptel/scope`: Core scope control system including validation strategies, configuration loading, tool categorization, and permission checking
- `gptel/scope-presets`: Preset configuration format, template structure, and tool permission declarations
- `gptel/scope-expansion`: Interactive expansion UI, transient menu workflow, and temporary allow-once permissions

### Modified Capabilities

<!-- No existing specs are being modified -->

## Impact

**Documentation Only** - No code, APIs, or systems are affected.

**Enables**:
- Future spec-driven changes to the scope system
- Delta specs for scope enhancements
- Clear behavioral contracts for scope validation
- Understanding of scope system architecture and integration points

**Affected Areas**:
- `openspec/specs/gptel/` directory (new spec files)
- Future changes to `config/gptel/scope/*.el` modules can reference these specs
