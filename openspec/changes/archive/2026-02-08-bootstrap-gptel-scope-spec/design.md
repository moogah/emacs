## Context

The gptel scope control system has been implemented and is operational across 7 modules (`scope-core`, `scope-manager`, `scope-expansion`, `scope-commands`, `scope-filesystem-tools`, `scope-org-roam-tools`, `scope-shell-tools`). The system uses a macro-based approach with three validation strategies (path-based, pattern-based, command-based) and integrates with gptel's tool execution via `gptel-make-scoped-tool`.

**Current State:**
- Implementation exists in `config/gptel/scope/*.el` files
- Presets define tool permissions in YAML frontmatter (e.g., `config/gptel/presets/claude-plan.md`)
- No behavioral specs exist - blocking spec-driven development for future enhancements
- Legacy code exists (`scope-manager.org` v1.0 advice-based approach) but is not active

**Constraints:**
- Specs must document actual behavior, not ideal behavior
- Must distinguish between active code (v2.0 macro-based) and legacy code (v1.0 advice-based)
- Implementation files (`.el`) are source of truth - `.org` files may contain stale documentation

## Goals / Non-Goals

**Goals:**
- Create baseline behavioral specs enabling future delta specs
- Document three orthogonal validation strategies (path/pattern/command)
- Document preset configuration format and tool permission model
- Document expansion UI workflow and allow-once mechanism
- Organize specs by capability (core, presets, expansion) not by module structure

**Non-Goals:**
- Not redesigning the scope system architecture
- Not updating implementation code or fixing bugs
- Not documenting legacy v1.0 code (focus on active v2.0 implementation)
- Not creating implementation-level API documentation (that belongs in `.org` files)
- Not documenting every function - specs describe behavior and contracts, not internals

## Decisions

### Decision 1: Three-Spec Organization

**Chosen**: Split into three capability-based specs:
- `gptel/scope.md` - Core validation engine
- `gptel/scope-presets.md` - Configuration format
- `gptel/scope-expansion.md` - User interaction flow

**Rationale**: Capabilities map to user-facing concerns (validation behavior, configuration, UI) rather than implementation modules. This makes specs more stable as implementation refactors.

**Alternatives Considered**:
- Single `gptel/scope.md` monolith - Rejected: Too large, mixes concerns
- Module-mirroring (7 specs matching 7 modules) - Rejected: Too implementation-focused, unstable under refactoring
- Subsystem-based (validation, tools, commands) - Rejected: Splits related behaviors across boundaries

### Decision 2: Focus on Active v2.0 Implementation

**Chosen**: Document only the active macro-based (v2.0) implementation. Mention legacy v1.0 code exists but is not active.

**Rationale**: Specs describe current behavior for future changes. Documenting inactive code adds confusion and maintenance burden.

**Alternatives Considered**:
- Document both v1.0 and v2.0 - Rejected: v1.0 is not used, would clutter specs
- Ignore legacy code entirely - Rejected: Should note it exists to avoid confusion when reading codebase

### Decision 3: Preset Files as Source of Truth

**Chosen**: Document preset configuration based on `config/gptel/presets/*.md` files (especially `claude-plan.md` as the reliable reference), not template functions in `scope-commands.el`.

**Rationale**: User's clarification that template functions are legacy code; actual presets are authoritative.

**Alternatives Considered**:
- Use template functions as reference - Rejected: User stated these are outdated
- Synthesize from both sources - Rejected: Creates inconsistency risk

### Decision 4: Behavioral Focus, Not Implementation Details

**Chosen**: Specs describe **what** the system does (validation rules, permission layers, UI choices) and **why** (security model, trust boundaries), not **how** (macro expansion, YAML parsing, buffer-local variables).

**Rationale**: Implementation details belong in literate `.org` files. Specs provide stability for delta specs when implementation changes.

**Alternatives Considered**:
- Include implementation architecture - Rejected: Makes specs brittle to refactoring
- Purely user-facing behavior - Rejected: Need some internal contracts for system integration

### Decision 5: Tool Categorization as Core Contract

**Chosen**: Document the tool categorization system (`jf/gptel-scope--tool-categories`) as a core behavioral contract, specifying validation strategy and operation type for each tool.

**Rationale**: This mapping is the integration contract between scope system and gptel tools. Changes to this mapping affect system behavior.

**Alternatives Considered**:
- Treat categorization as implementation detail - Rejected: It's a behavioral contract
- Document in expansion spec - Rejected: It's core validation logic, not UI

## Risks / Trade-offs

**[Risk]** Specs may diverge from implementation as code evolves
→ **Mitigation**: Verification protocol requires checking specs against `.el` files before archiving

**[Risk]** Implementation documentation in `.org` files may conflict with spec behavior
→ **Mitigation**: Design explicitly states `.el` files are source of truth; note discrepancies as potential doc bugs

**[Risk]** Legacy template functions may still be referenced in older presets
→ **Mitigation**: Scope spec notes preset format evolution; future changes can audit preset files

**[Trade-off]** Three specs vs single monolith
→ Chose clarity and modularity over single-file simplicity. Future changes will likely affect only one capability.

**[Trade-off]** Capability-based vs module-based organization
→ Chose stability under refactoring over direct code mapping. Requires discipline to keep specs aligned with capabilities.

## Migration Plan

Not applicable - this is documentation only, no deployment or migration needed.

## Open Questions

None - scope of work is clear and constrained to documenting existing behavior.
