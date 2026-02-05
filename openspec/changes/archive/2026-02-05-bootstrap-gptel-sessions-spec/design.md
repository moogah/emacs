## Context

The gptel sessions persistence system exists across 6 modules in `config/gptel/sessions/`:
- `constants.{org,el}` - Configuration constants and buffer-local state variables
- `filesystem.{org,el}` - Directory structure, path resolution, and session discovery
- `registry.{org,el}` - In-memory hash table for fast session lookup
- `metadata.{org,el}` - YAML file reading/writing for session metadata
- `logging.{org,el}` - Leveled logging system for observability
- `commands.{org,el}` - User-facing commands and auto-initialization

This design documents the **existing architecture** to create a behavioral specification. No code changes are planned.

**Current State:**
- Sessions store conversations in `~/.gptel/sessions/<session-id>/branches/<branch-name>/`
- Each session has metadata (scope-plan.yml, preset.md), conversation history (session.md), and logs
- Registry maintains in-memory index of sessions for O(1) lookup
- Auto-initialization detects session files on open via find-file-hook
- Sessions integrate with activities system for workspace persistence

**Constraints:**
- This is documentation-only - we're describing what already works
- Scope limited to persistence fundamentals (excluding branching and PersistentAgent integration)
- The spec must be behavioral (contracts and guarantees), not implementation details

## Goals / Non-Goals

**Goals:**
- Document the session lifecycle (create, open, save, close)
- Document persistence guarantees (what survives restart, what's transient)
- Document the registry/filesystem separation (memory vs disk responsibilities)
- Document metadata structure and file formats
- Document auto-save behavior and initialization contracts
- Create foundation for future spec-driven development

**Non-Goals:**
- Changing any existing behavior
- Documenting branching operations (separate spec)
- Documenting PersistentAgent integration (separate spec)
- Documenting activities integration details (belongs in activities spec)
- Implementation details (those stay in literate .org files)

## Decisions

### Decision 1: Spec Structure - Single Behavior-Focused Document

**Choice:** Create one spec file (`openspec/specs/gptel/sessions.md`) covering all persistence behavior.

**Rationale:**
- The 6 modules work together as a cohesive persistence system
- Splitting by module would duplicate information about shared contracts
- Behavioral perspective focuses on "what guarantees does persistence provide" not "what does each module do"

**Alternatives Considered:**
- One spec per module → Too granular, loses sight of system-level contracts
- Separate specs for lifecycle/metadata/registry → Creates artificial boundaries

### Decision 2: Content Organization - Lifecycle-Driven Flow

**Choice:** Organize spec around user-visible lifecycle (create → open → save → close) rather than module structure.

**Rationale:**
- Users think in terms of operations, not modules
- Easier to validate behavior against user workflows
- Natural entry points for testing scenarios

**Structure:**
1. Directory structure and file formats (foundation)
2. Session lifecycle (create, open, save, close)
3. Registry and discovery (how sessions are tracked)
4. Metadata contracts (what's tracked, why, format)
5. Auto-save and initialization (convenience behaviors)
6. Logging and observability (what's visible)

### Decision 3: Scope Boundary - Persistence Fundamentals Only

**Choice:** Exclude branching and PersistentAgent integration from this initial spec.

**Rationale:**
- Branching adds complexity (parent/child relationships, symlinks, lineage tracking)
- PersistentAgent has its own lifecycle separate from basic persistence
- Starting with fundamentals establishes the foundation cleanly
- Future specs can reference this one for base contracts

**Migration Path:** Create `openspec/specs/gptel/branching.md` and `openspec/specs/gptel/persistent-agent.md` later.

### Decision 4: Detail Level - Behavioral Contracts, Not Implementation

**Choice:** Document "what the system guarantees" not "how it's implemented."

**Examples:**
- ✓ "Session IDs are immutable after creation"
- ✓ "Registry provides O(1) lookup by session-id/branch-name"
- ✗ "The registry uses `make-hash-table` with `equal` test"
- ✗ "Path construction uses `expand-file-name` with `concat`"

**Rationale:**
- Implementation details belong in literate .org files
- Specs enable validation without coupling to specific implementation
- Focus on invariants that must survive refactoring

### Decision 5: File Format Documentation - Structure and Purpose

**Choice:** Document file formats (scope-plan.yml, preset.md, session.md) including:
- Required vs optional fields
- Field semantics and constraints
- Why each file exists (separation of concerns)

**Rationale:**
- File formats are the persistence contract
- Future changes need to maintain backward compatibility
- Clear documentation enables migration strategies

## Risks / Trade-offs

### Risk: Spec Becomes Implementation Documentation

**Risk:** Without clear boundaries, the spec could duplicate literate .org file content.

**Mitigation:**
- Focus on "what" and "why", not "how"
- Document contracts and invariants, not code structure
- Reference .org files for implementation details
- Review spec against test cases, not code

### Risk: Incomplete Coverage of Edge Cases

**Risk:** Edge cases in the existing code may not be captured in behavioral spec.

**Mitigation:**
- Use agent exploration findings as comprehensive source
- Document defensive behaviors (duplicate hook detection, cleanup)
- Include error conditions and failure modes
- Mark areas needing test coverage

### Trade-off: Single File vs Multiple Files

**Trade-off:** One large spec is harder to navigate but maintains cohesion.

**Decision:** Start with single file, split later if it exceeds 500 lines or distinct subsystems emerge.

**Rationale:** Premature splitting creates maintenance burden and artificial boundaries.

### Trade-off: Excluding Branching

**Trade-off:** Branching is implemented and working, but excluded from this spec.

**Decision:** Acceptable because branching builds on persistence fundamentals. Documenting the base layer first provides foundation for branching spec.

**Implication:** Future branching spec will reference this spec's contracts.

## Migration Plan

N/A - This is documentation creation, not code changes.

**Deployment:**
- Write spec to `openspec/specs/gptel/sessions.md`
- No runtime changes, no Emacs restart needed
- Spec becomes reference for future changes

## Open Questions

None - The existing implementation is stable and well-understood through agent exploration. This spec documents current state as-is.
