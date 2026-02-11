## Why

The gptel module lacks behavioral specifications, making it difficult to plan and validate changes to the sessions system. Creating specs for existing behavior establishes a foundation for spec-driven development, enabling future changes to be validated against documented contracts rather than reverse-engineered from implementation details.

We're starting with sessions persistence because it's the foundational subsystem that all other gptel features depend on. Branching and PersistentAgent integration will be documented in subsequent specs once the core persistence contracts are established.

## What Changes

- Create behavioral specification for gptel sessions persistence subsystem
- Document session lifecycle contracts (create, open, save, close)
- Document persistence guarantees and file format contracts
- Document registry/filesystem separation of concerns
- Document metadata tracking and auto-save behavior

This is a **documentation change only** - no code modifications. The spec describes what already exists to enable future spec-driven development.

## Capabilities

### New Capabilities

- `gptel/sessions-persistence`: Session lifecycle management, file-based persistence, metadata tracking, and auto-save behavior for gptel conversations

### Modified Capabilities

<!-- No existing specs are being modified - this is bootstrapping new documentation -->

## Impact

**Documentation created:**
- `openspec/specs/gptel/sessions.md` - Behavioral spec for sessions persistence

**Code affected:** None (documentation only)

**Dependencies:** None - this documents existing behavior in:
- `config/gptel/sessions/constants.{org,el}`
- `config/gptel/sessions/filesystem.{org,el}`
- `config/gptel/sessions/registry.{org,el}`
- `config/gptel/sessions/metadata.{org,el}`
- `config/gptel/sessions/logging.{org,el}`
- `config/gptel/sessions/commands.{org,el}`

**Excluded from scope:** Branching operations and PersistentAgent integration (to be documented in future specs)
