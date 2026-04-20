---
name: create-core-module-system-spec
description: Create openspec/specs/core/module-system.md documenting the module-loading contract
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:fix-resolve-module-path-extension
---

## Files to modify
- `openspec/specs/core/module-system.md` (new)

## Implementation steps
1. Create `openspec/specs/core/module-system.md` as a new module
   behaviour spec following the repo's "specs are behavioral
   contracts" philosophy (see `CLAUDE.md` §Spec Philosophy).
2. Document at minimum:
   - **Purpose**: what the module system provides (registration,
     path resolution, error-handled loading, reload for testing).
   - **Module identifier contract**: logical identifiers like
     `"core/defaults"` or `"gptel/chat/chat"` — zero or more `/`
     separators; final segment is basename; preceding segments are
     subdirectory path. Resolves via `jf/resolve-module-path` against
     `config/` under `jf/emacs-dir`.
   - **Registration**: `jf/enabled-modules` list in `init.org` is
     the single source of truth.
   - **Load semantics**: `jf/load-module` loads with error handling;
     `jf/reload-module` is interactive for testing; load-order matters
     (document ordering constraints at the contract level, not just
     module-by-module notes in `init.org`).
   - **What the system does NOT promise**: no validation of
     identifiers, no auto-discovery, no load-time dependency graph.
3. Keep the spec behavioral — do not duplicate `.org` implementation
   details. Reference `init.org` as the implementation.
4. Update `CLAUDE.md` §Path Resolution to link to the new spec as
   authoritative.

## Design rationale
Reviewer noted that the `fix-resolve-module-path-extension` task
changed the module-system contract (single-slash → zero-or-more
slashes) but the only normative record of the contract is a
paragraph in `CLAUDE.md`. Per the repo's bootstrapping strategy
("fill in organically … as you touch them"), this is the right
moment to record the contract in `openspec/specs/`. Future contract
changes then have a target to diff against.

## Verification
- `openspec/specs/core/module-system.md` exists and documents the
  module identifier contract, registration, and load semantics.
- `CLAUDE.md` §Path Resolution references the new spec.
- The new spec does not repeat implementation details already in
  `init.org`.

## Context
- Review of fix-resolve-module-path-extension (orchestrator session
  2026-04-20) Finding #3
- `CLAUDE.md` §Path Resolution (currently the sole contract record)
- `init.org:70-82` (implementation)
- Repo's Spec Philosophy (`CLAUDE.md` §Spec Philosophy)
