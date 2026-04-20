---
name: fix-design-md-module-count
description: Fix design.md:18 "six-module breakdown" to match post-sanitize-removal module count
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:remove-orphan-sanitize-module
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (modify — line ~18,
  the "six-module breakdown" reference)

## Implementation steps
1. Open `design.md` and locate the reference that says "six-module
   breakdown" (line ~18).
2. Change to "seven-module breakdown" to match `architecture.md`
   §Components header, or change to the neutral phrase "module
   breakdown" if future-proofing is preferred.
3. Scan the rest of `design.md` for any other stale module counts
   and fix all occurrences in this pass.
4. No tangling required — markdown only.

## Design rationale
`remove-orphan-sanitize-module` reconciled `architecture.md`
§Components header and `scaffold-chat-subsystem` task body to seven
modules. `design.md:18` was not in that task's Files-to-modify and
still says "six-module breakdown". Left unfixed, it contradicts
architecture.md and misleads anyone reading the design doc.

Trivial fix, trivial scope. Filed as its own task per the
one-finding-one-task review convention.

## Verification
- `grep -n "six-module" openspec/changes/gptel-chat-mode/design.md`
  returns nothing.
- `grep -n "seven-module\|module breakdown"
   openspec/changes/gptel-chat-mode/design.md` returns the updated
  line.
- No other stale module counts anywhere in `design.md`.

## Context
- Review of remove-orphan-sanitize-module (orchestrator session
  2026-04-20) sole finding
- `design.md:18`
- `architecture.md` §Components (header and sub-sections both say
  seven)
