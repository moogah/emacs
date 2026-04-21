---
name: update-architecture-md-helpers-rename
description: Update gptel-chat-mode architecture.md to reflect helpers-spec.el → test-helpers.el rename
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:helpers-spec-naming-convention
---

## Files to modify
- `openspec/changes/gptel-chat-mode/architecture.md` (lines 212, 284, 311)

## Implementation steps
1. Replace each reference to `helpers-spec.el` with `test-helpers.el`
   in `openspec/changes/gptel-chat-mode/architecture.md`.
2. Do not touch other artifacts; the historical task files in
   `tasks/closed/` (e.g., `parser.md`, `helpers-spec-stale-docstring.md`)
   and the unrelated `helpers-spec.el` files under `config/gptel/scope/`
   and `config/gptel/tools/` are out of scope.

## Design rationale
Post-rename audit for `helpers-spec-naming-convention`. `architecture.md`
is the live design artifact for this change and should reflect the
actual test-tree layout to avoid confusing future readers.

## Verification
- `grep -n helpers-spec openspec/changes/gptel-chat-mode/architecture.md`
  returns no hits.
- `grep -n test-helpers openspec/changes/gptel-chat-mode/architecture.md`
  shows three (or more) references matching the rename.

## Context
- Review of `helpers-spec-naming-convention` (2026-04-21,
  orch-review-1776789773) Finding #1.
