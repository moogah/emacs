---
name: clarify-spec-auto-mode-alist
description: Clarify spec on auto-mode-alist as user configuration vs shipped behavior
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (mode-activation section)

## Implementation steps
1. The spec mentions "auto-mode-alist configuration" without
   specifying whether it is a user-facing capability statement
   ("users may add their own entry") or a shipped feature
   ("chat-mode ships entries"). The task body and design.md treat
   it as user-side configuration.
2. Add a one-sentence clarification to the spec, e.g.:
   "auto-mode-alist activation is a user-side configuration —
   users add their own pattern as desired; chat-mode does not
   register entries by default."

## Design rationale
Without the clarification, a future reviewer may flag the
unimplemented "auto-mode-alist registration" as missing. Pin the
intent now.

## Verification
- spec.md mode-activation section is unambiguous about
  auto-mode-alist responsibility.

## Context
- Review of `mode-definition` task spec-level signal #3.
