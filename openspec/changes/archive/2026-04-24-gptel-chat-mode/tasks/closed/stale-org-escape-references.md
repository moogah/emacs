---
name: stale-org-escape-references
description: Remove or contextualize org-escape-code-in-string references in spec.md and architecture.md
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sanitize-chunks
---

## Files to modify
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (line ~87)
- `openspec/changes/gptel-chat-mode/architecture.md` (line ~312
  Dependencies)

## Implementation steps
1. spec.md §"Response streaming and sanitization" line ~87 says
   "The mode sanitizes streamed assistant content using the
   `org-escape-code-in-string` pattern…". Reword to: "The mode
   sanitizes streamed assistant content using the same convention
   (prepending `,` to lines that would otherwise close a turn or
   tool block), but with a targeted three-delimiter regex — see
   design.md Decision 4 for rationale."
2. architecture.md §Dependencies line ~312 lists
   `org-escape-code-in-string`. Either remove the line or annotate
   "(referenced by convention only — not imported; see design.md
   Decision 4)".

## Design rationale
Decision 4 in design.md explicitly rejects calling
`org-escape-code-in-string`. The current text in spec.md and
architecture.md reads as if the function is in use; a reader
flipping between docs sees a contradiction. Reconcile in the
direction the implementation already took.

## Verification
- `grep -n "org-escape-code-in-string" openspec/changes/gptel-chat-mode/`
  shows only documentation references that explicitly note the
  function is NOT used.

## Context
- Review of `sanitize-chunks` task spec-level signals (#1 and #2 in
  the Spec-level signals section).
