---
name: revisit-decision-3b-tool-marker
description: Revisit design.md Decision 3b on tool-marker ownership in the stream closure
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sanitize-chunks
  - enables:expose-tool-marker-setter
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (Decision 3b)

## Implementation steps
1. Decision 3b currently says "tool-marker is captured inside the
   closure". The implementation revealed this is awkward: the
   closure can't expose mutation without changing its return
   shape, and the routing branch ends up untestable from outside
   the closure.
2. Decide between two refactored framings and update Decision 3b:
   - **Frame A**: The factory returns a small handle (plist or
     cl-struct) exposing both `:insert` and tool-marker
     setters. This keeps the marker inside the closure but makes
     the slot externally controllable.
   - **Frame B**: tool-marker is a caller-owned mutable cell
     (`(list nil)`) passed into the factory; the closure reads
     its `car` on each chunk. The closure no longer "owns" the
     marker, which simplifies the testability story.
3. Pick one and rewrite Decision 3b's wording. The implementation
   task `expose-tool-marker-setter` will follow the new framing.

## Design rationale
The "closure owns it but can't expose it" stance produces the
worst-of-both-worlds situation flagged in the review. Cheaper to
fix the design now than to ship awkward scaffolding into
`stream-callback`.

## Verification
- design.md Decision 3b unambiguously specifies one of the two
  framings.
- The follow-up `expose-tool-marker-setter` task aligns with the
  chosen framing.

## Context
- Review of `sanitize-chunks` task spec-level signal #4.
