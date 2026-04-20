---
name: architecture-sanitize-holdback-claim
description: Reword architecture.md claim that gptel-chat--sanitize-chunk handles holdback
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sanitize-chunks
---

## Files to modify
- `openspec/changes/gptel-chat-mode/architecture.md` (around line 115)

## Implementation steps
1. Locate the claim near line 115:
   "`gptel-chat--sanitize-chunk` handles partial-line holdback via state".
2. The implementation correctly separates concerns: the sanitizer
   is pure (line in → line out) and the holdback lives in the
   closure factory, not in `sanitize-chunk`.
3. Reword to two entries:
   - `gptel-chat--sanitize-chunk` — pure line-level escape (one
     complete line in, one line out, prepends `,` for the three
     delimiter forms).
   - `gptel-chat--make-stream-closure` (or whatever name is in
     effect after `rename-make-stream-closure`) — holdback-bearing
     closure factory.

## Design rationale
The architecture doc currently misrepresents the function split.
A reader expects `sanitize-chunk` to do more than it does, and
will be confused by the simple regex they find when reading the
implementation.

## Verification
- `grep -n "sanitize-chunk\|holdback" openspec/changes/gptel-chat-mode/architecture.md`
  shows the two responsibilities are separated.

## Context
- Review of `sanitize-chunks` task spec-level signals (#3 in the
  Spec-level signals section).
