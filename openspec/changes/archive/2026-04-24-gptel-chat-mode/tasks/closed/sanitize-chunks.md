---
name: sanitize-chunks
description: Line-level delimiter escape and line-holdback closure for streaming
change: gptel-chat-mode
status: done
relations:
  - blocked-by:scaffold-chat-subsystem
  - blocked-by:remove-orphan-sanitize-module
---

## Review (2026-04-21, orch-review-1776770835)

Re-reviewed after all six follow-ups landed. Original findings adequately
addressed. Three non-blocking findings — captured in follow-up task
`sanitize-chunks-contract-hardening` (loud-fail branch test,
stream-insert-flush docstring, spec encoding of insertion-type contract)
and `sanitize-chunk-newline-guard-cr-handling` (CR-only handling + negative
test message pinning). Neither blocks downstream dependents.


## Files to modify
- `config/gptel/chat/stream.org` (new — sanitizer section)
- `config/gptel/chat/stream.el` (tangled)
- `config/gptel/chat/test/stream/streaming-spec.el` (new)
- `config/gptel/chat/test/stream/chunk-split-spec.el` (new)

## Implementation steps
1. Implement `gptel-chat--sanitize-chunk`: given a *complete line* of text,
   return either the input unchanged or a version with a leading `,` prepended.
   - Match rule (case-insensitive, `case-fold-search t`):
     `^#\+end_\(user\|assistant\|tool\)\b` → prepend `,`.
   - Nothing else is escaped. Do not use `org-escape-code-in-string`.
2. Implement the streaming closure factory (call it
   `gptel-chat--make-stream-closure` or similar). It returns a function with
   captured state:
   - `insertion-marker` — Emacs marker for the active assistant block
   - `holdback` — string of trailing partial-line content not yet processed
   - `tool-marker` — Emacs marker set when a tool block is active (nil
     otherwise); when non-nil, inserts go here instead of the assistant
     marker (wired up in task `stream-callback`)
3. On each text chunk:
   a. `(setq chunk (concat holdback chunk))`
   b. Split at `\n`; the trailing partial (if the chunk did not end in `\n`)
      becomes the new holdback. Complete lines move forward.
   c. For each complete line, run `gptel-chat--sanitize-chunk`, then insert
      at the active marker (tool-marker if set, else insertion-marker),
      followed by `\n`.
4. On stream completion the caller flushes the holdback — at completion
   there is no partial line by construction (upstream sends `t` after a
   newline-terminated final chunk, or the final content with no newline).
   Sanitize the holdback flush the same way (single-line path).
5. Use `markers` not integer positions — user edits above the insertion
   point during stream must not corrupt them.
6. Tests (cover spec §"Response streaming and sanitization"):
   - Multi-chunk response with no collisions inserts in order.
   - Chunk containing `#+end_assistant` on its own line → escaped to
     `,#+end_assistant`.
   - Case-insensitive: `#+END_ASSISTANT`, `#+End_Assistant` → escaped.
   - **Split across chunks**: chunk 1 ends with `#+end_ass`, chunk 2 begins
     with `istant\nmore` → final inserted text has `,#+end_assistant\n`
     (the spec's headline split-across-chunks scenario).
   - Non-matching lines like `#+end_src` or `#+begin_assistant` pass through
     untouched.

## Design rationale
Decision 4: a targeted three-delimiter regex gives minimal output
perturbation. `org-escape-code-in-string` (upstream's approach) escapes all
org directive-looking lines including `* Heading` and `#+begin_src`, which
clutters assistant content with spurious commas on benign lines. We escape
*only* the three collisions that can break our parser.

Decision 3b: streaming state is a per-request closure, not buffer-local or
global state. Closure scope is exactly the send's lifetime — no cross-send
leakage, no cleanup on abort, directly testable by invoking the returned
callback with scripted chunks.

Line-buffering with a holdback is the standard streaming-safe pattern for
line-oriented sanitization; without it a `#+end_assistant` split across a
chunk boundary would be inserted as two unescaped fragments.

## Design pattern
Closure + markers is the Emacs idiom for safe position-tracking across
concurrent edits. The test pattern stubs `gptel-request` with `cl-letf` and
invokes `:callback` synchronously with scripted chunks — see architecture.md
§"Backend stubbing" for the shape.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes streaming +
  chunk-split suites.
- Scenarios from spec §"Response streaming and sanitization":
  - Normal stream completion produces a well-formed block
  - Response containing `#+end_assistant` collision is escaped
  - Response containing collision split across chunks is escaped
  - (Abort scenario and tool-call rendering are covered by task
    `stream-callback`, not here.)

## Context
- design.md §Decision 3b (per-chunk text hygiene, closure + marker + holdback)
- design.md §Decision 4 (sanitization — targeted three-delimiter regex)
- specs/gptel-chat-mode/spec.md §"Response streaming and sanitization"
- architecture.md §`gptel-chat-stream`
- architecture.md §"Streaming split tests"
