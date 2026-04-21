---
name: stream-callback-multi-round-t-signal
description: Gate end-of-turn close in the `'t` completion arm on (null (plist-get info :tool-use)) so multi-round tool-use turns don't prematurely close the assistant block
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:stream-callback
---

## Files to modify
- `config/gptel/chat/stream.org` (the `'t` completion arm of
  `gptel-chat--stream-callback`)
- `config/gptel/chat/stream.el` (re-tangled)
- `config/gptel/chat/test/stream/streaming-spec.el` (or a new
  multi-round-tool-use-spec.el)

## Implementation steps
1. In `stream.org:717-719` (the `'t` arm), rename the ignored `_info`
   parameter to `info` so we can consult the `:tool-use` flag.
2. Only run the end-of-turn close-and-append sequence
   (flush holdback → insert `#+end_assistant` → append empty
   `#+begin_user`/`#+end_user` → position point) when
   `(null (plist-get info :tool-use))`. During tool-use rounds,
   continue to flush the holdback but leave the assistant block open
   so that subsequent `(tool-result . ...)` events and the next
   request's streaming text land inside the same block.
3. Re-tangle.
4. Add a test that scripts a multi-round tool-use sequence into the
   callback:
   - chunk streams (TYPE), fires with `:tool-use` unset
   - `(tool-call . ...)` fires
   - `t` fires with `:tool-use` set (upstream marks the request as
     tool-use before handle-tool-use runs)
   - `(tool-result . ...)` fires
   - subsequent chunks stream and `t` fires again with `:tool-use`
     unset (final assistant turn)
   Assert the buffer ends in a single `#+end_assistant` on the final
   `t`, not after the first `t`.
5. Also assert a single-turn (no tool-use) request still closes
   correctly on `t`.

## Design rationale
Upstream's `gptel-curl--stream-cleanup` calls
`(funcall callback t info)` on every HTTP success
(`gptel-request.el:2669`). For a multi-round tool-use turn the flow
is Request-1 streaming text → `t` → FSM TYPE→TOOL → handle-tool-use →
`(tool-result . ...)` → WAIT → handle-wait → Request-2 streaming text
→ `t` → DONE. Our current `'t` arm unconditionally closes the block
on the first `t`, so tool results and Request-2 output land in an
already-closed block or a new user block. `persistent-agent.org:733`
handles this correctly via `(unless (plist-get info :tool-use) ...)`.

**Blocking follow-up** — `send-command` and `verify-change` depend on
correct multi-round behavior (re-pointed from `stream-callback`).

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes, with
  a new multi-round spec.
- Manual: a prompt that triggers tool use (e.g. `run_bash_command`)
  produces exactly one `#+end_assistant` after the final text, not
  after the first `t` signal.

## Context
- Review of `stream-callback` (2026-04-21, orch-review-1776774164),
  Finding #2 (blocking drift).
- Upstream reference: `gptel-request.el:2669`.
- Canonical pattern: `config/gptel/tools/persistent-agent.org:733`.
