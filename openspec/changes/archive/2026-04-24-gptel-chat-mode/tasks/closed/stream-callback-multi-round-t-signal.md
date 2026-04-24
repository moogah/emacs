---
name: stream-callback-multi-round-t-signal
description: Gate end-of-turn close in the `'t` completion arm on (null (plist-get info :tool-use)) so multi-round tool-use turns don't prematurely close the assistant block
change: gptel-chat-mode
status: done
relations:
  - discovered-from:stream-callback
---

## Review (2026-04-21, orch session `orch-1776779279`)

Clean merge. The `'t` arm gates `gptel-chat--stream-close-assistant`
on `(null (plist-get info :tool-use))` while keeping the holdback
flush (`stream-insert t`) unconditional — precisely what the task
specifies and what `config/gptel/tools/persistent-agent.org:733`
models. The `_info → info` rename is confined to the one callback
that consumes it. The new spec is genuinely behavioural — it scripts
the full
`stream → t(:tool-use t) → tool-call → tool-result → stream → t(nil)`
trace through the real closure and asserts buffer equality including
exactly one `#+end_assistant` at buffer end. A two-round variant plus
three single-turn regression guards (nil INFO, `:tool-use nil`,
plain single turn) are present. The post-merge `d85f931` repair is a
narrow fixture update for the three-list tool-call shape that landed
from `stream-callback-tool-element-shape-and-tests`; assertions
(buffer equality, `#+end_assistant` count) are preserved. 81/81 specs
in `config/gptel/chat/test/stream` pass.

### Findings

1. **Spec-level — chat spec doesn't capture `t`-per-round semantics.**
   `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
   "Response streaming and sanitization" (line 208) says nothing
   about the `t` signal firing once per HTTP round-trip rather than
   once per turn. That invariant is load-bearing for this fix — a
   future "simplify the `'t` arm" change would silently regress
   buffer corruption in tool-using turns. Currently the invariant
   lives only in code comments, design.md Decision 10, and this
   task body. **Tracked as follow-up task**:
   [`chat-spec-t-signal-round-semantics`](../open/chat-spec-t-signal-round-semantics.md)
   (non-blocking; code is correct, spec is under-specified).

2. **Non-blocking — synthetic INFO plist.**
   `config/gptel/chat/test/stream/multi-round-tool-use-spec.el:~165`
   constructs INFO as `'(:tool-use t)` rather than going through an
   upstream plist builder. This matches the task's own guidance and
   `persistent-agent.org`'s usage; the two defensive specs (nil
   INFO, `:tool-use nil`) pin the plist-get contract. Known gap of
   the unit-level approach — if upstream ever renames `:tool-use`,
   only integration tests catch it. Not worth fixing here.

### Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` → 81 specs,
  0 failed.
- Manual trace check not performed by reviewer agent; scripted
  buffer-equality assertion covers the exact sequence the task's
  manual step calls out.

### Dependents
- `send-command`, `verify-change` — blocked-by this task. Not
  repointed: spec-level finding #1 is non-blocking for CODE
  dependents.

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
