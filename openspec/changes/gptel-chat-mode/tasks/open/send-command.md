---
name: send-command
description: gptel-chat-send with preconditions and in-flight guard via gptel--fsm-last
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:messages
  - blocked-by:fsm-handlers
  - blocked-by:stream-callback
---

## Files to modify
- `config/gptel/chat/send.org` (modify — add `gptel-chat-send`)
- `config/gptel/chat/send.el` (tangled)
- `config/gptel/chat/test/send/send-command-spec.el` (new)

## Implementation steps
1. Implement the interactive command `gptel-chat-send`:
   a. **Precondition: in-flight check.** If buffer-local `gptel--fsm-last`
      is non-nil AND `(gptel-fsm-state gptel--fsm-last)` is NOT in
      `(DONE ERRS)`, signal `(user-error "gptel-chat: a request is already
      in flight in this buffer")`. No queueing, no parallel flag.
   b. **Precondition: point location.** Must be inside an outer
      `#+begin_user`...`#+end_user` block whose body contains non-whitespace
      content. Edge cases:
      - Inside an assistant block → `(user-error "Send from a user block")`.
      - Empty user block → print a user-visible message ("prompt is empty")
        and return nil, do NOT signal.
      - Point between blocks (e.g. just after `#+end_assistant`) → find
        the last user block in the buffer; if it's empty, "prompt is empty";
        if it's populated, proceed.
   c. **Parse + construct messages.** Call `gptel-chat--parse-buffer` for
      the turn list and `gptel-chat--turns-to-messages` for the `:prompt`
      value. Parse errors propagate as user-visible errors.
   d. **Open assistant block.** If the current user block is unclosed
      (rare: user mid-edit), close it first. Insert `#+begin_assistant\n`
      after the user's closing `#+end_user` and create the insertion
      marker for the stream.
   e. **Invoke `gptel-request`.** Pass:
      - `:prompt` = constructed message list
      - `:stream t`
      - `:callback` = closure from `gptel-chat--stream-callback` (task
        `stream-callback`), capturing the insertion marker
      - `:fsm` = `(gptel-make-fsm :handlers gptel-chat--fsm-handlers)` (task
        `fsm-handlers`)
      - Do NOT pass `:buffer` explicitly unless needed; upstream uses
        current-buffer by default, which is what we want.
2. Precondition tests use fixtures with specific point positions and block
   structures. Assert `(spy-on 'gptel-request)` was called with the correct
   `:prompt`, `:stream`, `:callback`, `:fsm` keys and no unexpected extras.
3. Error-path tests: empty user block, point-in-assistant, in-flight request.

## Design rationale
Decision 11: detecting an in-flight request via buffer-local
`gptel--fsm-last` (which upstream sets on every `gptel-request`; see
`gptel.el:1104` and `:1328`) gives us **one source of truth**. A parallel
`gptel-chat--stream-active-p` flag would drift out of sync on abort, error,
or upstream bugs. Reusing the FSM also buys us state-aware policy (a future
UX could queue during `TOOL` but reject during `TYPE`) with no additional
bookkeeping.

Trade-off: we depend on `gptel--fsm-last` remaining the canonical handle
for in-flight requests. It is marked internal by name convention but is
used pervasively across upstream (rewrite mode, tool confirmation UI, send
menus), which makes it de facto stable. If upstream renames it, the check
is a one-line edit in one place.

Decision 8 (append flow) is implemented in the stream-callback's
completion branch, not here — `gptel-chat-send`'s job ends after invoking
`gptel-request`.

## Design pattern
Keep `gptel-chat-send` narrow: validate, parse, open the block, invoke
`gptel-request`. Everything after the `gptel-request` call is the
stream-callback and FSM handlers' responsibility. Testability depends on
this narrowness — most of the command is pure validation that can be
unit-tested with buffer fixtures and a spy on `gptel-request`.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/send.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/send` passes.
- Scenarios (spec §"Send command" and §"gptel-request backend usage"):
  - Send from inside open user block
  - Send with empty user block is a no-op with user-visible message
  - Send from inside assistant block is rejected
  - Send uses `gptel-request` with explicit `:prompt` (no
    `gptel--parse-buffer` call)

## Context
- design.md §Decision 11 (send-during-stream protection via FSM state)
- design.md §Decision 8 (append flow — implemented in stream-callback)
- specs/gptel-chat-mode/spec.md §"Send command"
- specs/gptel-chat-mode/spec.md §"gptel-request backend usage"
- architecture.md §`gptel-chat-send`
