---
name: stream-callback
description: pcase dispatch on response shapes with tool-block rendering and completion/abort handling
change: gptel-chat-mode
status: done
relations:
  - blocked-by:sanitize-chunks
  - blocked-by:mode-definition
---

## Files to modify
- `config/gptel/chat/stream.org` (modify — add callback dispatch)
- `config/gptel/chat/stream.el` (tangled)
- `config/gptel/chat/test/stream/tool-call-spec.el` (new)
- `config/gptel/chat/test/stream/streaming-spec.el` (extend — abort, append
  new user block)

## Implementation steps
1. Implement `gptel-chat--stream-callback` as a factory returning the
   closure passed to `gptel-request` as `:callback`. It captures the
   `insertion-marker`, `holdback`, and `tool-marker` state from task
   `sanitize-chunks`.
2. Dispatch on upstream's documented response shapes via `pcase`:

   | Response | Case |
   |---|---|
   | string | text chunk — run through the holdback-aware sanitizer (task `sanitize-chunks`); insert at the active marker |
   | `` `(reasoning . ,chunk) `` | v1: **ignore**. A later change may render into a `#+begin_reasoning` block. |
   | `` `(tool-call . ,calls) `` | open a nested `#+begin_tool (<name> :args <sexp>)` block inside the active assistant block; set `tool-marker` to be the new insertion target |
   | `` `(tool-result . ,results) `` | insert stringified result at `tool-marker`; append `#+end_tool`; clear `tool-marker` |
   | `t` | normal completion — flush holdback; insert `#+end_assistant`; append a fresh empty `#+begin_user`/`#+end_user` block; position point on the inner blank line |
   | `nil` | error / network failure — close the block with a visible error marker, then `#+end_assistant` |
   | `'abort` | user abort (triggered by `M-x gptel-abort`) — close the block with an interruption marker, then `#+end_assistant` |

3. Tool-call and tool-result elements are **plists** carrying `:name`,
   `:args`, and (for results) `:result`. The `#+begin_tool` opening line
   formats as `(<name> :args <sexp>)` to match the existing session-file
   convention.
4. Handle multiple tool calls in one assistant turn: each `tool-call`
   event opens a new sibling `#+begin_tool` block, and each `tool-result`
   closes it. Prose chunks between tool events are inserted normally at
   the assistant-level marker (i.e. `tool-marker` is cleared between
   tool events, so prose goes to `insertion-marker`).
5. On completion (`t`), also append a fresh empty `#+begin_user`
   /`#+end_user` block after the closing `#+end_assistant` and move point
   inside it (Decision 8's shell-like append flow).
6. **Do NOT invoke `gptel-post-response-functions` or
   `gptel-pre-response-hook`.** Those are consumed by gptel-mode's default
   callback, which we bypass intentionally. If a future chat-mode-specific
   hook is needed, add one with a distinct name in a follow-up change.
7. Tests:
   - Single tool call: assistant block contains one `#+begin_tool`
     /`#+end_tool` with correct args and result.
   - Multiple tool calls interleaved with prose.
   - Abort: buffer closes cleanly with an interruption marker and
     `#+end_assistant` on its own line.
   - Append-new-user-block on completion: buffer ends in an empty user
     block with point inside.
   - Bypass assertion: confirm `gptel-post-response-functions` is not
     triggered by our callback (spy).

## Design rationale
Decision 10: `pcase` on cons-cell patterns is both the upstream idiom
(`gptel-request.el:1684-1752`) and what `persistent-agent.org` already
does. An earlier design draft described tool events as "plists with a
tool-call sentinel" — that was **incorrect**; upstream emits
`` `(tool-call . ,calls) `` and `` `(tool-result . ,results) `` cons cells
whose cdrs are lists of plists.

Decision 8 chose shell-like append (auto-insert a fresh `#+begin_user`
block on completion) because it matches the shell-like ergonomics that
motivated the mode. The cost is one extra empty block pair on disk —
trivial. Users who want to regenerate or edit a past turn still can, via
`gptel-chat-regenerate` or normal navigation.

Decision 10 also notes: `gptel-post-response-functions` and friends assume
gptel-mode's prompt/response-prefix conventions, which we don't use.
Bypassing them is intentional.

"How `'abort` is triggered": `M-x gptel-abort` (upstream) finds the active
FSM in the current buffer and invokes our callback with `'abort`. No
chat-mode-specific abort wrapper is needed — the `C-c C-k` binding in the
mode-map (task `mode-definition`) goes directly to `gptel-abort`.

## Design pattern
`config/gptel/tools/persistent-agent.org` calls `gptel-request` with full
tool history and pattern-matches every response shape in a single `pcase`.
Start there for the dispatcher skeleton.

Test pattern: stub `gptel-request` with `cl-letf` and synchronously fire
scripted response sequences into `:callback`. See architecture.md §
"Backend stubbing" for the shape.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes streaming,
  chunk-split, and tool-call suites.
- Scenarios (spec §"Response streaming and sanitization", §"Tool-call
  rendering"):
  - Normal stream completion (well-formed block)
  - Stream abort
  - Single tool call
  - Multiple tool calls
  - Append new user block on completion

## Context
- design.md §Decision 3b (closure state for streaming)
- design.md §Decision 8 (shell-like append flow)
- design.md §Decision 10 (callback dispatch on response shapes)
- `config/gptel/tools/persistent-agent.org` — canonical reference
- specs/gptel-chat-mode/spec.md §"Response streaming and sanitization"
- specs/gptel-chat-mode/spec.md §"Tool-call rendering inside assistant blocks"

## Review (2026-04-21, orch-review-1776774164)

Three **blocking findings** (two drift + coupled test-gap) and two
non-blocking findings. Under the revised workflow, reviewed tasks flip
to `done` regardless of findings — blocking follow-ups live as their
own tasks in the open queue and downstream dependents re-point
`blocked-by:` at the follow-ups (not at this parent).

Blocking — drift from upstream contract:

1. **Tool-call / tool-result element shape.** Implementation destructures
   each call via `(plist-get call :name)` / `(plist-get call :args)` /
   `(plist-get result :result)`. Upstream (`gptel-request.el:1812-1827`,
   `gptel.el:1801, 1855`) emits each element as a 3-list
   `(TOOL-STRUCT ARGS CB-OR-RESULT)`. Reading the real shape yields nil
   for `:name`/`:args`; tool headers render as
   `#+begin_tool ( :args nil)` and all results render as empty strings.
   Tests only passed because stubs feed synthetic plists through the
   callback. → `stream-callback-tool-element-shape-and-tests.md`
2. **`t` completion signal fires per HTTP request, not per assistant
   turn.** Upstream fires `(funcall callback t info)` on every HTTP
   success (`gptel-request.el:2669`). For a tool-use turn this fires
   between Request-1 and Request-2, so our `'t` arm unconditionally
   closes the assistant block and appends a fresh user block mid-turn.
   `persistent-agent.org:733` handles this correctly via
   `(unless (plist-get info :tool-use) ...)`. →
   `stream-callback-multi-round-t-signal.md`
3. **Tests are structural, not behavioral.** Every tool-call/tool-result
   spec constructs the cdr as `((:name ... :args ... :result ...))` —
   a shape upstream never emits. This is why Finding #1 slipped through.
   Coupled fix: folded into `stream-callback-tool-element-shape-and-tests.md`.

Non-blocking:

4. **spec-signal**: `specs/gptel-chat-mode/spec.md:35` shows
   `#+begin_tool (run_bash_command :command "uname")` (inline kwargs);
   implementation writes the `:args`-wrapped shape
   `(run_bash_command :args (:command "uname"))`. Spec example is out of
   sync. → `spec-tool-header-shape-alignment.md`
5. **code-quality**: `config/gptel/chat/stream.org:707-714` silently
   drops a `tool-result` with no matching pending marker. Inconsistent
   with the loud-fail style of the rest of the file. →
   `stream-callback-orphan-result-loud-fail.md`

`send-command` repoints off `stream-callback` onto
`stream-callback-tool-element-shape-and-tests` and
`stream-callback-multi-round-t-signal`. `verify-change` repoints
similarly (re-verify once both blocking follow-ups close).
