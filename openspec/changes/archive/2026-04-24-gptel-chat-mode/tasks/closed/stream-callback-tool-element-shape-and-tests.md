---
name: stream-callback-tool-element-shape-and-tests
description: Destructure tool-call/tool-result elements as the (TOOL-STRUCT ARGS CB-OR-RESULT) triple upstream emits, and add behavioral tests that use the real shape
change: gptel-chat-mode
status: done
relations:
  - discovered-from:stream-callback
---

## Review (2026-04-21, orch session `orch-1776779279`)

**Bookkeeping note:** the prior orchestrator transition (`b1997a3`)
intended to flip this file to `status: needs-review` but left it at
`status: ready` while still moving it to `tasks/closed/`. The merge
(`0341d26`) and implementation had already landed. This review
closes the gap.

Clean merge. Handlers in `stream.org` destructure `(TOOL-STRUCT ARGS
CB)` / `(TOOL-STRUCT ARGS RESULT)` triples via `pcase-dolist`, names
come from `gptel-tool-name` on the struct (not `plist-get :name`),
and the result handler defensively handles string / nil / non-string
sexp per upstream's loose contract at
`gptel-request.el:1812-1827`. Every test fixture in `tool-call-spec.el`
was converted — no stale plist fixtures remain — and fixtures are
built with the real `gptel-make-tool` constructor so a future shape
drift fails the tests. Decision 10 and the parent `stream-callback.md`
Review section both document the correction with explicit upstream
line pointers. 81/81 `config/gptel/chat/test/stream` specs pass.

### Findings

None that clear the signal/noise bar. The reviewer agent
(`aeb05dcd57f21e605`) explicitly pressure-tested for:

- Stale plist fixtures masking the fix (none remain).
- `:name` / `:args` plist drift in Decision 10 (corrected with
  upstream line pointers).
- Scope creep (stayed within declared files; a small defensive
  extension to accept non-string results via `prin1-to-string` is
  reasonable hardening consistent with upstream's loose docstring).
- Parser.org / parser.el still using `:name :args :result` plist
  language — confirmed as a legitimate *different* data path
  (session-file / wire-format tool representation), not
  stream-callback's element shape.

### Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` → 81/81 pass.
- `grep -rn "plist-get.*:name" config/gptel/chat/stream.el config/gptel/chat/stream.org`
  → 0 hits in tool-event code.
- Design.md Decision 10 updated; parent stream-callback.md Review
  note added.

### Dependents
- `send-command`, `verify-change` — transitively blocked-by this
  task (listed via `stream-callback`'s chain). No repoint needed.

## Files to modify
- `config/gptel/chat/stream.org` (`gptel-chat--stream-open-tool-block`
  and the tool-result handler)
- `config/gptel/chat/stream.el` (re-tangled)
- `config/gptel/chat/test/stream/tool-call-spec.el` (convert to
  behavioral shape)
- `config/gptel/chat/test/stream/streaming-spec.el` (any tests using
  the plist shape)
- `openspec/changes/gptel-chat-mode/design.md` (Decision 10 callback-
  dispatch table — plist claim is wrong)
- `openspec/changes/gptel-chat-mode/tasks/closed/stream-callback.md`
  (task-body claim "plists with :name, :args, :result" is wrong; note
  the correction for future readers)

## Implementation steps
1. Study upstream's emitted shape before coding:
   - `runtime/straight/build/gptel/gptel-request.el:1812-1827`
     (callback docstring for tool events)
   - `runtime/straight/build/gptel/gptel.el:1801` (`pcase-dolist`)
   - `runtime/straight/build/gptel/gptel.el:1855`
     (`cl-loop for (tool args result) in tool-results`)

   Each element of `(tool-call . <calls>)` and `(tool-result . <results>)`
   is a 3-list `(TOOL-STRUCT ARGS CB-OR-RESULT)`. Do NOT trust the
   stream-callback task body's plist claim — it is drift.

2. Rewrite the tool-call handler in `stream.org` to destructure:
   ```elisp
   (pcase-dolist (`(,tool-spec ,args ,_cb) calls)
     (let ((name (gptel-tool-name tool-spec)))
       ;; ... open #+begin_tool (<name> :args <args>) ...))
   ```
3. Rewrite the tool-result handler similarly:
   ```elisp
   (pcase-dolist (`(,_tool-spec ,_args ,result) results)
     ;; ... insert result at tool-marker, close #+end_tool ...)
   ```
4. Re-tangle `stream.org`.
5. Convert `tool-call-spec.el`'s structural plist fixtures to real
   triple-list shape. At minimum one happy-path spec per branch should
   feed `((,(gptel-tool-name ...) ,args ,cb-or-result) ...)` with a
   real or stubbed `gptel-tool` struct. Existing plist-shaped tests
   can be dropped or converted; retaining them masks the bug this
   task is fixing.
6. Update `design.md` Decision 10 to reference the triple-list shape
   with file:line pointers to upstream. Correct any other prose that
   still says "plist carrying :name".
7. Add a short note to `tasks/closed/stream-callback.md` under the
   existing "Review" section pointing at this task as the corrective
   follow-up.

## Design rationale
The original task body described tool events as plists — inconsistent
with upstream's own contract and with `persistent-agent.org`'s
handling. The implementing agent wrote tests matching the (wrong) task
body description, so the suite passed while real tool-use broke at
runtime: every tool header would render as
`#+begin_tool ( :args nil)` and every result as an empty string.

This task fixes the code, fixes the tests, and corrects the design
record so future readers don't re-introduce the same bug.

**Blocking follow-up** — `send-command` and `verify-change` depend on
it transitively (re-pointed from `stream-callback`).

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes. At
  least one spec per branch uses the triple-list shape.
- `grep -rn "plist-get.*:name" config/gptel/chat/stream.el
  config/gptel/chat/stream.org` returns no hits in tool-event code.
- Manual: run a tool-using conversation; confirm `#+begin_tool` lines
  render the real tool name and `:args` sexp.

## Context
- Review of `stream-callback` (2026-04-21, orch-review-1776774164),
  Findings #1 and #3 (both blocking, coupled).
- Upstream references: `gptel-request.el:1812-1827`, `gptel.el:1801, 1855`.
- Canonical handling pattern: `config/gptel/tools/persistent-agent.org`.
