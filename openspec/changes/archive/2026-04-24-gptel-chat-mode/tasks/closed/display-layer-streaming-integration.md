---
name: display-layer-streaming-integration
description: Skip overlay refresh during streaming TYPE, refresh once on DONE, cancel pending timer on kill-buffer, and add after-change path tests
change: gptel-chat-mode
status: done
relations:
  - discovered-from:display-layer
---

## Files to modify
- `config/gptel/chat/display.org`
- `config/gptel/chat/display.el` (re-tangled)
- `config/gptel/chat/test/display/display-layer-spec.el`

## Implementation steps

### Streaming-aware refresh (design.md Decision 5 risk; display-layer Finding 1)
1. Guard `gptel-chat--display-schedule-refresh` (or the inner
   debounced callback) against running during active streaming:
   ```elisp
   (unless (memq (gptel-chat--state) '(TYPE TOOL))
     ;; existing scheduling logic
     )
   ```
   where `(gptel-chat--state)` is the accessor added by `fsm-handlers`
   (or the `gptel-chat--lifecycle-state` buffer-local that
   `fsm-handlers` writes).

2. Add a `DONE` handler hook (or extend `gptel-chat--on-done`) that
   invokes `gptel-chat--refresh-overlays` once after streaming
   completes, so overlays reflect the final buffer state without
   churning on every chunk.

### Timer cleanup on buffer kill (display-layer Finding 3)
3. Add `(add-hook 'kill-buffer-hook
                  #'gptel-chat--display-uninstall-hooks nil t)`
   inside `gptel-chat--display-activate`. Prevents a dangling
   idle timer from firing a no-op wake-up after the buffer is
   killed.

### After-change path test coverage (display-layer Finding 2)
4. Add Buttercup specs:
   - Inserting into the buffer schedules exactly one refresh after
     the debounce delay elapses (`(sit-for (+ gptel-chat-display-refresh-delay 0.05))`),
     not N refreshes for N inserts.
   - When `gptel-chat--lifecycle-state` is `'streaming`, inserts do
     NOT trigger `gptel-chat--refresh-overlays` (spy on the refresh
     fn).
   - On simulated `DONE` transition, a single refresh fires.
   - Killing a buffer mid-debounce cancels the pending timer
     (assert timer is no longer in `timer-list`).

## Design rationale
`design.md:418,424` (Decision 5 Risks section) explicitly identifies
the O(n) full-buffer reparse per after-change tick as a performance
risk that "streaming will expose" and suggests the exact mitigation
here: skip refresh during streaming, refresh once at DONE. The
`display-layer` review flagged that neither the mitigation nor
tests for the after-change path were implemented in v1.

This task is the v2 of display-layer — the v1 shipped the overlay
lifecycle and the toggle command, which is sufficient for steady-
state buffers. Streaming integration becomes observable only when
`stream-callback` lands and chat buffers actually see rapid
after-change events.

Coordinates with:
- `fsm-handlers` (state accessor availability)
- `fsm-handlers-upstream-integration` (ensures DONE handler can be
  augmented without dropping upstream's :post)
- `stream-callback` (the task that will produce rapid after-change
  events)

None of those are strictly blockers — this task can be implemented
against the current state accessor and extended later — but the
test that simulates streaming benefits from landing after
`fsm-handlers-upstream-integration` so the DONE-handler augment
pattern is settled.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/display` passes with
  new specs.
- Manual (after stream-callback lands): run a long request with a
  large chat buffer and observe no overlay flicker during streaming;
  single redraw on completion.

## Context
- Review of `display-layer` (2026-04-21, orch-review-1776770835),
  Findings 1, 2, and 3.
- design.md Decision 5 Risks (`design.md:418,424`).

## Review
- **Session:** orch-review-1776785000 (2026-04-21), agent `a21426201b8fad957`
- **Verdict:** clean
- **Findings:** none
- **Checked and ruled out:**
  - **Accessor choice**: `'streaming`/`'tool-running` match the values
    written by `gptel-chat--on-type` / `gptel-chat--on-tool` at
    send.el:126,135. The task spec offered either the FSM accessor
    or the lifecycle-state buffer-local; the agent picked the latter.
  - **`'waiting` correctly excluded**: WAIT fires before the network
    request — no body content arrives, so after-change events there
    are user-driven and should refresh.
  - **Architecture rule**: `declare-function` + `defvar` forward
    decl + `with-eval-after-load` (no `require`) matches
    architecture.md:137-139 (forbids cross-module require, permits
    byte-compiler hints).
  - **DONE advice buffer-scoping**: advice reads FSM info `:buffer`
    and wraps with `with-current-buffer` (display.el:305-319), so
    it targets the originating chat buffer, not global.
  - **kill-buffer-hook**: buffer-local (`t` 4th arg, display.el:378);
    `gptel-chat--display-uninstall-hooks` calls `cancel-timer` on the
    stored timer (display.el:343-345). No dangling timer.
  - **Advice lifecycle**: advice is function-scoped, idempotent under
    reload (`advice-add` no-ops on duplicate NAME+FUNCTION), and
    self-guards with `buffer-live-p`. Not a leak.
  - **Tests**: debounce spec drives `timer-event-handler` on the
    captured timer (pragmatic given batch-mode idle-timer behaviour,
    still exercises the real callback). Streaming/tool-running guards
    use `let` binding + spy assertion on `gptel-chat--refresh-overlays`.
    All 17 display specs pass.
- **Spec-level signals:** the pre-existing `gptel-chat--lifecycle-state`
  / `gptel-chat--state` duality is documented as intentional
  (lifecycle = advisory UI indicator, state = FSM source-of-truth);
  this task consumed the existing shape without drift. Formalising a
  DONE hook in place of `advice-add` would be nice but is out of
  scope for a v2 integration task.
- **Follow-ups:** none
- **Dependents repointed:** none
