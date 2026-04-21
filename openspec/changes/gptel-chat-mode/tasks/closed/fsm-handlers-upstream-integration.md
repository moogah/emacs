---
name: fsm-handlers-upstream-integration
description: Chain gptel--handle-post on DONE/ERRS and handle the ABRT state so :post hooks and aborts don't silently drop
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:fsm-handlers
---

## Files to modify
- `config/gptel/chat/send.org` (handler-alist section + new on-abrt handler)
- `config/gptel/chat/send.el` (re-tangled)
- `config/gptel/chat/test/send/backend-invocation-spec.el` (add ABRT +
  :post coverage)
- `openspec/changes/gptel-chat-mode/design.md` (Decision 3 state taxonomy
  + Decision 10 cross-references to list ABRT)

## Implementation steps
1. Augment `gptel-chat--fsm-handlers` to chain `gptel--handle-post` on
   the terminal states:
   ```elisp
   (DONE ,#'gptel-chat--on-done ,#'gptel--handle-post)
   (ERRS ,#'gptel-chat--on-errs ,#'gptel--handle-post)
   ```
   This preserves upstream's contract that caller-supplied `:post` hooks
   run after our UI teardown.

2. Add an ABRT handler and row:
   ```elisp
   (ABRT ,#'gptel-chat--on-abrt ,#'gptel--handle-post)
   ```
   The handler should clear `gptel-chat--lifecycle-state` (or set it to
   `'aborted`) so the send-guard in `send-command` will not wedge after
   a user invokes `gptel-abort` (design.md:241 already envisions
   `C-c C-k` → `gptel-abort`).

3. Extend the send-guard's idle-state set (once `send-command` lands)
   to include the cleared/aborted state. If `send-command` is not yet
   landed, document the required behaviour here so the downstream task
   can implement it without rediscovering the contract.

4. Update `design.md` §Decision 3 state taxonomy to list ABRT explicitly
   (currently only WAIT/TYPE/TOOL/DONE/ERRS appear; the table at the top
   of design.md similarly omits ABRT). Cross-reference Decision 10 and
   the state-accessor docstring.

5. Tests:
   - ABRT transition via `(gptel--fsm-transition fsm 'ABRT)` clears the
     indicator.
   - A `:post` callback passed through to `gptel-request` fires on
     DONE, ERRS, and ABRT (spy to confirm).
   - The send-guard (once introduced) treats ABRT as idle.

## Design rationale
The `fsm-handlers` review (2026-04-21) found that the handler alist
REPLACES rather than chains `gptel--handle-post` on DONE/ERRS, and omits
ABRT entirely. Both are design-drift from upstream's contract at
`gptel-request.el:1589-1594` and `:2124`. Without the fix:

- Any caller-supplied `:post` hook (including future session export,
  budget tracking, or activities integration) silently fails.
- User aborts via `gptel-abort` leave the lifecycle state stuck,
  wedging the send-guard and blocking further sends.

Both issues surface only once `send-command` lands, making them "hard to
debug silent failures" rather than test-failures. Close the loop here
before downstream work proceeds.

This task is a **blocking follow-up** — `fsm-handlers` stays at
`needs-review` until this closes; `send-command` remains blocked by
`fsm-handlers`, so this also gates the send-command path.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/send` passes with new
  ABRT and :post specs.
- `grep -n "ABRT" openspec/changes/gptel-chat-mode/design.md` shows ABRT
  in the state taxonomy and Decision 3.
- Manual: interactive `gptel-abort` during a streaming request clears
  the indicator (buffer-local `gptel-chat--lifecycle-state` is nil or
  `'aborted`).

## Context
- Review of `fsm-handlers` (2026-04-21, orch-review-1776770835),
  Findings 1 and 2 (both blocking).
- Upstream: `runtime/straight/build/gptel/gptel-request.el:1589-1594`
  (`:handlers` default), `:2124` (ABRT transition).
- `config/gptel/tools/persistent-agent.org:541-544` (chains WAIT/TOOL
  only; leaves DONE/ERRS/ABRT to upstream — the pattern this task
  realigns with).
