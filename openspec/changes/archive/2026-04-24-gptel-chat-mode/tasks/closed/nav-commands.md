---
name: nav-commands
description: next-turn, previous-turn, regenerate commands
change: gptel-chat-mode
status: done
relations:
  - blocked-by:mode-definition
---

## Review (2026-04-21, orch-review-1776770835)

Commands delegate cleanly to `gptel-chat--parse-buffer` (single source of
truth for turn boundaries); no regex reinvention. All spec scenarios
covered. Real correctness concern (regenerate mid-stream) captured in
follow-up `nav-regenerate-in-flight-guard`. Three informational findings
(atomic-change-group wrap, test precision, parser-error propagation) folded
into the same follow-up.


## Files to modify
- `config/gptel/chat/nav.org` (new)
- `config/gptel/chat/nav.el` (tangled)
- `config/gptel/chat/test/nav/navigation-spec.el` (new)
- `config/gptel/chat/test/nav/regenerate-spec.el` (new)

## Implementation steps
1. `gptel-chat-next-turn` (interactive):
   - `re-search-forward` for `^#\+begin_\(user\|assistant\)\b`
     (case-insensitive), respecting the state-machine rule that a match
     inside an open user or assistant block is body, not a turn start. For
     v1, because parser output is authoritative, a simpler implementation
     is acceptable: call `gptel-chat--parse-buffer`, find the first turn
     whose start is > `(point)`, jump there.
   - If no next turn → `(message "No next turn")`, leave point unchanged.
2. `gptel-chat-previous-turn` (interactive): symmetric to next-turn; find
   the last turn whose start is < `(point)`.
3. `gptel-chat-regenerate` (interactive):
   - Parse the buffer; find the last turn.
   - If the last turn is an assistant block: delete from its `:start` to
     its `:end` (inclusive of `#+end_assistant` line), then find the
     preceding user turn and invoke `gptel-chat-send`.
   - If the last turn is a user block (no response yet): `(message "No
     response to regenerate")`, no change.
   - If the buffer has no turns: same "no response" message.
4. Navigation must **not shadow org heading nav**. The keymap (task
   `mode-definition`) uses `C-c n` / `C-c p` (single-`C-c` prefix) so
   `C-c C-n` / `C-c C-p` remain bound to `org-next-visible-heading` /
   `org-previous-visible-heading`.
5. Tests:
   - Next turn from inside a user block moves to the following assistant.
   - Previous turn from inside an assistant block moves to the preceding
     user.
   - Next turn at end-of-buffer emits "No next turn" and doesn't move.
   - Regenerate after a completed response deletes the trailing assistant
     block and re-sends (spy on `gptel-chat-send`).
   - Regenerate when only a user block exists emits "No response to
     regenerate" with no buffer change.

## Design rationale
Decision 7 explicitly preserves org heading navigation by not shadowing
`C-c C-n`/`C-c C-p`. Chat-mode buffers may use headings to organize long
logs (Decision 12), so both navigation systems must coexist.

Regenerate is a thin wrapper over send — it deletes the trailing assistant
block and re-invokes `gptel-chat-send` on the preceding user turn. This
avoids duplicating the send pipeline (precondition check, in-flight guard,
parse, `gptel-request` invocation) and keeps the semantic of "regenerate"
identical to "send the same prompt again."

## Design pattern
Parser output is the single source of truth for turn positions. Nav
commands should not reinvent parsing — calling `gptel-chat--parse-buffer`
is fine even if it's slightly less efficient than a hand-rolled
`re-search-forward`, because v1 correctness matters more than micro-
optimization and the parser already handles heading-nesting and
body-literal edge cases.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/nav.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/nav` passes.
- Scenarios (spec §"Turn navigation" and §"Regenerate last response"):
  - Next turn from inside a user block
  - Previous turn from inside an assistant block
  - Next turn at end of buffer (message + no move)
  - Regenerate after a completed response
  - Regenerate when no response exists (message + no change)

## Context
- design.md §Decision 7 (keybindings — non-shadowing single-`C-c` prefix)
- design.md §Decision 12 (heading-allowed buffer structure)
- specs/gptel-chat-mode/spec.md §"Turn navigation"
- specs/gptel-chat-mode/spec.md §"Regenerate last response"
- architecture.md §`gptel-chat-nav`
