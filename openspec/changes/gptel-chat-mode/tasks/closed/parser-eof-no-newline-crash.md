---
name: parser-eof-no-newline-crash
description: Parser crashes args-out-of-range on begin_user/begin_assistant at EOF without trailing newline
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:parser
---

## Files to modify
- `config/gptel/chat/parser.org` (modify — outer `gptel-chat--parse-buffer` walker)
- `config/gptel/chat/parser.el` (tangled)
- `config/gptel/chat/test/parser/buffer-format-spec.el` (extend — new scenario)

## Implementation steps
1. In `gptel-chat--parse-buffer`, when computing `body-start = 1+ (line-end-position)`,
   the `line-end-position` returns `point-max` for a `#+begin_user` (or
   `#+begin_assistant`) on the buffer's last line with no trailing newline.
   `body-start` is then out of range and the subsequent `goto-char body-start`
   in `gptel-chat--scan-user-body` / `gptel-chat--scan-assistant-body`
   signals `args-out-of-range` instead of the documented
   `user-error: gptel-chat: unclosed <kind> block at line N`.
2. Fix by clamping `body-start` to `point-max`, OR by short-circuiting
   when `(>= (line-end-position) (point-max))` directly to the
   unclosed-block branch.
3. Add a Buttercup spec in `buffer-format-spec.el` under §"Buffer
   format validation" with input `"#+begin_user"` (literal, no trailing
   newline). Expect `user-error` with the unclosed-block message shape.
4. Repeat for `#+begin_assistant`.

## Design rationale
Parser spec guarantees "structurally invalid buffers signal a
user-visible error identifying the offending line". `args-out-of-range`
is neither user-visible nor informative. Reachable from
streaming-partially-inserted buffers and from manually-opened
malformed `.org` files.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- New scenario(s) cover the EOF-without-newline case for both
  `#+begin_user` and `#+begin_assistant`.

## Context
- Review of `parser` task (orchestrator session 2026-04-20) Finding #1.
