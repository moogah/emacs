---
name: parser-eof-no-newline-crash
description: Parser crashes args-out-of-range on begin_user/begin_assistant at EOF without trailing newline
change: gptel-chat-mode
status: done
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

## Review
- **Session:** orch-review-1776785000 (2026-04-21), agent `ae98797b8157ffd47`
- **Verdict:** clean
- **Findings:** none
- **Checked and ruled out:**
  - **Clamp correctness**: `(min (1+ (line-end-position)) (point-max))`
    is correct. For buffers with trailing newlines it equals
    `(1+ line-end-position)` (unchanged); for EOF-without-newline it
    equals `point-max`, so the body scanners enter, `re-search-forward`
    returns nil, and the `'unclosed-<kind>` branch fires.
  - **No off-by-one regression** on valid buffers — `min` is a no-op
    when `point-max > (1+ line-end-position)`.
  - **Sibling call site parser.el:242** (`tool-body-start = (1+ tool-line-end)`
    inside `scan-assistant-body`): exercised `"#+begin_assistant\n#+begin_tool (foo)"`
    with no trailing newline — raised `user-error: unclosed tool block
    at line 2` cleanly via Emacs' silent `goto-char` clamping.
  - **Test coverage**: specs use literal `"#+begin_user"` /
    `"#+begin_assistant"` via `gptel-chat-test--with-buffer` (no
    trailing newline), genuinely exercising the EOF path.
  - **Message shape**: two of four new specs pin
    `:to-match "unclosed (user|assistant) block at line 1"`, matching
    `gptel-chat--parse-error`'s format at parser.el:96-98.
- **Follow-ups:** none
- **Dependents repointed:** none
