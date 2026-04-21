---
name: messages-tool-result-unescape
description: Un-escape chat delimiters in tool :result before emitting to gptel-request
change: gptel-chat-mode
status: done
relations:
  - discovered-from:messages
---

## Files to modify
- `config/gptel/chat/parser.org` (`gptel-chat--segment-to-messages`)
- `config/gptel/chat/parser.el` (re-tangled)
- `config/gptel/chat/test/parser/` (add targeted spec)

## Implementation steps
1. At `parser.el:495` (or wherever `gptel-chat--segment-to-messages`
   emits `(tool . PLIST)`), route `(plist-get segment :result)` through
   `gptel-chat--unescape-end-delimiters` before assigning to the
   plist's `:result`.
2. Add a Buttercup spec: build a segment whose `:result` contains a
   literal `,#+end_tool` (or `,#+end_assistant`) line; assert the
   emitted plist's `:result` has the leading `,` stripped.
3. Verify the un-escape is idempotent on results that contain no
   escaped delimiters (should be a no-op).

## Design rationale
The stream sanitizer escapes any line that begins with a chat
delimiter by prepending `,` (see `stream.el:46` and the
`sanitize-chunk-newline-guard` contract). User and assistant turns
route their content through `gptel-chat--unescape-end-delimiters` at
emit time, so the model sees the canonical form. Tool-result content
currently does NOT — `:result` is copied verbatim from the parsed
segment into the outgoing plist, so if a tool happened to print a
literal `#+end_tool` line, the escaped artifact ships to the model.

The gap is narrow (few tools intentionally print our exact sentinel
strings) but is a real correctness issue: the un-escape pass should
be symmetric across all content types that the sanitizer can touch.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes with new
  spec.
- Round-trip: sanitize a tool-result string containing a literal
  `#+end_tool` line → parse back via `gptel-chat--parse-buffer` →
  convert via `gptel-chat--segment-to-messages` → the un-escaped
  original is recovered.

## Context
- Review of `messages` (2026-04-21, orch-review-1776770835), Finding 1.
- Stream sanitizer: `config/gptel/chat/stream.el:46,139`.
- Parser un-escape: `config/gptel/chat/parser.el:493-496`.

## Review (orch-review-1776792000, 2026-04-21)

Reviewer verified:
- `gptel-chat--unescape-end-delimiters` is the exact inverse of the sanitizer's `,`-prefix escape — regex `\(\`\|\n\),\(#\+end_\(user\|assistant\|tool\)\b\)` with case-folding covers all three delimiters (`#+end_user`, `#+end_assistant`, `#+end_tool`). Nil and empty-string inputs return `""`. `#+begin_*` is never escaped upstream, so no begin-form inverse is needed.
- Symmetry is now complete across all line-level-sanitized content paths: user prose (`parser.el:534`), assistant prose (`parser.el:505`), and tool `:result` (`parser.el:513`). `:name`/`:args` are structured data never touched by the sanitizer; `:system` prompts come from presets, not chat-buffer content.
- Round-trip coverage: the existing `escape-round-trip-spec.el` already exhaustively covers `,,#+end_assistant` (no-op), `,#+end_src` (preserved), and sanitize → parse composition for user/assistant for all three delimiters. The new specs in `message-construction-spec.el` pin the tool-result converter contract at the unit level; a dedicated tool-result end-to-end round-trip would be tighter but is not load-bearing since the composition is mechanically equivalent.
- Scope: three production lines + docstring + two specs. No drive-by changes.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` → 89 specs, 0 failed.

### Findings
None blocking. None non-blocking worth a follow-up task.
