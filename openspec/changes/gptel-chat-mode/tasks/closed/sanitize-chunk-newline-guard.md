---
name: sanitize-chunk-newline-guard
description: Guard gptel-chat--sanitize-chunk against embedded newlines
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sanitize-chunks
---

## Files to modify
- `config/gptel/chat/stream.org` (sanitizer section)
- `config/gptel/chat/stream.el` (tangled)
- `config/gptel/chat/test/stream/streaming-spec.el`

## Implementation steps
1. The function docstring says "LINE must be a complete single line
   with no embedded newline" but the function does not enforce it.
   Misuse silently produces lines where only the prefix (up to the
   first `\n`) is sanitized.
2. Add a guard:
   `(when (string-match-p "\n" line) (error "gptel-chat--sanitize-chunk: LINE must not contain newlines"))`.
3. Add a negative test that asserts the guard signals when called
   with `"#+end_assistant\nmore"`.

## Design rationale
The closure factory currently splits lines correctly, so the bug
is unreachable in practice. But the sanitizer is a public-shape
helper that downstream tasks (or future tooling) might call
directly; an enforced contract is cheap insurance against
re-introducing the split-chunk bug the holdback exists to prevent.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.
- Negative test confirms the guard signals.

## Context
- Review of `sanitize-chunks` task Finding #5.
