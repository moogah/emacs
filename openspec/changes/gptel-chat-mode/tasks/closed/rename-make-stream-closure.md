---
name: rename-make-stream-closure
description: Rename gptel-chat--make-stream-closure to describe its role
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sanitize-chunks
  - blocked-by:expose-tool-marker-setter
---

## Files to modify
- `config/gptel/chat/stream.org`
- `config/gptel/chat/stream.el` (tangled)
- `config/gptel/chat/test/stream/*.el` (call-site updates)

## Implementation steps
1. Pick a role-describing name; recommended:
   `gptel-chat--make-chunk-inserter` or
   `gptel-chat--make-stream-inserter`.
2. Rename the function and all call sites (currently only tests
   and the future `stream-callback` task).
3. Update any docstrings or comments that reference the old name.

## Design rationale
"Closure" is implementation detail. A name like
`make-chunk-inserter` describes what the thing does — useful for
future readers. Worth doing while the API is being refactored
(see `expose-tool-marker-setter`) so all callers move together.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.
- `grep -rn "gptel-chat--make-stream-closure" config/gptel/chat`
  is empty.

## Context
- Review of `sanitize-chunks` task Finding #7.

## Review (2026-04-21, orch-review-1776770835)

Rename is well-executed in source and tests (verification grep empty in
`config/gptel/chat`). **Blocking finding**: `design.md:144` still references
`make-stream-closure` in the Decision 3b Rationale paragraph, two lines
below the correctly-renamed shape table. The verification grep was scoped
to `config/gptel/chat` only and missed the artifact directory. Also two
non-blocking findings: `stream.org` section headings ("closure factory")
and one `describe` block name in `streaming-spec.el` still use the old
framing.

Blocking follow-up: `rename-make-stream-closure-cleanup` (stays at
`needs-review` until that task closes).

