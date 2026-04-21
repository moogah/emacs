---
name: rename-make-stream-closure-cleanup
description: Sweep stale "make-stream-closure" references in design.md and "closure factory" headings after the rename
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:rename-make-stream-closure
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (Decision 3b Rationale prose)
- `config/gptel/chat/stream.org` (section headings)
- `config/gptel/chat/stream.el` (re-tangled)
- `config/gptel/chat/test/stream/streaming-spec.el` (describe block name)

## Implementation steps
1. Replace `` `make-stream-closure` `` with
   `` `gptel-chat--make-stream-inserter` `` (or `make-stream-inserter`)
   at `design.md:144` inside the Decision 3b Rationale paragraph.
2. Retitle `stream.org` headings that currently read "Streaming closure
   factory" / "The closure factory" to use "inserter" framing to match the
   function name. Re-tangle.
3. Update the `describe` block at `streaming-spec.el:505` ("... through
   the closure") to match the new framing (e.g., "... through the
   inserter" or "... through the stream handle").
4. Extend the verification grep to also cover the artifact directory.

## Design rationale
The original `rename-make-stream-closure` task scoped its verification grep
to `config/gptel/chat` and missed one reference in `design.md` and three
cosmetic occurrences in headings/describe blocks. The function name itself
is consistent everywhere; this task closes the loop on the surrounding
prose so a future reader isn't left with two names for the same thing.

This task is a **blocking follow-up** — `rename-make-stream-closure`
stays at `needs-review` until this closes.

## Verification
- `grep -rn "make-stream-closure" openspec/changes/gptel-chat-mode/ config/gptel/chat/`
  returns only historical task files in `tasks/closed/`.
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes.

## Context
- Review of `rename-make-stream-closure` (2026-04-21, orch-review-1776770835),
  Findings 1-3.
