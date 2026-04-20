---
name: parser-multi-tool-segment-test
description: Add parser test for assistant turn with multiple tool-call segments
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:parser
---

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el` (extend — new scenario)

## Implementation steps
1. Add an `it` block under the existing assistant-segments describe.
2. Input fixture: assistant block containing
   `prose1` → `#+begin_tool (tool_a :x 1)\n...result_a...\n#+end_tool` →
   `prose2` → `#+begin_tool (tool_b :y 2)\n...result_b...\n#+end_tool` →
   `prose3`.
3. Assert resulting `:segments` has shape
   `(text tool-call text tool-call text)` with five segments, each
   with the expected `:type` and (for tool-calls) the expected name
   and result content.

## Design rationale
The tool-scanning branch in `gptel-chat--scan-assistant-body` iterates
via the outer `while` loop, so a bug where the loop fails to advance
past `#+end_tool` (e.g., off-by-one in `text-from`) only surfaces with
≥2 tool calls. Spec §"Tool-call rendering inside assistant blocks"
explicitly enumerates "Multiple tool calls in a response" as a
scenario; current suite exercises only the single-tool case.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- The new scenario asserts segment count = 5 and segment type sequence.

## Context
- Review of `parser` task Finding #2.
