---
name: parser-multi-tool-segment-test
description: Add parser test for assistant turn with multiple tool-call segments
change: gptel-chat-mode
status: done
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

## Review
- **Session:** orch-review-1776789773 (2026-04-21), agent `a6a74e6dcaf10d119`
- **Verdict:** clean with one fixture-extension follow-up
- **Findings:**
  - [follow-up] `config/gptel/chat/test/parser/buffer-format-spec.el:564-590`
    — fixture uses two tool calls, but `spec.md:249-252` calls for
    **three** sequential tool calls with interleaved prose. Loop-
    advancement invariant is adequately pinned with two, but
    spec/test divergence warrants closing. Follow-up task
    `parser-multi-tool-segment-three-calls` created.
- **Checked and ruled out:**
  - **Loop-branch coverage**: fixture drives the outer `while` through
    three cond iterations (two `#+begin_tool`, one `#+end_assistant`),
    exercising the ≥2-tool path.
  - **Assertion strictness**: type sequence `(text tool-call text
    tool-call text)`, distinguishable names/args (`tool_a :x 1` vs
    `tool_b :y 2`), distinguishable results and prose per segment.
    Dedup/collapse/reorder would fail on at least three expectations.
  - **Shape-mismatch behavior**: `(length segs) :to-equal 5` fails
    first; subsequent `nth` accesses either return nil and fail
    subsequent expectations or error — loud fail, not silent pass.
  - **Downstream impact**: no open task lists this as blocker.
    `sessions-branching` and `verify-change` are `blocked-by:parser`
    (the parent), not this sub-task.
- **Verification:** `./bin/run-tests.sh -d config/gptel/chat/test/parser`
  passes 86/86 specs.
- **Follow-ups:** `parser-multi-tool-segment-three-calls` (ready, test-only)
- **Dependents repointed:** none
