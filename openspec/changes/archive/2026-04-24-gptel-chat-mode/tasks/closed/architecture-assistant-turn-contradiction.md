---
name: architecture-assistant-turn-contradiction
description: Fix architecture.md internal contradiction about assistant turn shape
change: gptel-chat-mode
status: done
relations:
  - discovered-from:parser
---

## Files to modify
- `openspec/changes/gptel-chat-mode/architecture.md`

## Implementation steps
1. Line ~16 (Components section) describes assistant turns as
   `(:role user|assistant :content STR :tool-calls (...))`.
2. Line ~123 (Data contracts section) describes them as
   `(:role 'assistant :segments ((:type text ...) ...))`.
3. The implementation follows the Data contracts shape (segments).
   Update line 16 to match: `(:role user :content STR)` for user
   turns and `(:role assistant :segments (...))` for assistant turns.
4. Cross-reference the Data contracts block from the Components
   block so future edits keep them in sync.

## Design rationale
A reader of the Components section currently expects `:content` /
`:tool-calls` and finds neither in parser output. This was a
pre-existing spec bug that did not block the parser task but will
mislead reviewers and downstream implementers.

## Verification
- `grep -n ":content\|:segments\|:tool-calls" openspec/changes/gptel-chat-mode/architecture.md`
  shows a single, consistent shape per role across the file.

## Context
- Review of `parser` task Finding #4.
