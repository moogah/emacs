---
name: fix-chat-loader-nav-rationale
description: Correct nav-after-send dependency rationale in chat.org loader docstring
change: gptel-chat-mode
status: done
relations:
  - discovered-from:scaffold-chat-subsystem
---

## Files to modify
- `config/gptel/chat/chat.org` (modify — dependency note around lines 71-72)
- `config/gptel/chat/chat.el` (tangled)

## Implementation steps
1. In `chat.org`, locate the dependency note that currently reads roughly:
   "`nav` consumes the turn list from `parser`; loads after `send` so its
   commands can reuse send-path utilities for regenerate."
2. Replace with a corrected rationale: nav loads after `send` because
   `regenerate` is a thin wrapper over `gptel-chat-send` (per the
   `nav-commands` task body). Turn navigation itself uses
   `re-search-forward` on block delimiters and does NOT depend on
   `parser` (architecture.md §gptel-chat-nav explicitly denies coupling
   to parser internals).
3. Re-tangle.

## Design rationale
The loader doc conflated two stories. Architecture.md:38 says nav has
"No coupling to parser internals — uses `re-search-forward` on block
delimiters." But the open `nav-commands` task does describe regenerate
as "a thin wrapper over send — it deletes the trailing assistant block
and re-invokes `gptel-chat-send`." So the load-after-send dependency is
real, but its justification is regenerate-uses-send, not
nav-consumes-parser.

## Verification
- `grep -n "consumes the turn list" config/gptel/chat/chat.org` returns
  nothing.
- Re-tangle succeeds.
- Boot is unaffected (this is a comment-only change).

## Context
- Review of scaffold-chat-subsystem (orchestrator session 2026-04-20)
  Finding #3
- architecture.md §gptel-chat-nav
- tasks/open/nav-commands.md (regenerate description)
