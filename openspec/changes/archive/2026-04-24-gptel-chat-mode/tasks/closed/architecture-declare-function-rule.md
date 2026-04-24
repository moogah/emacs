---
name: architecture-declare-function-rule
description: Soften architecture.md no-cross-module-references rule to permit declare-function
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `openspec/changes/gptel-chat-mode/architecture.md` (around lines
  135-144 — Module load order section)

## Implementation steps
1. The architecture currently states: "None of the chat-mode
   modules issue cross-module `require` or `declare-function`
   calls; sibling-symbol references all happen inside command
   bodies, transient suffix bodies, or keymap entries — i.e.,
   resolved at call-time, not load-time."
2. The mode-definition implementation added six `declare-function`s
   (five siblings + one upstream `gptel-abort`) to keep the
   byte-compiler quiet about forward-referenced keymap targets.
3. Reword the architecture rule to clarify intent:
   - The hard rule is "no cross-module `require`" (which would
     create real load-order coupling).
   - `declare-function` is permitted — it's a compiler hint with
     no runtime effect.
4. Update the loader-section text in `chat.org` if it repeats the
   same claim, so the two artifacts agree.

## Design rationale
`declare-function` is the idiomatic Emacs solution for the exact
situation mode-definition faced. Leaving the rule as written
guarantees future tasks will either violate it or accept
byte-compile noise for no real benefit.

## Verification
- architecture.md no longer forbids `declare-function`
  categorically.
- chat.org loader docstring (if affected) is consistent with the
  updated rule.

## Context
- Review of `mode-definition` task Finding #2 (and related
  spec-level signal).

## Review
- **Session:** orch-review-1776785000 (2026-04-21), agent `ae9fd1f16149850dc`
- **Verdict:** clean
- **Findings:** none
- **Checked and ruled out:**
  - Hard prohibition on cross-module `require` remains intact in
    architecture.md:137 and chat.org:65.
  - architecture.md and chat.org/chat.el wording agree: both state the
    hard rule is "no cross-module require" and explicitly permit
    `declare-function` as a byte-compiler hint.
  - New wording matches actual practice — mode.el uses 5 sibling + 1
    upstream (`gptel-abort`) declare-functions; nav.el 1, send.el 5,
    menu.el 2, display.el 1. Grep confirms no cross-module `require`
    to sibling chat-mode modules in any chat/*.el.
  - Task file properly moved to tasks/closed/; chat.el is in sync with
    chat.org (tangled correctly).
- **Follow-ups:** none
- **Dependents repointed:** none (no open task is blocked by this one)
