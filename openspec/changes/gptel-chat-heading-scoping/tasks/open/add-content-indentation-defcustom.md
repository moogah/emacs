---
name: add-content-indentation-defcustom
description: Add gptel-chat-content-indentation defcustom controlling escape width
change: gptel-chat-heading-scoping
status: ready
relations:
  - enables:extend-stream-sanitizer-heading-rule
  - enables:add-user-typed-heading-escape
  - enables:add-paste-heading-escape
  - enables:add-migration-on-read
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)

## Implementation steps

1. Add a `defcustom` named `gptel-chat-content-indentation` to `mode.org`, in the same section as the other buffer-local settings (near `org-adapt-indentation`).
2. Type: integer. Default value: `1`. Group: `gptel-chat`. Custom type: `integer`.
3. Docstring should reference the heading-collision escape and link to the chat-mode spec.
4. Re-tangle and confirm the defcustom is loadable.

## Design rationale

A single space is the minimum escape that breaks org's heading regex (`^\*+ `). Defaulting to `1` minimizes visual noise. Users wanting parity with `org-edit-src-content-indentation` (default 2) can set it to `2`. See design.md Decision 8.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `grep -n 'gptel-chat-content-indentation' config/gptel/chat/mode.el` shows the defcustom.
- In a fresh emacs: `M-x customize-variable RET gptel-chat-content-indentation RET` opens the customize buffer with the new variable.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 8.
- `openspec/changes/gptel-chat-heading-scoping/specs/gptel/chat-mode.md` Requirement: Heading-collision escape.
