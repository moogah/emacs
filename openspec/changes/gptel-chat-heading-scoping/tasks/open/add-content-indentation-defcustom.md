---
name: add-content-indentation-defcustom
description: Add gptel-chat-content-indentation defcustom controlling escape width
change: gptel-chat-heading-scoping
status: done
merge_commit: 6b3632c990a2b9276243fbd67c292df6a542e038
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

## Observations

- The `gptel-chat` defgroup is defined in `config/gptel/chat/display.org` (line 87), which loads *after* `mode.el` per the load order in `chat.org`. Emacs `defcustom :group` accepts forward references — the group symbol is stored as metadata and resolved at customize-time, not load-time — so this is harmless. I noted the forward reference explicitly in the surrounding org prose. A future cleanup could either (a) hoist the defgroup into `mode.org` (the module that loads first), or (b) split it into its own tiny `customize.org` module loaded ahead of everything else, but neither is required by this task.
- The display.org commentary above the defgroup says "Reuse the parent =gptel-chat= group when it exists; otherwise create a minimal local group" — the actual code unconditionally calls `(defgroup gptel-chat ...)` with no `featurep`/`get` guard. The prose-vs-code mismatch is pre-existing and out of scope for this task.
- The task body specifies "in the same section as the other buffer-local settings (near `org-adapt-indentation`)". The only existing buffer-local setting is the `setq-local org-adapt-indentation nil` *inside* the `define-derived-mode` body — there is no dedicated buffer-local-settings section. I created a new top-level "Buffer content indentation" section immediately before "Major mode definition" so the defcustom reads as a peer of the keymap and the major mode definition itself. This satisfies the spirit of the instruction (logical proximity) without forcing the customization variable into the mode-activation body where buffer-local `setq-local` lives.
- `gptel-chat-content-indentation` is a *global* user option (not a buffer-local variable). Per design.md §Decision 8 it acts as the indent-width knob for the heading-collision escape pipeline; consumers (stream sanitizer, user-typing escape, paste escape, migration-on-read) read its current value at call time. Nothing about its semantics requires `make-variable-buffer-local`, so I did not declare it buffer-local.

## Discoveries

(none)
