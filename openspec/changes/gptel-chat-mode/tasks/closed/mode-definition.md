---
name: mode-definition
description: Major mode derived from org-mode with keymap, column-0 delimiters, and new-chat command
change: gptel-chat-mode
status: needs-review
relations:
  - blocked-by:scaffold-chat-subsystem
---

## Files to modify
- `config/gptel/chat/mode.org` (new)
- `config/gptel/chat/mode.el` (tangled)
- `config/gptel/chat/test/parser/buffer-format-spec.el` (extend — basic
  activation scenario)

## Implementation steps
1. `(define-derived-mode gptel-chat-mode org-mode "GPTEL-Chat" ...)`. Minimal
   docstring; no overrides of org's indentation, fontification, or folding —
   we inherit them on purpose.
2. On activation:
   - Set `org-adapt-indentation` to `nil` as a buffer-local variable (so
     delimiter lines always begin at column 0 regardless of the user's
     global setting).
   - Install the keymap defined below.
   - Run a mode hook `gptel-chat-mode-hook` so later tasks (display, preset)
     can register their own activation logic.
3. Keymap (`gptel-chat-mode-map`):
   | Key       | Command                            | Source task |
   |-----------|------------------------------------|-------------|
   | `C-c C-c` | `gptel-chat-send`                  | task `send-command` |
   | `C-c n`   | `gptel-chat-next-turn`             | task `nav-commands` |
   | `C-c p`   | `gptel-chat-previous-turn`         | task `nav-commands` |
   | `C-c C-r` | `gptel-chat-regenerate`            | task `nav-commands` |
   | `C-c C-t` | `gptel-chat-toggle-display-layer`  | task `display-layer` |
   | `C-c C-k` | `gptel-abort`                      | upstream (see Decision 10) |
   - Bind to symbols even if the functions don't exist yet — later tasks
     define them. The mode loads first, so forward-declaring the binding is
     safe.
4. Implement `gptel-chat-new` (interactive): create a fresh buffer, activate
   `gptel-chat-mode`, insert initial content `#+begin_user\n\n#+end_user\n`,
   position point on the empty line inside the user block. No model, system
   prompt, preset, or metadata keywords are pre-populated.
5. Register the file-local mode cookie: `.org` files with first-line
   `-*- gptel-chat -*-` should activate chat-mode. (Standard Emacs behaviour
   from `define-derived-mode` + the `-*- mode-name -*-` cookie — no extra
   wiring needed, just confirm by test.)
6. Basic test: a fresh buffer, `M-x gptel-chat-mode`, assert `major-mode` is
   `gptel-chat-mode` and `org-adapt-indentation` is buffer-locally `nil`.

## Design rationale
Decision 6: deriving from `org-mode` gives us `#+begin_src` syntax
highlighting, block folding, and all standard org editing inside user and
assistant blocks *for free*. This is the whole point of using special blocks
— we inherit org's inside-block machinery. Deriving from `text-mode` or
defining a standalone mode would forfeit this.

Decision 7: single-`C-c` prefix (not `C-c C-n`/`C-c C-p`) avoids shadowing
org heading navigation. Because chat-mode buffers MAY use headings as
organization (Decision 12), preserving heading-nav matters. Users developing
muscle memory across `gptel-mode` and `gptel-chat-mode` must remember the
different bindings — documented trade-off.

Decision 14: pinning delimiters to column 0 by disabling
`org-adapt-indentation` locally keeps the parser's `^#\+begin_...` anchor
valid and the on-disk format uniform across users' org settings. The
alternative (permissive parser accepting leading whitespace) complicates the
write path — streaming insertion and assistant-block opening would need to
know what indentation to apply per line.

Decision 9: minimum viable initialization for `gptel-chat-new` — preset
application is a separate concern (task `preset-wiring`) and users who want
presets invoke `gptel-menu` or save the buffer with a property drawer.

## Design pattern
Use `define-derived-mode`. Do not re-implement things org-mode already does.
The mode is a thin layer; heavy logic lives in the parser, stream, and send
modules.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes (the basic
  activation test lives with parser tests since they share the helper).
- Interactive smoke: `M-x gptel-chat-new` opens a buffer with the expected
  initial content and `gptel-chat-mode` active.
- Scenario from spec §"Mode definition and activation":
  - Interactive activation
  - File-local cookie activation (`# -*- gptel-chat -*-`)

## Context
- design.md §Decision 6 (derive from org-mode)
- design.md §Decision 7 (keybindings)
- design.md §Decision 9 (new-chat initialization)
- design.md §Decision 14 (`org-adapt-indentation` nil, column-0 delimiters)
- specs/gptel-chat-mode/spec.md §"Mode definition and activation"
- architecture.md §`gptel-chat-mode` (mode)
