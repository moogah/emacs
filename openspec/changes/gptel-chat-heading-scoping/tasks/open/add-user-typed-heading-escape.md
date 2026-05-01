---
name: add-user-typed-heading-escape
description: post-self-insert-hook escapes column-0 * typed inside chat blocks
change: gptel-chat-heading-scoping
status: ready
relations:
  - blocked-by:add-content-indentation-defcustom
  - blocked-by:add-point-in-block-body-predicate
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
- New test file: `config/gptel/chat/test/mode/user-typed-escape-spec.el`

## Implementation steps

1. In `mode.org`, define a function `gptel-chat--escape-typed-heading` (or similar). Behavior:
   - Run from `post-self-insert-hook`.
   - Test: was the just-typed character `*` AND is point now at column equal to `1` (i.e., the `*` was inserted at column 0) AND does `gptel-chat--point-in-block-body-p` return non-nil at the line's start?
   - If yes: save-excursion, move to beginning-of-line, insert the configured indent prefix (`(make-string gptel-chat-content-indentation ?\s)`).
   - If no: return without modification.
2. Add the function to `post-self-insert-hook` buffer-locally on `gptel-chat-mode` activation (alongside the existing buffer-local setup near the `org-adapt-indentation` setq).
3. Use `add-hook` with the LOCAL flag (`(add-hook 'post-self-insert-hook ... nil t)`).
4. Write Buttercup specs in `user-typed-escape-spec.el`:
   - Type `*` at column 0 inside a `#+begin_user` body → buffer text becomes ` *` at that line.
   - Type `*` at column 5 inside a chat block → no escape applied.
   - Type `*` at column 0 outside any chat block → no escape applied.
   - Type `*` at column 0 on a `#+begin_user` line itself (impossible since the line starts with `#`, but verify the predicate excludes it).
   - Type `*` at column 0 inside a nested `#+begin_tool` block (inside `#+begin_assistant`) → escape applied.
   - Type `a` at column 0 inside a chat block → no escape (only `*` triggers).
   - Sequential `*`+`*` at column 0 → first becomes ` *`, second appends to make ` **` (the second `*` is now at column 2, no further escape).

## Design rationale

`post-self-insert-hook` is the cheapest way to catch the keypress path. Cursor-after semantics work naturally (the user typed `*`, cursor moved past it; the inserted prefix appears before, the cursor stays past). See design.md Decision 2.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` passes.
- Interactive smoke test: open a chat-mode buffer, position point at column 0 inside a `#+begin_user` block, type `*` — see ` *` appear with cursor after.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 2.
- Predicate: `gptel-chat--point-in-block-body-p` (added by task `add-point-in-block-body-predicate`).
- Existing buffer-local setup: search for `org-adapt-indentation` in `config/gptel/chat/mode.el`.
