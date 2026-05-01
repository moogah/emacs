---
name: add-paste-heading-escape
description: after-change-functions escapes column-0 * lines in pasted/yanked content
change: gptel-chat-heading-scoping
status: ready
relations:
  - blocked-by:add-content-indentation-defcustom
  - blocked-by:add-point-in-block-body-predicate
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
- New test file: `config/gptel/chat/test/mode/paste-escape-spec.el`

## Implementation steps

1. In `mode.org`, define a function `gptel-chat--escape-inserted-headings` (or similar). Behavior:
   - Run from `after-change-functions` with args `(BEG END LENGTH)`.
   - When `LENGTH` is 0 (insertion, not deletion or replacement) AND the inserted range `[BEG, END)` overlaps with at least one chat-block body (use `gptel-chat--point-in-block-body-p` against BEG and possibly intermediate positions), proceed.
   - Bind `inhibit-modification-hooks` to non-nil to prevent the hook from re-firing on its own rewrites.
   - Walk the inserted range line by line. For each line whose start is at column 0 inside a chat-block body and which matches `^\*+ `, insert the configured indent prefix at the line's start.
   - Adjust END as needed to account for inserted prefix characters (so the after-change semantics remain correct for any downstream hooks).
2. Clip the operation to chat-block bodies only. If the inserted range crosses a `#+end_*` delimiter line, only escape lines that fall inside body extent.
3. Add the function to `after-change-functions` buffer-locally on `gptel-chat-mode` activation. Use `add-hook` with the LOCAL flag.
4. Idempotence: re-running on already-escaped content is a no-op (the escape only applies to lines starting with `*` at column 0; an already-escaped line starts with whitespace).
5. Write Buttercup specs in `paste-escape-spec.el`:
   - Yank `* H1\n- list\n** H2` at any position inside a `#+begin_user` body → resulting text has ` * H1`, `- list`, ` ** H2`.
   - Yank text containing only non-heading lines → no escape applied.
   - Yank text where the first line is `* H1` and point is mid-line (insertion column > 0) → only lines starting at column 0 of the inserted text get the escape; the first line (which gets concatenated to existing text) is not escaped.
   - Yank into a region that crosses `#+end_user` → only the portion inside the body is escaped.
   - Yank outside any chat block → no escape applied.
   - Re-yank already-escaped content → idempotent (no double escape).
   - Programmatic insert (e.g., `(insert "* Test\n")`) inside a chat-block body → escape applied.

## Design rationale

`after-change-functions` catches every insertion path: paste, yank, drag-and-drop, mouse paste, programmatic insert. Idempotence is what makes it safe to run on its own rewrites under `inhibit-modification-hooks`. See design.md Decision 3.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` passes.
- Interactive smoke test: copy `* Test\n- list` to the kill ring, position point inside a chat block, `C-y` — verify the heading line gets the escape.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 3.
- Predicate: `gptel-chat--point-in-block-body-p` (added by task `add-point-in-block-body-predicate`).
