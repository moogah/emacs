---
name: add-paste-heading-escape
description: after-change-functions escapes column-0 * lines in pasted/yanked content
change: gptel-chat-heading-scoping
status: done
merge_commit: 8da8891c6a0345fbef259f1799d8c5aa6e844745
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

## Observations

- New function `gptel-chat--escape-inserted-headings` lives in its own
  top-level org section `* Paste / yank heading escape` in `mode.org`,
  immediately above `* Provide`. The `add-hook 'after-change-functions`
  call sits as a separate line in the `define-derived-mode` body,
  directly after the existing `setq-local org-adapt-indentation nil`.
  Both placements respect the wave-2 coordination note: the new
  section is well-separated from where the other concurrent tasks
  (`add-user-typed-heading-escape`, `add-migration-on-read`) will
  place their hooks, minimizing textual proximity for merge.
- The implementation uses a marker for the upper bound of the
  per-line scan (`(copy-marker end t)` with insertion-advance) so
  positional drift caused by the function's own prefix inserts does
  not invalidate the loop's termination condition. The standard
  `inhibit-modification-hooks` guard prevents re-entry.
- Per-line clipping to body extent uses
  `gptel-chat--point-in-block-body-p` directly: the predicate
  already returns nil for delimiter lines and for positions outside
  any block, so iterating per-line drops the post-closer portion of
  a straddling paste without separate range arithmetic. This
  also handles scenario 5 (paste outside any block) for free.
- Mid-line yank semantics (scenario 3) emerge from a single
  guard: the scan starts at the first BOL `>=` BEG. When BEG is
  mid-line the first line's BOL is `<` BEG and is correctly
  skipped, so the segment that gets concatenated onto the existing
  line is never mistaken for a fresh column-0 line.
- A `declare-function` form for `gptel-chat--point-in-block-body-p`
  is added in the forward-declaration block of `mode.org` because
  `mode.el` loads before `parser.el` (per `chat.org`); the symbol
  is undefined at byte-compile time of `mode.el` but always
  defined at hook-fire time, since the hook is added in mode
  activation (runtime, after the chat loader has finished).
- A defensive 8th spec was added beyond the 7 required scenarios
  in the task body: it confirms that a deletion (LENGTH > 0) is
  properly skipped by the `(zerop length)` gate even when the
  buffer's surviving content does contain a column-0 `*` line.
  This pins the LENGTH gate against accidental future regressions.

## Discoveries

- `gptel-chat--point-in-block-body-p` correctly returns nil for a
  `#+end_user` delimiter line *introduced by the paste itself* —
  the predicate scans backward through delimiter lines without
  tracking which lines were "original" vs. "newly inserted".
  This is exactly the right behavior for our boundary-clipping
  story but is worth flagging: a paste whose inserted text
  contains a `#+end_user` line effectively splits the original
  block from the predicate's perspective, and any line after that
  pasted closer is correctly considered outside the body.
  Scenario 4 of the spec pins this behaviour explicitly.
- The full chat-test suite (388 specs) still passes, including
  `parser/`, `display/`, `menu/`, `nav/`, `send/`, `stream/`.
  The new hook is buffer-local via `add-hook ... nil t`, so it
  cannot leak into the temp buffers used by other test files
  (which do not call `gptel-chat-mode`).
