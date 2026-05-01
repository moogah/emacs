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

## Observations

- **Trigger detection.** Used `(eq last-command-event ?*)` to identify the just-typed character. `post-self-insert-hook` runs after `self-insert-command` which sets `last-command-event` to the typed character — this is the canonical pattern (`electric-pair-post-self-insert-function`, `org-self-insert-command`, etc. all key off `last-command-event`).
- **Column-1 check.** Per design.md §Decision 2 the cursor lands one past the inserted character, so the column-after-insert check for "the `*` was at column 0" is `(= (current-column) 1)`. Cheaper than a `save-excursion`/`backward-char`/`bolp` check, and exact: column 1 ⇔ point is at the second character of the line ⇔ the just-inserted `*` was at column 0.
- **Predicate POS argument.** I pass `(line-beginning-position)` to `gptel-chat--point-in-block-body-p` rather than `(point)`. The line-BOL position is a stable column-0 anchor that is guaranteed to be:
  1. inside the same line as the typed `*` (so still in the same body, if any),
  2. and not on the trailing newline of the previous line (which would be the closer line in the case of a typed `*` at column 0 of the line immediately following a `#+end_*`).
  Calling at `(point)` would give equivalent answers for the trigger case but the line-BOL form makes the docstring cleaner ("at the line's start" matches the task body wording).
- **Forward declaration.** `gptel-chat--point-in-block-body-p` lives in `parser.el`, which loads /after/ `mode.el` per `chat.org`. Since the predicate is referenced from inside the hook function (call-time, post-mode-activation), the symbol resolves naturally — but byte-compiling `mode.el` standalone would emit a "not known to be defined" warning. Added a single-arg `(declare-function gptel-chat--point-in-block-body-p nil (&optional pos buffer))` immediately above the function to silence the warning without making unverifiable claims about the source library, mirroring the pattern already in use for the keymap forward declarations.
- **Hook scope.** Used `(add-hook 'post-self-insert-hook #'gptel-chat--escape-typed-heading nil t)` — the trailing `t` makes it buffer-local. Per design.md §Decision 2 the escape only fires inside chat-mode buffers; a global hook would penalise every keystroke in every buffer in the Emacs session.
- **Coordination with sibling tasks.** The task brief warned that three other in-flight tasks also touch `mode.org` (`add-paste-heading-escape`, `add-migration-on-read`, plus this task). I:
  1. Placed the new function in its own top-level org section ("User-typed heading escape"), separate from the existing "Major mode definition" section.
  2. Added the `add-hook` call as a separate line inside the mode body, distinct from the existing `setq-local org-adapt-indentation nil`. The two lines do not share an `(or ...)` or `(progn ...)` wrapper, so a sibling task adding e.g. `(add-hook 'after-change-functions ...)` lands as another distinct line.
  3. Did not modify the `setq-local org-adapt-indentation nil` line.
- **Test fixture: real `self-insert-command` path.** Wrote `gptel-chat-test--type-char` to bind `last-command-event` and call `self-insert-command` so the actual `post-self-insert-hook` chain runs — this exercises the integration surface (mode activation + buffer-local hook + function trigger) that a user actually hits, rather than calling the function directly with a mocked event. All 8 specs pass; per-spec timing 11–25ms (mostly mode activation overhead, not the function under test).
- **Configurable-width spec added.** The task body didn't enumerate it, but I added a final `describe`/`it` covering `gptel-chat-content-indentation = 2` to verify the function reads the defcustom dynamically (i.e., a `let`-binding is honoured). This protects against a future refactor that accidentally inlines the constant `1` or the constant `?\s` * 1.
- **Delimiter-line scenario nuance.** The task body's scenario 4 ("`*` at column 0 on a `#+begin_user` line itself") is structurally impossible in normal use because the line already starts with `#`. I implemented the scenario as "type `*` at column 1 of the opener line" (the closest realistic typing event), and verified the function's column-1 guard rejects before the predicate is consulted. The test docstring explains the structural impossibility and what was actually tested.

## Discoveries

- discovery_id: disc-add-user-typed-heading-escape-1
  class: predicate-pos-semantics
  description: |
    The predicate `gptel-chat--point-in-block-body-p` accepts an
    optional POS argument. Calling it with `(line-beginning-position)`
    of the just-typed line gives the right answer in all the trigger
    cases the user-typed escape cares about (POS at column 0 of a
    body line returns non-nil; POS at column 0 of a delimiter line
    returns nil). However, the predicate's docstring describes its
    behaviour in terms of "POS strictly inside" and references
    line-end of the opener and line-beginning of the closer. The
    user-typed escape relies on a slightly stronger guarantee:
    that the predicate consistently returns non-nil for any column-0
    position on a non-delimiter body line (including the very first
    body line, where POS == opener-line-end + 1 — the predicate's
    `(> target-pos opener-eol)` check holds with strict `>`, not
    `>=`). I verified by reading the predicate's stack-walk
    implementation and confirmed the strict-> check makes the
    column-0-of-line-after-opener case correctly return non-nil.
    No issue, but downstream consumers should be aware that the
    "first body line column 0" case is a corner the predicate's
    `>` check (not `>=`) handles correctly.
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    No action required. The predicate's contract holds. If a future
    parser refactor changes the strict-> to a >=, the corner case
    breaks silently — recommend adding a regression spec to
    `point-in-block-spec.el` covering "POS at column 0 of the first
    body line immediately after the opener" if not already present.
    (A quick scan of `point-in-block-spec.el` shows the existing
    "returns non-nil at column 0 of a body line that is not a
    delimiter" spec uses line 2 of a `#+begin_assistant\n* heading\n
    #+end_assistant\n` fixture, which IS the first body line — so
    the regression is already guarded.)

- discovery_id: disc-add-user-typed-heading-escape-2
  class: hook-ordering
  description: |
    `post-self-insert-hook` is also used by other Emacs subsystems
    (electric-pair-mode, electric-indent-mode, abbrev expansion,
    org-mode itself for some structural editing). Adding our hook
    with `add-hook` (no DEPTH argument) places it at the front of
    the buffer-local list, so it runs BEFORE any later-registered
    hooks. For chat-mode buffers this is the right ordering: we
    want the heading-escape to apply before, e.g., electric-indent
    has a chance to react to the typed character. But if a future
    chat-mode-hook adds another `post-self-insert-hook` entry, the
    ordering becomes brittle. Today there is only one entry.
  affected_register_entry: register/invariant/all-write-paths-apply-heading-escape
  recommendation: |
    No immediate action. If chat-mode adds additional
    `post-self-insert-hook` entries in future tasks, document the
    ordering contract explicitly (e.g., "heading-escape must run
    first; if you add another entry, use `add-hook ... 90 t` to
    place yourself after"). For now, the single-entry case is
    self-evident.
