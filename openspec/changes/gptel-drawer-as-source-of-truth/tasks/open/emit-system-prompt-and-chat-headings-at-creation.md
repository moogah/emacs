---
name: emit-system-prompt-and-chat-headings-at-creation
description: A freshly created session.org is a flat file-level drawer plus a bare `#+begin_user` block. Restructure the creation output so the file is the file-level config drawer (unchanged location), then a folded `* System Prompt` heading whose body is seeded from the preset's system prompt, then a `* Chat` heading holding the empty user block. Establishes the canonical document shape; making the heading authoritative is a follow-up task.
change: gptel-drawer-as-source-of-truth
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — `jf/gptel--initial-session-body` (and `jf/gptel--initial-session-content`) gain the heading structure; `jf/gptel--create-session-core` threads the preset's `:system` text through
- `config/gptel/scope-profiles.org` (modify, if the heading emission belongs with the renderer) — confirm whether `--create-for-session` or the body helper owns the new headings; keep the file-level drawer text exactly where it is
- `openspec/changes/gptel-drawer-as-source-of-truth/specs/gptel/sessions-persistence.md` (modify) — revise the session-creation shape requirement to the new layout
- `config/gptel/sessions/test/commands/preset-application-spec.el` and/or `session-org-creation-spec.el` (modify) — assert the new shape

## Why

design.md §Addendum Finding B (Decision B). The system prompt is moving out of preset-only invisibility into the document as a visible `* System Prompt` heading body — an org heading body carries multi-line, special-character text with no escaping, so Decision 2's "unwieldy as a property value" objection does not apply to a heading body. This task lays down the canonical shape at creation time; `make-system-prompt-heading-authoritative` then makes the heading load-bearing on save/restore.

Layout (Option B — file-level drawer, then headings; chosen for lowest code risk: `org-entry-put` / `gptel-org--entry-properties` at `point-min` are unchanged):

```
:PROPERTIES:
...config + scope keys...
:END:

* System Prompt
:PROPERTIES:
:VISIBILITY: folded
:END:
<preset :system text — seeds the body>

* Chat
#+begin_user

#+end_user
```

## Implementation steps

1. The file-level config drawer text is unchanged — keep `jf/gptel-scope-profile--render-drawer-text` / `--create-for-session` output exactly as is, emitted first, at `point-min`.
2. Extend the body template so it emits, after the drawer:
   - `* System Prompt` with a `:PROPERTIES:`/`:END:` drawer containing `:VISIBILITY: folded`, followed by the system-prompt body text.
   - `* Chat`, followed by the empty `#+begin_user` / `#+end_user` block.
   `jf/gptel--initial-session-body` currently takes no arguments — give it a `system-prompt` parameter (string or nil), and have `jf/gptel--create-session-core` pass `(plist-get preset-spec :system)` (the spec it already resolves at line ~388). When the preset has no `:system`, emit the `* System Prompt` heading with an empty body — the heading shape stays canonical and consistent.
3. Decide where the heading emission lives — recommend a single small helper (e.g. `jf/gptel--session-headings-block`) so the exact heading/`:VISIBILITY:` shape has one source of truth, reused by `make-system-prompt-heading-authoritative`'s save path. Avoid duplicating the literal heading strings across modules.
4. Keep `gptel-chat-new` scratch buffers unchanged — they emit a bare `#+begin_user` block with no headings (design.md §Decision 9). The parser's heading-indifference (Decision 12) is what lets both shapes coexist.
5. Re-tangle the touched `.org` files and update `specs/gptel/sessions-persistence.md`: the session-creation requirement now describes the file-level drawer + `* System Prompt` (folded, preset-seeded) + `* Chat` layout.
6. Update creation specs to assert: file-level drawer still at `point-min`; a `* System Prompt` heading with `:VISIBILITY: folded` and the preset's `:system` as body; a `* Chat` heading; the empty user block under `* Chat`. Verify `gptel-chat-parse-buffer` still returns the expected (empty) turn list for the created file — the `* System Prompt` body must not be parsed as a turn.

## Verification

```bash
./bin/tangle-org.sh config/gptel/sessions/commands.org
./bin/tangle-org.sh config/gptel/scope-profiles.org
./bin/run-tests.sh -d config/gptel/sessions
./bin/run-tests.sh -d config/gptel/chat/test/parser
grep -n 'System Prompt\|VISIBILITY\|\* Chat\|initial-session-body' config/gptel/sessions/commands.el
```

Expect: a created `session.org` matches the layout above; parser tests still pass (heading body is commentary, not a turn).

## Context

design.md §Addendum Finding B. Parser heading-indifference: `config/gptel/chat/parser.el` (`gptel-chat-parse-buffer`, design.md §Decision 12). Preset spec is already resolved in `jf/gptel--create-session-core` (`config/gptel/sessions/commands.el` ~line 388). Known limitation recorded in design.md: a system-prompt body with a literal `^#+begin_user` would be mis-parsed — accept or add a narrow guard.

Cited register entries (cycle-7 plan, both `status: speculated`):
- `interfaces.org#register-shape-session-document-layout` — this task is the **producer**: the creation renderer emits the file-level drawer + `* System Prompt` (folded, preset-seeded) + `* Chat` layout the entry describes. The entry carries an embedded `validator` defun; assert against it.
- `interfaces.org#register-invariant-system-prompt-heading-authoritative` — this task lays the `* System Prompt` heading shape the invariant depends on; making the body *authoritative* is the sibling task. No scaffold work here.
