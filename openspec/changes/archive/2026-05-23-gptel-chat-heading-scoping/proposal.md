# Chat-mode body indentation

## Why

`gptel-chat-mode` uses `#+begin_user` / `#+begin_assistant` special blocks to delimit turns. Decision 13 of the original chat-mode design committed to "the body of a `#+begin_user` block supports the full org-mode editing experience." Subsequent investigation (`research.md`) established that this promise is structurally unachievable for one specific feature: an `*` line at column 0 anywhere inside a special block causes `org-element-parse-buffer` to **lose the special-block entirely** — the AST shows `paragraph | headline | paragraph`, with the `#+begin_*` and `#+end_*` lines demoted to paragraph content of the heading's section. The block visually absorbs every line below it (including subsequent turn delimiters) into the heading's outline subtree.

The chat-mode regex parser (`gptel-chat-parse-buffer`) is unaffected because it ignores org structure — so message construction still works. The break is in the **editing surface**: the host buffer's outline, font-lock, fold, imenu, and any tool that consults `org-element` see a broken document. The most important consequence in practice is that **assistant responses containing `*` headings corrupt the document outline of the chat session itself**: the assistant's heading absorbs all subsequent turn delimiters into its outline subtree, producing an unusable editing surface even though the parser still extracts messages correctly.

The same failure mode is latent for *any* column-0 org structural token inside a body — a `#+end_assistant` line in assistant prose closes the block early; a column-0 `#+begin_src`, drawer, or keyword is mis-parsed. The corruption is not specific to `*`; it is specific to **structural tokens at column 0 inside body content**.

This is the immediate user-visible bug demonstrated by `~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org`.

## What Changes

This change makes a chat-block body an **indented region**: every line of content inside a `#+begin_user` / `#+begin_assistant` / `#+begin_tool` block is indented by a fixed width (`gptel-chat-content-indentation`, default 2 spaces). Indentation moves all body content off column 0, so org's column-0-anchored structural scanners — the heading regex `^\*+ `, the special-block delimiters `^#\+end_…` — never recognise body text as document structure.

The governing model is **`column 0 = structure (delimiters)` · `indented = content`**. Indentation *is* the escape, applied uniformly to the whole body rather than per-offending-token.

- **Write side — indent.** Streaming insertion indents each assistant line as it arrives. Typing produces indented lines via a buffer-local `indent-line-function` plus electric-indent. Paste / yank shifts the inserted region so its content sits at the body indent. Migration-on-read normalises existing sessions.
- **Read side — dedent.** Before a turn's content is sent to the model, the parser strips the common leading indentation from each message segment — the same `org-do-remove-indentation` measure-and-strip org uses for src blocks. The model never sees the indentation, and the round-trip is robust to later changes of `gptel-chat-content-indentation`.
- **Subsumes the prior escapes.** This redesign retires *both* per-token escapes shipped in earlier waves of this change — the per-`*` heading escape and the `,`-prefix `#+end_*` delimiter escape. An indented body neutralises every column-0 org token at once; the two targeted escapes are no longer needed.
- **Tool blocks.** Nested `#+begin_tool` / `#+end_tool` delimiter lines stay at *real* column 0 — their position is load-bearing for the parser's column-0 anchors. The display layer renders those two lines with a cosmetic `line-prefix` so they visually align with the indented prose. Tool *result* content is indented like all other body content.
- **Spec hygiene.** The chat-mode spec is updated: the "full org" promise carves out *all* column-0 org structure inside a body (not just `*`), which renders as indented plain text. Real heading/structure affordances inside a turn remain the province of the deferred `gptel-chat-edit-turn` indirect-editing kernel.

**Supersedes earlier waves.** This change previously shipped a per-`*` heading escape (14 tasks, now in `tasks/closed/`). Real-world use showed a per-token escape leaves bodies visually ragged — escaped `*` lines carried a 1-space prefix while every other line sat at column 0. The body-indentation model replaces it. The earlier tasks remain as history; the register entries and specs are reframed.

**Deferred to a follow-up change.** `gptel-chat-edit-turn` — a `C-c '`-style indirect-editing kernel that pops a fresh org buffer for working with real heading affordances inside a turn — is identified by `research.md` as the natural escalation. It is not required to fix the corruption bug. This change ships the indentation mechanism only.

**Obsoleted.** The earlier deferral of "visual font-lock to render escaped `*` lines with `org-level-N` faces" is moot: an indented body is plain text, with no heading faces to render.

## Impact

**Specs:**
- `specs/gptel/chat-mode.md` — Decision 13 reframe. The `Heading-collision escape` requirement is replaced by `Chat-block body indentation`. The `Response streaming`, `Message construction`, and `Migration of pre-escape session files` requirements and their scenarios are rewritten from escape/un-escape to indent/dedent.

**Code:**
- `config/gptel/chat/mode.el` — repurpose the `gptel-chat-content-indentation` defcustom (default `1 → 2`); add indent/dedent helpers; add a buffer-local `indent-line-function` and electric-indent wiring; replace the migration pass; repurpose the `after-change-functions` handler to a paste region-shift; drop the `post-self-insert-hook`.
- `config/gptel/chat/parser.el` — replace `gptel-chat--unescape-headings` / `gptel-chat--unescape-end-delimiters` with a per-segment dedent wired into `gptel-chat--turn-to-messages` and `gptel-chat--segment-to-messages`; fix tool-result capture (per-line dedent, not whole-string trim). Column-0 delimiter anchors unchanged.
- `config/gptel/chat/stream.el` — `gptel-chat--sanitize-chunk` becomes a per-line indenter; tool-block emitters indent result content while keeping `#+begin_tool` / `#+end_tool` at column 0.
- `config/gptel/chat/display.el` — install a `line-prefix` overlay on `#+begin_tool` / `#+end_tool` lines so they render aligned with the indented body.

**Tests:**
- `config/gptel/chat/test/mode/` — `user-typed-escape-spec.el` removed (no keystroke hook); `paste-escape-spec.el` → paste region-shift; `migration-spec.el` → migrate-to-indent.
- `config/gptel/chat/test/parser/escape-round-trip-spec.el` → body indent/dedent round-trip.
- `config/gptel/chat/test/stream/streaming-spec.el` — sanitizer-transform specs rewritten to the per-line indenter (line-holdback specs unchanged).
- `config/gptel/chat/test/parser/message-construction-spec.el` — un-escape specs adapted to dedent.
- New specs for `indent-line-function` / electric-indent behaviour and the display `line-prefix`.

**On-disk format change.** Body content is stored indented. Existing session files — both pre-escape (column-0 content) and escape-era (1-space `*` escapes, `,#+end_*` escapes) — are normalised on read; the next save persists the indented form. Non-destructive: the on-disk file is unchanged until the user saves.
