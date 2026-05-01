# Chat-mode heading scoping

## Why

`gptel-chat-mode` uses `#+begin_user` / `#+begin_assistant` special blocks to delimit turns. Decision 13 of the original chat-mode design committed to "the body of a `#+begin_user` block supports the full org-mode editing experience." Subsequent investigation (`research.md`) established that this promise is structurally unachievable for one specific feature: an `*` line at column 0 anywhere inside a special block causes `org-element-parse-buffer` to **lose the special-block entirely** — the AST shows `paragraph | headline | paragraph`, with the `#+begin_*` and `#+end_*` lines demoted to paragraph content of the heading's section. The block visually absorbs every line below it (including subsequent turn delimiters) into the heading's outline subtree.

The chat-mode regex parser (`gptel-chat-parse-buffer`) is unaffected because it ignores org structure — so message construction still works. The break is in the **editing surface**: the host buffer's outline, font-lock, fold, imenu, and any tool that consults `org-element` see a broken document. The most important consequence in practice is that **assistant responses containing `*` headings corrupt the document outline of the chat session itself**: the assistant's heading absorbs all subsequent turn delimiters into its outline subtree, producing an unusable editing surface even though the parser still extracts messages correctly.

This is the immediate user-visible bug demonstrated by `~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org`.

## What Changes

This change introduces a **column-0 `*` escape** for content inside chat blocks, with symmetric write-side escape and read-side strip:

- **Streaming sanitizer extension.** `gptel-chat--sanitize-chunk` (currently escapes `^#\+end_(user|assistant|tool)`) gains a second rule: lines matching `^\*+ ` are escaped before insertion into the assistant block. Same per-line holdback machinery, second predicate.
- **Parser-side strip.** A new helper, sibling to `gptel-chat--unescape-end-delimiters`, strips the escape from `*` lines in turn content before messages are sent to the LLM. The LLM never sees the escape character.
- **User-typed escape.** A `post-self-insert-hook` and an `after-change-functions` filter detect `*` typed or pasted at column 0 inside a chat-block body and apply the escape. Delimiter lines themselves (`#+begin_*` / `#+end_*`) are excluded — those must remain at column 0.
- **Migration on read.** When `gptel-chat-mode` activates on a buffer whose chat blocks contain unescaped `*` lines (existing sessions written before this change), the mode applies the escape on read. No destructive on-disk migration; the next save persists the normalized form.
- **Spec hygiene.** The chat-mode spec is updated to reflect that the "full org" promise has one carveout: `*` lines inside chat blocks are escaped automatically and behave as paragraph text in the document outline. The escape is round-trip transparent for message content but visible in the buffer.

**Deferred to a follow-up change.** `gptel-chat-edit-turn` (a `C-c '`-style indirect-editing kernel that pops a fresh org buffer for working with real heading affordances inside a turn) is identified by the research doc as the natural escalation. It is not required to fix the immediate corruption bug. This change ships the escape mechanism only.

**Also deferred.** Visual font-lock to render escaped `*` lines with `org-level-N` faces. Recoverable later; not load-bearing for correctness.

## Impact

**Specs:**
- `openspec/specs/gptel/chat-mode.md` — Decision 13 reframe. New requirement for heading-collision escape (mirroring the existing delimiter-collision escape requirement). Updated scenarios for "User prompt composed with org structural features" and "Delimiter escape round-trip" to include heading-escape symmetry.

**Code:**
- `config/gptel/chat/stream.el` — extend `gptel-chat--sanitize-chunk` with the heading rule.
- `config/gptel/chat/parser.el` — add `gptel-chat--unescape-headings` (inverse of the new escape rule); wire into `gptel-chat--segment-to-messages` alongside the existing un-escape.
- `config/gptel/chat/mode.el` — register `post-self-insert-hook` and `after-change-functions` for the user-typed/paste escape; add migration-on-read pass; add `gptel-chat-content-indentation` defcustom.

**Tests:**
- `config/gptel/chat/test/parser/escape-round-trip-spec.el` — extend existing round-trip specs with heading-escape cases.
- `config/gptel/chat/test/stream/` — new spec for the heading-escape path through the streaming sanitizer.
- `config/gptel/chat/test/mode/` (new directory) — specs for `post-self-insert-hook` behavior, `after-change` paste handling, and migration-on-read.

**On-disk format change.** Existing session files with unescaped `*` lines inside chat blocks are silently normalized on read. The next save writes the escaped form. This is intentional and matches the research-doc recommendation (indent-on-read, reversible).
