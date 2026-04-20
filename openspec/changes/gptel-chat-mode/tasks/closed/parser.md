---
name: parser
description: State-machine regex walk producing turn list with structural validation
change: gptel-chat-mode
status: done
relations:
  - blocked-by:scaffold-chat-subsystem
---

## Files to modify
- `config/gptel/chat/parser.org` (new)
- `config/gptel/chat/parser.el` (tangled)
- `config/gptel/chat/test/parser/buffer-format-spec.el` (new — Buttercup)
- `config/gptel/chat/test/helpers-spec.el` (new — shared fixtures)

## Implementation steps
1. Implement `gptel-chat--parse-buffer` as a hand-rolled `re-search-forward`
   state machine:
   - Bind `case-fold-search t` (matches org's own case handling for delimiters).
   - Outside any block, look for `^#\+begin_\(user\|assistant\)\b`.
   - Once inside a user block, look *only* for the matching
     `^#\+end_user\b` — anything else (including literal `#+begin_assistant`
     lines) is body.
   - Once inside an assistant block, look for `^#\+end_assistant\b` *and* for
     nested `^#\+begin_tool\b` / `^#\+end_tool\b` pairs.
   - Track block start/end positions as Emacs markers so later consumers can
     navigate or truncate without re-parsing.
2. Output a list of plists per the data contract:
   ```elisp
   (:role user :content STR :start MARKER :end MARKER)
   (:role assistant
    :segments ((:type text :content STR)
               (:type tool-call :name STR :args SEXP :result STR)
               (:type text :content STR))
    :start MARKER :end MARKER)
   ```
3. Implement structural validation. Each rule, with its error shape:
   - `#+begin_*` without a matching `#+end_*` → `(user-error "gptel-chat:
     unclosed <user|assistant|tool> block at line N")`
   - `#+begin_tool` outside any assistant block → same shape, with
     `tool-block-outside-assistant` message
   - `#+begin_user` or `#+begin_assistant` inside another user/assistant block
     → `turn-inside-turn` message
   - Empty blocks (content is whitespace-only) are VALID but the turn emits
     no message (see scenario "Empty blocks are skipped" in spec).
4. Skip content outside turn blocks without interpretation: org headings
   (at any depth), paragraphs, drawers, `#+keyword:` lines, blank lines.
   These do not affect turn ordering.
5. Write Buttercup tests in `parser/buffer-format-spec.el` covering every
   "Buffer format validation" scenario and every "Message construction"
   scenario that is parser-only (not the message-shape conversion — that's
   task `messages`). Use string fixtures via a
   `gptel-chat-test--with-buffer` helper in `helpers-spec.el`.

## Design rationale
Decision 1 chose a state-machine regex walk over `org-element-parse-buffer`
because (a) we own the format and turn delimiters are always paired, always at
column 0, with exact strings we control; (b) allocating a full AST on every
parse is 10–100ms on large logs and couples tests to org internals; (c) the
state machine naturally resists the two real edge cases — literal delimiters
inside block bodies, and turns nested under org headings.

Decision 12 (blocks-only model) means headings are human-only affordances that
do NOT participate in message construction. The state machine is indifferent
to heading depth, so this is a specification choice the parser implements by
*ignoring* headings rather than by walking outline structure.

## Design pattern
- The "inside user" / "inside assistant" state is enforced by which regex the
  loop searches for — not by a depth counter. This eliminates the whole class
  of "user block body contains `#+begin_assistant`" false positives.
- Use markers (not integer positions) for `:start` / `:end` — subsequent
  buffer edits (streaming insertion, user edits) must not invalidate parse
  output that's held across yields.
- Parser output is structured plists, not message-list directly (task
  `messages` does the conversion). This keeps the branching rewrite's
  consumer (task `sessions-branching`) able to reuse parse output without a
  round-trip through messages.

## Verification
- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds (paren-validated).
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- Scenarios covered (from specs/gptel-chat-mode/spec.md §"Buffer format
  validation" and §"Message construction from buffer"):
  - Empty buffer is valid
  - Metadata-only buffer is valid
  - Headings and commentary outside turn blocks are valid
  - Turn blocks nested under org headings are valid (any depth)
  - Unmatched delimiter → user-error
  - Tool block outside assistant block → user-error
  - Turn nested inside another turn → user-error
  - User block body containing `#+begin_assistant` literal is body content
  - User prompt composed with org structural features (headings, src blocks,
    lists) parses as a single user turn

## Context
- design.md §Decision 1 (state-machine regex walk)
- design.md §Decision 12 (blocks-only model for headings)
- design.md §Decision 13 (full org editing inside user blocks)
- architecture.md §`gptel-chat-parser` — data contract
- architecture.md §Test Organization — Buttercup layout
