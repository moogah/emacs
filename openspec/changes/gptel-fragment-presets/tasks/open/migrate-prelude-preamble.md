---
name: migrate-prelude-preamble
description: Move the chat Emacs-prelude and the persistent-agent preamble into static fragment sources (pre-rendered at tangle), rewire the chat composer and agent system-prompt writer to consume them, and delete the two defconsts.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:composer"
---

## Files to modify

- `config/gptel/presets/sources/emacs-prelude.org` (create) → `emacs-prelude.txt`
  (pre-rendered static artifact, committed).
- `config/gptel/presets/sources/agent-preamble.org` (create) → `agent-preamble.txt`.
- `config/gptel/chat/menu.org/el` (modify) — chat composer consumes the prelude
  fragment; remove `gptel-chat--emacs-prelude` defconst.
- `config/gptel/tools/persistent-agent.org/el` (modify) — agent system-prompt
  writer consumes the preamble fragment; remove
  `jf/gptel-persistent-agent--system-preamble` defconst.
- `config/gptel/chat/test/menu/*-spec.el`,
  `config/gptel/tools/test/persistent-agent/*-spec.el` (modify) — assert
  fragment-sourced composition (no regression net required).

## Implementation steps

1. Author `emacs-prelude.org` as a static fragment (runtime-framing prose:
   operating inside GNU Emacs via gptel, prefer Org markup, editor/file tools
   within session scope, user is an Emacs user). Free to improve the text — no
   byte-identical obligation (design.md §Decision 8).
2. Author `agent-preamble.org` as a static fragment carrying the agent operating
   contract: do the task itself / no `PersistentAgent` self-delegation; headless,
   state assumptions; stay in task+scope, request expansion; terminate with one
   self-contained final message.
3. Pre-render both to committed `.txt` artifacts at tangle time (babel/build step
   invoking `jf/gptel-fragment-render … 'claude`).
4. Rewire the chat composer (`gptel-chat--refresh-system-prompt-from-file`) to
   place the prelude fragment as the **leading** element via the composer
   (design §Decision 3), present even with an empty role. Delete the
   `gptel-chat--emacs-prelude` defconst.
5. Rewire `jf/gptel-persistent-agent--write-system-prompt` to compose
   preamble-fragment-first, then the preset's rendered role content (preamble
   alone when no role). Delete the
   `jf/gptel-persistent-agent--system-preamble` defconst.
6. Update affected specs to assert fragment sourcing + leading position; drop any
   assertions tied to the old defconst symbols.
7. Tangle all touched `.org`; run the chat + persistent-agent suites.

## Design rationale

The prelude and preamble are the same role (static leading framing) for two
contexts; making them fragments removes the hard-coded `defconst`s and lets them
be edited/composed (proposal.md; design §Decision 5). No regression net — they
are prototypes (user-confirmed).

## Verification

- `grep -n "emacs-prelude\|system-preamble" config/gptel/chat/menu.el config/gptel/tools/persistent-agent.el` (expect the defconsts gone).
- `test -f config/gptel/presets/sources/emacs-prelude.txt && test -f config/gptel/presets/sources/agent-preamble.txt`
- `./bin/run-tests.sh -d config/gptel/chat/test`
- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent`

## Context pointers

- Specs: `specs/gptel/chat-mode.md` (Emacs prelude sourced from a static
  fragment); `specs/gptel/persistent-agent.md` (Agent system-prompt preamble).
- Current code: `config/gptel/chat/menu.el:508` (prelude),
  `config/gptel/tools/persistent-agent.el:163` (preamble).
