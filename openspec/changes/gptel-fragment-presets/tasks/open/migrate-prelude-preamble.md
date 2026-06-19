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

## Cycle 1 updates (cycle-1781883616)

> fragment-core landed and its interface is now **confirmed** in the register.
> Build against these concrete facts (no longer speculation):

- **Fragment value is a plist** `(:kind SYMBOL :sections ((name . body) ...))`
  — `:kind` ∈ `{static, dynamic}` (default `static`); a section is a cons
  `(name . body)` where `name` is the verbatim trimmed heading string and
  `body` is a whitespace-trimmed string. (`register/shape/fragment` reconciled;
  `register/shape/section` confirmed.)
- **Renderer:** `jf/gptel-fragment-render (fragment backend)` — `claude` only;
  each section → `"<tag>\nbody\n</tag>"`, body **verbatim**, sections joined
  with a **blank line** (`"\n\n"`). Unimplemented backend logs-then-signals
  `jf/gptel-fragment-unimplemented-backend` (`define-error` ⊂ `error`) — catch
  with `condition-case` on `error` if needed.
- **Parser:** `jf/gptel-fragment--parse-source` (Org string **or** file path →
  the fragment plist); batch-loadable (no interactive Org deps).
- **Kind is declared in source** via a `#+fragment_kind: static|dynamic` Org
  keyword (case-insensitive); unrecognized → `static`.
- **Section-name → tag:** `jf/gptel-fragment--section-name-to-tag` downcases,
  trims, and collapses whitespace runs to a single `_` (`Output Format` →
  `output_format`). Multi-word headings are safe.
- **Reviewer note:** a fragment with no sections renders to `""`. Decide
  explicitly whether the composer skips/rejects empty fragments rather than
  silently emitting an empty block.

## Cycle 2 updates (cycle-1781885402)

> composer + context-default composition landed and are **confirmed**. Build against
> these concrete facts:

- **Context defaults (confirmed):** `jf/gptel-fragment--default-composition (context
  &optional role-ref)` yields chat → `[emacs-prelude(static), role, environment(dynamic)]`
  and agent → `[agent-preamble(static), role, environment(dynamic)]`. Empty/nil
  contributions are **skipped** (decided + documented), so an absent role collapses
  cleanly while prelude/preamble still lead.
- **Wire prelude/preamble via the seam defvars the composer already exposes** — do NOT
  re-implement composition:
  - `jf/gptel-fragment-chat-prelude-text` (currently `""`) — set to the pre-rendered
    chat prelude fragment text (static, rendered at tangle time).
  - `jf/gptel-fragment-agent-preamble-text` (currently `""`) — set to the pre-rendered
    agent preamble text.
- prelude/preamble are **static** references (`(:kind static :text STRING)`,
  `register/shape/fragment-reference`) — pre-rendered, consumed verbatim, NOT re-rendered
  per send (`register/invariant/static-prerender-dynamic-compose`, load-bearing, confirmed).
- `register/shape/fragment` was amended: its speculative `:fn` key was removed — dynamic
  behavior lives on the *reference*, not the parsed fragment.
