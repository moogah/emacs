---
name: migrate-prelude-preamble
description: Move the chat Emacs-prelude and the persistent-agent preamble into static fragment sources (pre-rendered at tangle), rewire the chat composer and agent system-prompt writer to consume them, and delete the two defconsts.
change: gptel-fragment-presets
status: ready
task_class: refactor
on_critical_path: true
cites_register_entries:
  - register/boundary/composer-compose
  - register/invariant/context-default-composition
  - register/invariant/static-prerender-dynamic-compose
  - register/shape/fragment-reference
relations:
  - "blocked-by:composer"
---

## Cited register entries

- **register/boundary/composer-compose** (confirmed) — the prelude (lead) and
  preamble (lead) wire through this composer; do NOT re-implement composition.
  Set the seam defvars (`jf/gptel-fragment-chat-prelude-text`,
  `jf/gptel-fragment-agent-preamble-text`) to your pre-rendered text and let the
  composer place them. Note the inert-`backend`-arg refinement: static refs are
  consumed verbatim, so the composer never re-renders your prelude/preamble.
- **register/invariant/context-default-composition** (confirmed) — chat default is
  `[emacs-prelude(static), role, environment(dynamic)]`, agent default is
  `[agent-preamble(static), role, environment(dynamic)]`. Empty/nil contributions
  are SKIPPED, so the prelude/preamble still lead even with no role. Confirm the
  defaults already produce this once you populate the seam text.
- **register/invariant/static-prerender-dynamic-compose** (confirmed, load-bearing)
  — prelude/preamble are STATIC: pre-render to committed `.txt` at tangle time and
  consume verbatim. They must land in the stable (cacheable) prefix; never re-render
  per send. This is the load-bearing property — if your wiring would re-render a
  static fragment per send, push back in `## Discoveries`.
- **register/shape/fragment-reference** (confirmed) — prelude/preamble are static
  references `(:kind static :text STRING)`, not parsed fragments re-rendered each
  send. The `:fn` key lives on dynamic refs only.

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

## Cycle 3 updates (cycle-1781900938)

> migrate-environment + both presets landed this cycle. This task now REBASES on a
> changed `menu.org` and shares infrastructure with two new in-change tasks. Read
> before starting:

- **`config/gptel/chat/menu.org/el` already changed** by migrate-environment (merged
  5e583755): the `gptel-chat--build-environment-block` defun is **GONE** and the env is
  now a dynamic tail reference wired via `jf/gptel-fragment-environment-fn`. Rebase your
  prelude rewiring onto the new menu.org — do NOT reintroduce or assume the old env
  builder. The composer + `jf/gptel-fragment--default-composition` are the integration
  point for prelude (lead) and env (tail); your job is the prelude/preamble seam
  defvars (`jf/gptel-fragment-chat-prelude-text`, `jf/gptel-fragment-agent-preamble-text`).
- **Source-load wiring is NOT your job — it is `wire-fragment-sources-load`'s.** That new
  task (created this cycle) loads `presets/sources/*.el` in `gptel.org`. Your
  `emacs-prelude.org` / `agent-preamble.org` sources will be picked up by the SAME
  directory-load mechanism — author them under `presets/sources/` and coordinate; do not
  add ad-hoc `gptel.org` load lines (you'd collide with wire-fragment-sources-load).
  Without that task landing, your prelude/preamble seam stays dark in production too — the
  two are coupled; sequence accordingly.
- **TANGLE HAZARD — column-0 `*` headings inside a `#+begin_src` string silently truncate
  the tangled block** (bit BOTH presets this cycle; arch-cycle-1781900938-3; durable fix
  tracked in `.tasks/harden-tangle-against-silent-block-drop.md`). When authoring the
  prelude/preamble fragment `.org`, avoid literal column-0 `*` inside src strings. Two
  proven precedents: `workspace-assistant/preset.org` uses `string-join` of lines;
  `system-explorer/preset.org` authors the fragment as an Elisp `(:kind static :sections
  ((NAME . BODY) ...))` plist literal instead of an embedded Org string. Prefer the plist
  form — it sidesteps the hazard entirely and matches the parser's output shape.
- `:backend` reconcile (preset-config-plist) does NOT apply here — prelude/preamble are
  *fragments*, not presets; they never call `gptel-make-preset`.

## Observations

- **Composer rewiring (chat) — no re-implementation.** `gptel-chat--refresh-system-prompt-from-file`
  now resolves the role, wraps it in `(jf/gptel-fragment-ref-static role)`, and
  hands it to `jf/gptel-fragment--default-composition` + `jf/gptel-fragment-compose`.
  The old `concat prelude "\n\n" (if empty role env (concat role ... env))` is gone.
  The composer skips empty contributions, so the prior explicit empty-role branch
  is now handled by the seam. Verified observable order (prelude → role → env) and
  no-accumulation idempotency still hold via the existing menu specs (566 pass).

- **Agent writer (creation-time) reads the seam, not the per-send composer.**
  `jf/gptel-persistent-agent--write-system-prompt` materializes the sibling
  `system-prompt.<ext>` as `preamble "\n\n" preset-:system` (preamble alone when no
  role). It reads `jf/gptel-fragment-agent-preamble-text` (the composer seam set by
  the `agent-preamble` source). I deliberately did NOT route this through
  `jf/gptel-fragment-compose`: the env tail must NOT be baked into the sibling file
  (env is composed per send when the agent reloads as a chat-mode buffer, which is
  already wired by the chat composer). Routing the writer through the full default
  composition would have embedded a stale env block into the on-disk role file.
  This is a real divergence from "consume via the composer seam defvars" only in
  that the writer uses the seam TEXT directly rather than calling the compose
  function — which is correct for a static-prefix-only artifact. See Discoveries.

- **Static pre-render satisfied without a true tangle-time hook.** `org-babel-tangle-file`
  does not execute code blocks, so I could not literally render the `.txt` "at
  tangle time" from babel. Instead the source `.el` renders the fragment plist once
  at LOAD time (system-explorer precedent) and idempotently mirrors the rendered
  text to the committed `.txt` (write only when content differs, wrapped in
  `ignore-errors` for read-only load locations). The committed `.txt` is the
  diffable artifact and the spec golden; the seam holds the same rendered string.
  Load-time render (not per-send) upholds `register/invariant/static-prerender-
  dynamic-compose`. I generated the committed `.txt` files by loading the modules in
  batch and committed them.

- **Tangle hazard avoided.** Both sources author the fragment as an Elisp
  `(:kind static :sections ((NAME . BODY) ...))` plist literal (system-explorer
  precedent), so no column-0 `*` appears inside a `#+begin_src` string. Tangle of
  all four `.org` exits 0 and check-parens passes.

- **Test sourcing assertions added.** Both specs now assert the prelude/preamble
  text equals the committed `.txt` artifact byte-for-byte AND leads the composition
  (chat) / heads the sibling (agent). Old defconst symbols are no longer referenced
  anywhere in the specs.

- **Source-load wiring (production) is the sibling `wire-fragment-sources-load`
  task's job (Cycle 3).** I did NOT add load lines to `gptel.org`. The specs load
  the two sources by absolute path so they exercise the real seam wiring regardless
  of init-time load order. Until `wire-fragment-sources-load` lands, the
  prelude/preamble seams stay `""` (empty, skipped) in production — the two tasks
  are coupled, as the task body notes.

## Discoveries

- discovery_id: disc-migrate-prelude-preamble-1
  class: scope-question
  description: |
    The task says "rewire ... to consume the fragments via the composer seam
    defvars" and "Do NOT re-implement composition — the composer already places
    prelude (lead) and env (tail)." For the CHAT path this maps cleanly: the
    refresh fn calls `jf/gptel-fragment--default-composition` + `compose`. For the
    AGENT path it does NOT: the agent system-prompt WRITER produces an on-disk
    sibling file at agent-creation time, and that file must contain only the static
    prefix (preamble + role) — never the dynamic env block, which is composed per
    send when the agent reloads as a chat-mode buffer (the chat composer already
    handles env for reloaded agent buffers). So the writer reads the preamble seam
    TEXT directly and concats preamble + role, rather than calling
    `jf/gptel-fragment-compose` (which would append the env tail into the file).
  affected_register_entry: register/invariant/context-default-composition
  recommendation: |
    Treat the agent default composition `[agent-preamble, role, environment]` as the
    PER-SEND composition that applies when the agent buffer is live, NOT as the
    shape of the on-disk creation-time sibling file. The sibling file is the static
    prefix only (`[agent-preamble, role]`); the env tail is added by the same chat
    composer the agent reuses on reload. Consider noting in the register that the
    agent preamble seam has two consumers with different obligations: the per-send
    composer (full default composition) and the creation-time writer (static prefix
    only). No code change needed — flagging the dual-consumer semantics so a future
    reader does not "fix" the writer to call compose and bake in a stale env block.

- discovery_id: disc-migrate-prelude-preamble-2
  class: interface-drift
  description: |
    `register/invariant/static-prerender-dynamic-compose` reads as "pre-rendered to
    committed .txt at TANGLE time." In practice `org-babel-tangle-file` (the repo's
    tangle path, `bin/tangle-org.sh`) does NOT execute source blocks, so a literal
    tangle-time render-and-write of the `.txt` is not achievable with plain babel.
    The realized mechanism is LOAD-time render (once, at module load — not per send)
    plus an idempotent mirror-to-`.txt`. This still satisfies the load-bearing
    property (no per-send re-render; static prefix is cacheable) but the artifact is
    produced/refreshed at load, not at tangle. The same applies to the existing
    `system-explorer` preset, which renders at load and does not write a `.txt` at
    all.
  affected_register_entry: register/invariant/static-prerender-dynamic-compose
  recommendation: |
    Reword the invariant from "pre-rendered at tangle time" to "pre-rendered once
    ahead of send (at module load), committed as a diffable .txt artifact, consumed
    verbatim, never re-rendered per send." The cacheable-prefix / no-per-send-render
    guarantee is the load-bearing part and is fully upheld; "tangle time" is an
    implementation detail that the babel tangler cannot literally provide. If a true
    tangle-time write is desired later, it needs a dedicated build step (emacs
    --batch load + write) rather than a `:tangle` block — out of scope here.
