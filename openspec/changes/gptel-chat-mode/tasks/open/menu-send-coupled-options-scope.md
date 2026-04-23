---
name: menu-send-coupled-options-scope
description: Resolve gptel-chat-menu's Send-coupled option groups (Prompt from / Response to / Dry Run) that are displayed but silently dropped by the rebound Send suffix
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:menu-integration
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (modify Decision 15 —
  clarify which upstream menu groups belong in the chat-mode mirror)
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (add/clarify scenarios under "gptel-menu integration with rebound
  Send")
- `config/gptel/chat/menu.org` (remove or replace the Prompt-from,
  Response-to, and Dry-Run groups — whichever direction the design
  update commits to)
- `config/gptel/chat/menu.el` (tangled)
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el` (add one
  behavioral spec: stand up a chat-mode buffer, invoke a
  configuration infix, assert a buffer-local variable changed —
  currently only transitively verified)

## Implementation steps

1. **Resolve the spec gap.** Decision 15 commits to "reuses
   `gptel-menu`'s configuration layout and replaces the Send
   suffix," and lists configuration suffixes explicitly
   (preset, model, backend, system message, tools, context,
   temperature). The current implementation (`menu.el:412-496`)
   mirrors *every* upstream group, including:
   - `" <Prompt from"` group (Minibuffer / Kill-ring / Respond in
     place) — toggles that `gptel--suffix-send` reads from
     `transient-args` but `gptel-chat-send` ignores.
   - `" >Response to"` group (Echo area / Other buffer / gptel
     session / Kill-ring) — same pattern; Send-coupled redirection.
   - `"Dry Run"` group (Inspect query Lisp / JSON) — suffixes that
     call `(gptel--suffix-send (cons "I" (transient-args ...)))`
     directly. Invoked from a chat-mode buffer this leaks upstream's
     gptel-mode prompt-extraction semantics into the preview
     (chat-mode has no `gptel` text-property bounds, so the
     extracted prompt is effectively "everything from point-min").
   - `"Logging"` group — unaffected; logging is a global variable
     mutation, works identically.

   Pick a policy (add to Decision 15 and the spec):

   - **A. Omit Send-coupled groups from `gptel-chat-menu`.** Remove
     the Prompt-from, Response-to, and Dry-Run groups from our
     prefix. Keep only groups that are configuration
     (preset/model/backend/system/tools/context/temperature/scope)
     or global (logging). Smallest diff, clearest contract:
     "chat-mode menu is configuration + Send; redirection and dry
     run are upstream-only."

   - **B. Replace the Dry-Run group with chat-mode-aware
     inspectors** and omit Prompt-from / Response-to. Useful if
     "Inspect query" is a feature worth keeping for chat-mode
     (which runs its own `gptel-request` call path with its own
     prompt construction). Requires a new inspector that understands
     chat-mode's block-based prompt format.

   - **C. Honor a narrow subset of transient-args in
     `gptel-chat--suffix-send`.** Keep the groups; parse a whitelist
     of args (`"m"`, `"y"`, `"i"` from Prompt-from) and pass them
     through to `gptel-chat-send`. Largest surface area; every new
     upstream arg is one we'd need to decide about. Probably wrong.

   Recommended: **A** unless the Dry-Run inspector is valuable
   enough to justify (B).

2. Apply the chosen policy in `menu.org`:
   - Option A: remove the three offending vector blocks from the
     `gptel-chat-menu` prefix definition.
   - Option B: replace the Dry-Run suffixes with chat-mode-aware
     variants (new suffix functions that build a query from the
     current chat-mode buffer and feed it to `gptel--inspect-query`).

3. Update `design.md §Decision 15` — add a subsection explicitly
   listing which upstream groups are included in `gptel-chat-menu`
   and why the Send-coupled groups are out of scope for the
   chat-mode mirror.

4. Update `specs/gptel-chat-mode/spec.md` under "gptel-menu
   integration with rebound Send" — add a scenario that pins the
   new contract. E.g. for Option A:

   > **WHEN** the user invokes `gptel-chat-menu` in a chat-mode
   > buffer **THEN** only configuration suffixes and Send are
   > available; redirection options (Prompt-from, Response-to) and
   > Dry-Run inspectors are not present in the chat-mode prefix
   > layout.

5. **Add the missing behavioral test** (Review Finding #2). The
   current `menu-send-rebind-spec.el` has 15 specs, all of which
   are structural introspection (symbol membership in the prefix
   layout). None invoke the menu from a chat-mode buffer and
   verify that a configuration suffix actually mutates a
   buffer-local variable. The spec scenario

   > **WHEN** point is in a `gptel-chat-mode` buffer **AND** the
   > user invokes `M-x gptel-menu` **THEN** configuration actions
   > (preset pick, model change, tool selection) mutate
   > buffer-local variables as upstream does

   is verified only transitively today. Add one spec:

   - In `with-temp-buffer` under `(gptel-chat-mode)`, directly set
     `gptel-model` to a known baseline, then simulate a
     `gptel--infix-provider` or equivalent infix call (via the
     infix's suffix function or a direct mutation path), and
     assert `gptel-model` changed buffer-locally. Alternative: set
     `gptel--preset` via `gptel--apply-preset` (what the preset
     infix does under the hood) and assert a tools/model change
     stuck in the buffer.

   Drop one or two of the redundant "references the upstream X
   infix" layout-introspection specs to keep the suite size
   stable.

## Design rationale

Two findings from the menu-integration review (2026-04-23) group
here because both concern the scope and coverage of
`gptel-chat-menu` — what lives in the mirror and what's actually
exercised by tests. Addressing Finding 1 requires a spec update
plus a menu.org edit; Finding 2 is a test addition that lives in
the same spec file but is logically independent. Grouping keeps
the design/spec/test change as one coherent diff rather than two
artifacts read the same context twice.

Finding 1's severity is user-visible: a chat-mode user who toggles
"Respond in place" expects the response to be inserted in place
(as it is in `gptel-mode`), but `gptel-chat-send` silently ignores
the toggle. Dry-Run is more obscure but worse — it silently
produces a wrong query preview because it calls
`gptel--suffix-send` with the current chat-mode buffer, and
`gptel--suffix-send`'s prompt extraction depends on
`gptel`-text-property bounds that chat-mode doesn't emit.

Finding 2 is hygiene: the menu rebind spec suite has very broad
structural coverage and zero behavioral coverage of Scenario 1.
That's defensible (transitive argument: if the infix symbols are
shared, the behavior is shared), but the spec scenario says
"configuration actions … mutate buffer-local variables" and not
"the infix symbols match upstream" — the tests should match the
scenario.

## Design pattern

When mirroring an upstream UI with a narrow override, explicitly
list what's in the mirror and what's out of scope. Silently
reusing layouts that depend on the overridden behavior is a common
Send-button trap — the button is visible, the user presses it, and
their configuration is silently discarded. "Configuration is free,
Send is rebound" (Decision 15) is the right intent; the spec
should extend that principle to Send-coupled options too.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes,
  including the new behavioral spec.
- For Option A: manual check — invoke `M-x gptel-chat-menu` in a
  chat-mode buffer; confirm only configuration groups + Send are
  visible (no Prompt-from / Response-to / Dry-Run rows).
- For Option B: manual check — invoke Dry-Run from
  `gptel-chat-menu`; confirm the preview shows chat-mode's
  block-based prompt (not upstream's gptel-prefix-extracted
  prompt).

## Context

- Review of menu-integration (2026-04-23, orch-review session).
  Findings #1 (Send-coupled menu options) and #2 (missing
  behavioral test for Scenario 1).
- `config/gptel/chat/menu.el:348-496` — `gptel-chat-menu` prefix
  definition; the three offending groups live at roughly
  :417-424 (Prompt from), :425-445 (Response to), and :466-486
  (Dry Run).
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el:182-212`
  — "shared configuration infixes" describe block, target for
  replacing a layout-only spec with a behavioral one.
- design.md §Decision 15 — "gptel-menu integration; configuration
  is free, Send is rebound."
- specs/gptel-chat-mode/spec.md §"Requirement: gptel-menu
  integration with rebound Send".
- runtime/straight/repos/gptel/gptel-transient.el `gptel-menu`
  prefix — reference for upstream's layout.
