---
name: make-system-prompt-heading-authoritative
description: With the `* System Prompt` heading in place, make its body the authoritative system prompt. On mode activation the chat-mode restore path reads the heading body and installs it buffer-locally as gptel--system-message, winning over the preset. On save the current system message is written back into the heading body. Old sessions with no heading fall back to the preset and materialize the heading on first save.
change: gptel-drawer-as-source-of-truth
status: done
relations:
  - blocked-by:emit-system-prompt-and-chat-headings-at-creation
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — restore path reads the `* System Prompt` body into `gptel--system-message`; save path writes it back
- `config/gptel/chat/mode.org` (modify, if the restore hook ordering lives there) — ensure the system-prompt read runs after preset application and the drawer overlay
- `openspec/changes/gptel-drawer-as-source-of-truth/specs/gptel/chat-mode.md` (modify) — revise the system-prompt restore/save requirements
- `config/gptel/chat/test/menu/preset-wiring-spec.el` and `save-state-spec.el` (modify) — add restore + save + round-trip scenarios

## Why

design.md §Addendum Finding B (Decision B). This task makes the `* System Prompt` heading load-bearing — the document, not the preset, is the source of truth for the system prompt, completing the WYSIWYG contract for the one key Decision 2 had excluded.

Restore precedence becomes: `* System Prompt` heading body > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`. The writer still never emits `:GPTEL_SYSTEM:` (Decision 2's write-exclusion stands; that property is now legacy-read-only).

## Implementation steps

1. **Restore.** Add a step to the mode-activation path (after `gptel--apply-preset` and `gptel-chat--apply-drawer-overrides`, so it wins) that locates the `* System Prompt` heading, skips its property drawer, and reads the body text. When the body is non-blank, install it as buffer-local `gptel--system-message`. When the heading is absent, or its body is blank, do nothing — the preset's `:system` (or a legacy `:GPTEL_SYSTEM:` drawer entry already applied by the overlay) stands. Treat a blank body as "not authored" so an empty heading does not silently wipe the prompt.
   - Read the body with a simple narrow/regexp scan (e.g. `org-narrow-to-subtree` then skip the drawer) — do **not** introduce `org-element-parse-buffer` (chat-mode parser Decision 1).
2. **Save.** In the `before-save-hook` path (`gptel-chat--save-state`), after the drawer write, serialize the current buffer-local `gptel--system-message` into the `* System Prompt` heading body. When the heading does not exist (old session), create it — file-level drawer, then `* System Prompt` with `:VISIBILITY: folded`, then the body — using the **same heading-construction helper** introduced by `emit-system-prompt-and-chat-headings-at-creation` (single source of truth for the heading shape). The `* Chat` heading is created at the same time if missing so turn blocks remain under it.
3. **Precedence + back-compat.** Confirm a legacy `:GPTEL_SYSTEM:` drawer entry is still honored when there is no `* System Prompt` heading (the existing overlay in `gptel-chat--apply-drawer-overrides` already handles this — the new heading read simply runs after and supersedes it when the heading is present).
4. Re-tangle the touched `.org` files; update `specs/gptel/chat-mode.md`: the "system prompt comes from preset" requirement becomes "system prompt comes from the `* System Prompt` heading body, falling back to the preset"; add the save-writes-heading requirement; keep the "save never writes `:GPTEL_SYSTEM:`" requirement.
5. Add tests:
   - Restore: open a session whose `* System Prompt` body differs from the preset `:system`; assert `gptel--system-message` equals the body.
   - Restore fallback: open an old session with no `* System Prompt` heading; assert `gptel--system-message` equals the preset `:system`.
   - Save: change `gptel--system-message`, save, assert the `* System Prompt` body reflects the new value and no `:GPTEL_SYSTEM:` property was written.
   - Round-trip: create → restore → save → re-restore yields a stable system prompt and a stable file (idempotent save).
6. Re-run `./bin/run-tests.sh -d config/gptel/chat` and `./bin/run-tests.sh -d config/gptel/sessions`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/tangle-org.sh config/gptel/chat/mode.org
./bin/run-tests.sh -d config/gptel/chat
./bin/run-tests.sh -d config/gptel/sessions
grep -n 'System Prompt\|gptel--system-message\|GPTEL_SYSTEM' config/gptel/chat/menu.el
```

Expect: restore reads the heading body; save writes it; precedence heading > drawer > preset; `:GPTEL_SYSTEM:` is never written.

## Context

design.md §Addendum Finding B (Decision B). Current restore/overlay: `gptel-chat--apply-declared-preset` and `gptel-chat--apply-drawer-overrides` in `config/gptel/chat/menu.el`. Current save: `gptel-chat--save-state` / `gptel-chat--write-config-drawer`. Heading-construction helper comes from sibling task `emit-system-prompt-and-chat-headings-at-creation` — reuse it, do not duplicate the heading literals.

This task is `task_class: contract` (per Architect cycle-7 forward-mode drift signal) — it redefines restore/save precedence semantics, not just adds behaviour.

Cited register entries (cycle-7 plan):
- `interfaces.org#register-invariant-system-prompt-heading-authoritative` (`speculated`) — this task **enforces** it; the scaffolded failing test at `openspec/changes/gptel-drawer-as-source-of-truth/scaffolding/invariants/system-prompt-heading-authoritative.test.el` is this task's acceptance criterion. Make it pass, or revise the scaffold and explain in `## Discoveries`.
- `interfaces.org#register-shape-session-document-layout` (`speculated`) — the save-path materialisation for old sessions produces this layout.
- `interfaces.org#register-invariant-drawer-system-key-write-exclusion` (`confirmed`, **load-bearing**) — composes with this task: the writer must still never emit `:GPTEL_SYSTEM:`. Do not violate the write-exclusion while adding heading-body serialisation.
- `interfaces.org#register-invariant-drawer-overlay-wins-over-preset` (`confirmed`, **load-bearing**) — this task changes the restore precedence chain (heading body now sits ahead of the entry's asymmetric `:system` preset-fallback exception). An on-touch Architect audit fires against this entry; integrate will reconcile its `:system`-exception wording. Read it before changing the restore order.

## Observations

- Restore ordering lives entirely in `config/gptel/chat/menu.org`, not
  `mode.org`. The task body allowed for `mode.org` "if the restore hook
  ordering lives there". It does not: `gptel-chat--apply-declared-preset`
  already orchestrates preset-apply → drawer-overlay in one function, so
  the new `gptel-chat--apply-system-prompt-heading` step is appended as
  the final call inside that function. Ordering (heading read runs after
  the overlay) is enforced structurally by call position, in one place.
  `mode.org`/`mode.el` are untouched.
- The save-path materialiser reuses `jf/gptel--session-headings-block`
  (the sibling `emit-...` task's helper) by passing the *existing*
  post-drawer buffer content as the helper's `user-block` argument. This
  is a clean reuse — the helper emits `* System Prompt` + folded drawer +
  body + `* Chat` + whatever-you-pass — but it means the helper's
  `user-block` parameter is now load-bearing for two distinct callers
  (creation: the empty `#+begin_user` template; materialisation: a whole
  pre-existing turn-block region). Not a defect; worth noting the
  parameter's contract widened in use.
- Known-limitation acceptance (per design.md §Addendum / register/shape/
  session-document-layout `known_limitation`): the chat parser is
  heading-indifferent, so a `* System Prompt` body containing a literal
  column-0 `#+begin_user` line would be mis-parsed as a turn block. This
  task does not add a guard — preset system prompts in this repo do not
  contain such a line, and guarding belongs with the parser, not the
  restore/save path. Flagged as accepted, not fixed.
- The `apply-declared-preset` no-preset branch runs the drawer overlay
  and the heading read unconditionally. A buffer with neither a preset
  nor a heading nor a `:GPTEL_SYSTEM:` drawer entry simply keeps the
  global `gptel--system-message` default — the heading read is a no-op.
  This is correct but means the heading-read function fires on every
  chat-mode activation, including bare `gptel-chat-new` scratch buffers
  (where it scans, finds no heading, returns nil — cheap, no error).

## Discoveries

- discovery_id: disc-make-system-prompt-heading-authoritative-1
  class: interface-drift
  description: |
    register/invariant/drawer-overlay-wins-over-preset (status
    reconciled, load_bearing) states the restore-side contract for
    `:system` as a two-tier asymmetric exception: "when absent from the
    drawer, the preset's :system is applied; when present in the drawer
    (back-compat for manual entries), the drawer entry wins." This task
    inserts a THIRD, higher tier ahead of both: the `* System Prompt`
    heading body. After this change the system-prompt restore precedence
    is `* System Prompt` heading body > legacy :GPTEL_SYSTEM: drawer
    entry > preset :system. The entry's `statement` and `why_it_matters`
    text describe a two-tier model that no longer matches the code — the
    drawer overlay is now the back-compat MIDDLE tier for `:system`, not
    the top of the chain. The overlay's general "drawer always wins, no
    delta check" contract for the non-`:system` keys is unchanged and
    still correct; only the `:system` exception clause drifted.
  affected_register_entry: register/invariant/drawer-overlay-wins-over-preset
  recommendation: |
    Reconcile the entry's `:system` exception wording: the drawer
    overlay's `:GPTEL_SYSTEM:` read is the back-compat middle tier, not
    the authoritative source. Add a cross-reference to
    register/invariant/system-prompt-heading-authoritative, which now
    owns the top tier (heading body) and the full three-tier precedence.
    The overlay still installs a legacy drawer `:GPTEL_SYSTEM:` entry
    buffer-locally; `gptel-chat--apply-system-prompt-heading` then runs
    after it and supersedes it when a heading body is authored.

- discovery_id: disc-make-system-prompt-heading-authoritative-2
  class: spec-signal
  description: |
    register/invariant/system-prompt-heading-authoritative (status
    speculated, load_bearing) is now enforced. The scaffolded buttercup
    test at scaffolding/invariants/system-prompt-heading-authoritative.
    test.el was satisfied without revising any speculated assertion: all
    nine `it` bodies were replaced with real assertions exercising the
    live restore path (`gptel-chat-mode` activation) and the live save
    path (`gptel-chat--save-state`) against real registered presets. The
    three-level precedence, the blank-body fallback, the never-write-
    :GPTEL_SYSTEM: composition, and the create→restore→save→re-restore
    idempotency all hold as the entry's `statement` speculates. One
    mechanical scaffold fix: the round-trip `it` used the ERT macro
    `skip-unless`, which is void under buttercup — replaced with
    buttercup's `assume`. That is a framework-API correction, not a
    contract revision.
  affected_register_entry: register/invariant/system-prompt-heading-authoritative
  recommendation: |
    Flip status speculated → confirmed. The entry's `statement` matches
    the implementation verbatim; no wording change is needed beyond the
    status flip and recording the satisfying task/commit.

- discovery_id: disc-make-system-prompt-heading-authoritative-3
  class: spec-signal
  description: |
    register/shape/session-document-layout (status speculated,
    load_bearing) — the save-path materialiser for pre-Addendum sessions
    produces exactly this layout: file-level config drawer at point-min,
    then a singleton `* System Prompt` heading carrying :VISIBILITY:
    folded with the system-prompt body, then a singleton `* Chat`
    heading with the turn blocks beneath it. The materialiser builds the
    heading shape through `jf/gptel--session-headings-block`, the same
    helper the creation renderer uses, so the layout is single-sourced
    across the creation producer and the save-path producer exactly as
    the entry's `producers` list speculates. The entry's `validator`
    (the four structural invariants) passes against a materialised
    buffer — exercised by the save-state-spec.el "materialises the
    heading for a pre-Addendum session" scenario.
  affected_register_entry: register/shape/session-document-layout
  recommendation: |
    No revision needed for this task's surface. The entry remains
    speculated pending the sibling creation-renderer task's assertions;
    this task confirms the save-path producer half of the `producers`
    list. Integrate may note the save-path producer is now verified.
