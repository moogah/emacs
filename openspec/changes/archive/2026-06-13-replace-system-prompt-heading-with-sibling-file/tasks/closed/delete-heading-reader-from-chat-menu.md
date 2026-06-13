---
name: delete-heading-reader-from-chat-menu
description: Delete the chat-mode restore-side heading reader (`gptel-chat--system-prompt-heading-body-region`, `--heading-body`, `--apply-system-prompt-heading`) and unwire it from `gptel-chat--apply-declared-preset`. The `* System Prompt` heading is gone from the canonical layout; the sibling-file restore (task `add-sibling-file-restore-to-chat-mode`) replaces this surface.
change: replace-system-prompt-heading-with-sibling-file
status: done
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — delete the three reader functions and their call site in `gptel-chat--apply-declared-preset`
- `config/gptel/chat/test/menu/preset-wiring-spec.el` (modify) — delete heading-restore `it` blocks; keep the legacy `:GPTEL_SYSTEM:` drawer overlay scenarios (they remain valid as the back-compat middle tier)

## Why

design.md §Goals — the `* System Prompt` heading is removed from the canonical session layout because long markdown system prompts (with code blocks, XML-like tags) interact badly with org-mode fontification, folding, and emphasis parsing. The restore-side reader that materializes the heading body into `gptel--system-message` is dead code under the new shape.

The sibling-file restore (task `add-sibling-file-restore-to-chat-mode`) will wire in the replacement contract. This task removes the heading-based surface first so the new wiring lands against a clean call site.

The legacy `:GPTEL_SYSTEM:` drawer overlay (back-compat for hand-authored entries) is **not** touched — Decision 6 keeps it as the back-compat middle tier in the new precedence (sibling file > legacy drawer > preset).

## Implementation steps

1. Delete the three functions from `config/gptel/chat/menu.org` (and the commentary block that introduces them — search for the heading "System Prompt heading restore"):
   - `gptel-chat--system-prompt-heading-body-region`
   - `gptel-chat--system-prompt-heading-body`
   - `gptel-chat--apply-system-prompt-heading`
2. Remove the final-step call to `(gptel-chat--apply-system-prompt-heading)` from `gptel-chat--apply-declared-preset`. The function's docstring currently documents the three-tier precedence (heading > legacy drawer > preset); update it to the two-tier middle-of-the-chain shape (legacy drawer > preset) for now — the third tier (sibling file) is added in `add-sibling-file-restore-to-chat-mode`.
3. Remove any `declare-function` forms for these symbols in this file or callers.
4. In `config/gptel/chat/test/menu/preset-wiring-spec.el`, delete `it` blocks asserting the heading-body precedence. Keep the legacy `:GPTEL_SYSTEM:` drawer overlay scenarios. Adjust any helper fixtures that materialize a `* System Prompt` heading in test setup.
5. Re-tangle `chat/menu.org`. Run the chat-mode test suite.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n 'system-prompt-heading\|apply-system-prompt-heading' config/gptel/chat/menu.el config/gptel/chat/menu.org
```

Expect: no matches in `menu.el` or `menu.org` for the three deleted symbol names. Test suite passes (any remaining heading-related `it` block should already be deleted).

## Context

architecture.md §Components and §Interfaces — chat/menu.org owns the restore-side heading reader; this task is the first half of its removal. The save-side counterpart is removed in sibling task `delete-heading-writer-from-chat-menu`.

design.md §Decision 7 — wholesale heading deletion in one change, no deprecation interval.

The deleted scaffolding at `openspec/changes/archive/2026-05-23-gptel-drawer-as-source-of-truth/scaffolding/invariants/system-prompt-heading-authoritative.test.el` is already archived and not in the live tree; no cleanup needed there.

## Observations

- Reader-side deletion in `chat/menu.org` was wholesale: the entire `* System Prompt heading restore` section and its babel block went. `gptel-chat--apply-declared-preset` no longer calls anything heading-related; its docstring was rewritten to document the now-two-tier precedence (legacy drawer entry > preset `:system`) and to note that the top tier (sibling file) is wired in by the subsequent `add-sibling-file-restore-to-chat-mode` task.
- The body-region helper describe block in `save-state-spec.el` (left in place by `delete-heading-writer-from-chat-menu` with a heads-up note) was removed in the same diff — the helper has no remaining consumers after the reader is gone.
- `preset-wiring-spec.el` had three nested describe blocks under `system prompt heading restore` totalling ~13 `it` blocks. Replaced with a single sibling block (`system prompt restore precedence (legacy drawer middle tier)`) holding one `it` that asserts the preset `:system` fallback when no legacy drawer entry exists. The legacy-drawer overlay middle tier was already covered by the existing `drawer overrides overlay` describe block — no new coverage needed for that path.
- 483 chat specs pass after the edit (down from ~497-ish before; the ~13 deleted heading-restore scenarios accounted for the drop, minus the 1 fallback spec I kept).

## Discoveries

- discovery_id: disc-delete-heading-reader-1
  class: dead-branch
  description: |
    `gptel-chat--apply-declared-preset` collapses from a four-step
    function (apply-preset, drawer-overrides, drawer-overrides
    again, apply-system-prompt-heading) to a three-step function
    (apply-preset, drawer-overrides, drawer-overrides again). The
    final step (the new sibling-file applier) lands in the next
    task. The middle drawer-overrides re-call is intentional — it
    covers no-preset buffers whose drawer carries only non-preset
    keys; the comment in the code documents the rationale.
  affected_register_entry: register/invariant/system-prompt-heading-authoritative
  recommendation: |
    Mark the invariant fully superseded at integrate (the heading is
    deleted from both producers and both consumers; both `chat/menu.org`
    and `sessions/commands.org` no longer have any heading-aware
    code). The replacement invariant is "sibling file is the system-
    prompt source of truth," to be enumerated in the integrate
    register reconciliation.

- discovery_id: disc-delete-heading-reader-2
  class: interface-drift
  description: |
    The restore-precedence contract is currently in a transient
    two-tier state (legacy drawer > preset) until
    `add-sibling-file-restore-to-chat-mode` lands. The docstring of
    `gptel-chat--apply-declared-preset` documents this explicitly,
    but a maintainer reading the function in isolation between this
    task and the next might think two tiers are the final state.
  affected_register_entry: register/invariant/system-prompt-heading-authoritative
  recommendation: |
    The next task (add-sibling-file-restore-to-chat-mode) MUST
    update both the docstring and the function body to re-introduce
    the third tier. Worth flagging at integrate as a cycle-coupling
    note — if the next task slips, the two-tier docstring becomes
    misleading. Mitigation: this task and the next are in the same
    cycle / batch, so the window is bounded.
