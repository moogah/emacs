---
name: chat-drawer-overrides-overlay
description: Add gptel-chat--apply-drawer-overrides overlay function and call it from gptel-chat--apply-declared-preset so drawer deltas and GPTEL_PARENT_SESSION_ID are restored on mode activation.
change: gptel-chat-state-persistence
status: ready
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — add `gptel-chat--apply-drawer-overrides`; call from `gptel-chat--apply-declared-preset` after preset apply (and in the no-preset branch).
- `config/gptel/chat/test/menu/preset-wiring-spec.el` (modify) — add overlay scenarios.

## Implementation steps

1. In `menu.org`, add forward declarations for `gptel-org--entry-properties` and a `defvar` declaration for `jf/gptel--parent-session-id` (the session-local is defined in `sessions/commands.org` — declaring lets menu.el compile without requiring that module).
2. In `menu.org`, add a new section "Drawer overrides overlay" with `gptel-chat--apply-drawer-overrides`:
   - Destructure `(gptel-org--entry-properties (point-min))` via `pcase-let` into `(_preset system backend model temperature tokens num tools)` — the preset is intentionally discarded here; it was already applied by the caller.
   - For each non-nil field, install as buffer-local: `gptel--system-message`, `gptel-backend`, `gptel-model`, `gptel-temperature`, `gptel-max-tokens`, `gptel--num-messages-to-send`, `gptel-tools`.
   - Additionally read `(org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID" 'selective)`. When non-nil and non-empty, `(set (make-local-variable 'jf/gptel--parent-session-id) value)`.
   - No-op for each absent field.
3. In `menu.org`, extend `gptel-chat--apply-declared-preset`:
   - After the existing preset-apply branch (inside the `when-let`), call `(gptel-chat--apply-drawer-overrides)`.
   - Also call `(gptel-chat--apply-drawer-overrides)` at the end of the function — covers the no-preset branch so drawers with only non-preset keys still get honored.
4. Tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`.
5. In `preset-wiring-spec.el`, add `describe "drawer overrides overlay"` with specs for:
   - Overlays `GPTEL_TOOLS` on top of the applied preset (use a real preset registered in `gptel-get-preset` via a `before-each` fake; assert `gptel-tools` buffer-local value after activation).
   - Overlays `GPTEL_PARENT_SESSION_ID` into `jf/gptel--parent-session-id`.
   - No-op when drawer contains only `GPTEL_PRESET` (no overlay writes for absent keys — spy on the setter and assert call count).
   - No-op when buffer has no drawer (neither preset apply nor overlay runs).
   - All overlaid values are `local-variable-p` — no globals touched.
6. Run `./bin/run-tests.sh -d config/gptel/chat/test/menu`.

## Design rationale

Upstream `gptel-org--entry-properties` already returns a structured tuple of the configuration keys. Reusing it means the drawer shape stays bit-for-bit compatible with what upstream's own `gptel-org--restore-state` reads. We deliberately discard the preset field from the tuple at this layer because the caller (`gptel-chat--apply-declared-preset`) has already applied it via `gptel--apply-preset` — re-applying would undo the preset's composite logic (design.md §Decision 2, 5).

`GPTEL_PARENT_SESSION_ID` is our extension, not in upstream's tuple. It reads via a targeted `org-entry-get` call (design.md §Decision 3).

Calling the overlay from *both* the preset-apply branch and the no-preset branch covers every buffer shape: drawers with preset + deltas, drawers with only deltas, drawers with only `GPTEL_PARENT_SESSION_ID`.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org`
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` — all specs pass including the new ones.
- Manually: open a file containing `:PROPERTIES:\n:GPTEL_PRESET: coding\n:GPTEL_TOOLS: foo bar\n:END:\n#+begin_user\n\n#+end_user\n` in chat-mode, inspect `gptel-tools` buffer-local.

## Context

- proposal.md §What Changes (BREAKING restore path)
- specs/gptel/chat-mode.md §"Configuration drawer overlay on restore", §"Preset system integration" (MODIFIED)
- architecture.md §"`gptel-chat--apply-drawer-overrides` (new)"
- design.md §Decisions 2, 3, 5
