---
name: verify-and-spec-drawer-overlay-wins
description: Verify `gptel-chat--apply-drawer-overrides` already implements drawer-wins-over-preset, then add new spec scenarios in `preset-wiring-spec.el` covering the new contract — drawer model wins over preset model, and preset `:system` survives when drawer omits `:GPTEL_SYSTEM:`.
change: gptel-drawer-as-source-of-truth
status: needs-review
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify, comment-only) — refresh comments / docstring on `gptel-chat--apply-drawer-overrides` to describe the new "drawer wins for every key" contract (formerly "delta overlay")
- `config/gptel/chat/test/menu/preset-wiring-spec.el` (modify) — add new scenarios

## Implementation steps

1. Open `config/gptel/chat/menu.org` and read `gptel-chat--apply-drawer-overrides` (around `menu.el:173`). Confirm the implementation is already "for each non-nil drawer key, set buffer-local". This is the new contract — no code change needed.
2. Update the function's docstring and surrounding org commentary to remove "non-preset" language. New language: "Overlay every drawer-present configuration value buffer-locally. The drawer wins over the preset for every key it carries (Decision 3 of `openspec/changes/gptel-drawer-as-source-of-truth/design.md`). When the drawer omits a key (e.g. `:GPTEL_SYSTEM:`, which the writer never emits), the preset's value survives intact."
3. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`. Confirm tests still pass (no behavior change).
4. Open `config/gptel/chat/test/menu/preset-wiring-spec.el`. Locate the existing "overlays GPTEL_TOOLS on top of the applied preset" scenario (around line 616). Add adjacent scenarios:
   - **"drawer GPTEL_MODEL wins over preset model"**: register a test preset with `:model 'preset-model-symbol`; spy on `gptel-org--entry-properties` to return a tuple where `model` is `'drawer-model-symbol`; activate chat-mode; assert buffer-local `gptel-model` is `drawer-model-symbol`.
   - **"preset :system survives when drawer omits :GPTEL_SYSTEM:"**: register a preset with `:system "preset-system"`; spy on `gptel-org--entry-properties` to return a tuple where `system` is nil; activate chat-mode; assert buffer-local `gptel--system-message` is `"preset-system"`.
   - **"drawer-authored :GPTEL_SYSTEM: still wins on read (back-compat)"**: register a preset with `:system "preset-system"`; spy on `gptel-org--entry-properties` to return a tuple where `system` is `"drawer-system"`; activate; assert `gptel--system-message` is `"drawer-system"`.
5. Update the existing scenario "overlays GPTEL_TOOLS on top of the applied preset" comment from "non-preset deltas overlay after preset apply" to "drawer wins over preset for every key on apply" — semantic clarification, behavior unchanged.

## Design rationale

Decision 3 in `design.md` removes the "delta" semantic from the overlay: every drawer-present key wins, no comparison against the preset's value. The existing implementation already behaves this way (the overlay does `(when KEY (set (make-local-variable 'sym) KEY))` for each upstream-tuple field), so the code change is comments-only. The test additions lock in the contract so a future "optimize to skip if matches preset" change cannot quietly regress.

The `:GPTEL_SYSTEM:` back-compat scenario is the asymmetric case: writer never emits, but reader honors. This must be tested explicitly so a future overlay refactor doesn't drop the read path.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds with comment-only changes.
- `./bin/run-tests.sh -d config/gptel/chat` passes (existing tests still green; new scenarios pass).
- Visual review of the new docstring confirms the "drawer wins" framing.

## Context

- design.md § Decision 3 — "Drawer wins over preset for every key it carries"
- specs/gptel/chat-mode.md — Requirement: Configuration drawer overlay on restore; Requirement: Preset system integration
