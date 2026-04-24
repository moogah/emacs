---
name: chat-real-upstream-integration-tests
description: Add real-upstream integration tests covering the overlay restore path and the preset-applied save path so future upstream signature changes surface in our tests, not just in mocks.
change: gptel-chat-state-persistence
status: needs-review
relations:
  - "discovered-from:chat-drawer-overrides-overlay"
  - "discovered-from:chat-save-state-hook"
---

## Files to modify

- `config/gptel/chat/test/menu/preset-wiring-spec.el` (modify) — add a real-upstream integration spec under the existing `describe "drawer overrides overlay"` block.
- `config/gptel/chat/test/menu/save-state-spec.el` (modify) — add a real-upstream integration spec that exercises the preset-applied save path end-to-end.

## Implementation steps

1. In `preset-wiring-spec.el`, under `describe "drawer overrides overlay"`, add an `describe "integration with real gptel-org helper"` block gated by `(unless (fboundp 'gptel-org--entry-properties) (signal 'buttercup-pending nil))`. Add one spec that:
   - Creates a temp org-mode buffer with a concrete PROPERTIES drawer at `point-min`, e.g.
     ```
     :PROPERTIES:
     :GPTEL_PRESET: coding
     :GPTEL_TEMPERATURE: 0.5
     :END:
     #+begin_user

     #+end_user
     ```
   - Calls `(gptel-chat--apply-drawer-overrides)` directly — no spies on the upstream helper.
   - Asserts `gptel-temperature` is buffer-local (`local-variable-p`) and equals `0.5`.
   - `GPTEL_TEMPERATURE` / `GPTEL_SYSTEM` are preferred over `GPTEL_BACKEND`/`GPTEL_MODEL`/`GPTEL_TOOLS`: the first group round-trips without needing `gptel--known-backends` or preset registration, keeping the test self-contained.
2. In `save-state-spec.el`, add one integration spec (alongside the existing real-helper section) that:
   - Registers a real preset via `gptel-make-preset` inside `before-each` (wrap in `unwind-protect` or use `after-each` to unregister — follow the pattern in `preset-application-spec.el`).
   - Sets `gptel--preset` on the buffer.
   - Calls `(gptel-chat--save-state)` without spying `gptel-org-set-properties`.
   - Searches the buffer text and asserts `:GPTEL_PRESET: <name>` is present.
   - Also asserts `:GPTEL_BOUNDS:` is NOT present (re-asserting the invariant for the preset-applied path).
3. Run `./bin/run-tests.sh -d config/gptel/chat/test/menu` — new specs pass; existing specs unchanged.

## Design rationale

Architecture §"Test Patterns / End-to-end (real upstream)" requires at least one integration test per path that exercises the real `gptel-org-set-properties` / `gptel-org--entry-properties` helpers against a temp buffer, so a future upstream-signature change fails a test we actually run, not just a mock. The current specs satisfy this for the save path's no-preset branch but not for:
- The overlay restore path (every spec spies `gptel-org--entry-properties`), leaving the `pcase-let` destructuring order/shape covered only by mock tuples.
- The save path's preset-applied branch (the spec scenario "Drawer written on save when preset is applied"), which is covered only via the delegation contract.

Both are the same kind of gap — the architecture contract is explicit about wanting both boundaries exercised.

## Verification

- `./bin/tangle-org.sh` not required (edits are Buttercup specs, not `.org` sources).
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` — all specs pass, including the two new integration specs.
- Intentionally renaming a field in upstream's `gptel-org--entry-properties` return tuple would now surface as a failing test (manual verification — optional).

## Context

- Discovered during review of `chat-drawer-overrides-overlay` (the overlay gap) and `chat-save-state-hook` (the preset-apply gap).
- `openspec/changes/gptel-chat-state-persistence/architecture.md` §"Test Patterns" — contract that drove the findings.
- `openspec/changes/gptel-chat-state-persistence/specs/gptel/chat-mode.md` — scenario mapping.
- Existing integration section in `save-state-spec.el` (lines ~166-237) is the pattern to mirror for the overlay side.
