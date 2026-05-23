---
name: replace-chat-save-with-full-snapshot-writer
description: Replace the upstream `gptel-org-set-properties` call in `gptel-chat--save-state` with a chat-mode writer that emits the full configuration snapshot from buffer-local state, excluding `:GPTEL_SYSTEM:` and `:GPTEL_BOUNDS:`.
change: gptel-drawer-as-source-of-truth
status: done
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — replace the save hook body
- `config/gptel/chat/test/menu/save-state-spec.el` (modify) — rewrite the unit specs to assert drawer text directly (no spies on upstream); rewrite the integration spec to assert no delete-on-match

## Implementation steps

1. Open `config/gptel/chat/menu.org` and locate `gptel-chat--save-state` (around `menu.el:337` in tangled form). It currently calls `(gptel-org-set-properties (point-min) nil)`.
2. Define a new helper `gptel-chat--write-config-drawer` (or inline the body — caller's choice). The helper:
   - Resolves `pt = point-min` inside an `org-with-wide-buffer` form.
   - Writes `:GPTEL_PRESET:` via `org-entry-put` when `gptel--preset` is non-nil; otherwise `org-entry-delete`s it.
   - For each of `:GPTEL_MODEL:` (`gptel-model` → `gptel--model-name`), `:GPTEL_BACKEND:` (`gptel-backend` → `gptel-backend-name`), `:GPTEL_TEMPERATURE:` (`gptel-temperature` → `number-to-string`), `:GPTEL_MAX_TOKENS:` (`gptel-max-tokens` → `number-to-string`), `:GPTEL_NUM_MESSAGES_TO_SEND:` (`gptel--num-messages-to-send` → `number-to-string`): write via `org-entry-put` when the source variable is non-nil and a sensible type; `org-entry-delete` when nil.
   - For `:GPTEL_TOOLS:`: build `(mapcar #'gptel-tool-name gptel-tools)`; write via `org-entry-put` with `(string-join tool-names " ")` when the list is non-empty; `org-entry-delete` when empty.
   - NEVER write `:GPTEL_SYSTEM:`. Do not delete it either — leave whatever the user authored. (Comment in code: Decision 2.)
   - NEVER write `:GPTEL_BOUNDS:`. (Existing invariant.)
3. Update `gptel-chat--save-state` to call the new helper instead of `gptel-org-set-properties`. Keep the existing `:GPTEL_PARENT_SESSION_ID:` write (after the helper returns) and the `derived-mode-p` guard. Keep the `(require 'gptel-org)` line — `gptel-org--entry-properties` is still used by the overlay, so we still need the feature loaded; but the save path no longer depends on `gptel-org-set-properties`. (If the require is purely for the save-side helper, we can drop it; verify by grepping for other call sites.)
4. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`.
5. Rewrite `config/gptel/chat/test/menu/save-state-spec.el`:
   - Remove the `spy-on 'gptel-org-set-properties` setup. The new writer doesn't call it.
   - Replace "delegates to gptel-org-set-properties with point-min and nil msg" with "writes full configuration snapshot to drawer" (asserts text via `search-forward` after `gptel-chat--save-state` against a temp buffer with simulated buffer-local state).
   - Replace "Drawer is delta-from-preset" / "Drawer captures user overrides as deltas" with "Drawer carries full snapshot when buffer matches preset" and "Drawer carries user overrides on top of preset snapshot" (text-level assertions).
   - Add a positive assertion for `:GPTEL_SYSTEM:` absence: register a preset with `:system "test sys"`, set `gptel--preset` and `gptel--system-message` buffer-locally, run save, assert `(search-forward ":GPTEL_SYSTEM:" nil t) :to-be nil`.
   - Keep the GPTEL_BOUNDS-never-written invariant; keep the GPTEL_PARENT_SESSION_ID branches; keep the cold-load `(require 'gptel-org)` regression (still load-bearing for the overlay).
   - Drop the "preset-applied save path (real upstream helper)" describe — no longer load-bearing because we don't call the upstream helper. Replace with a real-buffer integration spec that asserts the snapshot is present in saved drawer text.

## Design rationale

Decision 1 picks a dedicated writer over wrapping or rewriting after the upstream helper. The dedicated writer is the smallest surface that gives the WYSIWYG contract; behavior is easier to spec and easier to test (no preset-spec required in the test harness — the writer reads from buffer-local state directly).

Decision 2 places the `:GPTEL_SYSTEM:` exclusion in the writer — write path skips it, read path (overlay) still respects it for back-compat.

The new tests assert text content of the saved drawer (string assertions), which is a stronger contract than the previous spy-based assertions and survives future writer-internal refactors.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat` passes (rewritten save-state-spec, plus existing preset-wiring and other specs).
- Manual: open a fresh session in `./bin/emacs-isolated.sh`, send a message, `C-x C-s`. Inspect `session.org` — drawer carries the preset's full snapshot keys (`:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, …). No `:GPTEL_SYSTEM:`. No `:GPTEL_BOUNDS:`.

## Context

- design.md § Decision 1 — "Replace `gptel-org-set-properties` with a chat-mode full-snapshot writer"
- design.md § Decision 2 — `:GPTEL_SYSTEM:` exclusion in the writer
- specs/gptel/chat-mode.md — Requirement: Configuration drawer save on buffer save
