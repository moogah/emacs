---
name: chat-save-state-hook
description: Add buffer-local before-save-hook in gptel-chat-mode that writes a PROPERTIES drawer via gptel-org-set-properties (+ GPTEL_PARENT_SESSION_ID), excluding GPTEL_BOUNDS.
change: gptel-chat-state-persistence
status: needs-review
relations: []
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — add `gptel-chat--save-state` and register it from `gptel-chat--install-preset-hooks`.
- `config/gptel/chat/test/menu/save-state-spec.el` (new) — Buttercup specs for the save path.

## Implementation steps

1. In `menu.org`, add forward declarations for `gptel-org-set-properties` (from `gptel-org.el`) alongside the existing `declare-function` forms.
2. In `menu.org`, add a new section "Save state hook" with `gptel-chat--save-state`:
   - Guard `(derived-mode-p 'gptel-chat-mode)` as defense-in-depth.
   - Call `(gptel-org-set-properties (point-min) nil)` — `nil` suppresses the interactive echo message.
   - When `(bound-and-true-p jf/gptel--parent-session-id)` is a non-empty string, call `(org-entry-put (point-min) "GPTEL_PARENT_SESSION_ID" jf/gptel--parent-session-id)`.
   - Wrap in `save-excursion` + `org-with-wide-buffer` to match upstream's `gptel-org--save-state` style.
3. Extend `gptel-chat--install-preset-hooks` to also register the save hook buffer-locally:
   `(add-hook 'before-save-hook #'gptel-chat--save-state nil t)`.
4. Tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`.
5. Create `config/gptel/chat/test/menu/save-state-spec.el` (Buttercup) with describe `"gptel-chat save state"`, covering scenarios from the chat-mode spec:
   - Drawer written on save when preset is applied (spy on `gptel-org-set-properties` — verify called with `point-min` and `nil`).
   - Drawer omitted (helper not called) when no preset is applied AND `jf/gptel--parent-session-id` is nil AND no buffer-local config deltas exist. (One integration test exercises the real helper against a temp buffer for end-to-end coverage.)
   - `GPTEL_PARENT_SESSION_ID` written when `jf/gptel--parent-session-id` is a non-empty string.
   - `GPTEL_PARENT_SESSION_ID` NOT written when nil or empty.
   - `gptel-mode` is never called (spy + `expect :not :to-have-been-called`).
   - `GPTEL_BOUNDS` is never present in the saved drawer (integration test — write real buffer, search the buffer text for `:GPTEL_BOUNDS:`, expect no match).
6. Run `./bin/run-tests.sh -d config/gptel/chat/test/menu` — expect all specs to pass.

## Design rationale

`gptel-org-set-properties` is upstream's delta-from-preset writer. It does NOT write `GPTEL_BOUNDS` — that's the wrapper `gptel-org--save-state` that adds the bounds write. Calling the narrower helper directly gives us upstream semantics bit-for-bit compatible with a gptel-mode org buffer, without the one line we don't want (design.md §Decisions 1, 10).

`GPTEL_PARENT_SESSION_ID` is a chat-mode extension (design.md §Decision 3). Upstream ignores unknown drawer keys so cross-compat is preserved. It's written as a targeted `org-entry-put` after the upstream helper returns.

The save hook is registered buffer-locally from inside the mode-hook, mirroring the existing `hack-local-variables-hook` registration pattern. Global registration would force a `derived-mode-p` check on every Emacs save — wasteful for unrelated buffers (design.md §Decision 10).

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` — validates parens.
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` — all specs pass.
- `grep -n "gptel-chat--save-state\|before-save-hook" config/gptel/chat/menu.el` — confirm function defined and registration site present.
- Manually open a chat buffer, apply a preset via `gptel-menu`, save, inspect the file — `:GPTEL_PRESET:` line present; no `:GPTEL_BOUNDS:` line.

## Context

- proposal.md §What Changes (BREAKING Decision 16 reversal, BREAKING Decision 18 clarification)
- specs/gptel/chat-mode.md §"Configuration drawer save on buffer save"
- architecture.md §"`gptel-chat--save-state` (new)"
- design.md §Decisions 1, 3, 10
