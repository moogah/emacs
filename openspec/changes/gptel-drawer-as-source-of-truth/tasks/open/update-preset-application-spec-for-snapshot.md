---
name: update-preset-application-spec-for-snapshot
description: Refresh `preset-application-spec.el` so the drawer-driven auto-init scenarios assert the full preset snapshot in fresh sessions and the drawer-wins overlay contract on reopen.
change: gptel-drawer-as-source-of-truth
status: needs-review
relations:
  - blocked-by:wire-snapshot-into-session-creation
  - blocked-by:replace-chat-save-with-full-snapshot-writer
---

## Files to modify

- `config/gptel/sessions/test/commands/preset-application-spec.el` (modify)
- Possibly `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el` (modify) if it carries similar drawer-shape assertions

## Implementation steps

1. Open `config/gptel/sessions/test/commands/preset-application-spec.el`. Read the current scenarios under `describe "Drawer-driven auto-init (metadata.yml is NOT consulted)"`.
2. Update `jf-gptel-test--write-session-with-drawer` (the helper) to optionally accept a snapshot plist argument (`extra-snapshot-keys`) and emit the corresponding lines (`:GPTEL_MODEL: ...`, `:GPTEL_TOOLS: ...`, etc.) alongside `:GPTEL_PRESET:` and the existing extra-properties alist. Most existing scenarios won't pass it; new scenarios for the full-snapshot contract will.
3. Add or update scenarios:
   - **"Fresh session.org carries full preset snapshot drawer"**: register a test preset with `:model`, `:tools`, `:temperature`; create a session via `jf/gptel-persistent-session` (or `jf/gptel--create-session-core` directly); read the saved file from disk; assert the drawer contains `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`. Assert no `:GPTEL_SYSTEM:`. Assert no `:GPTEL_BOUNDS:`.
   - **"Drawer model wins over preset model on reopen"**: write a `session.org` with `:GPTEL_PRESET: <preset>` AND `:GPTEL_MODEL: <different-model>`; open via `find-file-noselect`; assert buffer-local `gptel-model` is the drawer's model, not the preset's.
   - **"Save after drawer-wins reopen re-emits the drawer model (not the preset model)"**: open the session above, run `save-buffer`, re-read the file, assert `:GPTEL_MODEL:` still carries the drawer's value (not the preset's).
4. Remove or update any scenarios that explicitly asserted "drawer is delta-only" or "GPTEL_TOOLS absent when matches preset" — those contracts are reversed by this change. If a scenario doesn't make sense under the new contract, delete it; if it can be reframed, update it.

## Design rationale

`preset-application-spec.el` is the integration spec for the auto-init → mode-activation → drawer-overlay path. Under the new contract:
- Fresh sessions show the snapshot, so creation tests assert the snapshot.
- Drawer wins on reopen, so the "drawer model wins over preset model" scenario locks in Decision 3.
- Save round-trips drawer values (not preset values), so the post-save assertion confirms the writer (Task 3) and the overlay together produce a stable drawer.

## Verification

- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` passes.
- `./bin/run-tests.sh -d config/gptel/sessions` passes (full sessions suite).
- `./bin/run-tests.sh --report` shows no regressions outside `gptel/chat-mode` and `gptel/sessions-persistence`.

## Context

- design.md § Decision 1, 3, 4
- specs/gptel/sessions-persistence.md — Requirement: Session creation
- specs/gptel/chat-mode.md — Requirement: Configuration drawer overlay on restore; Requirement: Preset system integration
