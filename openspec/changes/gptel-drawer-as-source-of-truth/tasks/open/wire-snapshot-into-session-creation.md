---
name: wire-snapshot-into-session-creation
description: Thread the resolved preset spec from session-creation entrypoints into the (now snapshot-aware) `jf/gptel-scope-profile--create-for-session` so fresh `session.org` files carry the full snapshot drawer.
change: gptel-drawer-as-source-of-truth
status: needs-review
relations:
  - blocked-by:extend-render-drawer-text-with-preset-snapshot
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify)
- `config/gptel/tools/persistent-agent.org` (modify)
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` (modify) — assertions on the full-snapshot drawer at creation
- `config/gptel/test/session-creation-spec.el` (modify if it still asserts drawer shape) — keep regression sweep green
- `config/gptel/tools/test/persistent-agent/...` (modify) — assertions on agent-session drawer including snapshot keys

## Implementation steps

1. Open `config/gptel/sessions/commands.org`. Locate `jf/gptel--create-session-core` (around `commands.el:325` in tangled form). The `drawer-text` binding currently calls `jf/gptel-scope-profile--create-for-session preset-name main-branch-dir project-root worktree-paths parent-session-id`.
2. Resolve the preset spec at the call site (`(gptel-get-preset preset-name)`) and pass it through. Either extend `--create-for-session`'s signature with a `preset-spec` arg or have it resolve internally (matches whatever pattern Task 1 picked).
3. Locate `jf/gptel--initial-session-content` (around `commands.el:328` in tangled form). This helper currently builds the drawer manually as a string for the legacy path. Decide:
   - **Preferred**: Replace its body with a call into `jf/gptel-scope-profile--render-drawer-text` (passing nil scope-plist if no scope, plus the resolved preset spec). Single source of truth for drawer text.
   - **Fallback**: If the legacy path needs to stand alone (e.g. tests use it directly), mirror the snapshot-emission logic here too. Less ideal — duplicates the writer.
4. Re-tangle: `./bin/tangle-org.sh config/gptel/sessions/commands.org`.
5. Open `config/gptel/tools/persistent-agent.org`. Locate the `--task` flow that calls `jf/gptel-scope-profile--create-for-session` (around `persistent-agent.el:281` in tangled form). Same change: resolve preset spec via `gptel-get-preset` for the agent's preset, pass through.
6. Re-tangle: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.
7. Update `session-org-creation-spec.el` assertions:
   - "Fresh branch session.org has full preset snapshot drawer" — assert presence of `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:` (whatever the test preset declares); assert absence of `:GPTEL_SYSTEM:`.
   - "Sparse preset produces sparse drawer" — register a test preset that only declares `:model`; assert the drawer has `:GPTEL_MODEL:` but no `:GPTEL_TEMPERATURE:` etc.
8. Update `persistent-agent` creation spec (likely `tools/test/persistent-agent/creation-spec.el` or similar) to assert the agent's `session.org` carries `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, AND the agent preset's snapshot keys. No `:GPTEL_SYSTEM:`.

## Design rationale

This task threads the preset spec through the existing creation entrypoints without adding new abstractions. Folding `--initial-session-content` into the renderer (preferred path) collapses two drawer-text producers into one — important because the round-trip idempotency invariant assumes a single renderer.

The persistent-agent path is identical in shape to the standalone path; the only difference is the parent-session-id argument that's already present.

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org` and `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` succeed.
- `./bin/run-tests.sh -d config/gptel/sessions` passes (creation specs assert expanded drawer).
- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` passes.
- `./bin/run-tests.sh -d config/gptel/test` passes (legacy session-creation-spec.el still green).
- Manual: `M-x jf/gptel-persistent-session RET smoke-test RET y RET` (or similar), then inspect `~/.gptel/sessions/smoke-test-*/branches/main/session.org`. Drawer carries `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:`, `:GPTEL_SCOPE_*:`. No `:GPTEL_SYSTEM:`.

## Context

- design.md § Decision 4 — renderer extension path
- design.md § Decision 6 — existing sessions degrade gracefully (no migration)
- specs/gptel/sessions-persistence.md — Requirement: Session creation; Requirement: session.org as authoritative session file
