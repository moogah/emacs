---
name: regression-sweep-step-8-agent-verification
description: Run the deferred step 8 of the regression-sweep manual smoke (PersistentAgent sub-agent session.org drawer contents) once the unrelated agent-infrastructure issue blocking sub-agent creation on 2026-04-25 is resolved.
change: gptel-chat-state-persistence
status: blocked
relations:
  - "discovered-from:regression-sweep-and-manual-smoke"
---

## Files to modify

None (verification task — no code changes). If the verification reveals bugs, open follow-up tasks rather than modifying here.

## Implementation steps

1. Confirm the unrelated agent-infrastructure issue (whatever was preventing sub-agent creation in `./bin/emacs-isolated.sh` on 2026-04-25) is fixed. Until then this task stays `blocked`.
2. Launch `./bin/emacs-isolated.sh`, create or reopen a parent session via `M-x jf/gptel-persistent-session`.
3. Trigger a `PersistentAgent` sub-agent. The usual route is a tool call that the parent session is permitted to make (e.g. a `create_persistent_agent` tool exposed by one of the agent presets). If unsure, run `M-x apropos-command RET persistent-agent RET` to find the entry point.
4. Locate the resulting agent session directory (typically a sibling under the parent's `agents/` subdirectory, but exact layout depends on the persistent-agent module — confirm by reading `config/gptel/tools/persistent-agent.org` if needed).
5. Inspect the agent's `session.org`:
   - The `:PROPERTIES:` drawer at `point-min` MUST contain `:GPTEL_PRESET: <name>` (any preset; the agent's own preset).
   - The drawer MUST contain `:GPTEL_PARENT_SESSION_ID: <parent-id>` (the spawning session's id).
   - The drawer MUST NOT contain `:GPTEL_BOUNDS:`.
6. Confirm no `metadata.yml` file in the agent directory (a `branch-metadata.yml` is fine — that's a different, preserved file).
7. If all five assertions hold, mark this task complete and flip to needs-review.
8. If any assertion fails, open a follow-up task per finding (do not patch under this task) and link it via `discovered-from`.

## Design rationale

This is the deferred step 8 of the parent regression-sweep task. The parent was closed at step 7 because the user hit an unrelated agent-infrastructure issue on 2026-04-25 that prevented running step 8 in the same orchestration session. Steps 1–7 of the parent passed — the change's session-creation, save-on-menu, branching, drawer, and metadata-removal paths are all verified end-to-end. Step 8 covers the fourth session shape (PersistentAgent sub-agent) and exists because purely-automated tests stub at the upstream boundary; this is the only end-to-end coverage of the agent path's drawer contents post-metadata-module-deletion.

Splitting it out (rather than blocking the parent at `ready` indefinitely) keeps the change's archive path clear once the unrelated issue is fixed: this task can run independently and flip to `done` without re-opening the parent. The `discovered-from:regression-sweep-and-manual-smoke` relation preserves the audit trail.

## Verification

- All five assertions in step 5–6 above hold against a freshly-spawned agent in an isolated Emacs session.
- No `metadata.yml` produced anywhere in the agent's directory tree.

## Context

- proposal.md §Impact (manual verification across the four session shapes).
- design.md §"Migration Plan" (no migration required; smoke verifies acceptably degraded path for drawerless files).
- Parent task: `tasks/closed/regression-sweep-and-manual-smoke.md` — see its **Smoke results** section for the 2026-04-25 outcome, including which agent-infrastructure issue blocked this step.
- Status `blocked` rather than `ready` because the precondition (working agent infrastructure) is environmental, not internal to this task. Flip to `ready` once the unrelated issue resolves.
