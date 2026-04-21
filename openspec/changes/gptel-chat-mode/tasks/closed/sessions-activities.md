---
name: sessions-activities
description: Activities integration emits session.org unconditionally
change: gptel-chat-mode
status: needs-review
relations:
  - blocked-by:auto-init-metadata-preset-precedence
  - blocked-by:auto-init-agent-path-handling
---

## Files to modify
- `config/gptel/sessions/activities-integration.org` (modify)
- `config/gptel/sessions/activities-integration.el` (tangled)
- `config/gptel/sessions/test/activities/activity-session-chat-spec.el` (new
  or rewrite)

## Implementation steps
1. Update `jf/gptel-session-create-persistent` (and any peer activity-backed
   creation helpers) so they:
   - Emit `session.org` unconditionally.
   - Drop any mode-selection parameter — there is only one mode now
     (`gptel-chat-mode`), so the parameter is noise.
2. Verify the worktree-tracking logic (`gptel-activity-worktrees` buffer-
   local, activity directory layout) is **unchanged** — only the session
   file name and mode change.
3. Any activity resume path (opening a session buffer via an activity hook)
   relies on the auto-init logic from task `sessions-auto-init`. Confirm
   by test: creating a session via an activity and then resuming via the
   activity hook activates chat-mode and sets the five session vars.
4. Tests:
   - Activity-backed session creation writes `session.org` with chat-mode
     initial content.
   - `metadata.yml` fields populated as before (session_id, created,
     updated, preset).
   - Resume path (open session buffer) activates `gptel-chat-mode` and
     does not enable `gptel-mode`.
   - Worktree tracking buffer-local variables are set identically to the
     previous behaviour.

## Design rationale
Decision 16 applies uniformly to all session-creation paths: activities-
integration is one such path. Keeping a mode-selection parameter would
perpetuate the impression that gptel-mode remains a valid alternative for
session buffers — it is not (Decision 16 is categorical).

The rest of the activities-integration module (worktree tracking, activity
hooks) is **orthogonal** to the chat-mode change and should not be touched
by this task. If regressions appear in that logic during verification
(task `verify-change`), file a follow-up task — don't rewrite scope here.

## Design pattern
Session-creation helpers across the sessions subsystem should have a
single code path for writing the initial session file. If this task
surfaces duplication between `jf/gptel-persistent-session` (task
`sessions-persistent-create`) and `jf/gptel-session-create-persistent`, 
consolidate into a shared helper — but scope that consolidation to what
directly affects `session.org` creation. Do not refactor activity
infrastructure opportunistically.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/activities-integration.org`
  succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/activities` passes.
- Creating a session via an activity and opening the resulting buffer
  leaves `major-mode` as `gptel-chat-mode` with all five session vars set
  and preset applied.
- Scenario: activity-backed sessions emit `session.org` unconditionally.

## Discovered during review of sessions-filesystem (2026-04-20)
`config/gptel/sessions/activities-integration.el:36` and `:73` (and the
corresponding `.org`) still reference `session.md` in docstrings or
comments after the filesystem rename. Update those references to
`session.org` as part of this task. (Sessions-filesystem review
Finding #9.)

## Context
- design.md §Decision 16 (sessions use chat-mode, never gptel-mode)
- architecture.md §`sessions/activities-integration` (modified)
- specs/gptel/sessions-persistence.md §"Session file format" (MODIFIED —
  applies to activity-created sessions too)
- Review of sessions-filesystem (orchestrator session 2026-04-20) Finding #9
