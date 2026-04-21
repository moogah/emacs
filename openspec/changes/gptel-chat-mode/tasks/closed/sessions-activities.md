---
name: sessions-activities
description: Activities integration emits session.org unconditionally
change: gptel-chat-mode
status: done
relations:
  - blocked-by:auto-init-metadata-preset-precedence
  - blocked-by:auto-init-agent-path-handling
---

## Review (2026-04-21, orch session `orch-review-1776796835`)

Reviewer agent (`a07eb863c909ce2bc`) inspected `git show 444f331`, the
full `activities-integration.{org,el}`, the new 354-line
`activity-session-chat-spec.el`, the sibling `sessions/commands.el`
for duplication-vs-consolidation, and the `jf/gptel--auto-init-
session-buffer` / `jf/gptel--ensure-mode-once` chain to confirm
chat-mode is hard-wired. Compared against Decisions 16/17/18 and
`specs/gptel/sessions-persistence.md`. Grepped for residual
`session.md` / `"###\n"` / `gptel-mode` and verified the five-
session-var contract on the loaded buffer.

**Findings:**

1. *(Inline fix applied — commit `d0e7727`)* —
   `config/gptel/sessions/activities-integration.org:366` (tangled
   to `:202`). Docstring of `jf/gptel-session--create-buffer` still
   read *"preset application and gptel-mode initialization
   automatically when the session file is opened."* Decision 16 is
   categorical; the previous discovered-from note (sessions-
   filesystem Finding #9) addressed `:36` and `:73` but missed
   this third stale reference. Replaced `gptel-mode` →
   `gptel-chat-mode`. Re-tangled; regression test identical to
   baseline (ERT 9 pre-existing + Buttercup 3 pre-existing).

Ruled out:
- **Duplication between `jf/gptel-persistent-session` and
  `jf/gptel-session-create-persistent`** — both call
  `jf/gptel--create-session-core` with `nil` initial content; the
  shared core already holds the chat-mode template default. No
  meaningful duplication to consolidate.
- **Mode-selection parameter drop** — prior signature never had a
  mode-selection parameter; task item was precautionary, correctly
  a no-op.
- **Decision 16 hard-wiring** — `jf/gptel--ensure-mode-once`
  unconditionally calls `gptel-chat-mode`; no fallback branch
  reaches `gptel-mode`.
- **Worktree tracking unchanged** — HTML-comment annotation
  byte-identical to pre-444f331; pre-existing
  `gptel-activity-worktrees` setq behaviour is orthogonal.
- **5 session vars on loaded buffer** — resume test uses real
  `jf/gptel--auto-init-session-buffer` with find-file stubs only
  at the filesystem boundary; assertions read `setq-local`-set
  vars on the live buffer.

**Spec-level signals:** none. Decision 16's categorical stance
composed cleanly; the helper just deleted the `"###\n"` literal and
fell through to the shared core default.

**Follow-ups:** none (the single finding was trivial and applied
inline; the fix is a docstring-only change that does not warrant its
own review pass).

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
