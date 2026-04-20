---
name: fix-sessions-filesystem-task-verification
description: Rewrite closed task's verification grep to scope to files actually modified
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sessions-filesystem
---

## Files to modify
- `openspec/changes/gptel-chat-mode/tasks/closed/sessions-filesystem.md`
  (modify — Verification section step 3)

## Implementation steps
1. Locate the verification step that currently reads:
   `grep -rn "session.md" config/gptel/sessions/` returns only archival
   comments or nothing — no live code references.
2. Replace with a scoped grep that matches what was actually implemented:
   `grep -n "session.md" config/gptel/sessions/filesystem.{org,el} \
      config/gptel/sessions/constants.{org,el}` returns nothing or only
   archival comments.
3. Add an explanatory clause noting that other live `session.md` callers
   in `commands.org`, `branching.org`, `activities-integration.org`, and
   under `config/gptel/test/` are owned by the parallel tasks
   `sessions-auto-init`, `sessions-persistent-create`,
   `sessions-branching`, and `sessions-activities`, and by follow-up
   tasks discovered during review.

## Design rationale
The closed task's verification step was wrong as written: a recursive
grep across `config/gptel/sessions/` returns 13 live references in
`commands.{org,el}` (including two `(string= file-name "session.md")`
conditionals in `jf/gptel--auto-init-session-buffer`), plus references in
`activities-integration.el` and `branching.el`. The implementing agent
correctly scoped its work to the filesystem/constants slice (per task
title), but the verification step asks for something broader than the
task's scope. Anyone running the verification literally would conclude
the task is incomplete.

This is housekeeping — the task is functionally done. The fix preserves
an honest historical record.

## Verification
- The amended verification step in the closed task file matches the
  actual file scope of `ef5e0b7`.
- The amended step still calls out the larger-scope work as owned by
  parallel tasks.

## Context
- Review of sessions-filesystem (orchestrator session 2026-04-20)
  Finding #1
- sessions-filesystem implementation commit: `ef5e0b7`
- fix-forward commit: `a5c126a`
