---
name: audit-persistent-agent-session-md-refs
description: Audit persistent-agent.org session.md references for stale post-rename hits
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:fix-sessions-filesystem-task-verification
---

## Files to modify
- `config/gptel/tools/persistent-agent.org` (modify — lines 158, 164,
  369 per review)
- `config/gptel/tools/persistent-agent.el` (tangled)

## Implementation steps
1. Grep `config/gptel/tools/persistent-agent.org` for `session.md`.
   Current matches at lines 158, 164, 369.
2. For each hit, classify:
   - **Live reference** (used at runtime in a buffer-name lookup,
     load path, or string comparison) → rename to `session.org` to
     match the post-rename filesystem.
   - **Doc/diagram reference** (explanatory prose, directory layout
     ASCII) → rewrite to `session.org` so the doc stays accurate.
   - **Historical/archival** (past-tense describing old state) →
     leave as-is but add a brief note that naming changed.
3. Re-tangle and run the full suite:
   `./bin/run-tests.sh` — no regressions.
4. Verify: `grep -n "session.md" config/gptel/tools/persistent-agent.{org,el}`
   returns only intentional historical references (if any).

## Design rationale
`fix-sessions-filesystem-task-verification` review surfaced that
`persistent-agent.org` has three `session.md` hits not owned by any
sessions-* parallel task. The `sessions-filesystem` rename happened
at the sessions subsystem level; `tools/persistent-agent` is outside
that scope but uses the same filename. Left untouched, it will either
break silently (if live) or mislead future readers (if documentary).

## Verification
- Post-rename `grep -n "session.md" config/gptel/tools/persistent-agent.{org,el}`
  shows only deliberate historical hits.
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` succeeds.
- `./bin/run-tests.sh` reports no new failures.

## Context
- Review of fix-sessions-filesystem-task-verification (orchestrator
  session 2026-04-20) Observation #1
- `config/gptel/tools/persistent-agent.org:158,164,369`
