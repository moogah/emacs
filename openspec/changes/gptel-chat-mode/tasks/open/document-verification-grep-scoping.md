---
name: document-verification-grep-scoping
description: Add CLAUDE.md guideline that task verification greps must scope to modified files
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:fix-sessions-filesystem-task-verification
---

## Files to modify
- `CLAUDE.md` (modify — OpenSpec Workflow section or Task Tracking
  subsection, wherever it fits)

## Implementation steps
1. Open `CLAUDE.md` and find the OpenSpec Workflow / Task Tracking
   section.
2. Add a short guideline (2-4 sentences) stating:
   - Task `Verification` grep commands should scope to the **files the
     task modifies**, not to the subsystem directory broadly.
   - Broad recursive greps (`grep -rn <term> <dir>/`) tend to match
     callers owned by parallel tasks and will falsely read as
     "incomplete".
   - Prefer explicit file-list greps: `grep -n <term> <specific files>`.
3. Keep it tight — this is guidance, not a rule. One paragraph.
4. Where to place: inside `### Task Tracking` is probably best, near
   the existing description of self-contained task files.

## Design rationale
The `sessions-filesystem` task's original verification used
`grep -rn "session.md" config/gptel/sessions/`, which returned 13
live hits owned by four parallel tasks — making the closed task read
as unfinished when run verbatim. This is a recurring anti-pattern in
parallel-task changes. Documenting the convention prevents
recurrence.

Keep the guidance short and prescriptive; longer guidelines get
ignored.

## Verification
- `CLAUDE.md` has a new guideline block on verification-grep scoping.
- The guideline is under 5 lines.

## Context
- Review of fix-sessions-filesystem-task-verification (orchestrator
  session 2026-04-20) Observation #4
- Concrete example: `tasks/closed/sessions-filesystem.md` before
  `a10e515` fix-forward.
