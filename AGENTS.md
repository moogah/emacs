# Agent Instructions

This project uses **OpenSpec** with file-based task tracking. Implementation
work for a change lives in `openspec/changes/<name>/tasks/open/` and
`tasks/closed/` as self-contained markdown files.

## Quick Reference

| Command | What it does |
|---------|--------------|
| `/opsx-explore` | Think through a problem before committing to an approach |
| `/opsx-new <name>` | Scaffold a new change (proposal → specs → architecture → design → tasks) |
| `/opsx-ff <name>` | Fast-forward through all artifacts in one pass |
| `/opsx-continue <name>` | Create the next artifact for an active change |
| `/opsx-tasks <sub> [name]` | Manage task files (list/show/create/update/generate) |
| `/opsx-apply <name>` | Implement tasks from `tasks/open/` in dependency order |
| `/tasks-orchestrator` | Implement multiple ready tasks in parallel worktrees |
| `/opsx-verify <name>` | Validate implementation matches artifacts |
| `/opsx-archive <name>` | Archive a completed change |

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT
complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **Capture remaining work** — For anything that needs follow-up, either
   create a new task via `/opsx-tasks create` or note it in the active
   change's design/proposal.
2. **Run quality gates** (if code changed):
   - Tangle and validate: `./bin/tangle-org.sh <file>`
   - Tests: `./bin/run-tests.sh -d <dir>` or the pattern from the task's
     Verification section
3. **Close completed tasks** — Move task files from `tasks/open/` to
   `tasks/closed/`, set `status: done`, and re-evaluate any tasks that were
   `blocked-by` the closed one.
4. **PUSH TO REMOTE** — This is MANDATORY:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** — Clear stashes, prune remote branches.
6. **Verify** — All changes committed AND pushed.
7. **Hand off** — Provide context for the next session.

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing — that leaves work stranded locally
- NEVER say "ready to push when you are" — YOU must push
- If push fails, resolve and retry until it succeeds
