---
name: tasks-orchestrator
description: Implement multiple ready tasks in parallel using isolated worktrees and spawned agents. Use when the user wants to execute multiple ready tasks from a change concurrently with sequential merging and regression testing after each merge.
---

Implement multiple ready tasks from an OpenSpec change in parallel using isolated worktrees and spawned agents. Merges happen sequentially with regression testing after each.

## 1. Discovery & Selection

**Select the change:**

Prompt user for change name if not provided. Then scan
`openspec/changes/<name>/tasks/open/*.md` and read frontmatter.

Build the ready set:
- A task is **ready** if `status: ready` OR if `status: blocked` with every
  `blocked-by:<dep>` relation resolved (i.e. `<dep>.md` is in `tasks/closed/`)
- Everything else is **blocked** until prerequisites close

Display overview:
```
Change: <change-name>
Ready tasks: 3
  - setup-module — Create module structure
  - write-spec-file — Draft spec.md
  - characterize-current — Capture current behaviour
Blocked: 2 (by setup-module, write-spec-file)
```

Use **AskUserQuestion**:
- 0 ready tasks: Exit (suggest `/opsx-tasks generate` or close the change)
- 1-3 ready: auto-select with confirmation
- 4+ ready: multiSelect, max 5 per batch

**Batch strategy**: default to parallel unless the user specifies otherwise.
For sequential mode, spawn agents one at a time.

## 2. Baseline Capture

```bash
./bin/run-tests.sh --snapshot
TIMESTAMP=$(date +%s)
mkdir -p .orchestrator
cp test-results.txt .orchestrator/baseline-${TIMESTAMP}.txt
```

**Parse test-results.txt**:
```bash
SUMMARY_LINE=$(grep -E "^(Passed|Aborted|Failed):" test-results.txt | head -1)
TOTAL_TESTS=$(echo "$SUMMARY_LINE" | grep -oE "Ran [0-9]+ tests" | grep -oE "[0-9]+")
UNEXPECTED=$(echo "$SUMMARY_LINE" | grep -oE "[0-9]+ unexpected" | grep -oE "[0-9]+")
ABORTED=$(grep -c "^  ABORTED " test-results.txt || echo "0")
UNKNOWN=$(grep -c "^  UNKNOWN " test-results.txt || echo "0")
```

If baseline has failures: use AskUserQuestion to confirm proceeding (regression
detection still works by comparing diffs).

## 3. Worktree & Agent Setup

For each selected task (sequential setup to avoid race conditions):

```bash
# CRITICAL: run from the main repository root
cd /Users/jefffarr/emacs
REPO_ROOT=$(git rev-parse --show-toplevel)

WORKTREE_NAME="task-${TASK_NAME}-$(date +%s)"
WORKTREE_PATH="${REPO_ROOT}/.worktrees/${WORKTREE_NAME}"

git worktree add "$WORKTREE_PATH" -b "$WORKTREE_NAME"
./bin/init-worktree-runtime.sh "$WORKTREE_PATH"
git -C "$WORKTREE_PATH" submodule update --init
```

**CRITICAL Worktree Requirements**:
- **Always create from main repo root**: `cd` to main repo before
  `git worktree add`
- **Use absolute paths**: constructed from `$(git rev-parse --show-toplevel)`
- **Never create from within worktrees**: causes nesting
  (`.worktrees/A/.worktrees/B/`)
- **Siblings, not children**: every worktree lives at `.worktrees/task-*`

Ensure `.worktrees/` is gitignored.

**State file** (`.orchestrator/state.json`):
```json
{
  "session_id": "orch-1234567890",
  "repo_root": "/Users/jefffarr/emacs",
  "change_name": "<change-name>",
  "baseline_snapshot": ".orchestrator/baseline-1234567890.txt",
  "baseline_test_count": 588,
  "current_branch": "main",
  "tasks": [
    {
      "task_name": "setup-module",
      "task_file": "openspec/changes/<change>/tasks/open/setup-module.md",
      "worktree_path": "/Users/jefffarr/emacs/.worktrees/task-setup-module-1234567890",
      "branch_name": "task-setup-module-1234567890",
      "agent_task_id": "<TaskCreate id>",
      "status": "setup_complete",
      "regression_detected": false,
      "closed": false,
      "worktree_removed": false
    }
  ]
}
```

**Agent spawning** (use the Agent tool, NOT TaskCreate):

```
Agent(
  subagent_type: "general-purpose",
  description: "Implement task <task-name>",
  prompt: <see Agent Prompt Template>,
  run_in_background: false   # CRITICAL: must be false for file write access
)
```

The Agent tool returns an id. Store it as `agent_task_id` in the state file.

**NEVER use `run_in_background: true`** — blocks file writes.

### Agent Prompt Template

```markdown
Implement task <TASK_NAME> for change <CHANGE_NAME> in worktree <WORKTREE_PATH>
(branch: <BRANCH_NAME>).

Read the task body:
<FULL_TASK_FILE_CONTENTS>

**Literate Programming**: Edit .org files, tangle with
`./bin/tangle-org.sh`, commit both .org and .el.

**Testing**: Run the verification commands listed in the task (usually
`./bin/run-tests.sh -d <dir>` or a pattern). Must pass before you commit.

**Commit**: "Implement task <TASK_NAME>: <description from frontmatter>" with
Co-Authored-By. DON'T push/merge/close the task — the orchestrator handles
that.
```

## 4. Monitor Progress

Poll every 30s with TaskList:

```
While any tasks have status in ["setup_complete", "in_progress"]:
  Sleep 30 seconds
  TaskList()

  For each task's agent_task_id:
    If task.status == "completed":
      Verify commits: git -C <worktree> log -1
      If no commits: mark task as "failed"
      Else: mark task as "implementation_complete"

    If task.status == "failed":
      Mark task as "failed"

    If task.status == "in_progress":
      Update task status to "in_progress"

  Persist state file with latest statuses
```

Keep failed worktrees for debugging.

## 5. Sequential Merging

Merge in completion order (sequential, not parallel — avoids conflicts and
enables precise regression detection):

```bash
cd /Users/jefffarr/emacs
git merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
```

**Conflicts**: Abort with `git merge --abort`, mark as "merge_conflict", keep
worktree, continue with next task. If manually resolved, MUST re-test before
continuing.

## 6. Test After Each Merge

**MANDATORY after each merge**:

```bash
./bin/run-tests.sh --snapshot
SUMMARY_LINE=$(grep -E "^(Passed|Aborted|Failed):" test-results.txt | head -1)

if [ -z "$SUMMARY_LINE" ]; then
  echo "⚠️ Could not find test summary — check test-results.txt"
  exit 1
fi

if echo "$SUMMARY_LINE" | grep -q "^Aborted:"; then
  echo "⚠️ TEST RUN ABORTED after merge $TASK_NAME"
  exit 1
fi

TOTAL_TESTS=$(echo "$SUMMARY_LINE" | grep -oE "Ran [0-9]+ tests" | grep -oE "[0-9]+")
UNEXPECTED=$(echo "$SUMMARY_LINE" | grep -oE "[0-9]+ unexpected" | grep -oE "[0-9]+")
ABORTED_COUNT=$(grep -c "^  ABORTED " test-results.txt || echo "0")
UNKNOWN_COUNT=$(grep -c "^  UNKNOWN " test-results.txt || echo "0")

if [ "$UNEXPECTED" -gt 0 ] || [ "$ABORTED_COUNT" -gt 0 ] || [ "$UNKNOWN_COUNT" -gt 0 ]; then
  echo "⚠️ REGRESSION DETECTED after merge $TASK_NAME"
  exit 1
fi

TIMESTAMP=$(date +%s)
cp test-results.txt .orchestrator/after-${TASK_NAME}-${TIMESTAMP}.txt
```

**On regression**: stop merges, keep worktrees, use AskUserQuestion for
revert/investigate/continue options.

## 7. Cleanup

After successful merge + tests pass + no regression:

```bash
cd /Users/jefffarr/emacs

# Close the task: move file from tasks/open/ to tasks/closed/, set status: done
git mv "openspec/changes/$CHANGE/tasks/open/$TASK_NAME.md" \
       "openspec/changes/$CHANGE/tasks/closed/$TASK_NAME.md"
# Then update the status field in the moved file via Edit

# Remove the worktree
git worktree remove "$WORKTREE_PATH"
```

Update state file. Re-evaluate `blocked-by` relations on any open tasks: if a
`blocked` task's dependencies are now all closed, flip its frontmatter to
`ready`.

**Keep worktrees if**: merge conflict, agent failed, or regression detected.

## 8. Final Snapshot

After the last successful merge, run a final test snapshot:
`./bin/run-tests.sh --snapshot`, save to `.orchestrator/final-${SESSION_ID}.txt`,
commit with a summary. Update state file with final test counts.

## 9. Summary Report

Display session results: successfully merged, conflicts, failures, test
summary, cleanup status.

**If ready tasks remain (new ones unblocked by this batch):**

```
Batch Complete ✓

Successfully merged 3/3 tasks
Newly unblocked: 2 tasks (implement-core, wire-transient)

Ready to start next batch?
```

**If all tasks complete:**

```
All Tasks Complete ✓

Total tasks: 8/8 merged
Archive change: `/opsx-archive`
```

## Error Handling

- **Agent failures**: mark failed, keep worktree, continue with others
- **Merge conflicts**: abort merge, skip task, continue with others
- **Infrastructure failures**: skip task, report error, continue
- **Test failures**: stop merges, offer revert/investigate options
- **User interruption**: save state, report progress

## Critical Requirements

**NEVER**:
- Use `run_in_background: true` (blocks file writes)
- Create worktrees from within other worktrees (causes nesting)
- Use relative paths without `cd` to main repo first
- Skip test verification after merge
- Continue merging after regression detected

**ALWAYS**:
- Create worktrees from main repo root (`cd` to repo first)
- Use absolute paths constructed from `git rev-parse --show-toplevel`
- Test after EACH merge (sequential, not batched)
- Verify test summary line programmatically
- Check ABORTED and UNKNOWN test counts (stop if > 0)
- Keep failed/conflicted worktrees for debugging
- Update state file after each phase
- After each close, re-evaluate dependent tasks' `blocked-by` status

## Prerequisites

Check before starting:
- `openspec/changes/<name>/tasks/open/` has at least one ready task
- `git worktree --version` available
- `./bin/run-tests.sh --help` exists
- No existing `.orchestrator/state.json` (previous session not finalised)
