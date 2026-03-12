---
name: "Bead Orchestrator"
description: Implement multiple ready beads in parallel using isolated worktrees and spawned agents
category: Workflow
tags: [workflow, beads, parallel, experimental]
---

Implement multiple ready beads in parallel using isolated worktrees and spawned agents. Merges happen sequentially with regression testing after each.

## 1. Discovery & Selection

**Read execution plan from .openspec.yaml:**

Prompt user for change name if not provided. Then:

```bash
cat openspec/changes/<change-name>/.openspec.yaml
```

**If execution plan exists:**

Parse `execution.batches` to find first batch with open beads.

Display batch overview:
```
Batch: foundation (3 beads, parallel)
Strategy: parallel
Estimated tokens: 120k
Beads: emacs-a3f, emacs-a4g, emacs-a7j
```

Use AskUserQuestion:
- "Run current batch" → proceed
- "Select specific batch" → show list, let user choose
- "Manual selection" → fall back to legacy mode

**If no execution plan (legacy mode):**

```bash
bd list --ready --json
```

**Selection**:
- 0 beads: Exit
- 1-3 beads: Auto-select with confirmation
- 4+ beads: Use AskUserQuestion (multiSelect: true, max 5)

## 2. Baseline Capture

```bash
./bin/run-tests.sh --snapshot
TIMESTAMP=$(date +%s)
mkdir -p .beads/orchestrator
cp test-results.txt .beads/orchestrator/baseline-${TIMESTAMP}.txt
```

**Parse test-results.txt**:
```bash
SUMMARY_LINE=$(grep -E "^(Passed|Aborted|Failed):" test-results.txt | head -1)
TOTAL_TESTS=$(echo "$SUMMARY_LINE" | grep -oE "Ran [0-9]+ tests" | grep -oE "[0-9]+")
UNEXPECTED=$(echo "$SUMMARY_LINE" | grep -oE "[0-9]+ unexpected" | grep -oE "[0-9]+")
ABORTED=$(grep -c "^  ABORTED " test-results.txt || echo "0")
UNKNOWN=$(grep -c "^  UNKNOWN " test-results.txt || echo "0")
```

If baseline has failures: Use AskUserQuestion to confirm proceeding (regression detection still works by comparing diffs).

## 3. Worktree & Agent Setup

For each bead (sequential setup to avoid race conditions):

```bash
# CRITICAL: Ensure we're in the main repository root
cd /Users/jefffarr/emacs

# Use absolute path to prevent nesting
REPO_ROOT=$(git rev-parse --show-toplevel)
WORKTREE_NAME="bead-${BEAD_ID}-$(date +%s)"
WORKTREE_PATH="${REPO_ROOT}/.worktrees/${WORKTREE_NAME}"

# Create worktree from main repo (not from within another worktree)
git worktree add "$WORKTREE_PATH" -b "$WORKTREE_NAME"

# Initialize runtime (use absolute path)
./bin/init-worktree-runtime.sh "$WORKTREE_PATH"

# Initialize submodules (use -C to avoid cd)
git -C "$WORKTREE_PATH" submodule update --init
```

**CRITICAL Worktree Requirements**:
- **Always create from main repo root**: Use `cd` to main repo before `git worktree add`
- **Use absolute paths**: Construct with `$(git rev-parse --show-toplevel)/.worktrees/...`
- **Never create from within worktrees**: This causes nesting (.worktrees/A/.worktrees/B/)
- **Siblings, not children**: All worktrees should be `.worktrees/bead-*`, not nested

Ensure `.worktrees/` is gitignored.

**State file** (`.beads/orchestrator-state.json`):
```json
{
  "session_id": "orch-1234567890",
  "repo_root": "/Users/jefffarr/emacs",
  "baseline_snapshot": ".beads/orchestrator/baseline-1234567890.txt",
  "baseline_test_count": 588,
  "current_branch": "main",
  "execution_plan": {
    "change_name": "gptel-org-mode-sessions",
    "batch_id": "foundation",
    "batch_strategy": "parallel",
    "batch_estimated_tokens": 120000,
    "next_batch_id": "core-logic",
    "next_batch_depends_on": ["foundation"]
  },
  "beads": [
    {
      "bead_id": "emacs-abc1",
      "worktree_path": "/Users/jefffarr/emacs/.worktrees/bead-emacs-abc1-1234567890",
      "branch_name": "bead-emacs-abc1-1234567890",
      "task_id": "task-xyz",
      "estimated_tokens": 40000,
      "status": "setup_complete",
      "regression_detected": false,
      "bead_closed": false,
      "worktree_removed": false
    }
  ]
}
```

**Batch strategy enforcement:**

If batch strategy is `sequential`:
- Spawn agents ONE AT A TIME
- Wait for each to complete before starting next

If batch strategy is `parallel`:
- Spawn ALL agents simultaneously

**Agent spawning** (use Task tool, NOT TaskCreate):

```
Task(
  subagent_type: "general-purpose",
  description: "Implement bead $BEAD_ID",
  prompt: <see Agent Prompt Template>,
  run_in_background: false  # CRITICAL: Must be false for file write access
)
```

Task tool returns task_id. Store in state file's `beads[].task_id`.

**NEVER use `run_in_background: true`** - blocks file writes.

### Agent Prompt Template

```markdown
Implement bead <BEAD_ID> in worktree <WORKTREE_PATH> (branch: <BRANCH_NAME>).

<FULL_BEAD_DESCRIPTION>

**Literate Programming**: Edit .org files, tangle with `./bin/tangle-org.sh`, commit both .org and .el.

**Testing**: Run `./bin/run-tests.sh` (targeted with `-d config/foo` or `-p "^test-*"`). Must pass.

**Commit**: "Implement bead <BEAD_ID>: <Title>" with Co-Authored-By. DON'T push/merge/close - orchestrator handles this.
```

## 4. Monitor Progress

Poll every 30s with TaskList:

```
While any beads have status in ["setup_complete", "in_progress"]:
  Sleep 30 seconds

  TaskList()

  For each bead's task_id:
    If task.status == "completed":
      Verify commits: git -C <worktree> log -1
      If no commits: Mark bead status as "failed"
      Else: Mark bead status as "implementation_complete"

    If task.status == "failed":
      Mark bead status as "failed"

    If task.status == "in_progress":
      Update bead status to "in_progress"

  Update state file with latest statuses
```

Keep failed worktrees for debugging.

## 5. Sequential Merging

Merge in completion order (sequential, not parallel - avoids conflicts, enables precise regression detection).

```bash
# Ensure we're in main repo root
cd /Users/jefffarr/emacs

git merge --no-ff "$BRANCH_NAME" -m "Merge bead $BEAD_ID: $TITLE"
```

**Conflicts**: Abort with `git merge --abort`, mark as "merge_conflict", keep worktree, continue with next bead. If manually resolved, MUST re-test before continuing.

## 6. Test After Each Merge

**MANDATORY after each merge**:

```bash
./bin/run-tests.sh --snapshot

# Parse test results
SUMMARY_LINE=$(grep -E "^(Passed|Aborted|Failed):" test-results.txt | head -1)

# Verify summary exists
if [ -z "$SUMMARY_LINE" ]; then
  echo "⚠️ Could not find test summary - check test-results.txt"
  exit 1
fi

# Check if aborted
if echo "$SUMMARY_LINE" | grep -q "^Aborted:"; then
  echo "⚠️ TEST RUN ABORTED after merge $BEAD_ID"
  exit 1
fi

# Extract counts
TOTAL_TESTS=$(echo "$SUMMARY_LINE" | grep -oE "Ran [0-9]+ tests" | grep -oE "[0-9]+")
UNEXPECTED=$(echo "$SUMMARY_LINE" | grep -oE "[0-9]+ unexpected" | grep -oE "[0-9]+")
ABORTED_COUNT=$(grep -c "^  ABORTED " test-results.txt || echo "0")
UNKNOWN_COUNT=$(grep -c "^  UNKNOWN " test-results.txt || echo "0")

# Stop if any issues
if [ "$UNEXPECTED" -gt 0 ] || [ "$ABORTED_COUNT" -gt 0 ] || [ "$UNKNOWN_COUNT" -gt 0 ]; then
  echo "⚠️ REGRESSION DETECTED after merge $BEAD_ID"
  exit 1
fi

# Save snapshot
TIMESTAMP=$(date +%s)
cp test-results.txt .beads/orchestrator/after-${BEAD_ID}-${TIMESTAMP}.txt

# Update state file with test counts (use jq)
```

**On regression**: Stop merges, keep worktrees, use AskUserQuestion for revert/investigate/continue options.

## 7. Cleanup

After successful merge + tests pass + no regression:

```bash
# Ensure we're in main repo root
cd /Users/jefffarr/emacs

# Close bead and remove worktree
bd close "$BEAD_ID"
git worktree remove "$WORKTREE_PATH"
```

Update state file. **Keep worktrees if**: merge conflict, agent failed, or regression detected.

## 8. Final Snapshot

After last successful merge, run final test snapshot: `./bin/run-tests.sh --snapshot`, save to `.beads/orchestrator/final-${SESSION_ID}.txt`, commit with summary. Update state file with final test counts.

## 9. Summary Report

Display session results: successfully merged, conflicts, failures, test summary, cleanup status.

**If more batches remain:**

```
Batch Complete: foundation ✓

Successfully merged 3/3 beads
Estimated tokens: 120k (actual: ~118k)

Next Batch: core-logic
Strategy: sequential
Depends on: foundation ✓
Beads: emacs-a5h, emacs-a6i

Ready to start next batch?
```

**If all batches complete:**

```
All Batches Complete ✓

Total beads: 8/8 merged
Estimated tokens: 320k (actual: ~315k)
Batches: foundation, core-logic, testing

Archive change: `/opsx:archive`
```

## Error Handling

- **Agent failures**: Mark failed, keep worktree, continue with others
- **Merge conflicts**: Abort merge, skip bead, continue with others
- **Infrastructure failures**: Skip bead, report error, continue
- **Test failures**: Stop merges, offer revert/investigate options
- **User interruption**: Save state, report progress

## Critical Requirements

**NEVER**:
- Use `run_in_background: true` (blocks file writes)
- Create worktrees from within other worktrees (causes nesting)
- Use relative paths without cd to main repo first
- Skip test verification after merge
- Continue merging after regression detected

**ALWAYS**:
- Create worktrees from main repo root (cd to repo first)
- Use absolute paths constructed from `git rev-parse --show-toplevel`
- Test after EACH merge (sequential, not batched)
- Verify test summary line programmatically (check for "Passed:", not "Aborted:")
- Check ABORTED and UNKNOWN test counts (stop if > 0)
- Keep failed/conflicted worktrees for debugging
- Update state file after each phase

## Prerequisites

Check before starting: `bd list --ready --json` works, `git worktree --version` available, `./bin/run-tests.sh --help` exists, no existing `.beads/orchestrator-state.json`.
