---
name: "Bead Orchestrator"
description: Implement multiple ready beads in parallel using isolated worktrees and spawned agents
category: Workflow
tags: [workflow, beads, parallel, experimental]
---

Implement multiple ready beads in parallel using isolated worktrees and spawned agents. Merges happen sequentially with regression testing after each.

## 1. Discovery & Selection

```bash
bd list --ready --json
```

Parse JSON to find beads with status "open" and no `blockedBy` dependencies.

**Selection**:
- 0 beads: Exit with message
- 1-3 beads: Auto-select with confirmation
- 4+ beads: Use AskUserQuestion (multiSelect: true, max 5 recommended)

## 2. Baseline Capture

```bash
./bin/run-tests.sh --snapshot
TIMESTAMP=$(date +%s)
mkdir -p .beads/orchestrator
cp test-results.txt .beads/orchestrator/baseline-${TIMESTAMP}.txt
```

Parse `test-results.txt` for: total tests, unexpected failures, aborted, unknown counts.

If baseline has failures: Use AskUserQuestion to confirm proceeding (regression detection still works by comparing diffs).

## 3. Worktree & Agent Setup

For each bead (sequential setup to avoid race conditions):

```bash
WORKTREE_PATH=".worktrees/bead-${BEAD_ID}-$(date +%s)"
git worktree add "$WORKTREE_PATH" -b "bead-${BEAD_ID}-$(date +%s)"
./bin/init-worktree-runtime.sh "$WORKTREE_PATH"
git -C "$WORKTREE_PATH" submodule update --init
```

**CRITICAL**: Worktree path MUST be `.worktrees/` (relative, inside repo) for permissions inheritance. Never use absolute paths.

Ensure `.worktrees/` is gitignored.

Create `.beads/orchestrator-state.json` tracking: session_id, baseline info, bead statuses (setup_complete → in_progress → implementation_complete → merged).

**Agent spawning** (use Task tool, NOT TaskCreate):

```
Task(
  subagent_type: "general-purpose",
  description: "Implement bead $BEAD_ID",
  prompt: <see Agent Prompt Template>,
  run_in_background: false  # CRITICAL: Must be false for file write access
)
```

**NEVER use `run_in_background: true`** - blocks file writes.

### Agent Prompt Template

Agents can't invoke skills, so embed essential guidance inline:

```markdown
Implement bead <BEAD_ID> in worktree <WORKTREE_PATH> (branch: <BRANCH_NAME>).

<FULL_BEAD_DESCRIPTION>

**Literate Programming**: NEVER edit .el files directly - edit .org files then tangle with `./bin/tangle-org.sh path/to/file.org`. Commit both .org and .el together.

**Elisp**: Validate complex functions (3+ nesting, cl-loop, >50 lines) immediately with tangle script. Use lexical binding, cl-lib (not deprecated cl).

**Testing**: Run `./bin/run-tests.sh` (or targeted with `-d config/foo` or `-p "^test-*"`). Must pass before completing.

**Commit**: Format as "Implement bead <BEAD_ID>: <Title>" with Co-Authored-By. DON'T push/merge/close bead - orchestrator handles this.

**Success Criteria**: Changes implemented, .org edited (not .el), both committed, tests pass, follows existing patterns.
```

## 4. Monitor Progress

Poll every 30s with TaskList. On completion, verify commits exist (`git -C <worktree> log -1`). Update state file. Continue until all tasks complete or fail. Keep failed worktrees for debugging.

## 5. Sequential Merging

Merge in completion order (sequential, not parallel - avoids conflicts, enables precise regression detection).

```bash
git merge --no-ff "$BRANCH_NAME" -m "Merge bead $BEAD_ID: $TITLE"
```

**Conflicts**: Abort with `git merge --abort`, mark as "merge_conflict", keep worktree, continue with next bead. If manually resolved, MUST re-test before continuing.

## 6. Test After Each Merge

**MANDATORY after each merge**: Run `./bin/run-tests.sh --snapshot`. Parse test-results.txt for summary line (starts with "Passed:", "Aborted:", or "Failed:"). Extract: unexpected, ABORTED, UNKNOWN counts.

**Stop orchestration if**:
- Summary line starts with "Aborted:" (incomplete run)
- Unexpected > 0
- ABORTED count > 0 (runtime errors)
- UNKNOWN count > 0 (framework problems)

Save snapshot: `.beads/orchestrator/after-${BEAD_ID}-${TIMESTAMP}.txt`. Update state file with test counts.

**On regression**: Stop merges, keep worktrees, use AskUserQuestion for revert/investigate/continue options.

## 7. Cleanup

After successful merge + tests pass + no regression: Close bead (`bd close "$BEAD_ID"`), remove worktree (`git worktree remove "$WORKTREE_PATH"`), update state.

**Keep worktrees if**: merge conflict, agent failed, or regression detected.

## 8. Final Snapshot

After last successful merge, run final test snapshot: `./bin/run-tests.sh --snapshot`, save to `.beads/orchestrator/final-${SESSION_ID}.txt`, commit with summary. Update state file with final test counts.

## 9. Summary Report

Display session results: successfully merged (count, duration), conflicts (worktree paths), failures (worktree paths), test summary (baseline vs final), cleanup status, next steps.

## Error Handling

- **Agent failures**: Mark failed, keep worktree, continue with others
- **Merge conflicts**: Abort merge, skip bead, continue with others
- **Infrastructure failures**: Skip bead, report error, continue
- **Test failures**: Stop merges, offer revert/investigate options
- **User interruption**: Save state, report progress

## Critical Requirements

**NEVER**:
- Use `run_in_background: true` (blocks file writes)
- Use absolute worktree paths (use `.worktrees/bead-*` inside repo)
- Skip test verification after merge
- Continue merging after regression detected

**ALWAYS**:
- Test after EACH merge (sequential, not batched)
- Verify test summary line programmatically (check for "Passed:", not "Aborted:")
- Check ABORTED and UNKNOWN test counts (stop if > 0)
- Keep failed/conflicted worktrees for debugging
- Update state file after each phase

## Prerequisites

Check before starting: `bd list --ready --json` works, `git worktree --version` available, `./bin/run-tests.sh --help` exists, no existing `.beads/orchestrator-state.json`.
