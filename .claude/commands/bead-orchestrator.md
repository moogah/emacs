---
name: "Bead Orchestrator"
description: Implement multiple ready beads in parallel using isolated worktrees and spawned agents
category: Workflow
tags: [workflow, beads, parallel, experimental]
---

Orchestrate parallel implementation of multiple ready beads using isolated git worktrees and spawned agents. Each bead gets its own worktree and agent, implementations happen concurrently, then merges occur sequentially with regression testing after each merge.

**Input**: No arguments required. Command will discover ready beads and guide you through selection.

**Steps**

## 1. Discovery & Selection

Query ready beads from the Beads database:

```bash
bd list --ready --json
```

Parse JSON output to identify:
- All beads with status "open" (not closed)
- Filter to non-blocked beads (no `blockedBy` dependencies)
- Extract bead ID, title, description, dependencies

**Selection strategy**:
- If 0 ready beads: Show message "No ready beads found" and exit
- If 1-3 ready beads: Auto-select with confirmation
- If 4+ ready beads: Use **AskUserQuestion** for interactive selection

**Default parallelism**: 3 agents maximum (conservative, balanced)

User can override with explicit count if needed.

**Display ready beads**:
```
## Ready Beads

Available for implementation (no blockers):

1. emacs-abc1: Add session export
   Scope: ~2 hours
   Files: config/gptel/sessions/commands.el

2. emacs-abc2: Implement preset resolver
   Scope: ~3 hours
   Files: config/gptel/presets.el

3. emacs-abc3: Add tests for filesystem
   Scope: ~1 hour
   Files: config/gptel/test/sessions-test.el

4. emacs-xyz9: Update documentation
   Scope: ~30 minutes
   Files: README.md

Select beads to implement in parallel (max 3 recommended):
```

Use **AskUserQuestion** with multiSelect enabled:
- Header: "Select beads"
- Question: "Which beads should be implemented in parallel?"
- Options: Each ready bead as an option (label = bead ID + title)
- MultiSelect: true
- Max recommended: 3 (mention in question text)

## 2. Baseline Capture

Before starting parallel work, capture current test state for regression detection.

**Run test suite with snapshot**:
```bash
cd /Users/jefffarr/emacs
./bin/run-tests.sh --snapshot
```

This captures output to `test-results.txt` (default location for full suite).

**Copy baseline for comparison**:
```bash
TIMESTAMP=$(date +%s)
mkdir -p .beads/orchestrator
cp test-results.txt .beads/orchestrator/baseline-${TIMESTAMP}.txt
```

**Validation**:
- If tests fail: Report failures, use **AskUserQuestion** to ask if user wants to proceed anyway
  - Options: "Proceed despite failures" or "Cancel orchestration"
  - Note: Regression detection will still work, but baseline has existing failures
- Store baseline path in state file

**Display baseline capture**:
```
## Baseline Captured

Running test suite...
✓ Test baseline captured: .beads/orchestrator/baseline-1234567890.txt

Test Summary:
- Total tests: 127
- Expected: 127 passed
- Unexpected: 0 failed

Ready to start parallel implementation.
```

## 3. Worktree & Agent Setup

For each selected bead, create isolated worktree and spawn agent.

**Setup loop (sequential to avoid race conditions)**:

```bash
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
TIMESTAMP=$(date +%s)

for BEAD_ID in selected_beads; do
  # 1. Create unique worktree name
  WORKTREE_NAME="bead-${BEAD_ID}-${TIMESTAMP}"
  WORKTREE_PATH="$HOME/worktrees/$WORKTREE_NAME"

  # 2. Create worktree branching from current branch
  git worktree add "$WORKTREE_PATH" -b "$WORKTREE_NAME"

  # 3. Copy runtime packages (746MB, fast with rsync)
  ./bin/init-worktree-runtime.sh "$WORKTREE_PATH"

  # 4. Initialize submodules in worktree
  cd "$WORKTREE_PATH" && git submodule update --init

  # 5. Fetch full bead details
  BEAD_JSON=$(bd show "$BEAD_ID" --json)

  # 6. Track in state file
  # (JSON structure shown in section 3.1)
done
```

**State tracking**: Create `.beads/orchestrator-state.json`:

```json
{
  "session_id": "orch-1234567890",
  "baseline_snapshot": ".beads/orchestrator/baseline-1234567890.txt",
  "current_branch": "gptel-scoped-bash-tools",
  "max_parallel": 3,
  "beads": [
    {
      "bead_id": "emacs-abc1",
      "worktree_path": "~/worktrees/bead-emacs-abc1-1234567890",
      "branch_name": "bead-emacs-abc1-1234567890",
      "task_id": null,
      "status": "setup_complete",
      "regression_detected": false
    }
  ]
}
```

**Agent spawning** (parallel after setup completes):

For each bead, spawn agent using Task tool:

```
TaskCreate(
  subject: "Implement bead $BEAD_ID",
  activeForm: "Implementing bead $BEAD_ID",
  description: <agent prompt - see section 3.2>
)
```

Store returned task_id in state file.

### 3.1 Worktree Location

**Location**: `~/worktrees/bead-<BEAD_ID>-<TIMESTAMP>`

**Why separate from main worktrees?**
- Clear namespace for orchestrator-managed worktrees
- Separate from development worktrees (`~/emacs-*`)
- Easy to identify and bulk-clean if needed

**Create directory if needed**:
```bash
mkdir -p ~/worktrees
```

### 3.2 Agent Prompt Template

**Key challenge**: Agents cannot invoke skills directly (no Skill tool access).

**Solution**: Embed essential guidance from writing-elisp and emacs-literate-programming skills inline.

**Agent Prompt Structure**:

```markdown
Implement bead <BEAD_ID> in isolated worktree.

## Worktree Details

**Location**: <WORKTREE_PATH>
**Branch**: <BRANCH_NAME>
**Base branch**: <CURRENT_BRANCH>

## Bead Task

<FULL_BEAD_DESCRIPTION>

## Literate Programming Guidelines

**CRITICAL**: This Emacs config uses org-mode files as source of truth.

**Rules**:
- NEVER edit .el files directly - they are auto-generated and your changes will be overwritten
- ALWAYS edit .org files instead
- After editing .org files, tangle them to generate .el files

**Workflow**:
1. Find the .org file corresponding to the .el file you need to modify
2. Edit the .org file
3. Tangle with: `./bin/tangle-org.sh path/to/file.org`
4. The script validates elisp syntax automatically

**Required .org headers**:
```org
#+title: Module Name
#+property: header-args:emacs-lisp :tangle module-name.el
#+auto_tangle: y
```

**After tangling**:
- Commit BOTH .org and .el files together
- Keep them in sync

## Elisp Development Guidelines

**Always validate after writing complex functions**:

When writing elisp with 3+ nesting levels (cl-loop, multiple let*, lambdas), validate immediately:

```bash
./bin/tangle-org.sh file.org
```

This script tangles AND validates elisp syntax.

**Common patterns to validate immediately**:
- cl-loop with nested forms
- Functions >50 lines
- Complex backquote/unquote expressions
- Multiple nested when/if/cond forms

**Modern Elisp requirements**:
- Use lexical binding: `;;; -*- lexical-binding: t; -*-`
- Prefer cl-lib (use `cl-loop`, `cl-coerce`) over deprecated cl package
- Use `(require 'cl-lib)` when needed

**Keep changes minimal**:
- Follow existing code patterns in the codebase
- Don't add features beyond bead scope
- Don't refactor unrelated code
- Add comments only where logic isn't self-evident

## Testing

Run tests to verify your changes:

```bash
cd <WORKTREE_PATH>
./bin/run-tests.sh
```

If bead mentions specific test files or functions, run targeted tests:

```bash
./bin/run-tests.sh -d config/gptel
./bin/run-tests.sh -p "^test-sessions-"
```

Tests must pass before marking work complete.

## Commit Guidelines

When implementation is complete:

1. Stage your changes:
   ```bash
   git add <files>
   ```

2. Create commit with proper format:
   ```bash
   git commit -m "$(cat <<'EOF'
   Implement bead <BEAD_ID>: <Title>

   <Brief summary of changes>

   Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
   EOF
   )"
   ```

3. DO NOT push or merge - orchestrator will handle merging

## Success Criteria

Your work is complete when:
- ✓ All changes implemented per bead description
- ✓ .org files edited (not .el files directly)
- ✓ Both .org and .el files committed together
- ✓ Tests pass (./bin/run-tests.sh)
- ✓ Changes committed with proper message format
- ✓ Work follows existing codebase patterns

Report completion and wait for orchestrator to merge.

## Important Notes

- Stay in the worktree directory: <WORKTREE_PATH>
- Don't merge or push - orchestrator handles this
- Don't close the bead - orchestrator handles this
- Don't work on other beads - focus only on <BEAD_ID>
- If blocked or unclear, report the issue and wait for guidance
```

**Embedded context**:
- Bead description (self-contained)
- Worktree location
- Literate programming rules (extracted from emacs-literate-programming skill)
- Elisp validation guidelines (extracted from writing-elisp skill)
- Testing approach
- Commit format

## 4. Monitor Progress

Poll task statuses to detect completion or failures.

**Polling loop**:

```
While any beads have status in ["setup_complete", "in_progress"]:
  Sleep 30 seconds

  TaskList()

  For each task:
    If task.status == "completed":
      - Verify worktree has commits: git -C <worktree> log -1
      - If no commits: Mark as "failed", report to user
      - If has commits: Mark as "implementation_complete"
      - Report completion to user

    If task.status == "failed":
      - Mark bead status as "failed"
      - Report to user with error details
      - Keep worktree for debugging

    If task.status == "in_progress":
      - Update bead status to "in_progress"
      - Continue monitoring

  Update state file with latest statuses

  If all tasks are completed or failed:
    Exit monitoring loop
```

**Progress reporting**:

```
## Implementation Progress

✓ emacs-abc1: Complete (12 minutes)
✓ emacs-abc2: Complete (18 minutes)
⋯ emacs-abc3: In progress (23 minutes elapsed)

Waiting for remaining agents to complete...
```

**Handle agent failures**:
- Report failure to user
- Keep worktree for debugging
- Continue monitoring other agents
- Don't block entire orchestration on one failure

## 5. Sequential Merging

After agents complete, merge bead branches one-by-one to avoid conflicts.

**Why sequential despite parallel implementation?**
- Git doesn't support parallel merges to the same branch
- First-merge wins on conflicts
- Simpler, safer, cleaner merge history
- Testing after each merge identifies exactly which bead caused regression

**Merge order**: Process beads in completion order (first completed = first merged).

**Merge process for each implementation_complete bead**:

```bash
# Switch to main worktree
cd /Users/jefffarr/emacs

# Merge bead branch with no-fast-forward
git merge --no-ff "$BRANCH_NAME" -m "$(cat <<EOF
Merge bead $BEAD_ID: $TITLE

$SUMMARY

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
EOF
)"
```

**Conflict handling**:

```bash
if merge fails:
  # Abort the merge
  git merge --abort

  # Update state
  Mark bead status as "merge_conflict"
  Keep worktree for manual resolution

  # Report to user
  Report: "Merge conflict in bead $BEAD_ID"
  Report: "Worktree kept at: $WORKTREE_PATH"
  Report: "Resolve manually or skip this bead"

  # Continue with next bead
  Continue to next implementation_complete bead
```

**Success path**:

```bash
if merge succeeds:
  # Update state
  Mark bead status as "merged"

  # Proceed to testing (section 6)
```

**Display merge progress**:

```
## Merging Completed Work

✓ Merged: emacs-abc1 (Add session export)
✓ Merged: emacs-abc2 (Implement preset resolver)
⨯ Conflict: emacs-xyz9 (Update documentation)
  Location: ~/worktrees/bead-emacs-xyz9-1234567890
  Resolve manually or skip
```

## 6. Test After Each Merge

**CRITICAL**: Run tests and compare to baseline after EACH successful merge.

**Why test after each merge?**
- Immediate regression detection (user preference)
- Easy to identify culprit bead
- Can stop before additional bad merges
- Trade-off: Slower but much safer

**Test execution**:

```bash
cd /Users/jefffarr/emacs

# Run full test suite with snapshot
CURRENT_TIMESTAMP=$(date +%s)
./bin/run-tests.sh --snapshot -o .beads/orchestrator/current-${CURRENT_TIMESTAMP}.txt
```

**Regression detection**:

```bash
# Compare to baseline
diff -u .beads/orchestrator/baseline-*.txt \
        .beads/orchestrator/current-${CURRENT_TIMESTAMP}.txt \
        > .beads/orchestrator/regression-report.txt

# Check for new failures
NEW_FAILURES=$(grep "^+.*FAILED" .beads/orchestrator/regression-report.txt | wc -l)
NEW_ERRORS=$(grep "^+.*ERROR" .beads/orchestrator/regression-report.txt | wc -l)

if [ $NEW_FAILURES -gt 0 ] || [ $NEW_ERRORS -gt 0 ]; then
  # Regression detected!
  REGRESSION_DETECTED=true
fi
```

**Regression handling**:

If regressions detected:

1. **Stop processing new merges**: Prevent compounding regressions
2. **Let in-progress agents finish**: Their work can be merged manually later
3. **Keep all worktrees**: For debugging
4. **Report details**:
   ```
   ## ⚠️ Regression Detected

   Bead emacs-abc2 introduced test failures:

   Failed tests:
   - test-preset-resolver-chain (config/gptel/test/presets-test.el)
   - test-preset-fallback-behavior (config/gptel/test/presets-test.el)

   Regression report: .beads/orchestrator/regression-report.txt

   Options:
   1. Revert this merge: git revert HEAD
   2. Fix the issues in bead worktree
   3. Continue anyway (not recommended)

   All worktrees preserved for debugging.
   Orchestration paused.
   ```

5. **Offer to revert merge**:
   ```bash
   Use AskUserQuestion:
   - "Revert merge of emacs-abc2?"
   - Options: "Revert merge", "Keep merge and investigate", "Continue despite regression"
   ```

6. **Update state**:
   ```json
   {
     "beads": [
       {
         "bead_id": "emacs-abc2",
         "status": "merged",
         "regression_detected": true,
         "regression_report": ".beads/orchestrator/regression-report.txt"
       }
     ]
   }
   ```

7. **Exit orchestration**: Don't merge additional beads

**No regression path**:

If tests pass:
- Continue to next bead merge
- Update snapshot as new baseline for next comparison

**Display test results**:

```
## Testing After Merge

Bead: emacs-abc1

Running tests...
✓ All tests pass (127/127)
✓ No regressions detected

Proceeding to next merge.
```

## 7. Bead Closure & Cleanup

After successful merge with no regressions, close bead and clean up worktree.

**Close bead**:

```bash
bd close "$BEAD_ID" --comment "Implemented via parallel orchestrator"
```

**Automatic cleanup criteria** (ALL must be true):
- Bead status == "merged"
- No regressions detected (regression_detected == false)
- Tests passing

**Cleanup process**:

```bash
git worktree remove "$WORKTREE_PATH"
```

**Keep worktrees when**:
- Merge conflict (status == "merge_conflict")
- Agent failed (status == "failed")
- Regression detected (regression_detected == true)

**Cleanup tracking**:

Update state file with cleanup status:

```json
{
  "beads": [
    {
      "bead_id": "emacs-abc1",
      "status": "merged",
      "regression_detected": false,
      "bead_closed": true,
      "worktree_removed": true
    },
    {
      "bead_id": "emacs-xyz9",
      "status": "merge_conflict",
      "regression_detected": false,
      "bead_closed": false,
      "worktree_removed": false,
      "kept_reason": "merge_conflict"
    }
  ]
}
```

## Final Summary Report

Display comprehensive summary of orchestration session:

```
## Orchestration Complete

**Session**: orch-1234567890
**Duration**: 45 minutes
**Beads processed**: 4

### Successfully Merged (2)
✓ emacs-abc1: Add session export
  - Merged in 12 minutes
  - Tests passing
  - Bead closed
  - Worktree cleaned

✓ emacs-abc2: Implement preset resolver
  - Merged in 18 minutes
  - Tests passing
  - Bead closed
  - Worktree cleaned

### Merge Conflicts (1)
⨯ emacs-xyz9: Update documentation
  - Reason: Merge conflict
  - Worktree: ~/worktrees/bead-emacs-xyz9-1234567890
  - Action needed: Resolve manually and merge

### Agent Failures (1)
⨯ emacs-def4: Add integration tests
  - Reason: Agent encountered unclear requirements
  - Worktree: ~/worktrees/bead-emacs-def4-1234567890
  - Action needed: Review bead description, clarify requirements

### Cleanup Summary
- Removed: 2 worktrees (emacs-abc1, emacs-abc2)
- Kept: 2 worktrees (emacs-xyz9, emacs-def4)

### Next Steps
- Review kept worktrees for manual resolution
- Run: `bd list --ready` to see remaining work
- Consider: `/bead-implementation emacs-xyz9` for manual work

All state saved in: .beads/orchestrator-state.json
```

## Error Handling & Edge Cases

### Agent Timeouts
- **Timeout**: 2 hours per bead (reasonable for 1-4 hour tasks)
- **Action**: Mark as "failed", report to user, keep worktree
- **User options**: Continue with other beads, investigate timeout

### Agent Implementation Errors
- **Detection**: Agent reports error in task
- **Action**: Mark as "failed", keep worktree, continue with others
- **Reporting**: Show error message, suggest reviewing bead clarity

### Test Failures in Worktree
- **Agent responsibility**: Agent should report test failures
- **Orchestrator handling**: Can merge anyway (regression test will catch)
- **Why allow**: Agent might have partial work worth reviewing

### Multiple Merge Conflicts
- **Handling**: Skip conflicted bead, continue with others
- **Reporting**: Show all conflicts in summary
- **Preservation**: Keep all conflicted worktrees for manual resolution

### Infrastructure Failures

**Worktree creation fails**:
```
Error: git worktree add failed
Action: Skip bead, continue with others, report error
```

**Runtime copy fails**:
```
Error: init-worktree-runtime.sh failed
Action: Skip bead, continue with others, report error
```

**Test suite fails to run**:
```
Error: ./bin/run-tests.sh failed
Action: Cannot detect regressions, warn user
Options: Continue without regression detection, or abort
```

### Regression False Positives

**Test flakiness**:
```
Offer: "Rerun tests to confirm regression?"
Action: Run tests again, compare results
```

**User override**:
```
Offer: "Continue despite reported regression?"
Action: Allow user to proceed, mark regression as "acknowledged"
```

### User Interruption

**Ctrl-C during setup**:
- Clean up partially created worktrees
- Save state file with current progress
- Report which beads were affected

**Ctrl-C during monitoring**:
- Let agents continue in background (TaskList to check later)
- Save state file
- Report how to resume/check progress

**Ctrl-C during merging**:
- Abort current merge if in progress
- Save state file
- Report which beads merged, which remain

## Guardrails

- **Always validate prerequisites**: Check bd list succeeds, git worktree available
- **Never skip regression testing**: Test after EACH merge (user preference)
- **Never auto-cleanup failed work**: Keep worktrees for debugging
- **Never continue after regression**: Stop merging, preserve state
- **Always report clearly**: Show progress, errors, kept worktrees with reasons
- **Always save state**: Update state file after each phase
- **Never guess on conflicts**: Keep worktree, let user resolve manually
- **Never merge in parallel**: Sequential merging only (avoid conflicts)
- **Always embed skill guidance**: Agents can't invoke skills directly
- **Never timeout too quickly**: 2 hours gives agents time for 1-4 hour tasks

## Prerequisites Check

Before starting orchestration:

1. **Beads database initialized**:
   ```bash
   bd list --ready --json
   ```
   If fails: "Beads database not initialized. Run `bd init` first."

2. **Git worktree available**:
   ```bash
   git worktree --version
   ```
   If fails: "Git worktree not available. Upgrade git to 2.5+."

3. **Test suite works**:
   ```bash
   ./bin/run-tests.sh --help
   ```
   If fails: "Test runner not available. Check bin/run-tests.sh exists."

4. **Runtime initialization works**:
   ```bash
   ./bin/init-worktree-runtime.sh --help
   ```
   If fails: "Runtime initializer not available. Check bin/init-worktree-runtime.sh exists."

5. **Not already orchestrating**:
   ```bash
   [ ! -f .beads/orchestrator-state.json ]
   ```
   If exists: "Orchestration session already in progress. Review .beads/orchestrator-state.json or remove to start fresh."

## User Interaction Patterns

### Initial Selection

Use **AskUserQuestion** with:
- Header: "Select beads"
- Question: "Which beads should be implemented in parallel? (Max 3 recommended for resource management)"
- Options: Each ready bead (label = ID + short title, description = scope + files)
- MultiSelect: true

### Baseline Test Failures

Use **AskUserQuestion** with:
- Header: "Baseline failures"
- Question: "Baseline tests have failures. Proceed with orchestration?"
- Options:
  - "Proceed (regression detection still works)" (recommended)
  - "Cancel and fix tests first"
- Note: Regression detection compares diff, so works even with existing failures

### Regression Detected

Use **AskUserQuestion** with:
- Header: "Regression"
- Question: "Bead {bead_id} introduced test failures. How to proceed?"
- Options:
  - "Revert merge" (recommended)
  - "Keep merge and investigate"
  - "Continue despite regression" (not recommended)

## Output Format Examples

### Discovery Phase
```
## Discovering Ready Beads

Querying Beads database...
Found 5 ready beads (no blockers)

Ready Beads:
1. emacs-abc1: Add session export (~2 hours)
2. emacs-abc2: Implement preset resolver (~3 hours)
3. emacs-abc3: Add tests for filesystem (~1 hour)
4. emacs-def4: Update documentation (~30 min)
5. emacs-xyz9: Fix completion bug (~1 hour)

Select up to 3 for parallel implementation.
```

### Setup Phase
```
## Setting Up Worktrees

Creating isolated worktrees for 3 beads...

✓ emacs-abc1: Worktree created at ~/worktrees/bead-emacs-abc1-1234567890
  - Runtime copied (746MB in 45s)
  - Submodules initialized
  - Agent spawned (task-1)

✓ emacs-abc2: Worktree created at ~/worktrees/bead-emacs-abc2-1234567890
  - Runtime copied (746MB in 43s)
  - Submodules initialized
  - Agent spawned (task-2)

✓ emacs-abc3: Worktree created at ~/worktrees/bead-emacs-abc3-1234567890
  - Runtime copied (746MB in 46s)
  - Submodules initialized
  - Agent spawned (task-3)

All agents running. Monitoring progress...
```

### Monitoring Phase
```
## Implementation Progress

[12 minutes elapsed]

✓ emacs-abc3: Complete (8 minutes)
  - 2 files modified
  - 3 tests added
  - 1 commit

⋯ emacs-abc1: In progress (12 minutes)
  - Last activity: 2 minutes ago

⋯ emacs-abc2: In progress (12 minutes)
  - Last activity: 1 minute ago

Waiting for remaining agents...
```

### Merging Phase
```
## Merging Completed Work

[1/3] Merging emacs-abc3...
✓ Merged successfully
✓ Running tests... (30s)
✓ No regressions (127/127 passing)
✓ Bead closed
✓ Worktree removed

[2/3] Merging emacs-abc1...
✓ Merged successfully
✓ Running tests... (32s)
✓ No regressions (127/127 passing)
✓ Bead closed
✓ Worktree removed

[3/3] Merging emacs-abc2...
✓ Merged successfully
✓ Running tests... (35s)
⚠️ REGRESSION DETECTED

Failed tests:
- test-preset-resolver-chain
- test-preset-fallback-behavior

Orchestration paused.
```

## Integration Notes

This command introduces new patterns to the codebase:

1. **First use of Task tool for agent spawning**: Existing commands (/opsx:apply) use sequential execution. This is the first parallel agent pattern.

2. **Worktree management**: Existing scripts (init-worktree-runtime.sh) used for development worktrees. Now also used for orchestrator worktrees in separate namespace.

3. **Test-after-each pattern**: Existing test framework (run-tests.sh, snapshot mode) now used for regression detection in orchestration context.

4. **State tracking**: New JSON state file pattern for tracking multi-bead orchestration sessions.

5. **Skill guidance embedding**: New pattern of embedding skill guidance in agent prompts since agents can't invoke skills directly.

## Future Enhancements

Potential improvements for future iterations:

- **Parallel merging with conflict detection**: Advanced git workflow
- **Dynamic parallelism**: Adjust agent count based on system resources
- **Incremental testing**: Only run tests related to changed modules
- **Resume capability**: Resume interrupted orchestration sessions
- **Bead dependency resolution**: Automatically sequence dependent beads
- **Progress web UI**: Real-time progress dashboard
- **Agent resource limits**: CPU/memory constraints per agent
- **Smart baseline updates**: Update baseline after each successful merge
