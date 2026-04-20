---
name: tasks-orchestrator
description: Implement multiple ready tasks in parallel using isolated worktrees and spawned agents. Use when the user wants to execute multiple ready tasks from a change concurrently with sequential merging and regression testing after each merge.
---

Implement multiple ready tasks from an OpenSpec change in parallel using isolated worktrees and spawned agents. Merges happen sequentially with regression testing after each.

## Task lifecycle

Every task passes through these states:

```
ready → in progress (worktree + agent) → implementation complete (commit)
      → merged (with regression tests passing)
      → needs-review (file moved to tasks/closed/, status: needs-review)
      → reviewed → done
```

`needs-review` is the state right after a successful merge. A task only
becomes `done` after code review. See §2 Code Review.

## 1. Discovery & Selection

**Select the change:**

Prompt user for change name if not provided. Then scan:
- `openspec/changes/<name>/tasks/open/*.md` — candidates for the ready set
- `openspec/changes/<name>/tasks/closed/*.md` — check for `status: needs-review`

**If any `needs-review` tasks exist, resolve them FIRST** — see §2 Code Review.
New work does not start until pending reviews close.

Build the ready set from `tasks/open/`:
- A task is **ready** if `status: ready` OR if `status: blocked` with every
  `blocked-by:<dep>` relation resolved (i.e. `<dep>.md` is in `tasks/closed/`
  AND that file's `status` is `done` — `needs-review` does NOT unblock
  dependents)
- Everything else is **blocked** until prerequisites close

Display overview:
```
Change: <change-name>
Needs review: 1 (scaffold-module) ← review this before new work
Ready tasks: 3
  - setup-module — Create module structure
  - write-spec-file — Draft spec.md
  - characterize-current — Capture current behaviour
Blocked: 2 (by setup-module, write-spec-file)
```

Use **AskUserQuestion**:
- Any needs-review tasks: route to §2 Code Review first
- 0 ready tasks: Exit (suggest `/opsx-tasks generate` or close the change)
- 1-3 ready: auto-select with confirmation
- 4+ ready: multiSelect, max 5 per batch

**Batch strategy**: default to parallel unless the user specifies otherwise.
For sequential mode, spawn agents one at a time.

## 2. Code Review

Invoked when `status: needs-review` tasks are found in `tasks/closed/`. A
needs-review task is one whose primary implementation was merged with passing
regression tests but has not been reviewed. No dependent task should proceed
until its prerequisites are `status: done`, so clearing the review queue
unblocks further work.

### Reviewer mindset

The reviewer is **adversarial on purpose**. Implementing agents optimise to
satisfy the task as written and to make tests pass; that is not the same as
producing the right code. Passing tests and a green regression run are
necessary but not sufficient — they tell you nothing about design drift,
over-mocking, spec blind spots, or code quality.

Approach the review with three specific suspicions:

1. **Sub-par code from the implementing agent.** Agents gravitate toward
   solutions that look plausible and pass tests. Look for code smells:
   copy-paste, over-broad try/except, dead branches, implicit coupling,
   tests that re-state the implementation instead of verifying behaviour,
   shortcuts that cheat the verification step. Ask: "If I had written this
   from scratch, would it look like this?" If not, articulate the gap.

2. **Implementation drift from the design.** The implementation may pass
   its verification commands while quietly diverging from architecture or
   design decisions — renamed variables that leak across boundaries,
   responsibilities that crept into the wrong module, contracts that were
   weakened to make a test pass, extension points that were bypassed.
   Re-read the change's `architecture.md` and `design.md` and compare
   against the actual code, not the task description.

3. **Specs themselves may be wrong.** Implementation is the first time the
   design meets reality. If the work revealed friction — an awkward
   abstraction, a contract that doesn't compose, a case the spec didn't
   anticipate, a decision that now looks premature — that is a **signal
   from the code**, not a problem with the implementation. Be skeptical of
   the underlying specs. "Implemented as specified" is not a defence if
   the spec is the problem. Capture architectural signals that the design
   discussion missed as their own findings; they may warrant spec updates,
   new tasks, or even a pause to rethink before further work proceeds.

A clean review is possible; a review that finds nothing because the
reviewer deferred to the spec or the tests is a failure of the review
process, not a success.

### What the review covers

- **Testing**: Is the work adequately tested? Do tests exercise real code
  paths (behavioural / integration) rather than over-mock? Do tests cover
  the task's acceptance criteria and verification commands? Are there
  paths the tests silently skip? Would a subtle regression actually
  surface, or would the assertions still pass?
- **Best practices**: Is the Elisp idiomatic and modern? Correct use of
  `cl-lib`, closures, lexical binding, namespacing, docstrings? Any
  correctness, readability, or performance concerns? Any code that
  "works" but a human reviewer would reject?
- **Alignment with the change**: Does the work match the OpenSpec
  change's specs, architecture, and design? Does it stay within the
  task's stated scope? Are decisions consistent with neighbouring tasks,
  or has the task subtly broken a shared contract?
- **Signals against the spec**: Did the implementation reveal that a
  decision in `proposal.md`, `architecture.md`, `design.md`, or a spec
  file is wrong, incomplete, or worse than an alternative the agent was
  forced to invent? Call these out explicitly — they are the most
  valuable findings a review produces.

### How to review

Ask the user (via AskUserQuestion) whether to review inline (orchestrator
reads diffs and reports findings) or delegate to a reviewer agent. For a
reviewer agent, brief it with:
- The task file body
- The merge commit: `git log --merges --grep="Merge task <task-name>"`
  and the implementation commit under it
- Pointers to the change's `proposal.md`, `specs/`, `architecture.md`,
  `design.md` — marked as **reference material to pressure-test, not
  authority to defer to**
- The reviewer mindset and review concerns above
- An explicit instruction: "If you find nothing, explain what you looked
  for and why you ruled it out — silence is not a pass."

Run reviews one task at a time. Keep findings concrete (file:line,
specific concern, suggested fix). For spec-level findings, name the
artifact and section that needs revisiting.

### Findings become new tasks

Every review finding results in a **new task file** under
`openspec/changes/<change>/tasks/open/`. Each finding is its own task — do
not lump multiple issues together. Use `/opsx-tasks create` or write the
file directly with frontmatter:

```yaml
---
name: <finding-name>
description: <one-line summary>
change: <change-name>
status: ready         # or "blocked" if it has prerequisites
relations:
  - discovered-from: <reviewed-task-name>   # provenance
  - blocks: <downstream-task>               # if the fix must precede dependents
---
```

The `discovered-from` relation is mandatory — it preserves the audit trail
from review to remediation. If the finding must be resolved before
downstream work proceeds, add `blocks:` to pull the finding forward in the
dependency graph; otherwise the finding is informational and the reviewed
task can still flip to `done`.

**Spec-level findings** (§Reviewer mindset #3) are recorded the same way
but with additional handling:
- Task body names the artifact to revisit: `design.md §Decision <n>`,
  `architecture.md §<component>`, `specs/<path>` — and describes what
  the implementation revealed that the design didn't anticipate.
- If the signal invalidates downstream tasks' premises, add `blocks:`
  against those downstream tasks *and* raise it with the user before
  continuing — pausing to rework artifacts is often cheaper than
  implementing on a broken foundation.
- If the spec update is significant (not a clarification), recommend
  routing through `/opsx-continue` or explicit design-doc revisions
  rather than piecemeal task fixes.

### Closing the review

- **No findings**: flip reviewed task's frontmatter `status: needs-review`
  → `status: done`. File stays in `tasks/closed/`.
- **Findings raised, none blocking**: flip to `done`; the discovered-from
  tasks stand on their own in the open queue.
- **Findings raised, at least one blocking**: keep reviewed task at
  `needs-review` until the blocking discovered-from tasks close. This
  ensures downstream dependents stay blocked by the parent.

Only after every `needs-review` task is resolved (either `done` or explicit
user deferral) does the orchestrator proceed to §3 Baseline Capture for new
work.

## 3. Baseline Capture

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

## 4. Worktree & Agent Setup

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

## 5. Monitor Progress

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

## 6. Sequential Merging

Merge in completion order (sequential, not parallel — avoids conflicts and
enables precise regression detection):

```bash
cd /Users/jefffarr/emacs
git merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
```

**Conflicts**: Abort with `git merge --abort`, mark as "merge_conflict", keep
worktree, continue with next task. If manually resolved, MUST re-test before
continuing.

## 7. Test After Each Merge

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

## 8. Transition to needs-review

After successful merge + tests pass + no regression, the task's *primary
development* is done but it has not been reviewed yet. Move the task file to
`tasks/closed/` and set `status: needs-review`:

```bash
cd /Users/jefffarr/emacs

# Move file from tasks/open/ to tasks/closed/
mv "openspec/changes/$CHANGE/tasks/open/$TASK_NAME.md" \
   "openspec/changes/$CHANGE/tasks/closed/$TASK_NAME.md"
# (use plain mv, not git mv, if the tasks/ directory is untracked)

# Edit the moved file to set: status: needs-review

# Remove the worktree
git worktree remove "$WORKTREE_PATH"
```

Update state file. Do NOT yet re-evaluate `blocked-by` relations — a
`needs-review` task does not satisfy dependent tasks' blockers. Dependents
unblock only when the reviewed task reaches `status: done` (see §2 Code
Review).

**Keep worktrees if**: merge conflict, agent failed, or regression detected.

## 9. Final Snapshot

After the last successful merge, run a final test snapshot:
`./bin/run-tests.sh --snapshot`, save to `.orchestrator/final-${SESSION_ID}.txt`,
commit with a summary. Update state file with final test counts.

## 10. Summary Report

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
- Set a merged task directly to `status: done` — always go through
  `needs-review` first
- Treat a `needs-review` task as satisfying a downstream `blocked-by`
  relation — dependents unblock only when the reviewed task is `done`

**ALWAYS**:
- Create worktrees from main repo root (`cd` to repo first)
- Use absolute paths constructed from `git rev-parse --show-toplevel`
- Test after EACH merge (sequential, not batched)
- Verify test summary line programmatically
- Check ABORTED and UNKNOWN test counts (stop if > 0)
- Keep failed/conflicted worktrees for debugging
- Update state file after each phase
- At session start, check for `needs-review` tasks in `tasks/closed/` and
  resolve them before selecting new work (§2 Code Review)
- Every review finding becomes a new task with `discovered-from:` relation
- After a review task flips to `done`, re-evaluate dependent tasks'
  `blocked-by` status and flip newly-unblocked tasks to `ready`

## Prerequisites

Check before starting:
- `openspec/changes/<name>/tasks/open/` has at least one ready task
- `git worktree --version` available
- `./bin/run-tests.sh --help` exists
- No existing `.orchestrator/state.json` (previous session not finalised)
