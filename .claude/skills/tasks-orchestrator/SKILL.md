---
name: tasks-orchestrator
description: Implement multiple ready tasks in parallel using isolated worktrees and spawned agents. Use when the user wants to execute multiple ready tasks from a change concurrently with sequential merging and regression testing after each merge.
---

Implement multiple ready tasks from an OpenSpec change in parallel using isolated worktrees and spawned agents. Merges happen sequentially with regression testing after each.

## Task lifecycle

Every task passes through these states:

```
ready → in progress (agent, with or without worktree)
      → committed (inline) OR merged (worktree), regression tests passing
      → needs-review (file moved to tasks/closed/, status: needs-review)
      → reviewed → done
```

`needs-review` is the state right after a successful commit/merge. A task
only becomes `done` after code review. The review step always flips the
task to `done`, even when it produces blocking follow-ups (those live as
their own tasks with `discovered-from:` provenance; dependents get
repointed). See §2 Code Review.

**Inline vs worktree.** Both paths spawn an agent to do the work — the
only difference is setup. Straightforward tasks (docs edits, one-line
changes, single-call-site function edits) are handled **inline**: the
agent works in the main repo and commits directly, no worktree, no
merge. See §4 Task Triage. Larger or multi-module tasks go through the
worktree + agent flow (§6). Either path lands at `needs-review` and
goes through the same review (§2).

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

**Note on `.tasks/`.** The repo's root-level `.tasks/` directory holds
cross-cutting follow-ups that were externalised from prior changes
(see CLAUDE.md §"Cross-cutting follow-ups" and §2 "In-change vs
cross-cutting externalisation" below). These are deliberately **not**
part of the orchestrator's batch — do not include `.tasks/` items in
the ready set. They have their own triage cadence outside this skill.

## 2. Code Review

Invoked when `status: needs-review` tasks are found in `tasks/closed/`. A
needs-review task is one whose primary implementation was merged with passing
regression tests but has not been reviewed. No dependent task should proceed
until its prerequisites are `status: done`, so clearing the review queue
unblocks further work.

### Reviewer mindset

The reviewer is **rigorous and substantive, not contrarian**. Implementing
agents optimise to satisfy the task as written and to make tests pass;
that is not the same as producing the right code. Passing tests and a
green regression run are necessary but not sufficient — they tell you
nothing about design drift, over-mocking, spec blind spots, or code
quality. Review exists to catch what those signals miss.

"Rigorous" means reading the actual code against the actual design and
thinking hard about what could be wrong. "Not contrarian" means not
inventing objections to prove the review was thorough. A finding must
clear this bar:

> Would a thoughtful maintainer, familiar with this codebase, raise this
> in a PR review — and would the project be meaningfully worse if it
> shipped unchanged?

If the answer is no, don't raise it. Style preferences, bikeshed names,
speculative "future-proofing," re-litigations of settled decisions, and
nits that would churn a diff without changing behaviour all fail this
bar. A clean review is a valid outcome. If you find nothing, say what
you looked for and why you ruled it out — silence is not a pass, but
padding findings is worse than finding nothing.

Look hard in three specific directions:

1. **Sub-par code from the implementing agent.** Agents gravitate toward
   solutions that look plausible and pass tests. Look for real issues:
   copy-paste, over-broad try/except, dead branches, implicit coupling,
   tests that re-state the implementation instead of verifying behaviour,
   shortcuts that cheat the verification step. Ask: "If I had written this
   from scratch, would it look like this?" If not, articulate the gap —
   but only if the gap matters functionally or to maintainers, not just
   stylistically.

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
   from the code**, not a problem with the implementation. "Implemented
   as specified" is not a defence if the spec is the problem. Capture
   architectural signals that the design discussion missed; they may
   warrant spec updates, new tasks, or even a pause to rethink before
   further work proceeds.

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
- The reviewer mindset and review concerns above, including the
  signal/noise bar (thoughtful maintainer + meaningfully worse)
- An explicit instruction: "Rigorous, not contrarian. Raise findings
  that would matter in a real PR review. Skip style nits, speculative
  future-proofing, and re-litigation of settled decisions. If you find
  nothing, say what you looked for and why you ruled it out — silence
  is not a pass, but padding is worse than nothing."

Run reviews one task at a time. Keep findings concrete (file:line,
specific concern, suggested fix). For spec-level findings, name the
artifact and section that needs revisiting.

### Handling findings: inline fix vs new task

Findings split into three buckets based on the remediation cost:

**Inline fix during review** — choose this when the change is trivial
and unambiguous: typos, obviously-correct one-line fixes, a missing
docstring, a clearly-wrong comparator, a missed case in an already-
covered test file. The reviewer (orchestrator or reviewer agent)
makes the edit directly in the main repo, tangles if needed, re-runs
the task's verification commands, and records the fix in the reviewed
task's body under a **Review** section. **Run regression tests (§9)
after inline fixes** — treat them like any other commit.

- If the inline fix is trivial and self-evidently correct, the
  reviewed task can flip to `done` on the same pass (§Closing the
  review).
- If the inline fix is non-trivial enough that it could itself yield
  follow-up findings (touches unfamiliar code, adds new logic,
  changes a contract), leave the reviewed task at `needs-review` and
  re-queue it for another review pass. A fix that warrants its own
  review is **not** an inline fix — consider splitting it out as a
  new task instead.

**New task (follow-up)** — for everything that isn't a trivial inline
fix. Two destinations are possible (see §"In-change vs cross-cutting
externalisation" below for the decision rule):

- **In-change** (`openspec/changes/<change>/tasks/open/`) — work that
  belongs to the active change.
- **Cross-cutting** (`.tasks/` at repo root) — work the finding
  surfaced but that is genuinely external to the active change's
  scope. See CLAUDE.md §"Cross-cutting follow-ups" for repo
  conventions.

**Group clustered findings into a single task; split only when
findings are genuinely independent.** Specifically:

- **Group** when findings share an artifact cluster (same file,
  module, spec section, or a coordinated set of `design.md` +
  `architecture.md` + adjacent task files that need to move together)
  or form a coherent unit of work.
- **Split** when findings touch unrelated parts of the codebase, have
  different owners or dependencies, or would create conflicting
  diffs if batched together.

Grouping keeps the follow-up queue small, reduces the setup cost of
each fix, and produces coherent diffs instead of N small patches that
re-read the same context. One task per distinct concern, not one per
bullet point.

For an in-change follow-up, use `/opsx-tasks create` or write the
file directly:

```yaml
---
name: <finding-name>
description: <one-line summary>
change: <change-name>
status: ready         # or "blocked" if it has prerequisites
relations:
  - discovered-from: <reviewed-task-name>   # provenance, mandatory
---
```

For a cross-cutting follow-up at `.tasks/<finding-name>.md`, drop
`change:` and replace with `source:`:

```yaml
---
name: <finding-name>
description: <one-line summary>
status: ready         # or "blocked" if it has prerequisites
source: openspec/changes/<change>     # provenance survives archive
relations:
  - discovered-from: <reviewed-task-name>   # interpreted relative to source
---
```

The `discovered-from` relation is mandatory in both shapes — it
preserves the audit trail from review to remediation.

**In-change vs cross-cutting externalisation.** Default to in-change.
Externalise to `.tasks/` only when **all** of these hold:

- The finding is genuinely external to the active change's scope —
  the active change neither caused it, would benefit from it being
  fixed alongside, nor depends on it for its own correctness contract.
- The work would otherwise either (a) hold the active change open
  longer than its own work warrants, or (b) get buried inside the
  active change at archive time.
- The fix can stand on its own — it doesn't require the active
  change's design context to be implemented correctly.

Common externalisation triggers:

- Pre-existing failures uncovered by a regression sweep that pre-date
  the change and live in a different subsystem.
- Stale TODOs or dead code spotted in passing while reading
  neighbouring files.
- Defects in unrelated subsystems that surface as side-effects of
  the verification work.

Common keep-in-change triggers (do NOT externalise):

- Verification of the active change's own behaviour, even when
  deferred (e.g. an environmental blocker prevents running the test
  this session). The work is in-scope; it's just postponed.
- Bugs in code the active change introduced or modified.
- Spec / design / architecture artifact corrections that affect the
  active change's contract.
- Anything where the implementer needs the active change's design
  context loaded to do the work correctly.

When externalising, also update the parent (reviewed) task body's
follow-up list to note both the destination (`relocated to .tasks/`)
and the new path, so the audit trail stays readable. If a task was
originally created in-change and only on review is recognised as
out-of-scope, **move the file** (`git mv` from
`openspec/changes/<change>/tasks/open/<task>.md` to
`.tasks/<task>.md`) and rewrite its frontmatter from the in-change
shape to the cross-cutting shape in the same commit.

Externalised tasks are NOT picked up by the orchestrator's normal
batch flow — `.tasks/` is a separate backlog with its own triage
cadence. Do not include `.tasks/` items in the ready set computed in
§1 Discovery & Selection.

**Note:** `blocks:` is not a valid relation label in this repo (the
frontmatter validator rejects it). If the finding must be resolved
before a downstream task proceeds, **repoint the dependent's
`blocked-by:`** to the new follow-up task instead of marking the
follow-up with `blocks:`. See §Closing the review for the repoint
mechanics.

**Spec-level findings** (§Reviewer mindset #3) are recorded the same
way, with additional handling:
- Task body names the artifact to revisit: `design.md §Decision <n>`,
  `architecture.md §<component>`, `specs/<path>` — and describes what
  the implementation revealed that the design didn't anticipate.
- If the signal invalidates downstream tasks' premises, **repoint
  those dependents' `blocked-by:`** to the finding task *and* raise it
  with the user before continuing — pausing to rework artifacts is
  often cheaper than implementing on a broken foundation.
- If the spec update is significant (not a clarification), recommend
  routing through `/opsx-continue` or explicit design-doc revisions
  rather than piecemeal task fixes.

### Closing the review

**Reviewed tasks always flip to `done`**, regardless of findings. The
only exception is §Handling findings above: a non-trivial inline fix
that warrants its own review — in that case the task stays at
`needs-review` and goes through another review pass. Otherwise:

1. Append a **Review** section to the reviewed task's body. List:
   - Findings (one bullet each, with file:line and suggested fix)
   - Any inline fixes applied during review (with the fix commit SHA)
   - Follow-up task files created
   - Dependents whose `blocked-by:` was repointed, if any
2. Flip the reviewed task's frontmatter: `status: needs-review` →
   `status: done`. File stays in `tasks/closed/`.
3. If the review produced **blocking follow-ups** (findings that must
   resolve before a downstream task proceeds), **repoint the
   dependents**: edit each downstream task's frontmatter to replace
   `blocked-by: <reviewed-task>` with `blocked-by: <follow-up-1>` (and
   add additional `blocked-by:` entries for other blocking follow-ups).
   This keeps the dependency graph gating dependents without the
   reviewed task having to stay at `needs-review`.

**Why always flip to done:** Keeping the parent at `needs-review`
while waiting on follow-ups forces a re-review cascade — once the
follow-up closes, the parent has to be visited again just to flip
state, with no new code actually being reviewed. The follow-up already
gets review-equivalent scrutiny when it was opened; the parent's
second review is pure bookkeeping. Flipping to `done` immediately
preserves the audit trail (review notes live on the parent; findings
become tracked follow-up tasks with `discovered-from:` provenance)
without the cascade.

Only after every `needs-review` task is resolved (either `done` or
explicit user deferral) does the orchestrator proceed to §3 Baseline
Capture for new work.

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

## 4. Task Triage (Inline vs Worktree)

Before spawning worktrees, classify each selected task. Worktree setup
(`git worktree add` + `init-worktree-runtime.sh` + submodules) is ~30s
plus cleanup cost per task; for trivial changes that overhead dominates
the actual work. Agents still do the work either way — triage only
decides whether the agent runs in a worktree or directly in the main
repo.

**Handle inline (no worktree, still an agent) when ALL of these hold:**
- The change is self-contained and small: docs/README/CLAUDE.md edit,
  openspec artifact text edit, one-line fix, single-function edit with
  one call site, trivial test addition.
- Edits stay within one module (or one artifact file).
- The task body's Verification commands are fast (<60s) and scoped to
  the edited files.
- No other task in this batch touches the same files (conflict risk,
  since all inline work lands on the same branch).
- The task body is self-sufficient enough that an agent can execute it
  from the prompt without additional design context.

**Use a worktree when any of these hold:**
- Multi-file or multi-module changes.
- New module scaffolding, new test suites, or non-trivial refactoring.
- The task calls for design decisions best made in isolation.
- Tangling several `.org` files or writing non-trivial Elisp.
- Realistic risk of breaking neighbouring modules if scope spills.

**Bailout rule.** If an inline task turns out larger than triaged
(agent reports >2 files touched, verification takes far longer than
expected, or scope expanded during implementation), stop the inline
agent, revert any uncommitted work (`git stash` or `git reset`), and
reschedule the task through the worktree flow (§6). Triage is a cost
judgment, not a commitment.

Display the triage result:

```
Triage:
  Inline: 2 tasks (fix-typo-in-spec, update-readme)
  Worktree: 3 tasks (implement-core, wire-transient, add-tests)
```

**Execution order.** Process inline tasks first, **sequentially**
(they all commit to the same branch, so parallel inline runs would
race). Each inline task follows §5. Then proceed to §6 for the
worktree batch, which can run in parallel.

## 5. Inline Execution

For each inline-triaged task, running in the main repo (no worktree):

**Agent spawning** — same Agent tool, same template as §6 but with
the prompt adjusted for the inline context:

```
Agent(
  subagent_type: "general-purpose",
  description: "Implement task <task-name> (inline)",
  prompt: <see Inline Agent Prompt Template>,
  run_in_background: false
)
```

### Inline Agent Prompt Template

```markdown
Implement task <TASK_NAME> for change <CHANGE_NAME> directly in the main
repository at <REPO_ROOT>. This task was triaged as small enough to skip
the worktree flow — work directly on the current branch.

Read the task body:
<FULL_TASK_FILE_CONTENTS>

**Literate Programming**: Edit `.org` files (never the generated `.el`),
tangle with `./bin/tangle-org.sh`, commit both `.org` and `.el` together.

**Testing**: Run the Verification commands in the task body. They must
pass before you commit.

**Commit**: `git commit` with message
`"Implement task <TASK_NAME>: <description from frontmatter>"` and
Co-Authored-By. Commit directly to the current branch — no branch, no
merge.

**Scope discipline**: If the change turns out to require more than
~2 files or hits unexpected coupling, STOP. Report back without
committing so the orchestrator can reschedule through the worktree
flow. Do not expand scope to "make it work."
```

Store the agent in the state file with `"inline": true` and no
worktree/branch fields:

```json
{
  "task_name": "fix-typo-in-spec",
  "task_file": "openspec/changes/<change>/tasks/open/fix-typo-in-spec.md",
  "inline": true,
  "agent_task_id": "<Agent id>",
  "status": "in_progress",
  "regression_detected": false,
  "closed": false
}
```

**After agent completes:**

1. Verify a commit landed: `git log -1 --oneline`. If no commit, mark
   the task `failed` and surface to the user (either the agent bailed
   per scope discipline, or it errored).
2. **Run regression tests** (same logic as §9): `./bin/run-tests.sh
   --snapshot`, parse `test-results.txt`. On regression, revert
   (`git reset --hard HEAD~1`), mark task `failed`, and stop before
   starting the next inline task.
3. **Transition to needs-review** (same as §10): `mv` the task file
   from `tasks/open/` to `tasks/closed/`, flip frontmatter to
   `status: needs-review`.

Then move to the next inline task (or §6 once inline batch is done).

## 6. Worktree & Agent Setup

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

## 7. Monitor Progress

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

## 8. Sequential Merging

Merge in completion order (sequential, not parallel — avoids conflicts and
enables precise regression detection):

```bash
cd /Users/jefffarr/emacs
git merge --no-ff "$BRANCH_NAME" -m "Merge task $TASK_NAME: $DESCRIPTION"
```

**Conflicts**: Abort with `git merge --abort`, mark as "merge_conflict", keep
worktree, continue with next task. If manually resolved, MUST re-test before
continuing.

## 9. Test After Each Merge

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

## 10. Transition to needs-review

**This section covers the worktree path.** Inline tasks transition
themselves inside §5.

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

## 11. Final Snapshot

After the last successful merge, run a final test snapshot:
`./bin/run-tests.sh --snapshot`, save to `.orchestrator/final-${SESSION_ID}.txt`,
commit with a summary. Update state file with final test counts.

## 12. Summary Report

Display session results, broken out by execution path: inline commits,
worktree merges, conflicts, failures, test summary, cleanup status.

**If ready tasks remain (new ones unblocked by this batch):**

```
Batch Complete ✓

Inline: 2/2 tasks (fix-typo-in-spec, update-readme)
Worktree: 3/3 tasks merged (implement-core, wire-transient, add-tests)
Newly unblocked: 2 tasks (followup-x, followup-y)

Ready to start next batch?
```

**If all tasks complete:**

```
All Tasks Complete ✓

Total: 8/8 (3 inline, 5 worktree)
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
- Set a committed/merged task directly to `status: done` — always go
  through `needs-review` first
- Treat a `needs-review` task as satisfying a downstream `blocked-by`
  relation — dependents unblock only when the reviewed task is `done`
- Use `blocks:` in task frontmatter — the validator rejects it. Gate
  dependents by editing their `blocked-by:` instead
- Skip the worktree flow for tasks that turn out to be multi-file or
  cross-module once the agent starts working (see §4 bailout rule)
- Pull `.tasks/` items into the orchestrator's batch — they are a
  separate cross-cutting backlog, not part of any change's ready set
- Externalise an in-scope follow-up — verification of the active
  change's own behaviour (even when deferred on environmental
  blockers), bugs in code the change introduced, and spec/design
  corrections that affect the change's contract all stay in
  `openspec/changes/<change>/tasks/`

**ALWAYS**:
- Triage each task into inline vs worktree before setup (§4)
- Create worktrees from main repo root (`cd` to repo first)
- Use absolute paths constructed from `git rev-parse --show-toplevel`
- Process inline tasks sequentially (they share the main branch)
- Test after EACH commit/merge (sequential, not batched) — inline
  commits count
- Verify test summary line programmatically
- Check ABORTED and UNKNOWN test counts (stop if > 0)
- Keep failed/conflicted worktrees for debugging
- Update state file after each phase
- At session start, check for `needs-review` tasks in `tasks/closed/` and
  resolve them before selecting new work (§2 Code Review)
- Non-trivial review findings become new tasks with `discovered-from:`
  relation; trivial ones may be fixed inline during review. Group
  clustered findings into one task instead of fragmenting them
- Decide a follow-up task's destination at creation time: in-change
  (`openspec/changes/<change>/tasks/open/`) by default, or `.tasks/`
  at repo root when the finding is genuinely external to the active
  change's scope (see §2 "In-change vs cross-cutting externalisation"
  for the rule). When externalising a task that was originally
  created in-change, `git mv` it to `.tasks/` and rewrite the
  frontmatter (drop `change:`, add `source:`) in the same commit, and
  update the parent task body's follow-up list to note the new
  location
- Apply the signal/noise bar to findings: raise only what a thoughtful
  maintainer would flag in a PR review and that would make the project
  meaningfully worse if shipped unchanged
- Flip reviewed task to `done` after review, regardless of findings.
  When blocking follow-ups exist, repoint downstream dependents'
  `blocked-by:` to the follow-up tasks instead of leaving the parent
  at `needs-review`
- After a review task flips to `done`, re-evaluate dependent tasks'
  `blocked-by` status and flip newly-unblocked tasks to `ready`

## Prerequisites

Check before starting:
- `openspec/changes/<name>/tasks/open/` has at least one ready task
- `git worktree --version` available
- `./bin/run-tests.sh --help` exists
- No existing `.orchestrator/state.json` (previous session not finalised)
