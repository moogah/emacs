---
name: "OPSX: Apply"
description: Implement tasks from an OpenSpec change (Experimental)
category: Workflow
tags: [workflow, artifacts, experimental]
---

Implement tasks from an OpenSpec change.

**Input**: Optionally specify a change name (e.g., `/opsx:apply add-auth`). If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **Select the change**

   If a name is provided, use it. Otherwise:
   - Infer from conversation context if the user mentioned a change
   - Auto-select if only one active change exists
   - If ambiguous, run `openspec list --json` to get available changes and use the **AskUserQuestion tool** to let the user select

   Always announce: "Using change: <name>" and how to override (e.g., `/opsx:apply <other>`).

2. **Check status to understand the schema**
   ```bash
   openspec status --change "<name>" --json
   ```
   Parse the JSON to understand:
   - `schemaName`: The workflow being used (e.g., "spec-driven")
   - Which artifact contains the tasks (typically "tasks" for spec-driven, check status for others)

3. **Detect tracking mode (Beads vs Tasks.md)**

   Read `.openspec.yaml` from the change directory to check tracking mode:

   ```bash
   cat openspec/changes/<name>/.openspec.yaml
   ```

   Check for `metadata.tracking` field:
   - If `tracking: beads`: Use **Bead-based workflow** (proceed to step 3a)
   - If not present or `tracking: tasks`: Use **Task-based workflow** (proceed to step 3b)

   Store the tracking mode for use in later steps.

3a. **Get apply instructions (Bead-based workflow)**

   **Query beads for this change:**
   ```bash
   bd list --label openspec --long --limit 0 --json
   ```

   Filter results to beads with external_ref matching "opsx:<change-name>" (search in description or notes).

   Parse bead data:
   - Count total beads
   - Count open vs closed beads
   - Extract bead IDs and titles
   - Determine next bead to work on (first open bead by creation order)

   **Also get context files:**
   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   This returns context file paths (proposal, design, specs) - ignore task list since we're using beads.

   **Handle states:**
   - If no beads found: suggest running `/opsx:to-beads` to create beads from tasks
   - If all beads closed: congratulate, suggest archive
   - Otherwise: proceed to implementation

   **Skip to step 4** (Read context files)

3b. **Get apply instructions (Task-based workflow)**

   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   This returns:
   - Context file paths (varies by schema)
   - Progress (total, complete, remaining)
   - Task list with status
   - Dynamic instruction based on current state

   **Handle states:**
   - If `state: "blocked"` (missing artifacts): show message, suggest using `/opsx:continue`
   - If `state: "all_done"`: congratulate, suggest archive
   - Otherwise: proceed to implementation

4. **Read context files**

   Read the files listed in `contextFiles` from the apply instructions output.
   The files depend on the schema being used:
   - **spec-driven**: proposal, specs, design, tasks
   - Other schemas: follow the contextFiles from CLI output

5. **Show current progress**

   Display depends on tracking mode:

   **Bead-based:**
   ```
   ## Implementing: <change-name> (beads tracking)

   **Progress:** N/M beads complete
   **Open beads:**
   - emacs-a3f: 1.1 Create module
   - emacs-a4g: 1.2 Add deps

   **Closed beads:**
   - emacs-a5h: 2.1 Implement export

   Starting with: emacs-a3f
   ```

   **Task-based:**
   ```
   ## Implementing: <change-name> (tasks.md)

   **Progress:** N/M tasks complete
   **Remaining tasks:**
   - [ ] 1.1 Create module
   - [ ] 1.2 Add deps
   ```

6. **Implement work (loop until done or blocked)**

   **Bead-based workflow:**

   For each open bead (in creation order):
   - Show bead details: `bd show <bead-id>`
   - Extract task description and context
   - Make the code changes required
   - Keep changes minimal and focused
   - Close the bead: `bd close <bead-id> --comment "Implemented: <summary>"`
   - Update `.openspec.yaml` bead status to "closed"
   - Continue to next bead

   **Task-based workflow:**

   For each pending task:
   - Show which task is being worked on
   - Make the code changes required
   - Keep changes minimal and focused
   - Mark task complete in the tasks file: `- [ ]` → `- [x]`
   - Continue to next task

   **Pause if (both modes):**
   - Task is unclear → ask for clarification
   - Implementation reveals a design issue → suggest updating artifacts
   - Error or blocker encountered → report and wait for guidance
   - User interrupts

7. **On completion or pause, show status**

   **Bead mode:**
   - Beads completed this session
   - Overall progress: "N/M beads closed"
   - If all done: suggest archive
   - If paused: explain why and wait for guidance

   **Task mode:**
   - Tasks completed this session
   - Overall progress: "N/M tasks complete"
   - If all done: suggest archive
   - If paused: explain why and wait for guidance

**Output During Implementation**

**Bead-based tracking:**
```
## Implementing: <change-name> (beads tracking)

Working on bead emacs-a3f (1/5): 1.1 Create module
[...implementation happening...]
✓ Bead closed: emacs-a3f

Working on bead emacs-a4g (2/5): 1.2 Add deps
[...implementation happening...]
✓ Bead closed: emacs-a4g
```

**Task-based tracking:**
```
## Implementing: <change-name> (tasks.md)

Working on task 3/7: <task description>
[...implementation happening...]
✓ Task complete

Working on task 4/7: <task description>
[...implementation happening...]
✓ Task complete
```

**Output On Completion**

**Bead-based:**
```
## Implementation Complete

**Change:** <change-name>
**Tracking:** Beads
**Progress:** 5/5 beads closed ✓

### Completed This Session
- ✓ emacs-a3f: 1.1 Create module
- ✓ emacs-a4g: 1.2 Add deps
...

All beads closed! You can archive this change with `/opsx:archive`.
```

**Task-based:**
```
## Implementation Complete

**Change:** <change-name>
**Tracking:** Tasks.md
**Progress:** 7/7 tasks complete ✓

### Completed This Session
- [x] Task 1
- [x] Task 2
...

All tasks complete! You can archive this change with `/opsx:archive`.
```

**Output On Pause (Issue Encountered)**

```
## Implementation Paused

**Change:** <change-name>
**Tracking:** <beads|tasks.md>
**Progress:** 4/7 items complete

### Issue Encountered
<description of the issue>

**Options:**
1. <option 1>
2. <option 2>
3. Other approach

What would you like to do?
```

**Guardrails**
- Keep going through work items (beads or tasks) until done or blocked
- Always read context files before starting (from the apply instructions output)
- If work item is ambiguous, pause and ask before implementing
- If implementation reveals issues, pause and suggest artifact updates
- Keep code changes minimal and scoped to each work item
- **Bead mode**: Close bead and update `.openspec.yaml` immediately after completing
- **Task mode**: Update task checkbox immediately after completing: `- [ ]` → `- [x]`
- Pause on errors, blockers, or unclear requirements - don't guess
- Use contextFiles from CLI output, don't assume specific file names
- **Bead mode**: Always query current bead status with `bd show` before working on it
- **Bead mode**: Always update both the bead (close) and `.openspec.yaml` (metadata.beads status)

**Fluid Workflow Integration**

This skill supports the "actions on a change" model:

- **Can be invoked anytime**: Before all artifacts are done (if tasks exist), after partial implementation, interleaved with other actions
- **Allows artifact updates**: If implementation reveals design issues, suggest updating artifacts - not phase-locked, work fluidly
