---
name: "OPSX: To Beads"
description: Convert OpenSpec tasks to Beads issues for tracking
category: Workflow
tags: [workflow, beads, tracking, experimental]
---

Convert tasks from an OpenSpec change into Beads issues with proper linkage and tracking metadata.

**Input**: Optionally specify a change name (e.g., `/opsx:to-beads add-auth`). If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **Select the change**

   If a name is provided, use it. Otherwise:
   - Infer from conversation context if the user mentioned a change
   - Auto-select if only one active change exists
   - If ambiguous, run `openspec list --json` to get available changes and use the **AskUserQuestion tool** to let the user select

   Always announce: "Using change: <name>" and how to override (e.g., `/opsx:to-beads <other>`).

2. **Check if tasks.md exists**

   ```bash
   openspec status --change "<name>" --json
   ```

   Parse the JSON to find the "tasks" artifact:
   - If `status: "ready"` (not created yet): suggest creating tasks first with `/opsx:continue`
   - If `status: "done"` (exists): proceed to parse tasks
   - If `status: "blocked"`: show what's blocking and exit

3. **Verify change doesn't already have Beads**

   Read `.openspec.yaml` from the change directory. Check if `metadata.tracking` is set to "beads" or if `metadata.beads` array exists.

   If Beads already exist:
   - Show existing bead IDs
   - Ask user: "This change already has Beads. Do you want to: (1) View existing beads, (2) Add more beads from new tasks, (3) Recreate all beads"
   - Exit if user doesn't want to proceed

4. **Read and parse tasks.md**

   Read the tasks file from the change directory. Parse the checkbox format:
   - Pattern: `- [ ]` or `- [x]` followed by optional number `X.Y` and description
   - Extract: task number, description, completion status
   - Group by section headers (`## N. Group Name`)

   Create a task list structure:
   ```
   [
     { section: "1. Setup", number: "1.1", description: "Create module", done: false },
     { section: "1. Setup", number: "1.2", description: "Add deps", done: false },
     { section: "2. Core", number: "2.1", description: "Implement export", done: true },
     ...
   ]
   ```

5. **Show task summary and confirm**

   Display:
   ```
   ## Converting to Beads: <change-name>

   Found N tasks across M sections:
   - Section 1: X tasks (Y completed)
   - Section 2: X tasks (Y completed)

   Each bead will be created with:
   - External ref: opsx:<change-name>
   - Labels: openspec, <section-slug>
   - Notes: Task X.Y from <change>/tasks.md
   ```

   Use **AskUserQuestion** to confirm:
   - "Create N beads from these tasks?"
   - Options: "Yes, create all" | "Let me review tasks first" | "Cancel"

6. **Create Beads for each task**

   For each task in the parsed list:

   ```bash
   bd create "<number> <description>" \
     --description "## Task from OpenSpec Change\n\n**Change:** <change-name>\n**Section:** <section>\n**Status:** <done-or-pending>\n\n## Context\n\nSee design.md and specs/ in openspec/changes/<change-name>/" \
     --external-ref "opsx:<change-name>" \
     --labels "openspec,<section-slug>" \
     --notes "Task <number> from <change>/tasks.md#LXX" \
     --priority 2 \
     --silent
   ```

   Capture the bead ID from output (silent mode returns just the ID).

   **If task is already completed** (`- [x]`):
   - Create the bead
   - Immediately close it: `bd close <bead-id>`

   Track created beads:
   ```
   [
     { number: "1.1", bead_id: "emacs-a3f", status: "open" },
     { number: "1.2", bead_id: "emacs-a4g", status: "open" },
     { number: "2.1", bead_id: "emacs-a5h", status: "closed" },
     ...
   ]
   ```

7. **Update .openspec.yaml with bead metadata**

   Read the existing `.openspec.yaml`, merge in bead tracking metadata:

   ```yaml
   schema: spec-driven
   created: 2026-02-09
   metadata:
     tracking: beads
     beads:
       - id: emacs-a3f
         task: "1.1 Create module"
         status: open
       - id: emacs-a4g
         task: "1.2 Add deps"
         status: open
       - id: emacs-a5h
         task: "2.1 Implement export"
         status: closed
     beads_created: 2026-02-09T15:30:00Z
   ```

   Use the **Edit** tool to update `.openspec.yaml`.

8. **Optionally archive tasks.md**

   Use **AskUserQuestion**:
   - "Tasks.md has been converted to Beads. What should we do with tasks.md?"
   - Options:
     - "Keep as reference (add tracking note)" - Add header note to tasks.md
     - "Archive to tasks.md.archived" - Rename file
     - "Keep unchanged" - Leave as is

   If "Keep as reference":
   - Add to top of tasks.md:
     ```markdown
     > **Note**: These tasks are now tracked as Beads issues.
     > See `.openspec.yaml` for bead IDs or run: `bd search "opsx:<change-name>"`
     ```

9. **Show completion summary**

   Display:
   ```
   ## Beads Created: <change-name>

   Created N beads (M open, P closed):

   ### Open Beads
   - emacs-a3f: 1.1 Create module
   - emacs-a4g: 1.2 Add deps

   ### Closed Beads (already completed)
   - emacs-a5h: 2.1 Implement export

   ### Next Steps
   - View all beads: `bd list --label openspec`
   - Search this change: `bd search "opsx:<change-name>"`
   - Start working: `/bead-work emacs-a3f`
   - Apply change: `/opsx:apply` (will detect and use beads)
   ```

**Output Format**

Progress updates during creation:
```
Creating beads for <change-name>...
✓ Created emacs-a3f: 1.1 Create module
✓ Created emacs-a4g: 1.2 Add deps (closed - already done)
✓ Created emacs-a5h: 2.1 Implement export
...
```

**Guardrails**

- Do NOT create beads if change already has `metadata.tracking: beads` without asking first
- Do NOT proceed if tasks.md doesn't exist - suggest `/opsx:continue` instead
- Always confirm with user before creating beads (except if `--silent` flag is somehow provided)
- Preserve task completion status (`[x]` → create and close bead)
- Use consistent labeling: "openspec" + section-slug (lowercase, hyphenated)
- Store bidirectional linkage (beads → change, change → beads)
- Section slugs should be lowercase, kebab-case from section name (e.g., "1. Core Implementation" → "core-implementation")

**Error Handling**

- If `bd` command not found: suggest installing Beads CLI
- If bead creation fails: show error, ask if should continue with remaining tasks
- If .openspec.yaml write fails: warn user but show created bead IDs so they're not lost
- If tasks.md is empty or malformed: show parse errors and exit gracefully

**Integration with Other Skills**

This skill enables:
- `/opsx:apply` will detect beads and route to bead-based implementation
- `/opsx:verify` will query bead completion status instead of checkbox parsing
- Future `/bead-work` skill can reference back to change context
