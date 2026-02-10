---
name: opsx:to-beads
description: Convert OpenSpec tasks to Beads issues for tracking. Use when you want to track OpenSpec change implementation through Beads instead of tasks.md checkboxes.
license: MIT
compatibility: Requires openspec CLI and Beads (bd) CLI.
metadata:
  author: Jeff Farr
  version: "1.0"
---

Convert tasks from an OpenSpec change into Beads issues with proper linkage and tracking metadata.

**Input**: Optionally specify a change name. If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **Select the change**

   If a name is provided, use it. Otherwise:
   - Infer from conversation context if the user mentioned a change
   - Auto-select if only one active change exists
   - If ambiguous, run `openspec list --json` to get available changes and use the **AskUserQuestion tool** to let the user select

   Always announce: "Using change: <name>" and how to override.

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

   Create a task list structure with section context.

5. **Show task summary and confirm**

   Display a summary of found tasks across sections showing completion status.

   Use **AskUserQuestion** to confirm creating beads.

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

   Capture the bead ID from output.

   **If task is already completed** (`- [x]`):
   - Create the bead
   - Immediately close it: `bd close <bead-id>`

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
     beads_created: 2026-02-09T15:30:00Z
   ```

   Use the **Edit** tool to update `.openspec.yaml`.

8. **Optionally archive tasks.md**

   Use **AskUserQuestion** to determine what to do with tasks.md:
   - "Keep as reference (add tracking note)" - Add header note to tasks.md
   - "Archive to tasks.md.archived" - Rename file
   - "Keep unchanged" - Leave as is

9. **Show completion summary**

   Display created beads organized by status (open/closed) with next steps:
   - View all beads command
   - Search this change command
   - Start working command
   - Apply change command

**Guardrails**

- Do NOT create beads if change already has `metadata.tracking: beads` without asking first
- Do NOT proceed if tasks.md doesn't exist - suggest `/opsx:continue` instead
- Always confirm with user before creating beads
- Preserve task completion status (`[x]` → create and close bead)
- Use consistent labeling: "openspec" + section-slug (lowercase, hyphenated)
- Store bidirectional linkage (beads → change, change → beads)

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
