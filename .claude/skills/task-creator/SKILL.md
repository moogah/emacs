---
name: task-creator
description: Manage ad-hoc task files in `.tasks/` that are not tied to an OpenSpec change. Use when listing, creating, updating, or promoting standalone tasks — including work captured for later that doesn't yet justify a full change proposal.
---

Manage ad-hoc task files in the repository's `.tasks/` directory. These are
tasks that either don't need a full OpenSpec change, or don't yet have one
defined. Format aligns with `opsx-tasks` so promotion into an OpenSpec change
requires no restructuring.

**Input**: A subcommand and optionally a task name:
- `list` — show task status overview
- `show <task-name>` — display a single task's full content
- `create` — create a new ad-hoc task
- `update <task-name>` — update a task's status or content
- `promote <task-name> <change-name>` — promote the task into an OpenSpec
  change's `tasks/open/` directory

If no subcommand is given, default to `list`.

## When to use this skill vs. `opsx-tasks`

Use `task-creator` when:
- The work doesn't belong to any current OpenSpec change
- The work might eventually become a change but hasn't been scoped yet
- A full change proposal would be boilerplate-heavy with little value
  (small bugs, follow-ups, standalone chores)

Use `opsx-tasks` when the work belongs to an active change under
`openspec/changes/<name>/`.

## Subcommands

### list

Show task status for `.tasks/`.

1. Scan `.tasks/open/*.md` and `.tasks/closed/*.md`, read frontmatter
2. Resolve dependencies: if a `blocked-by` task is in `.tasks/closed/`,
   update status of the dependent from `blocked` to `ready`
3. Display:

```
## Ad-hoc Tasks

### Open (X)
| Name            | Description              | Status  | Blocked By      |
|-----------------|--------------------------|---------|-----------------|
| fix-tangle-bug  | Workaround for bad paren | ready   | —               |

### Closed (Y)
| Name          | Description             |
|---------------|-------------------------|
| rename-hook   | Rename pre-save hook    |

Total: Z tasks (X open, Y closed)
```

### show

Display a task's full content.

1. Find the task file in `.tasks/open/` or `.tasks/closed/`
2. Display the full file content (frontmatter + body)

### create

Create a new ad-hoc task.

1. Ask for: task name (kebab-case), description, and whether it's blocked
   by anything
2. If the user describes what the task should do, generate the full body
   following the task content guidelines below
3. Write to `.tasks/open/<name>.md`
4. If this task has `enables:` relations, remind the user the referenced
   task may need its status updated

### update

Update a task's status or content.

1. Find the task file
2. Apply the requested change:
   - **Status change**: Update frontmatter `status` field
   - **Move to closed**: Move file from `.tasks/open/` to `.tasks/closed/`,
     set `status: done`
   - **Reopen**: Move file from `.tasks/closed/` to `.tasks/open/`, set
     `status: ready`
   - **Add relation**: Append to `relations` list
   - **Edit content**: Update the markdown body
3. If completing a task, check whether it unblocks other tasks in `.tasks/`
   and update their status

### promote

Promote an ad-hoc task into an OpenSpec change. This is the bridge for when
a task outgrows its ad-hoc status, or when an existing change is opened
that the task naturally belongs to.

1. Verify the change directory exists at `openspec/changes/<change-name>/`
2. Verify `openspec/changes/<change-name>/tasks/open/` exists (create if
   missing)
3. Move the file: `.tasks/open/<task-name>.md` →
   `openspec/changes/<change-name>/tasks/open/<task-name>.md`
4. Add the `change: <change-name>` field to the task's frontmatter
5. If the task has `relations` referencing other `.tasks/` entries, warn
   the user — cross-boundary relations are not supported. Suggest either
   promoting those tasks too, or dropping the relations.
6. Report the new path

## Task File Format

Each task is a markdown file at `.tasks/open/<name>.md` or
`.tasks/closed/<name>.md`:

```markdown
---
name: <short-kebab-id>
description: <one-line human-readable summary>
status: ready
relations:
  - blocked-by:<other-task-name>
  - discovered-from:<other-task-name>
---

<rich markdown body>
```

### Frontmatter fields

- **name**: Short machine-readable identifier, kebab-case
  (e.g., `fix-tangle-paren-error`)
- **description**: One-line human-readable summary of the task
- **status**: One of `blocked`, `ready`, `needs-review`, `done`
- **relations**: List of labeled references to other `.tasks/` entries:
  - `blocked-by:<name>` — cannot start until the named task is done
  - `discovered-from:<name>` — spawned during work on another task
  - `enables:<name>` — completing this unblocks the named task

**No `change:` field.** That field is added only when a task is promoted
into an OpenSpec change (see `promote` subcommand).

**No cross-boundary relations.** Ad-hoc tasks cannot reference tasks inside
OpenSpec changes (or vice versa). If a dependency exists, promote the
ad-hoc task first.

### Markdown body

The body SHOULD be self-contained. An agent should be able to implement
the task by reading only this file. Ad-hoc tasks may have lighter sections
than opsx tasks (no design.md to extract from), but the structure is
shared so promoted tasks need no restructuring.

Body structure:

```markdown
## Files to modify
- path/to/file.org (new|modify)
- path/to/file.el (tangled from above)

## Implementation steps
1. [Specific actionable step with details]
2. [Another step — include code patterns if relevant]

## Design rationale
[WHY — the reasoning behind this task]
[Include any non-obvious context, constraints, or tradeoffs]

## Verification
- Tangle and validate: `./bin/tangle-org.sh <file>`
- Run tests: `./bin/run-tests.sh -d <dir>` (or pattern, if applicable)
- [Specific scenarios / tests that should pass]
- [Acceptance criteria — what "done" means]

## Context
[Any pointers: related files, related issues, relevant commits]
```

Omit sections that aren't relevant (e.g., a docs-only task may not need
"Design rationale"). Keep the body focused.

## Task Lifecycle

- New tasks are created in `.tasks/open/<name>.md` with status `ready` or
  `blocked`
- When a task is completed, move it to `.tasks/closed/<name>.md` and set
  status to `done`
- If work on a task reveals new work, create a new task with
  `discovered-from:<original>`
- If a task needs review, set status to `needs-review`
- To reopen a closed task, move it back to `.tasks/open/` and set status
  to `ready`
- To promote a task into an OpenSpec change, use the `promote` subcommand

## Guardrails

- **Self-contained**: Include enough context that the task is actionable
  without external lookup
- **Actionable**: Each task must have concrete implementation steps or a
  clear investigation goal
- **Scoped**: Target 1-4 hours per task (one session)
- **Verified**: Each task must have clear acceptance criteria
- **Sequenced**: Use `blocked-by` relations where order matters
- **No cross-boundary relations**: Promote first if a dependency exists
- **Literate programming**: For tasks touching `.org` files, remind the
  implementer to tangle (`./bin/tangle-org.sh`) and commit both `.org`
  and `.el`
