---
name: "OPSX: Create Beads"
description: Generate self-contained Beads issues from OpenSpec design and specs
category: Workflow
tags: [workflow, beads, implementation, experimental]
---

Generate self-contained, actionable Beads issues from OpenSpec design.md and specs/. Each bead embeds all necessary context for implementation.

**Input**: Optionally specify a change name (e.g., `/opsx:create-beads add-auth`). If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **Select the change**

   If a name is provided, use it. Otherwise:
   - Infer from conversation context if the user mentioned a change
   - Auto-select if only one active change exists
   - If ambiguous, run `openspec list --json` to get available changes and use the **AskUserQuestion tool** to let the user select

   Always announce: "Using change: <name>" and how to override (e.g., `/opsx:create-beads <other>`).

2. **Verify prerequisites**

   Check that design.md exists and is complete:
   ```bash
   openspec status --change "<name>" --json
   ```

   If design artifact is not done:
   - Show message: "Design not ready. Run `/opsx:continue` to create design.md first."
   - Exit

   If beads already exist (check `.openspec.yaml` for `metadata.beads`):
   - Show existing bead IDs
   - Use **AskUserQuestion**: "Beads already exist. What do you want to do?"
     - "View existing beads" → show bd list, exit
     - "Add more beads from updated design" → continue, skip existing sections
     - "Recreate all beads" → close existing, create new
     - "Cancel" → exit

3. **Read design, specs, and tests**

   Read all context files:
   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   Read from `contextFiles`:
   - design.md (required)
   - specs/**/*.md (if exist)
   - proposal.md (for high-level context)
   - architecture.md (if exists - for test info)
   - tests/** (if exist - for test coverage)

   Parse design.md structure:
   - Extract sections (## headings)
   - Extract subsections (### headings)
   - Extract key decisions, rationale, patterns
   - Identify dependencies between sections

4. **AI-assisted bead generation**

   For each logical chunk in design.md (typically ## or ### sections):

   Generate a self-contained bead structure:
   ```
   Title: [Action-oriented, specific]

   Description:
   [One-line summary]

   Files to modify:
   - [Specific file paths inferred from design + specs]

   Implementation steps:
   1. [Concrete, actionable step]
   2. [Another step with specifics]
   3. [Include code patterns if mentioned in design]

   Design rationale: [WHY - extracted from design.md]
   [Embed the architectural context needed to understand this bead]

   Design pattern: [Pattern to follow]
   [Reference similar code if design mentions examples]

   Verification:
   - [How to test this works]
   - [Acceptance criteria from specs if available]
   - [What "done" looks like]

   Context: design.md § [section reference]
   ```

   **Granularity guidance:**
   - Target: 1-4 hours of work per bead (one focused session)
   - If section feels larger: split into multiple beads
   - If section feels smaller: combine with related sections
   - Ask AI: "Can this be completed in one session without dependencies?"

   **Context extraction:**
   - Don't reference design.md - EXTRACT and EMBED relevant context
   - Include WHY (rationale) not just WHAT (steps)
   - Include patterns/examples mentioned in design
   - Include integration points from design
   - Extract acceptance criteria from specs if available

   **Dependencies:**
   - Identify beads that must be sequenced
   - Infrastructure before features
   - Shared utilities before dependents
   - Note: Will add with `bd dep add` after creation

5. **Generate execution plan**

   Group beads into batches with dependencies and strategy:

   ```yaml
   execution:
     batches:
       - id: foundation
         strategy: parallel  # or sequential
         beads:
           - emacs-abc1
       - id: core-logic
         strategy: sequential
         depends_on: [foundation]
         beads:
           - emacs-def4
   ```

   **Batch strategies:**
   - `parallel`: No shared files, no dependencies
   - `sequential`: Shared files or ordering requirements

   **Common batch groupings:**
   - Foundation: helpers, constants, utilities
   - Core: main logic
   - Integration: cross-module changes
   - Testing: verification

6. **Create beads in Beads DB**

   For each bead in the approved list:

   ```bash
   bd create "<number> <title>" \
     --description "<full self-contained description>" \
     --external-ref "opsx:<change-name>" \
     --labels "openspec,<change-name>,<section-slug>" \
     --priority 2 \
     --type task \
     --silent
   ```

   Capture bead ID from output.

   **Section slug:** Derive from design.md section name
   - "1. Setup" → "setup"
   - "2. Core Implementation" → "core-implementation"
   - Use lowercase, hyphenated format

   Track created beads:
   ```
   [
     { number: "1.1", bead_id: "emacs-a3f", title: "Create module structure" },
     { number: "1.2", bead_id: "emacs-a4g", title: "Add dependencies" },
     ...
   ]
   ```

7. **Add dependencies**

   For beads with identified dependencies:
   ```bash
   bd dep add <dependent-bead-id> <prerequisite-bead-id>
   ```

8. **Update .openspec.yaml**

   Add execution plan and bead tracking:
   ```yaml
   schema: spec-driven-beads
   created: 2026-03-10

   execution:
     batches:
       - id: foundation
         strategy: parallel
         beads:
           - emacs-a3f
       - id: core-logic
         strategy: sequential
         depends_on: [foundation]
         beads:
           - emacs-a5h

   beads:
     foundation:
       - emacs-a3f
     core-logic:
       - emacs-a5h

   metadata:
     tracking: beads
     beads_created: 2026-03-10T10:30:00Z
     total_beads: 8
   ```

9. **Show completion summary**

   Display:
   ```
   ## Beads Created: <change-name>

   Created N beads across M groups:

   ### Ready to Start (no dependencies)
   - emacs-a3f: 1.1 Create module structure
   - emacs-a5h: 2.1 Add tests

   ### Blocked (waiting on dependencies)
   - emacs-a4g: 1.2 Add dependencies (blocked by emacs-a3f)
   - emacs-a6i: 3.1 Integration (blocked by emacs-a4g)

   ### Next Steps
   - View all: `bd list --label <change-name>`
   - View ready: `bd ready --label <change-name>`
   - Start work: `/opsx:apply <change-name>`
   - Start specific bead: `/bead-work emacs-a3f`

   ### Context
   Each bead is self-contained with:
   ✓ Embedded design rationale
   ✓ Specific implementation steps
   ✓ File paths and code patterns
   ✓ Verification criteria

   No need to reference design.md during implementation -
   all context is in the bead description.
   ```

**Bead Description Template**

Use this structure for every generated bead:

```
[One-line summary of what this bead accomplishes]

Files to modify:
- [path/to/file1.ext]
- [path/to/file2.ext]

Test coverage (if tests exist):
- [test-file.el::test-function-name] (FAILING/SKIPPED)
- [another-test.el::test-name] (FAILING/SKIPPED)
- [If no tests: "No existing tests - verification manual"]

Implementation steps:
1. [Specific actionable step with details]
2. [Another step, include code patterns if relevant]
3. [Be concrete - mention functions, variables, patterns]
4. [If tests exist: Run tests to verify (command from architecture.md)]

Design rationale: [WHY from design.md - extract and embed]
[The architectural reasoning that explains this implementation]
[Include relevant context about technology choices, patterns, tradeoffs]

Design pattern: [Pattern to follow]
[If design mentions a pattern or example, include it here]
[Reference similar code: "See config/core/X.el for similar pattern"]

Verification:
- [If tests: Run <test-command> and verify tests pass]
- [Additional manual checks if needed]
- [Acceptance criteria - what "done" means]

Context: design.md § [section] '[section title]'
```

**Guardrails**

- **Self-contained:** Extract and embed design context, don't just reference
- **Actionable:** Each bead must have concrete implementation steps
- **Scoped:** Target 1-4 hours per bead (one session)
- **Test-aware:** If tests exist, include which tests this bead makes pass
- **Verified:** Each bead must have clear acceptance criteria
- **Linked:** Use labels and external-ref for traceability
- **Sequenced:** Add dependencies where order matters
- **Batched:** Group into execution plan with parallel/sequential strategy
- **User approval:** Always preview and get confirmation before creating
- **Context extraction:** Include WHY (rationale) not just WHAT (steps)
- **TDD-compatible:** Beads can be "write test + implement" or just "implement" depending on whether tests already exist

**Error Handling**

- If design.md doesn't exist: suggest `/opsx:continue` to create it
- If design.md is too vague: ask user to elaborate design before creating beads
- If can't infer file paths: include in bead as "[TODO: identify files]" and note in preview
- If bead creation fails: show error, continue with remaining beads, report failed ones
- If `.openspec.yaml` update fails: warn but show created bead IDs

**Integration**

This skill enables:
- `/opsx:apply` will work with these self-contained beads
- `/opsx:verify` will check bead completion
- Discovery workflow (`discovered-from`) tracks tactical issues
