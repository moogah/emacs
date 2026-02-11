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

3. **Read design and specs**

   Read all context files:
   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   Read from `contextFiles`:
   - design.md (required)
   - specs/**/*.md (if exist)
   - proposal.md (for high-level context)

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

5. **Show preview and get approval**

   Display generated beads in structured format:
   ```
   ## Proposed Beads for <change-name>

   ### Group 1: Setup (2 beads)

   **Bead 1.1: Create module structure**
   Files: config/gptel/presets.el (new)
   Scope: ~2 hours
   [Preview first 10 lines of description...]

   **Bead 1.2: Add dependencies**
   Files: config/gptel/gptel.el
   Scope: ~1 hour
   Depends on: 1.1
   [Preview...]

   ### Group 2: Core Implementation (3 beads)

   **Bead 2.1: Implement resolver chain**
   Files: config/gptel/presets.el
   Scope: ~3 hours
   [Preview...]

   ...

   Total: 8 beads across 3 groups
   Estimated: ~15 hours total
   ```

   Use **AskUserQuestion**:
   - "Create these beads?"
   - Options:
     - "Yes, create all" → proceed to step 6
     - "Show full descriptions" → display all, then ask again
     - "Adjust granularity" → ask for feedback, regenerate
     - "Cancel" → exit

   If "Adjust granularity":
   - Ask: "Should I split larger beads, combine smaller ones, or adjust specific beads?"
   - Get feedback, regenerate, show preview again

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

   Example: If bead 1.2 depends on 1.1:
   ```bash
   bd dep add emacs-a4g emacs-a3f
   ```

8. **Update .openspec.yaml**

   Add bead tracking metadata:
   ```yaml
   schema: spec-driven
   created: 2026-02-09
   metadata:
     tracking: beads
     beads:
       - emacs-a3f
       - emacs-a4g
       - emacs-a5h
     beads_created: 2026-02-10T10:30:00Z
   ```

   Use **Edit** tool to update `.openspec.yaml`.

   **Do NOT create tasks.md** - beads replace tasks entirely.

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

Implementation steps:
1. [Specific actionable step with details]
2. [Another step, include code patterns if relevant]
3. [Be concrete - mention functions, variables, patterns]

Design rationale: [WHY from design.md - extract and embed]
[The architectural reasoning that explains this implementation]
[Include relevant context about technology choices, patterns, tradeoffs]

Design pattern: [Pattern to follow]
[If design mentions a pattern or example, include it here]
[Reference similar code: "See config/core/X.el for similar pattern"]

Verification:
- [How to test this works - be specific]
- [Acceptance criteria - what "done" means]
- [Commands to run: tests, linters, manual checks]

Context: design.md § [section] '[section title]'
```

**Guardrails**

- **Self-contained:** Extract and embed design context, don't just reference
- **Actionable:** Each bead must have concrete implementation steps
- **Scoped:** Target 1-4 hours per bead (one session)
- **Verified:** Each bead must have clear acceptance criteria
- **Linked:** Use labels and external-ref for traceability
- **Sequenced:** Add dependencies where order matters
- **No tasks.md:** Beads replace tasks.md entirely - don't create it
- **User approval:** Always preview and get confirmation before creating
- **Context extraction:** Include WHY (rationale) not just WHAT (steps)

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
- `/bead-work` can implement individual beads
- Discovery workflow (`discovered-from`) tracks tactical issues
