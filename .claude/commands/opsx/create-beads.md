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
   - Target: <50k tokens per bead (~20-30 min focused work)

   **Split indicators** (break into separate beads):
   - Multiple behavioral scenarios or edge cases
   - Implementation + comprehensive tests
   - Complex conditional logic (split by major branch)
   - Multi-file changes unless tightly coupled

   **Combine indicators:**
   - Single function <30 lines with trivial test
   - Tightly coupled changes (add function + call site)

   **Token estimation heuristics:**
   - Simple function: ~20-30k
   - Function with edge cases: ~30-45k
   - Multi-file coordination: ~40-50k
   - Test suite: ~25-35k

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
         estimated_tokens: 120000
         beads:
           - id: emacs-abc1
             estimated_tokens: 40000
       - id: core-logic
         strategy: sequential
         depends_on: [foundation]
         estimated_tokens: 85000
         beads:
           - id: emacs-def4
             estimated_tokens: 45000
   ```

   **Batch strategies:**
   - `parallel`: No shared files, no dependencies
   - `sequential`: Shared files or ordering requirements

   **Common batch groupings:**
   - Foundation: helpers, constants, utilities
   - Core: main logic
   - Integration: cross-module changes
   - Testing: verification

6. **Show preview and get approval**

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

7. **Create beads in Beads DB**

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

8. **Add dependencies**

   For beads with identified dependencies:
   ```bash
   bd dep add <dependent-bead-id> <prerequisite-bead-id>
   ```

9. **Update .openspec.yaml**

   Add execution plan and bead tracking:
   ```yaml
   schema: spec-driven-beads
   created: 2026-03-10

   execution:
     batches:
       - id: foundation
         strategy: parallel
         estimated_tokens: 120000
         beads:
           - id: emacs-a3f
             estimated_tokens: 40000
       - id: core-logic
         strategy: sequential
         depends_on: [foundation]
         estimated_tokens: 85000
         beads:
           - id: emacs-a5h
             estimated_tokens: 45000

   beads:
     foundation:
       - emacs-a3f
     core-logic:
       - emacs-a5h

   metadata:
     tracking: beads
     beads_created: 2026-03-10T10:30:00Z
     total_beads: 8
     estimated_total_tokens: 320000
   ```

10. **Show completion summary**

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
- **Scoped:** Target <50k tokens per bead
- **Test-aware:** If tests exist, include which tests this bead makes pass
- **Verified:** Each bead must have clear acceptance criteria
- **Linked:** Use labels and external-ref for traceability
- **Sequenced:** Add dependencies where order matters
- **Batched:** Group into execution plan with parallel/sequential strategy
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
