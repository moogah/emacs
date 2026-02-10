---
name: opsx:create-beads
description: Generate self-contained Beads issues from OpenSpec design and specs. Use when design is complete and you need to create actionable, trackable implementation work items.
license: MIT
compatibility: Requires openspec CLI and Beads (bd) CLI.
metadata:
  author: Jeff Farr
  version: "2.0"
---

Generate self-contained, actionable Beads issues from OpenSpec design.md and specs/. Each bead embeds all necessary context for implementation.

**Input**: Optionally specify a change name. If omitted, infer from context or prompt user.

**Core Principle**: **Beads must be self-contained.** Extract and embed design context into bead descriptions. Agents should not need to reference design.md during implementation - all context is in the bead.

**Steps**

1. **Select the change** - Use provided name, infer from context, or prompt user

2. **Verify prerequisites** - Check design.md exists and is complete

3. **Read design and specs** - Read all context from OpenSpec CLI

4. **AI-assisted bead generation**
   - Parse design.md structure (sections, subsections, decisions)
   - For each logical chunk, generate self-contained bead
   - Target: 1-4 hours per bead (one focused session)
   - Extract and embed design rationale (WHY) not just steps (WHAT)
   - Include file paths, implementation steps, patterns, verification

5. **Show preview and get approval**
   - Display structured preview of all beads
   - Show grouping, dependencies, estimated scope
   - Get user approval or adjustment feedback

6. **Create beads in Beads DB**
   - Create each bead with full description
   - Use external-ref: "opsx:<change-name>"
   - Use labels: "openspec", "<change-name>", "<section-slug>"

7. **Add dependencies** - Link beads that must be sequenced

8. **Update .openspec.yaml** - Add bead IDs to metadata

9. **Show completion summary** - List created beads with next steps

**Bead Description Template**

```
[One-line summary]

Files to modify:
- [paths]

Implementation steps:
1. [Concrete actions]

Design rationale: [WHY - extracted from design.md]

Design pattern: [Pattern with examples]

Verification:
- [Tests and acceptance criteria]

Context: design.md § [section]
```

**Granularity**: 1-4 hours per bead. Split larger sections, combine smaller ones.

**Self-contained**: Extract design context into beads. Don't reference "see design.md".

**No tasks.md**: Beads replace tasks.md entirely.

**Integration**: Works with `/opsx:apply`, `/opsx:verify`, `/bead-work`.
