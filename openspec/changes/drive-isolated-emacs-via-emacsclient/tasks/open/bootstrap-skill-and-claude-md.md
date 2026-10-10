---
name: bootstrap-skill-and-claude-md
description: "Static seed: emacs-agent Claude Code skill + CLAUDE.md pointer (wrapper, policy, manifest)"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:emacs-agent-wrapper-targets"
---

## Files to modify
- .claude/skills/emacs-agent/SKILL.md (new)
- CLAUDE.md (modify) — one-line pointer

## Implementation steps
1. Skill content: when to drive Emacs; the target-selection table (data → live,
   unmerged code → isolated, data-feature development → isolated + scratch); the
   tiers and that live `eval` needs the user's agreement; wrapper usage; "list tools
   with `emacs-agent <target> tools`"; target lifecycle recipes; recommended
   permission rules. Do NOT enumerate tools.
2. CLAUDE.md: one line pointing at the skill.

## Design rationale
The manifest only helps if the agent knows to ask for it; the seed is the smallest
static pointer that names the entry point and the policy, so it never drifts when
tools change.

## Verification
- The skill names no individual tool (`grep -n 'org_graph\|workspace_add_layout' .claude/skills/emacs-agent/SKILL.md` returns nothing)
- Manual: a fresh Claude Code session asked to read an org-graph note uses `emacs-agent live call`

## Context
- design.md § Decisions 'D9' (L3), 'D12', 'D13'
- specs/control-surface-discoverability/spec.md § 'Minimal Static Bootstrap Seed'
- specs/agent-target-policy/spec.md § 'Target Selection Guidance'
