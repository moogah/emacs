---
name: write-discoverability-bootstrap-seed
description: Minimal static seed (skill/CLAUDE.md/RECIPES) naming the jf/agent-api entry point
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:implement-agent-api-manifest"
  - "blocked-by:write-client-recipes-doc"
---

## Files to modify
- config/emacs-test-targets/RECIPES.md (modify) — add the discoverability entry point
- CLAUDE.md (modify) — one-line pointer (decide placement; e.g. near testing infra)
- (optional) a Claude Code skill — decide channel below

## Implementation steps
1. Decide the seed channel (design.md Open Question 'Static seed channel'): a dedicated
   Claude Code skill, a `CLAUDE.md` pointer, or both. Recommended minimum: a `CLAUDE.md`
   line + a RECIPES section. A skill is optional and can be added later.
2. Add to RECIPES a short "Discovering the control API" section that names the entry
   point ONLY: to learn what operations a target exposes, run
   `emacsclient -s <name> --eval '(jf/agent-api)'`; for one operation's detail use the
   drill-down. Show the start/eval/observe/dispose context around it (links to existing
   recipes from write-client-recipes-doc).
3. Add ONE line to CLAUDE.md pointing at the entry point (e.g. under the testing /
   isolated-targets area): "To drive an isolated Emacs target, enumerate its control
   operations with `(jf/agent-api)` — see config/emacs-test-targets/RECIPES.md."
4. CRITICAL: the seed MUST NOT statically enumerate the operations. It names the entry
   point and stops; enumeration is the runtime manifest's job (L2). This is what keeps
   the static surface from drifting when the registry changes.
5. If adding a skill, keep it equally minimal — trigger description + the entry-point
   recipe, deferring the operation list to `(jf/agent-api)`.

## Design rationale
L2 (the runtime manifest) only helps if the client already knows to call
`(jf/agent-api)`. The bootstrap seed (L3) solves that — it is the smallest possible
static pointer that names the entry point. Keeping it minimal (no operation list) is the
whole point: a static enumeration would reintroduce exactly the drift the live manifest
was designed to eliminate. When the `jf/agent/*` registry changes, NO seed edit is
required because the seed points at the manifest, not the operations (design.md D9 / L3).

## Verification
- RECIPES contains the `(jf/agent-api)` entry-point recipe and does NOT list individual
  operations: `grep -n 'jf/agent-api' config/emacs-test-targets/RECIPES.md` matches;
  the section has no per-operation table.
- CLAUDE.md contains the one-line pointer:
  `grep -n 'jf/agent-api' CLAUDE.md` matches.
- A reader following only the seed can discover the full operation set at runtime
  (manual trace).

## Context
- design.md § Decisions 'D9' (L3 minimal static seed)
- specs/control-surface-discoverability/spec.md § 'Minimal Static Bootstrap Seed'
