---
name: docs-and-directionality
description: Update workspaces docs/prose for the menu + integrations + stub removal; confirm the directionality lint still passes
change: workspace-integration-menu
status: blocked
relations:
  - blocked-by:wire-create-dispatch
  - blocked-by:workspaces-transient-menu
  - blocked-by:gptel-session-integration
---

## Files to modify
- `config/workspaces/docs/README.org` (modify — document the menu + integrations + delete stale `*-initial.org` mentions)
- `config/workspaces/test/gptel-integration-spec.el` (modify — extend directionality grep to new files if it enumerates files explicitly)

## Implementation steps

1. **README.org**: add a section describing the `C-x w` transient menu (three
   states), the integration registry concept (how a subsystem registers
   `:on-create` / `:menu`), and the gptel-session integration as the first
   client. Remove/replace any description of the old eager
   `<date>-initial.org` scaffold session — it no longer exists; `sessions/` is
   created empty and populated by integrations on demand or at birth.

2. **Directionality lint**: confirm the existing grep test (the one asserting
   no `gptel-sessions-*` symbol appears under `config/workspaces/`) still
   passes against the NEW files (`integrations.el`, `workspaces-transient.el`).
   If the test enumerates files explicitly rather than globbing the directory,
   add the new tangled files to its list. The new workspaces code must name no
   gptel symbol (the gptel handler lives under `config/gptel/`, attached via
   `workspace-register-integration`).

3. **Sweep stale references**: `grep -rn "initial.org\|scaffold-initial-session"
   config/workspaces/` — fix any lingering prose in `scaffold.org`,
   `tabs.org`, `data-model.org`, `docs/README.org` left from the removal.

## Design rationale
The change removes a user-visible artifact (the eager initial session file) and
adds a user-visible surface (the menu + integrations), so the package docs must
track both. The directionality lint is the structural guarantee that the
registry — not a direct call — remains the only bridge between workspaces and
gptel; this task confirms the new files honor it.

## Verification
- `./bin/tangle-org.sh config/workspaces/docs/README.org` (if it tangles) or confirm it is prose-only
- `grep -rn "gptel-sessions-" config/workspaces/` returns nothing
- `grep -rn "initial.org\|scaffold-initial-session" config/workspaces/` returns only intentional historical notes (ideally nothing)
- `./bin/run-tests.sh -d config/workspaces`
- Done when: docs describe the menu + integrations; no stale stub references; directionality lint green.

## Context
design.md § Decision 6 and § Risks; spec `workspace-integrations` (Registry is
the published boundary); proposal § Impact.
