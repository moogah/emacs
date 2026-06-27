---
name: workspace-integration
description: Register the org-graph workspace integration with an on-create watch-add handler, a menu entry, and population of the workspace-assistant tools slot.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:gptel-tools
  - blocked-by:registry-discovery
---

## Files to modify
- `config/org-graph/workspace-integration.el` ← via
  `config/org-graph/org-graph.org` (Workspace integration section)
- `config/org-graph/test/workspace-integration-spec.el` (new)

## Implementation steps
1. Register an org-graph integration via `workspace-register-integration`
   (guard with `with-eval-after-load 'workspaces`), declaring `:label` plus:
   - `:on-create` — fired at workspace birth with the pushed anchor payload
     (`:name :home :sessions-dir :context`). Handler: when
     `org-graph-watch-workspace-homes`, append the new `:home` to
     `vulpea-db-sync-directories` and run `vulpea-db-sync-update-directory`
     on it (filenotify watchers are NOT auto-installed for dirs added after
     autosync starts, so this is the registration seam). Use ONLY the pushed
     payload — do not consult global workspace state. Return the
     `ok`/`skipped`/`failed` outcome protocol.
   - `:menu` — an Integrations-group command (pick a free key, e.g. `"G"`)
     that runs `org-graph/configure-sync` (re-index the current roots) or a
     graph-query command against the current workspace.
2. Populate the `workspace-assistant` preset's `:tools` slot with
   `org-graph/agent-tools` (from the gptel-tools task), so the per-workspace
   assistant gains the graph read/write tools. Do this additively — if the
   preset is absent (workspaces/gptel not loaded) skip silently.
3. Do NOT modify workspaces core or the preset's other slots. The integration
   is strictly additive — a failing handler is surfaced but never rolls back
   the workspace (per the workspace-integrations contract).
4. Write `workspace-integration-spec.el`: stub `workspace-register-integration`
   and capture the registered plist; assert the `:on-create` handler, given a
   fake payload, appends the home to `vulpea-db-sync-directories` and calls
   `vulpea-db-sync-update-directory`; assert the tools slot receives the tool
   list.

## Design rationale
RE-5 / RE-2: workspaces is the substrate. Registering through the published
integration registry (`:on-create` / `:menu`) is the sanctioned, directional
extension point — workspaces never names org-graph; org-graph attaches to
workspaces. The `:on-create` handler closes the "new directories aren't
auto-watched" gap from RE-2. Filling the `workspace-assistant` `:tools` slot
is exactly the seam workspaces left open for a tool palette.

## Design pattern
Follow `config/gptel/sessions/workspace-integration.org` (the gptel-session
integration) for registration shape, soft-dependency guarding, and the
payload-only / outcome-protocol discipline. See
`openspec/specs/workspace-integrations/spec.md`.

## Verification
- `./bin/run-tests.sh -d config/org-graph/test` — integration spec passes.
- Manual: create a new workspace; confirm its `:home` is added to
  `vulpea-db-sync-directories` and indexed (visible in `vulpea-doctor`); open
  the workspace transient and confirm the org-graph Integrations entry; the
  workspace assistant lists the org-graph tools.

## Context
design.md § Re-evaluation (RE-2, RE-5);
openspec/specs/workspace-integrations/spec.md;
config/gptel/sessions/workspace-integration.org.
</content>
