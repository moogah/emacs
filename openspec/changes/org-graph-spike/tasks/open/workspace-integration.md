---
name: workspace-integration
description: Register the org-graph workspace integration with an on-create watch-add handler, a menu entry, and population of the workspace-assistant tools slot.
change: org-graph-spike
status: ready
relations:
  - blocked-by:gptel-tools
  - blocked-by:registry-discovery
cites_register_entries:
  - register/boundary/org-graph-agent-tools
  - register/boundary/workspace-integration-registry
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

## Cycle 1782564058 updates (cycle-1782564058)
> **Unblocked: status flipped blocked → ready.** Both blockers are now done —
> `gptel-tools` landed this cycle (`135139b4`) and `registry-discovery` closed
> in a prior cycle.

Absorb before implementing:
- **`register/boundary/org-graph-agent-tools` is RECONCILED this cycle** (was
  speculated). The accessor `org-graph/agent-tools` returns the constructed
  **gptel-tool OBJECTS** — hand them directly to the `workspace-assistant`
  preset's `:tools` slot (step 2). It returns **nil until
  `org-graph-tools-register` has run**, and registration is gated on
  `(fboundp 'gptel-make-tool)`, so populating the slot must tolerate an empty
  list when gptel isn't loaded (your "skip silently if preset absent" guard
  already covers the preset side; mirror it for an empty tool list).
- The three LLM-facing tools are named (snake_case) `org_graph_query` /
  `org_graph_typed_edges` / `org_graph_write_node`; backing fns are
  `org-graph-tools/{query,typed-edges,write-node}`. You don't name them — you
  pass the objects from `org-graph/agent-tools` — but the smoke/integration
  assertions downstream key on the snake_case `:name`s.
- Loader placement: `tools.el` now loads in `org-graph.org`'s gptel-tools
  section; `workspace-integration.el` is still a loader placeholder
  (`org-graph.org`, Workspace integration section) — wiring it into the full
  ordered submodule sequence is `wire-into-init`'s job, after `tools` (since the
  `:tools` slot population needs `org-graph/agent-tools`).

## Cycle 1782566912 updates (cycle-1782566912)
> **Selected as this cycle's sole batch task** (it is the only `ready` task; the
> remaining chain `module-load-smoke → wire-into-init → spike-eval-checklist` is
> strictly linear and each link needs the prior MERGED, so they cannot share a
> single-baseline batch). On the critical path: this fills the
> `workspace-assistant` `:tools` slot — the proposal's "agent-facing graph
> surface, plugged into workspaces" pillar.

Absorb before implementing:
- **Now also cites `register/boundary/workspace-integration-registry`** (RECONCILED,
  load-bearing). `workspace-integration.el` is a NEW *consumer* of that registry —
  the on-touch Architect will audit the consumer attachment against the boundary's
  directionality contract: **consumers attach via
  `(with-eval-after-load 'workspaces (workspace-register-integration 'org-graph ...))`;
  workspaces never names org-graph.** Follow the registry's pinned shape: at least
  one of `:on-create`/`:menu`/`:on-purge`; PUSH-not-consult (use ONLY the pushed
  `register/shape/workspace-integration-anchor-payload`, never reach for the current
  workspace); ADDITIVE/never-load-bearing (a failing handler is surfaced, never
  rolls back the workspace); return the `register/vocabulary/workspace-integration-outcome`
  `ok`/`skipped`/`failed` protocol. Mirror `config/gptel/sessions/workspace-integration.org`.
- `register/boundary/org-graph-agent-tools` is RECONCILED (unchanged from last cycle):
  `org-graph/agent-tools` returns gptel-tool OBJECTS, nil until `org-graph-tools-register`
  runs (gated on `fboundp gptel-make-tool`). Step 2 must tolerate an empty list when
  gptel isn't loaded, mirroring the "skip silently if preset absent" guard.
- **New register entry to be aware of (not yet your job):**
  `register/invariant/org-graph-loader-ordered-sequence` (SPECULATED this cycle)
  pins the canonical submodule load order. Per that order, `workspace-integration`
  loads AFTER `tools` (it needs `org-graph/agent-tools`). You implement
  `workspace-integration.el` + its spec here; **`wire-into-init` owns consolidating
  the scattered loader sections into the ordered sequence** — do not pre-empt it,
  but keep your loader placeholder consistent with "after tools".
