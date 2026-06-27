---
name: workspace-integration
description: Register the org-graph workspace integration with an on-create watch-add handler, a menu entry, and population of the workspace-assistant tools slot.
change: org-graph-spike
status: done
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

## Observations

- **`vulpea-db-sync-update-directory` is a one-shot INDEXER, not a watcher
  installer — the task body conflates the two.** I implemented exactly the
  prescribed `:on-create` calls (append payload `:home` to
  `vulpea-db-sync-directories`, then `vulpea-db-sync-update-directory` on it),
  but reading the real vulpea v2.4 source (`runtime/.../vulpea-db-sync.el`)
  shows `vulpea-db-sync-update-directory` only lists+enqueues/indexes the org
  files under the dir (lines 896-...); it does NOT install a filenotify/fswatch
  watcher. Watchers are installed exclusively at autosync-START
  (`vulpea-db-autosync-mode` enable → `vulpea-db-sync-autosync-start`), which
  iterates the *then-current* `vulpea-db-sync-directories` via the private
  `vulpea-db-sync--watch-directory` (filenotify) or
  `vulpea-db-sync--setup-external-monitoring` (fswatch). On macOS the fswatch
  path is the default and the private filenotify watch is *skipped entirely*
  (sync.el:302-304). Net effect of the handler as prescribed: a workspace
  created mid-session has its existing notes **indexed once**, but ongoing
  edits under the new home are **not continuously watched** until the next
  autosync restart. That restart is exactly what the `:menu` →
  `org-graph/configure-sync` path performs (`configure-sync` calls
  `(vulpea-db-autosync-mode 1)`, re-running start over the full root set). So
  the two surfaces are complementary (on-create = immediate index; menu =
  full re-watch), and the spike behaviour is acceptable — but the task's
  stated rationale ("vulpea-db-sync-update-directory ... is the registration
  [watcher] seam") is factually wrong about that one call. See Discoveries.

- **The `:menu` COMMAND is invoked WITH the anchor payload (arity 1), so a bare
  no-arg interactive command cannot be used directly.** The task suggested the
  `:menu` entry "runs `org-graph/configure-sync`", but
  `workspace--menu-invoke-integration` (config/workspaces/workspaces-transient.org)
  does `(funcall command (workspace--integration-payload ...))`. `org-graph/configure-sync`
  is a zero-arg interactive command; calling it with one arg would error. I
  therefore wrapped it in `org-graph-workspace-integration--menu (_payload)`,
  which ignores the payload (configure-sync re-reads the live registry itself)
  and returns `ok`. This is the correct adaptation, not a deviation from intent.

- **`:tools` slot population mutates the stored preset plist in place** via
  `(setcdr (assq 'workspace-assistant gptel--known-presets) (plist-put ... :tools ...))`,
  rather than re-calling `jf/gptel-preset-register`/`gptel-make-preset`.
  Re-registering would re-run scope/mode extraction and is heavier; in-place
  mutation of only the `:tools` slot is strictly additive and preserves every
  other slot (asserted in the spec). Trigger is
  `(with-eval-after-load 'gptel-preset-workspace-assistant ...)` — fires exactly
  when `preset.el` finishes registering and `(provide)`s its feature, which is
  the precise moment the preset exists in `gptel--known-presets`.

- **Two soft-dependency triggers, no hard requires.** Registration is under
  `with-eval-after-load 'workspaces`; tools population under
  `with-eval-after-load 'gptel-preset-workspace-assistant`. The module hard-requires
  only `cl-lib`, so it tangles/loads standalone (the spec loads it with neither
  workspaces nor gptel present). `org-graph/agent-tools` /
  `org-graph/configure-sync` are reached via `fboundp`/`declare-function`, not a
  load-time require, keeping load order `wire-into-init`'s concern.

- **Environment gap (not a code issue):** vulpea was absent from this worktree's
  `runtime/straight/` (only package missing vs. the main checkout), so the
  `-d config/org-graph` gate could not load (helpers-spec/db-location-spec
  `(require 'vulpea)`). I copied vulpea's repo+build from the main checkout into
  the gitignored worktree runtime to run the gate. `init-worktree-runtime.sh`
  for this worktree predates vulpea landing in the main checkout; a re-run would
  fix it. Nothing committed.

## Discoveries

- discovery_id: disc-workspace-integration-1
  class: interface-drift
  description: |
    The task body (and its design rationale RE-2) prescribe the `:on-create`
    watch-add as "append :home to `vulpea-db-sync-directories` + call
    `vulpea-db-sync-update-directory` on it" with the justification "filenotify
    watchers are NOT auto-installed for dirs added after autosync starts, so
    this is the registration seam." Inspection of the pinned vulpea v2.4
    source shows `vulpea-db-sync-update-directory (dir &optional force)` is a
    one-shot indexer (lists org files under DIR and enqueues/indexes them); it
    does not install any watcher. Watcher installation happens only at
    `vulpea-db-autosync-mode` enable (autosync-start), iterating the
    then-current `vulpea-db-sync-directories` via private
    `vulpea-db-sync--watch-directory` (filenotify, SKIPPED when fswatch is the
    active backend — the macOS default) or `--setup-external-monitoring`
    (fswatch). So a directory appended mid-session and passed to
    `update-directory` is indexed once but never continuously watched until a
    later autosync restart.
  affected_register_entry: register/shape/workspace-integration-anchor-payload
  recommendation: |
    Decouple "index the new home now" from "watch the new home going forward"
    in the contract language. The implemented `:on-create` correctly delivers
    the former (immediate index, PUSH-only from the payload). For the latter,
    the only backend-agnostic seam is an autosync RESTART
    (`(vulpea-db-autosync-mode 1)` re-runs start over the current dir set,
    re-installing filenotify watchers AND re-setting up fswatch). The `:menu`
    → `org-graph/configure-sync` path already does this. If continuous watching
    of a mid-session workspace is required without a manual menu invoke, have
    `:on-create` additionally trigger an autosync restart — but note that
    `org-graph/configure-sync`/`org-graph/index-roots` CONSULT the live
    workspace registry, which conflicts with the PUSH-not-consult contract; a
    payload-only restart helper would be needed. Recommend updating the task /
    RE-2 prose to stop attributing watcher-install to
    `vulpea-db-sync-update-directory`, and to document on-create as
    "index-on-birth" with re-watch delegated to configure-sync/next-session.

- discovery_id: disc-workspace-integration-2
  class: shape-fragmentation
  description: |
    The `:menu` COMMAND calling convention is arity-1 (payload), set by
    `workspace--menu-invoke-integration` in
    config/workspaces/workspaces-transient.org:
    `(funcall command (workspace--integration-payload name home 'menu-invoke))`.
    The registry boundary doc (integrations.org) describes `:menu` only as an
    opaque `(KEY . COMMAND)` pair "stored as data; this module does not act on
    it" and does not state the COMMAND arity — that contract lives only in the
    transient module. The gptel-session integration's menu handler happens to
    take a payload, but the org-graph task body implied a no-arg interactive
    command (`org-graph/configure-sync`), which would error if used directly.
  affected_register_entry: register/boundary/workspace-integration-registry
  recommendation: |
    Pin the `:menu` COMMAND arity (one arg: the menu-invoke anchor payload, a
    `register/shape/workspace-integration-anchor-payload` with context
    `menu-invoke`) explicitly in the workspace-integration-registry boundary
    entry, not just implicitly in the transient module. Consumers otherwise
    have to read the transient source to learn the calling convention. The
    org-graph menu handler adapts correctly (`(_payload)` wrapper), so no code
    change is needed — this is a documentation/contract-completeness gap.
