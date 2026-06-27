---
name: wire-into-init
description: Add org-graph to jf/enabled-modules after gptel and workspaces, and verify the spike module loads cleanly during a real isolated Emacs boot.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:module-load-smoke
---

## Files to modify
- `init.org` (modify) — add `"org-graph"` to `jf/enabled-modules`
- `init.el` (generated) — tangled from `init.org`

## Implementation steps
1. Add `"org-graph"` to the `jf/enabled-modules` list, positioned AFTER both
   `"gptel"` and the workspaces module. org-graph registers into the gptel
   tool registry and the workspaces integration registry, so both must load
   first (RE-5).
2. Tangle `init.org` and validate: `./bin/tangle-org.sh init.org`.
3. Boot a real isolated Emacs and confirm the module loads with no error and
   the loader messages appear:
   `./bin/emacs-isolated.sh -nw` (or batch:
   `./bin/emacs-isolated.sh -nw --batch --eval "(message \"loaded: %s\" (featurep 'org-graph))"`).
4. Confirm `jf/module-debug`-style output shows org-graph loaded after gptel
   and workspaces; if load order is wrong, the integration/tool registration
   will silently no-op — verify the integration actually registered in the
   live boot, not just in tests.

## Design rationale
D7/RE-5: load order is critical. The original D7 ("after gptel") is tightened
by RE-5 to "after gptel AND workspaces" because org-graph now attaches to the
workspace integration registry and the workspace-assistant preset, not just
the gptel tool registry. Gating this on the passing smoke test keeps a broken
module out of the boot path.

## Design pattern
Follow the module-registration pattern in `init.org` and CLAUDE.md § Module
System (loading order). Use `jf/reload-module` for iterative testing before
committing the init change.

## Verification
- `./bin/tangle-org.sh init.org` succeeds.
- Isolated boot loads org-graph with no error; `(featurep 'org-graph)` (or the
  loader's success message) confirms it.
- In the live session, the org-graph workspace integration and gptel tools are
  actually registered (not no-ops from a load-order mistake).

## Context
design.md § Decisions D7; design.md § Re-evaluation (RE-5);
CLAUDE.md § Module System.
</content>

## Cycle 1782561220 updates (cycle-1782561220)
This task now owns a concrete, surfaced gap — the **full ordered submodule load
sequence inside `org-graph.org`** (architect findings arch-cycle-1782561220-eoc-1
and -eoc-2; meta-discovery "loader-wiring-prerequisite-gaps").

- **The loader's submodule loads are scattered and only partially ordered.** As
  of this cycle `org-graph.org` loads: `schemas` then `finders` (in the
  "* Submodules" section), `query` (in a separate "* Query" section), and sets
  `vulpea-db-location` in "* Packages". `extractor`, `coordinator`, `discovery`
  are NOT yet load-wired. **This task must consolidate these into ONE ordered
  Submodules sequence** and remove the scattered placeholders. Dependency-correct
  order: `schemas → extractor → coordinator → query → finders → discovery`
  (finders requires `org-graph-schemas`; gptel-tools, when it lands, after
  `query` + `coordinator`).
- **A schemas → finders inline fix already shipped** (`3fb895f2`) because the gap
  surfaced as a merge regression: `finders.el` `(require 'org-graph-schemas)` had
  no preceding schemas load, and file basename `schemas.el` ≠ feature
  `org-graph-schemas`, so `require` cannot auto-load it. Keep that ordering;
  extend it to the full sequence. **Watch the basename≠feature trap for every
  submodule** — load by path via `jf/load-module`, in dependency order.
- **`module-load-smoke` is the gate** that proves the consolidated loader loads
  cleanly standalone (all registrations fire, org-roam intact) before this task
  flips org-graph into `jf/enabled-modules`. Land the ordered sequence such that
  the smoke spec's assertions pass.
- Step 1's "after gptel AND workspaces" (RE-5) is unchanged and confirmed.

## Cycle 1782564058 updates (cycle-1782564058)
> Still blocked on `module-load-smoke`. Context update only.

- **`tools.el` is now load-wired** in `org-graph.org`'s gptel-tools section
  (added this cycle by `gptel-tools`, `135139b4`). When you consolidate the
  scattered loads into ONE ordered Submodules sequence, `tools` belongs **after
  `query` and `coordinator`** (it builds on both). Updated canonical order:
  `schemas → extractor → coordinator → query → finders → tools → discovery`
  (workspace-integration loads after `tools`, since it populates the assistant
  `:tools` slot from `org-graph/agent-tools`). Watch the basename≠feature trap
  for `tools.el` too.
- `tools.el` gptel registration is guarded on `(fboundp 'gptel-make-tool)`, so it
  is load-safe even when gptel isn't present — no load-order constraint against
  gptel at the submodule level, but the init-level RE-5 rule (org-graph after
  `gptel` AND `workspaces`) still governs when the whole module loads.
