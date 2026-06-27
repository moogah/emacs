---
name: wire-into-init
description: Add org-graph to jf/enabled-modules after gptel and workspaces, and verify the spike module loads cleanly during a real isolated Emacs boot.
change: org-graph-spike
status: ready
relations:
  - blocked-by:module-load-smoke
cites_register_entries:
  - register/invariant/org-graph-loader-ordered-sequence
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

## Cycle 1782566912 updates (cycle-1782566912)
> Still blocked on `module-load-smoke` (now `ready`). Context + a new cite.

- **Now cites `register/invariant/org-graph-loader-ordered-sequence`** (SPECULATED
  this cycle). This task is the **enforcement site** for that invariant: consolidate
  the scattered loader sections in `org-graph.org` into ONE ordered "* Submodules"
  sequence. The canonical order is now firm:
  `schemas → extractor → coordinator → query → finders → tools → discovery`, and
  **`workspace-integration` loads AFTER `tools`** (it populates the
  `workspace-assistant` `:tools` slot from `org-graph/agent-tools`, which only exists
  once `tools.el` has loaded). Watch the basename≠feature trap for every submodule
  (`workspace-integration.el` provides `org-graph-workspace-integration`); load by
  path via `jf/load-module`, never `require`-auto-resolve.
- **`workspace-integration.el` now exists** (merge `6c5fa7ce`) and is the LAST
  submodule in the order. As of this cycle the loader still has scattered sections
  (`query` in "* Query", `tools` in "* gptel tools", `schemas`+`finders` in
  "* Submodules"; `extractor`, `coordinator`, `discovery`, and now
  `workspace-integration` not yet load-wired). Consolidating ALL of them — including
  `workspace-integration` after `tools` — is this task's job.
- Step 1 ("after gptel AND workspaces" in `jf/enabled-modules`, RE-5) is unchanged and
  confirmed. Gate this on the passing `module-load-smoke` standalone-load spec before
  flipping org-graph into the boot path.

## Cycle 1782570180 updates (cycle-1782570180)
> **Now READY-blocking on nothing but its own predecessor.** `module-load-smoke`
> landed this cycle (merge `d74a0d55`, inline-fix `fd2c093f`); the smoke gate is green
> (org-graph suite 134 specs / 0 failed). The cited invariant
> `register/invariant/org-graph-loader-ordered-sequence` moved **speculated → DIVERGENT**
> this cycle — and THIS task is its named resolution site. Read before implementing.

The smoke spec PROVED the loader is broken (reconciliation note:
`.orchestrator/cycles/cycle-1782570180/reconciliations/org-graph-loader-ordered-sequence.md`;
finding `arch-cycle-1782570180-ot-1`). Direct inspection of `config/org-graph/org-graph.el`:
it loads `query → tools → workspace-integration → schemas → finders`, **OMITS
`extractor.el` and `coordinator.el` entirely**, and has **`discovery.el` commented out**
(line 70). So a cold `(require 'org-graph)` does NOT register the typed-edge extractor
and leaves discovery fns undefined. This task's loader-consolidation work is now
concretely scoped — it MUST:

1. **Add `extractor.el` and `coordinator.el`** to the consolidated loader (currently
   missing). `coordinator.el` was previously assumed "pulled in transitively by tools";
   verify and wire it explicitly.
2. **Un-comment / wire `discovery.el`** (line 70 is commented out today).
3. **Reorder to canonical** `schemas → extractor → coordinator → query → finders →
   tools → discovery`, `workspace-integration` after `tools`. Load every submodule BY
   PATH via `jf/load-module` (basename ≠ feature trap holds for all of them).
4. **Defer the discovery seed to a post-init hook (USER-RESOLVED ASK
   `ask-cycle-1782570180-1`: defer-to-post-init-hook).** `discovery.el` runs
   `org-graph/seed-org-id-locations` (a `vulpea-db-query`) AT LOAD today, which reaches
   the DB. A consolidated loader that simply loads `discovery.el` would attempt a DB
   query at module-load time. **The user chose to defer:** register the seed on
   `after-init-hook` / `emacs-startup-hook` (or equivalent) so module load stays
   DB-free, and the seed runs once the session is up. This is consistent with the
   existing design principle that org-graph registration is function-exposed precisely
   to avoid require-time DB opens — make the load of `discovery.el` side-effect-free and
   move the seed call out of load time.
5. **Add the REAL cold-load guard (reviewer Finding 1, spec-signal).** module-load-smoke
   currently asserts the END STATE by path-loading submodules itself — it documents the
   target but does NOT exercise the loader, so it stays green whether or not the loader
   is correct. Once you consolidate the loader, switch `config/org-graph/test/module-load-spec.el`
   to load via `(require 'org-graph)` ALONE (drop the explicit per-submodule
   `require`s in its setup) and assert every registration fires from the loader path.
   THAT is the test that guards the invariant. Only when it passes does the invariant
   move **divergent → reconciled** — closing the loop this task owns.

Step 1 ("after gptel AND workspaces" in `jf/enabled-modules`, RE-5) is unchanged and
confirmed.

## Observations
- **Deferral seam is `emacs-startup-hook`, not `after-init-hook`.** The
  user-resolved ask said "post-init hook"; I used `emacs-startup-hook`
  (the task explicitly sanctions "after-init-hook / emacs-startup-hook
  (or equivalent)"). Reason, verified empirically in this repo: every
  launch path (`bin/emacs-isolated.sh`, `Makefile` `EMACS_TEST_BATCH`)
  runs `emacs -q --load early-init.el --load init.el`. With `-q`,
  `after-init-hook` fires (and `after-init-time` is set) BEFORE the
  command-line `--load` actions, i.e. before any module loads — so an
  `after-init-hook` added from inside a module never fires. The existing
  `init.org` line ~249 `(add-hook 'after-init-hook ...)` is in fact dead
  under these launch methods. `emacs-startup-hook` runs AFTER the
  `--load` actions, so it is the only reliable post-init seam here; this
  matches `config/look-and-feel/themes.el`, which already uses it.
- **The test runner loads `init.el`.** `make`/`run-tests.sh` invoke
  `EMACS_TEST_BATCH`, which `--load`s `init.el`. Consequences the
  cold-load guard now leans on (and documents inline): `jf/load-module`
  and `jf/emacs-dir` are defined (so the loader's by-path loads work);
  `workspaces`, `gptel`, `org-roam` are already loaded; and because
  org-graph is now in `jf/enabled-modules`, `init.el` itself loads
  org-graph during the test boot — so the spec's `(require 'org-graph)`
  is effectively idempotent and the assertions verify the
  real-boot loader state. This is why `workspace-integration`'s
  `with-eval-after-load 'workspaces` fires from the loader path into the
  real `workspace--integrations`, and the gptel tools register at load.
- **Adding org-graph to `jf/enabled-modules` now pulls vulpea (via
  straight) into EVERY test-suite boot and the real boot.** Load errors
  are caught by `jf/load-module`'s `condition-case`, so a vulpea failure
  degrades rather than aborting init, but it does add load cost to all
  suites. Acceptable for the spike.
- **The seven skeleton placeholder sections in `org-graph.org`**
  (Discovery / Auto-ID scaffold / Note-type schemas / Finders /
  Typed-edge parser / Extractor / Coordinator) still carry their
  `;; implemented in <task>` no-op comment blocks. They are now pure
  documentation (the real loads live in the consolidated `* Submodules`
  section). Left in place — out of this task's scope — but a future
  cleanup could prune them to avoid the "implemented elsewhere" ambiguity.
- **In `-batch` neither deferred registration runs** (no startup hook),
  so the cold-load guard cannot observe the extractor registering "by
  itself". It instead asserts the loader WIRED `org-graph--register-extractor`
  onto `emacs-startup-hook` and then drives that function directly with
  the vulpea DB boundary stubbed. The discovery seed deferral is wired
  the same way but is not asserted by the module-load guard (its behavior
  is unit-tested in `discovery-spec.el`); the boot check confirms it is on
  the hook.

## Discoveries
- discovery_id: disc-wire-into-init-1
  class: invariant-gap
  description: |
    register/invariant/org-graph-loader-ordered-sequence is now REALIZED.
    The loader (`config/org-graph/org-graph.org`/`.el`) load-wires all
    eight submodules BY PATH via `jf/load-module` in ONE consolidated
    `* Submodules` sequence, canonical order
    schemas -> extractor -> coordinator -> query -> finders -> tools ->
    discovery, with workspace-integration last (after tools). The three
    scattered `jf/load-module` lines (Query / gptel tools / Workspace
    integration sections) were removed and folded into that one sequence;
    `extractor.el` and `coordinator.el` were added; `discovery.el` was
    un-commented and wired. The cold-load guard
    (`config/org-graph/test/module-load-spec.el`) was converted to load
    via `(require 'org-graph)` ALONE (per-submodule requires dropped) and
    now asserts every registration fired FROM THE LOADER PATH; it passes
    (org-graph suite 135 specs / 0 failed). A real isolated batch boot
    confirms `(featurep 'org-graph)` = t, the workspace integration is in
    the real `workspace--integrations`, and `org-graph/agent-tools`
    returns the 3 tools — i.e. org-graph loaded AFTER gptel and workspaces
    and its registrations took effect, not a load-order no-op.

    Point-C (registration-touches-DB tension) resolution — chose OPTION
    (b) from the task: the DB-free registrations (schemas register at
    load; gptel tools register at load when gptel present;
    workspace-integration attaches via `with-eval-after-load`) all fire as
    their files load, so `(require 'org-graph)` fires them. The ONE
    registration that genuinely opens the DB — the typed-edge extractor
    (`org-graph-extractor-register` -> `vulpea-db-register-extractor` ->
    `vulpea-db--apply-plugin-schema`, confirmed in vulpea source) — is
    deferred to `emacs-startup-hook` via a resilient wrapper
    `org-graph--register-extractor`, exactly like the discovery
    `org-id-locations` seed (deferred to the same seam inside
    `discovery.el`). This keeps `(require 'org-graph)` strictly DB-free in
    all contexts (verified: batch boot opens no DB). The cold-load guard
    asserts the extractor "differently": it verifies the loader WIRED the
    deferral onto `emacs-startup-hook`, then drives the wired function with
    the vulpea DB boundary stubbed and asserts the typed_edges extractor +
    schema landed.

    Premise check from the register entry: the entry's phrasing "a cold
    `(require 'org-graph)` does not fire all registrations" is realized
    WITH ONE PRINCIPLED NUANCE — a DB-free `require` cannot fire the
    DB-opening extractor registration without violating the (co-equal,
    user-resolved) DB-free-load contract. So "all registrations fire at
    require" holds for the DB-free registrations; the single DB-touching
    one is deferred-by-design to post-init. This is the coherent
    resolution, not a defeat of the invariant: the canonical ordered
    sequence and by-path loading are fully realized, and the guard
    meaningfully verifies the wiring.
  affected_register_entry: register/invariant/org-graph-loader-ordered-sequence
  recommendation: |
    Move register/invariant/org-graph-loader-ordered-sequence
    DIVERGENT -> RECONCILED. The consolidated by-path loader in canonical
    order + the require-alone cold-load guard are in place and green (135
    specs), and a real isolated boot confirms the registrations take
    effect after gptel+workspaces. When reconciling, RESTATE the invariant
    to carve out the DB-touching extractor registration: the statement
    should say the loader load-wires every submodule by path in the
    canonical order AND fires every DB-FREE registration at load, while
    the single DB-OPENING registration (typed-edge extractor) and the
    discovery org-id seed are deferred to `emacs-startup-hook` to preserve
    the DB-free-load contract — with the cold-load guard verifying the
    deferral is wired and the wired function registers correctly. Also
    worth capturing in the register/spec: `emacs-startup-hook` (not
    `after-init-hook`) is the load-bearing post-init seam under this
    config's `-q --load init.el` launch.
