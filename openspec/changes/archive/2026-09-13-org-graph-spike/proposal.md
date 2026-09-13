## Why

org-roam covers atomic Zettel-style notes well, but breaks down for the
rest of the user's note ecosystem: daily work logs, troubleshooting/debug
sessions, deep-dive technical references, and project notes that need to
live co-located with their git repos and ticket work directories. It also
lacks first-class typed/semantic relations and an agent-facing query/write
surface — both load-bearing once AI agents start producing nodes and links
in the background.

**This proposal was re-evaluated on 2026-06-27** after the branch sat cold
while two relevant capabilities landed on `main`:

1. **The `workspaces` package** (`config/workspaces/`) now ships
   project-co-located structure as a first-class concept. A *workspace* is
   a tab anchored to a filesystem `:home` directory; creating one scaffolds
   a git repo with a `home.org` skeleton and an empty `sessions/`
   directory. It publishes an **integration registry**
   (`workspace-register-integration` with `:on-create` / `:on-purge` /
   `:menu`), and gptel already integrates: every workspace gets a
   directory-scoped session (`GPTEL_WORK_ROOT` = `:home`) plus a
   `workspace-assistant` preset whose `:tools` slot is deliberately empty,
   awaiting a tool palette.

2. **vulpea 2.4** matured from the "API still evolving" library the
   original design pinned against into a 100k-note-scale database layer
   with: a **schema system** (`vulpea-schema-define` / `-validate`), a
   native **link/graph query API** (`vulpea-db-query-links` carrying link
   `:type`, plus `dead-links` / `orphan-notes` / `isolated-notes` /
   `backlink-counts`), a **parser epoch** that invalidates the file cache
   when extractors change, automatic DB rebuild on schema bumps, and
   **async-by-default** sync with a `vulpea-doctor` diagnostic.

The re-evaluation outcome: the spike's *infrastructure thesis* (a vulpea
typed-edge index + an agent-facing graph surface) is still sound, but two
of its four original pillars have moved. **Project co-location is now
delivered by workspaces**, so org-graph should treat workspaces as its
substrate — indexing workspace `:home` directories and registering through
the integration registry — rather than re-deriving discovery of
repo-co-located notes itself. And the **note-type taxonomy** should be
backed by `vulpea-schema` rather than a hand-rolled filetag convention.

This remains a spike: validate the layered approach day-to-day, learn
what's load-bearing, then commit to a long-term shape in a follow-up.
org-roam keeps running unchanged throughout.

## What Changes

- Add a new `config/org-graph/` literate module that loads vulpea (v2.4+)
  and registers org-graph as a **workspace integration**, layered on the
  existing org-roam install without touching it.

- **Workspace-substrate discovery (vulpea-only).** Instead of blindly
  walking `~/work/`, vulpea is the single index, fed *explicit* roots: the
  set of workspace `:home` directories the `workspaces` registry already
  enumerates (plus their `sessions/`), and the durable concept vault
  `~/org/roam/` (the typed-edge root). org-node is **not** adopted — vulpea
  already feeds the global `org-id-locations` so `id:` links resolve, making
  a second index redundant (see design.md RE-2). Two small shims cover the
  gaps: a startup seed of `org-id-locations` from the vulpea DB, and an
  `:on-create` hook that registers each new workspace home for watching.
  Because vulpea only indexes notes with an `:ID:`, the workspace scaffold
  and session creation are extended to auto-assign IDs (RE-2a).

- **Typed-graph index scoped to `~/org/roam/`.** `vulpea-db-sync-directories`
  stays scoped to the durable concept vault so transient project/session
  notes don't pollute the typed-edge index. Operational notes link *into*
  concept notes, not vice versa.

- **Schema-backed note-type taxonomy (vulpea-schema).** Note types
  (`log`, `debug`, `topic`, `reference`, `project`) are expressed as
  `vulpea-schema-define` definitions with field expectations and validation,
  rather than a bare filetag convention. Filetags become one validated
  field among others. Per-type finder commands (`org-graph/find-topic`,
  `-debug`, `-log`, `-project`) are backed by schema-aware queries.

- **Semantic typed-edges extractor.** A vulpea extractor plugin parses a
  PROPERTIES-drawer convention (`:IMPLEMENTS:`, `:CONTRADICTS:`,
  `:SUPERSEDES:`, `:RELATES_TO:`) into a queryable typed-edges table. This
  stays net-new: vulpea's native link `:type` is link-*kind* (id/file/https),
  not semantic relation-*kind*. But it now builds on vulpea 2.4's mature
  plugin + schema + parser-epoch infrastructure instead of bleeding-edge
  internals.

- **Agent-facing graph surface, plugged into workspaces.** Graph queries
  (`vulpea-db-query`, typed-edge lookups, schema-validated finders) are
  exposed as gptel tools and registered into the **`workspace-assistant`
  preset's `:tools` slot** — filling the seam workspaces left open — so the
  per-workspace agent can query the graph within its directory scope.

- **Agent-write coordinator.** Serializes file writes from gptel/agent
  tools to avoid concurrent-write corruption. Still net-new; the natural
  context is now per-workspace writes mediated by the existing
  `GPTEL_WORK_ROOT` scoping.

- **Co-exist with org-roam:** org-roam-directory and
  `vulpea-db-sync-directories` may overlap on `~/org/roam/`; the databases
  run independently and do not interfere.

The deliverable is a working layered system the user can use day-to-day for
evaluation, not a finalized architecture. Findings inform a follow-up change
that decides the long-term shape (keep both, retire org-roam, fold org-graph
fully into workspaces, etc.).

## Capabilities

### New Capabilities

- `org-graph`: A graph/agent layer over the durable concept vault that
  plugs into the `workspaces` substrate. Covers schema-backed note-type
  taxonomy (via vulpea-schema), semantic typed-edge extraction and queries,
  and the agent-facing query/write surface registered through the workspace
  integration registry and the `workspace-assistant` tool palette.

### Modified Capabilities

- `workspace-integrations` / `workspace-assistant`: extended **additively**
  — org-graph registers a new integration and populates the previously-empty
  `workspace-assistant` `:tools` slot. No existing workspace behavior is
  modified; a workspace without org-graph loaded is unaffected.

<!-- org-roam remains unchanged for the duration of the spike. -->

## Impact

- **New module**: `config/org-graph/` (org file + tangled .el, plus
  submodules for the extractor, schema/finders, query, coordinator, and the
  workspace-integration + gptel-tools wiring).

- **New packages** (via straight.el): `vulpea` (v2.4+), optionally
  `vulpea-journal` (Open Question 5). org-node / org-mem are **not** adopted
  (RE-2).

- **Module load order**: `org-graph` loads after both `gptel` and
  `workspaces` so it can register into the integration registry and attach
  tools to the `workspace-assistant` preset.

- **Integration points touched** (additively): `workspace-register-integration`
  (new `org-graph` integration) and the `workspace-assistant` preset `:tools`
  slot. No changes to `config/major-modes/org.org`, the existing org-roam
  configuration, the `workspaces` core, or `~/org/roam/` content. New
  PROPERTIES on existing notes are additive.

- **Filesystem**: vulpea creates a SQLite DB under `runtime/state/`
  (gitignored). Index scope follows the workspace registry + `~/org/roam/`.

- **Test surface**: new buttercup specs under `config/org-graph/test/` for
  the extractor, schema/finders, query API, write-coordinator, and the
  workspace-integration registration. No changes to existing test
  infrastructure.
</content>
</invoke>
