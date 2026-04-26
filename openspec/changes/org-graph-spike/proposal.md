## Why

org-roam covers atomic Zettel-style notes well, but breaks down for the rest
of the user's note ecosystem: daily work logs, troubleshooting/debug
sessions with org-babel, deep-dive technical references, and project notes
that need to live co-located with their git repos and JIRA-ticket work
directories. It also lacks first-class typed/semantic relations and
async file watching, both of which become load-bearing once AI agents start
producing nodes and links in the background.

This spike validates a layered alternative — **org-node for distributed
discovery + vulpea v2 for typed-edge extraction and agent-facing queries**
— before committing to a full migration. org-roam keeps running unchanged
during the spike.

## What Changes

- Add a new `config/org-graph/` literate module that loads the graph
  stack: `org-node`, `vulpea` (v2), and `vulpea-journal`.
- Configure **org-node** as the navigator: discovery via `org-id-locations`,
  with eager indexing of `~/org/roam/` and `~/work/` so project-co-located
  notes are picked up without per-directory configuration.
- Configure **vulpea** as the typed-graph index: `vulpea-db-sync-directories`
  scoped to `~/org/roam/` only (durable concept notes), so project-local
  notes don't pollute the typed-edge index.
- Implement a **typed-edges extractor** plugin for vulpea that parses a
  PROPERTIES-drawer convention (`:IMPLEMENTS:`, `:CONTRADICTS:`,
  `:SUPERSEDES:`, `:RELATES_TO:`) into a queryable `typed_edges` table.
- Implement a **note-type taxonomy** via filetags (`:log:`, `:debug:`,
  `:topic:`, `:reference:`, `:project:`) and per-type finder commands
  (`org-graph/find-topic`, `org-graph/find-debug`, `org-graph/find-log`,
  `org-graph/find-project`).
- Implement an **agent-write coordinator** that serializes file writes
  from gptel/agent tools to avoid concurrent-write corruption, and exposes
  graph queries (`vulpea-db-query`, typed-edge lookups) as gptel tools.
- Co-exist with org-roam: org-roam-directory and `vulpea-db-sync-directories`
  may overlap on `~/org/roam/`; the two databases run independently and
  do not interfere.

This is a spike — the deliverable is a working layered system the user
can use day-to-day for evaluation, not a finalized architecture. Findings
from the spike inform a follow-up change that decides the long-term shape
(keep both, retire org-roam, etc.).

## Capabilities

### New Capabilities

- `org-graph`: Distributed knowledge-graph indexing layered on org-node
  (discovery) and vulpea (typed-edge extraction). Covers note-type
  taxonomy, semantic-link conventions, project-local note co-location,
  and the agent-facing query/write surface.

### Modified Capabilities

<!-- None. org-roam remains unchanged for the duration of the spike;
     gptel tools are extended additively (new graph tools), not modified. -->

## Impact

- **New module**: `config/org-graph/` (org file + tangled .el, plus
  submodules for the extractor and gptel tools).
- **New packages** (via straight.el): `org-node`, `vulpea` (v2),
  `vulpea-journal`, and any transitive deps (e.g. `org-mem` if pulled
  in by org-node).
- **Module load order**: `org-graph` loads after `gptel` so the
  agent-tool registration can attach to the existing gptel tool registry.
- **No changes to** `config/major-modes/org.org`, the existing org-roam
  configuration, or `~/org/roam/` content. New PROPERTIES on existing
  notes are additive — files without typed-edge properties are unaffected.
- **Filesystem**: vulpea creates a new SQLite DB under `runtime/state/`;
  org-node uses the existing `org-id-locations` cache. Both are
  gitignored runtime artifacts.
- **Agent integration**: new gptel tools (e.g. `org-graph-query`,
  `org-graph-typed-edges`, `org-graph-write-node`) registered alongside
  existing tools in `config/gptel/tools/`. The write-coordinator wraps
  any tool that produces graph files.
- **Test surface**: new buttercup specs under `config/org-graph/test/`
  for the extractor, finders, and write-coordinator. No changes to
  existing test infrastructure.
