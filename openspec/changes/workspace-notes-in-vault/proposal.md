## Why

The org-graph spike bet on *co-location*: a workspace's notes (`home.org`,
gptel sessions) live inside its `:home` directory, and vulpea indexes every
workspace home recursively. Day-to-day use falsified both halves of that
bet. Indexing a home recursively swept up thousands of third-party `.org`
files under a checked-out worktree's `runtime/`, evaluating their file-local
variables and dropping into the debugger on the first `eval:` it could not
satisfy. And the directory the notes were tied to is the *least* durable
thing in the workspace: worktrees are short-lived by design, so notes that
inherit the home's lifetime lose the very continuity a project note is for.

A second latent defect surfaced in the survey: sessions created under a
workspace home are invisible to the session registry, the branch enumerator,
and every listing command, because enumeration only ever walks the global
sessions root. The spec's "filesystem-authoritative session inventory"
requirement was never implemented.

The fix is to stop co-locating. All notes move to the single org-roam vault,
each workspace is anchored by a **project note** whose org ID is the
workspace's canonical identity, and association is expressed as typed
`part-of` edges rather than directory containment. The co-located *workflow*
(find this project's notes, start a note in this project, see this project's
sessions) is preserved by an ergonomic layer that queries the graph. As a
side effect the vault becomes the cross-machine identity for a workspace:
the per-machine registry maps a synced note ID to a local home.

Because this reshapes the seams between three subsystems, the change also
adopts a hard rule they violate today: **workspaces, gptel sessions, and
org-graph are independent packages.** None may name another's symbols. Each
publishes an extension point (integration registry, hooks, query/write API)
and a thin integration layer, the only module that names two packages,
subscribes.

## What Changes

**Workspaces**
- Add a `:node-id` slot to the workspace record and persisted state: the org
  ID of the project note. The registry name remains the registry key; the
  node ID is the identity that survives re-anchoring and machines.
- Stop scaffolding `home.org` and `sessions/` in the home. **BREAKING**: the
  home directory holds no org content; it is purely operational (repo,
  worktrees, scratch).
- Replace the `home.org` reader with two inversion points: a display-name
  function variable (default: registry name) and the already-customisable
  home layout builder. The anchoring flow drops its `home.org`-presence
  discriminator (`anchored-scaffolded` vs `anchored-existing` collapse).
- Payload contract gains `:node-id`; `:sessions-dir` is removed.
- Delete the `workspace-sessions-dir` consult API.

**gptel sessions** (no external vocabulary; pure persistence changes)
- Sessions always target the single sessions root. **BREAKING**: the
  workspace-directory consult at creation and the `-global` escape-hatch
  command are removed; the root is a plain defcustom the user points at
  `~/org/sessions/`.
- Publish two hooks: session-created and branch-created, carrying file
  paths and identities, so a third party can react without being consulted.
- Stamp a `#+title:` at creation so indexed sessions are distinguishable.
- Branching: the new branch gets a fresh file-level org ID; heading-level
  `:ID:` properties inside the copied history are stripped (a topic node
  belongs to the branch that created it). **BREAKING**: the agents
  subdirectory is renamed `agents/` → `.agents/` so vulpea's hidden-directory
  rule excludes agent transcripts from the index.
- Remove the in-package workspace integration module and the stale
  "Activities integration" spec requirement.

**org-graph** (generic graph capabilities, no workspace or session vocabulary)
- Discovery indexes the vault root only. Delete workspace-home discovery,
  its `:on-create` handler, and the `org-graph-watch-workspace-homes`
  toggle. Collapse the typed-edge extraction root into the vault root so
  chats and top-level vault notes yield edges.
- Add an edge-writer API (append an EDGES item to a file under the
  coordinator lock) and a `project` note creation entry point.
- Add a connected-notes org dynamic block so any note can render its typed
  neighbourhood in place. This is the mechanism behind the virtual home.
- Harden vulpea's parser against file-local variables (`eval:` locals are
  never evaluated during indexing).
- Move gptel tool injection into the workspace-assistant preset out of
  org-graph into the integration layer.

**Integration layer** (new)
- workspaces ↔ org-graph: create the project note at workspace birth and
  record its ID; supply the display-name function and a home builder that
  opens the project note; `find-in-workspace`, `new-in-workspace`, and the
  menu entries.
- workspaces ↔ gptel: the birth session and menu entry (relocated from
  gptel), passing the home as `project-root` for scope expansion.
- gptel ↔ org-graph: subscribe to the two session hooks to write the
  `part-of` edge to the active workspace's project note and a
  `branched-from` edge to the parent branch note.

**Migration**: three existing workspaces (project notes to create, three
sessions plus their branches to relocate, layout blobs that embed `home.org`
paths), four global sessions to relocate, four agent directories to rename.

## Capabilities

### New Capabilities
- `workspace-graph-integration`: the integration layer. Birth-time project
  note and session creation, display-name and home-builder inversion,
  session-hook subscribers that write `part-of` / `branched-from` edges,
  and the workspace-scoped finder and note-creation commands. The only
  capability permitted to name two of {workspaces, gptel sessions,
  org-graph}.

### Modified Capabilities
- `workspaces`: `:node-id` slot and persistence; `home.org` and `sessions/`
  removed from scaffolding and identity coupling; display name and home
  layout via inversion points; anchoring discriminator simplified;
  `workspace-sessions-dir` removed; purge/delete wording updated.
- `workspace-integrations`: payload contract gains `:node-id`, drops
  `:sessions-dir`; creation contexts reduce to `fresh` / `anchored`;
  directionality rule generalised to "no package names another; only the
  integration layer subscribes".
- `sessions-persistence`: single sessions root (consult and `-global`
  command removed); session-created hook; `#+title:` stamping; `.agents/`
  directory name; activities requirement deleted.
- `sessions-branching`: fresh org ID per branch; heading IDs stripped from
  copied history; branch-created hook; agents copied under `.agents/`.
- `org-graph`: discovery = vault root only (workspace substrate and
  workspace integration requirements removed; indexable-workspace-notes
  requirement rewritten as "sessions are indexable"); extraction root =
  vault root; edge-writer API; project-note creation; connected-notes
  dynamic block; parser hardening; assistant tool injection moved out.
- `org-graph-menu`: entries for `find-in-workspace` and `new-in-workspace`
  (delegating to the integration layer when loaded).

## Impact

- **Code**: `config/workspaces/{data-model,persistence,scaffold,tabs,
  integrations,workspaces}.org` (home-org.org deleted);
  `config/gptel/sessions/{filesystem,commands,branching,registry}.org`
  (workspace-integration.org deleted); `config/org-graph/{org-graph,
  discovery,workspace-integration,tools,menu}.org` plus new authoring
  surfaces; a new integration module directory loaded after all three
  packages in `init.org`. Tests for each touched module; a new suite for
  the integration layer that asserts the no-cross-naming rule by grep.
- **Specs**: six delta specs listed above; the archived org-graph spike's
  "project co-location" pillar is formally reversed.
- **Data**: `~/emacs-workspaces/*` lose `home.org` and `sessions/`;
  `~/.gptel/sessions/*` move to `~/org/sessions/`; `workspaces.eld` gains
  `:node-id` and needs its layout blobs re-pointed; the vulpea DB rebuilds
  on the next full scan. A one-shot migration command covers all of it.
- **Out of scope**: git-worktrees (derive everything from `:home`,
  unchanged), the environment preamble (workspace-neutral by contract,
  unchanged), the four externalised typed-edge tasks in `.tasks/`
  (extractor union, `rel:` link type, inverse rendering, runbook) which
  remain independent follow-ups.
