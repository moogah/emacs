## Context

Three subsystems currently share one filesystem convention: a workspace's
`:home` holds `home.org` (identity) and `sessions/` (gptel sessions), and
org-graph feeds every home to vulpea as a recursive index root. The survey
behind this change (see proposal.md) found the convention broken in three
independent ways — recursive home indexing sweeps third-party org files
and evaluates their local variables; notes inherit the lifetime of
short-lived worktrees; and sessions under homes are invisible to the
session registry because enumeration only walks the global root.

The coupling is also asymmetric. `workspaces` forbids naming gptel
symbols (lint-enforced), yet `config/gptel/sessions/` names workspaces in
two places (the sessions-dir consult in `filesystem.org` and the whole
`workspace-integration.org` module), and `config/org-graph/` names both
(`discovery.org`, `workspace-integration.org`). gptel sessions exposes no
hooks, so nothing outside it can react to a session being created.

Facts that constrain the design:

- vulpea (v2.4.0 pinned) walks sync roots recursively, excludes only
  paths containing `/.`, has no file predicate, and its untracked-file
  cleanup enforces "in DB ⇔ under a sync root". Its `notes` table has the
  org ID as primary key with a plain `INSERT`, so a second file carrying
  an ID already in the DB fails to index.
- Branch creation byte-copies the parent's `session.org` up to the branch
  point (drawer included) and rewrites only `:GPTEL_SESSION_ID:` and
  `:GPTEL_BRANCH:`; the parent's `:ID:` and any heading IDs ride along.
  Agent directories are copied wholesale.
- The session directory is *derived* (walk up to the nearest enclosing
  `branches/`), identity is drawer-first, and one defcustom
  (`jf/gptel-sessions-directory`) is the sole root for creation and
  enumeration — relocating the tree is supported by construction.
- `gptel-org-branching-context` is on: headings inside a chat are
  context forks; per-heading `GPTEL_*` properties are already expected;
  property drawers are stripped from prompts by default, other drawers
  are not.
- vulpea heading-level notes inherit filetags, and the typed-edge
  extractor attributes to the nearest ID'd ancestor.
- The workspace record has no identity beyond `name == basename(home)`;
  the `:ID:` stamped into `home.org` is never read back. Persisted layout
  blobs embed absolute `home.org` paths in `workspace-buffer` records.
- The vault already contains a prior incarnation of this idea: 41
  `activity`-tagged anchor notes with hand-maintained session headings
  linking to external session files. Its weak points (write-only anchor,
  unindexed sessions) inform D7/D8.

## Goals / Non-Goals

**Goals:**

- All notes live in the vault; a workspace's `:home` is purely operational.
- The project note's org ID is the workspace's canonical, machine-
  independent identity; the per-machine registry maps it to a local home.
- Association is a typed edge (`part-of`), lineage is a typed edge
  (`branched-from`); no per-workspace filetag.
- The co-located *workflow* survives: find-in-workspace, new-in-workspace,
  and a home view rendered from the graph.
- gptel sessions (including branches and ID'd topic headings) are indexed
  notes with distinguishable titles; agent transcripts are not.
- `workspaces`, gptel sessions, and `org-graph` are independent packages;
  one integration layer is the only code naming two of them, and a lint
  proves it.
- The vulpea local-variable exposure is closed regardless of roots.

**Non-Goals:**

- Retiring org-roam (still coexists; vault root already covers `roam/`).
- The four externalised typed-edge tasks in `.tasks/` (extractor union,
  `rel:` link type, inverse rendering, runbook).
- Selective agent copying on branch; changing the chat-mode block format.
- Multi-vault or per-machine vault roots.
- A general "rename workspace" command (rename = retitle the project note).

## Decisions

### D1 — Three packages plus one integration layer (`config/integrations/`)

A new module directory `config/integrations/` holds the only code that
names two packages, loaded via one loader
`integrations/integrations.org` that `jf/load-module`s three submodules
in order: `workspaces-org-graph`, `workspaces-gptel`, `gptel-org-graph`.
Its `jf/enabled-modules` entry sits after `org-graph/org-graph` (the last
of the three packages to load). Every submodule wraps its body in
`(when (and (featurep 'workspaces) (featurep 'org-graph)) …)`-style guards
(and `with-eval-after-load` where a package may load later), so a missing
package makes the submodule a no-op rather than an error.

Alternatives: (a) make org-graph the hub — rejected, it would couple
org-graph to gptel sessions and workspaces permanently and the user's
independence rule forbids it; (b) keep the two existing in-package
integration modules and just fix the symbols — rejected, the rule is "no
package names another", not "name it politely".

The boundary lint (`config/integrations/test/boundary-lint-spec.el`)
greps tangled `.el` files: `config/workspaces/*.el` must not match
`jf/gptel-\|gptel-\|org-graph`; `config/gptel/sessions/*.el` must not
match `workspace-\|org-graph`; `config/org-graph/*.el` must not match
`workspace-\|jf/gptel-`. Existing `register/boundary/gptel-sessions-
workspace-consult` lint is subsumed.

### D2 — `:node-id` is identity; the registry name stays the key

`workspace--make` gains an optional `node-id` (string or nil) stored as
`:node-id`; `workspace-node-id` / `workspace-set-node-id` are public; the
setter flushes persistence synchronously (it is a deliberate identity
change, same class as `workspace-new`). The payload constructor adds
`:node-id` and drops `:sessions-dir`; `workspace--sessions-dir` and
`workspace-sessions-dir` are deleted. Persistence schema stays v3 —
`:node-id` is an optional key, absent loads as nil — so no version bump
and no file rejection for existing state.

Rekeying the registry by node ID was rejected: every `completing-read`,
tab name, and persistence lookup keys on the name, and a nil node ID
must remain a valid (integration-less) workspace.

### D3 — Inversion points replace the `home.org` reader

`config/workspaces/home-org.org` is deleted. `workspace--display-name`
calls `workspace-display-name-function` (default: registry name) inside
`condition-case`, falling back to the name. `workspace-home-builder`'s
default becomes "switch to `*scratch*`" and the builder receives the
workspace record (it previously relied on ambient context). The tab-bar
formatter, `workspace--new-anchor-existing`, and the persistence restore
fallback are updated accordingly; `workspace--capture-home-layout` is
unchanged (it stamps whatever the builder produced).

### D4 — Scaffold shrinks to `mkdir` + `git init`; contexts are `fresh` / `anchored`

Stages 4–6 of the pipeline (`home.org`, `sessions/`, initial commit) are
removed; with nothing to commit, the repository is left empty. The
prefix-arg flow keeps `read-directory-name` and reduces to: `.git` present
→ register (`anchored`); absent → `git init` then register (`fresh`).
`anchored-scaffolded` and `anchored-existing` disappear from the payload
vocabulary and the birth-offer gate now admits both contexts (the offer
writes nothing until the user picks a command). The context-demo /
emacs-mcp / test-graph-spike `.gitignore` entries are untouched.

### D5 — gptel sessions: root, hooks, keywords, `.agents/`, branch identity

- **Root.** `jf/gptel--target-sessions-root` collapses to
  `jf/gptel--ensure-sessions-root`; the `force-global` parameter threads
  out of `jf/gptel--create-session-directory` and
  `jf/gptel--create-session-core`; `jf/gptel-persistent-session-global`
  and `config/gptel/sessions/workspace-integration.org` are deleted. The
  defcustom default stays `~/.gptel/sessions/`; `init.org` (or the machine
  role file) sets it to `~/org/sessions/`. gptel never learns why.
- **Hooks.** `jf/gptel-session-created-hook` runs at the end of
  `jf/gptel--create-session-core` after the sibling file and the
  registry entry exist, via `run-hook-wrapped` with a `condition-case`
  that logs through `jf/gptel--log` and continues.
  `jf/gptel-branch-created-hook` runs at the end of
  `jf/gptel--create-branch-session` after the identity rewrite. Payloads
  are plists (extensible, like the workspaces anchor payload).
- **Keywords.** `jf/gptel--initial-session-body` emits, after the drawer
  text, `#+title: <session-id>` and `#+filetags: :gptel_session:` then
  the blank line and empty user block. The chat-mode save path's drawer
  materialiser only touches the property drawer, so the keywords are
  user-owned afterwards. The `magic-mode-alist` signature matches on the
  drawer head and is unaffected.
- **`.agents/`.** `jf/gptel--agents-dir-path` returns `.agents`;
  `jf/gptel--find-all-branches-with-agents`, `jf/gptel--copy-branch-agents`,
  and the PersistentAgent directory creation follow. No compatibility
  shim for `agents/`: the migration command renames existing ones.
- **Branch identity.** `jf/gptel--rewrite-branch-identity-keys` gains two
  steps: replace the file-level `:ID:` with `(org-id-new)` (registering
  it via `org-id-add-location` under `org-id-overriding-file-name`, the
  same trick `jf/gptel--stamp-session-org-id` uses), and rewrite
  `#+title:` to `<session-id> (<branch>)`. A new
  `jf/gptel--strip-heading-ids` post-processes the copied buffer: for
  every property drawer *below* the file-level one, delete the `:ID:`
  line; delete the drawer if it becomes empty. Regex on
  `org-property-drawer-re` is sufficient — drawers are line-anchored and
  the file-level drawer is skipped by position. The file-level `EDGES`
  drawer copies verbatim (D7).

### D6 — org-graph: vault-only discovery, whole-vault extraction, three new APIs

- **Discovery.** `org-graph/index-roots` returns `(list vault-root)`;
  `org-graph--active-workspace-homes`, `org-graph-watch-workspace-homes`,
  and `config/org-graph/workspace-integration.org` (both handlers and the
  preset tool-injection) are deleted; the loader drops that slot (eleven
  → ten modules; `module-load-spec.el` order updated).
- **Extraction root.** `org-graph-roam-root` is removed. The extractor's
  scope gate becomes "path under `org-graph-vault-root`" (i.e. always
  true for indexed notes; the gate stays as a defensive check).
  `org-graph-tools--roam-root` is replaced by a new defcustom
  `org-graph-agent-notes-directory` (default `~/org/roam/`) so agent
  drafts keep landing where they do today; the write tool refuses a
  directory outside the vault root. The edge-type seed installer targets
  the agent-notes directory as before.
- **Parser hardening.** In `discovery.org`, advise
  `vulpea-db--parse-with-temp-buffer` `:around` to bind
  `enable-local-variables nil` and `enable-dir-local-variables nil`; advise
  `vulpea-db-sync--update-file-if-changed` `:around` with a
  `condition-case` that logs `(file . error)` to `*org-graph*` and returns
  nil, so one bad file never enters the debugger from the sync timer.
  Advice rather than a fork of vulpea: two functions, both stable across
  2.x.
- **Edge-writer API.** `org-graph/write-edge FILE TYPE TARGET &optional
  NODE-ID` (in `authoring.org`): under `org-graph-coordinator/with-file-lock`,
  visit FILE in a temp buffer with `delay-mode-hooks` org-mode, locate the
  node (file-level: `point-min`; heading: `org-id-find` position within
  the file), find or create the `EDGES` drawer immediately after that
  node's property drawer, append `- TYPE :: [[id:TARGET]]` unless an
  identical item exists, write back, then `vulpea-db-update-file`. Reuses
  the extractor's drawer-name resolver so the name stays configurable.
- **Project-note API.** `org-graph/create-project-note TITLE &optional
  BODY` uses `vulpea-create` with a template carrying `#+filetags:
  :project:`, a `status: active` property, the dynamic block skeleton,
  and BODY; returns the note ID. Placement follows the existing
  `vulpea-default-notes-directory` / dash filename template.
- **Dynamic block.** `org-dblock-write:org-graph-connected` (in a new
  `dblock.org` module, slot after `query`) takes `:id` (default: the
  file-level node) and `:types`. It queries incoming edges, resolves each
  source note via `vulpea-db-get-by-id`, groups by the first note type
  whose schema predicate matches (`org-graph-schemas--note-types` order,
  `untyped` last), and for each source with `branched-from` edges renders
  the lineage as an indented sublist. Output is plain org list text so
  regeneration is idempotent.

### D7 — Edge placement and prompt hygiene for sessions

The integration layer writes a session's `part-of` edge to the
file-level node, so the `EDGES` drawer lands directly after the file-level
property drawer, ahead of `#+title:`. Consequences: the branch byte-copy
carries it (the branch point is always after the head), and the
identity-rewrite step never touches it. Because the chat subsystem builds
prompts from parsed `#+begin_user` / `#+begin_assistant` blocks
(`gptel-chat-parse-buffer`) rather than from raw buffer text, drawers
outside blocks are not sent to the model; the implementation task
verifies this with a spec on the send path and, if wrong, adds `drawer`
to `gptel-org-ignore-elements` in chat-mode buffers only.

### D8 — The integration layer's three submodules

**`workspaces-org-graph.org`** — registers integration `org-graph-project`
*first* (so its handler runs before the gptel one and later payloads see
`:node-id`; registration order = dispatch order): `:on-create` calls
`org-graph/create-project-note` with `:name` unless `:node-id` already
resolves (`vulpea-db-get-by-id`), then `workspace-set-node-id`.
`:on-purge` sets the project note's `status` to `done` (the note is never
deleted). Installs `workspace-display-name-function` (title via
`vulpea-db-get-by-id`, nil on miss) and a `workspace-home-builder` that
opens the note's file, regenerates the dynamic block when the buffer is
unmodified, and saves only in that case. Provides `find-in-workspace`
(`vulpea-find` with a filter built once per call: the set of IDs with
`part-of` incoming edges to the node, expanded to every note whose *path*
equals one of those notes' paths — that is how heading notes inside a
session are admitted) and `new-in-workspace` (`org-graph/find-or-create`
then `org-graph/write-edge` on the resulting file). Registers the
`session` schema (`gptel_session` filetag predicate) and contributes the
Workspace group via `org-graph-menu-extra-groups`, a new alist variable
the menu module reads at prefix-build time. Also provides
`workspace-adopt-project-note`, which prompts with the `project` finder
and calls `workspace-new` with that ID — the cross-machine path.

**`workspaces-gptel.org`** — the relocated birth session and `g` menu
entry: registers integration `gptel-session` with `:on-create` calling
`jf/gptel--create-session-core` with `project-root` = `:home` and the
workspace-initial preset (the defcustom moves here, keeping its name),
and populates the `workspace-assistant` preset's `:tools` with
`org-graph/agent-tools` under `with-eval-after-load`.

**`gptel-org-graph.org`** — adds to `jf/gptel-session-created-hook`: if
`(workspace--current-name)` resolves to a workspace with a `:node-id`,
`org-graph/write-edge` `part-of` from the new file to it. Adds to
`jf/gptel-branch-created-hook`: `org-graph/write-edge` `branched-from`
from the branch file to the parent file's file-level ID (read from the
parent's drawer head). Both run after the file exists, so vulpea's
autosync picks up the edited file on its next batch.

### D9 — Migration command

`workspace-migrate-to-vault` (in `workspaces-org-graph.org`), interactive,
idempotent, reporting each action:

1. For each registered workspace with nil `:node-id`: read
   `<home>/home.org` if present (title from `#+TITLE:`, body below the
   keywords); `org-graph/create-project-note`; `workspace-set-node-id`.
2. For each `<home>/sessions/<id>/`: `rename-file` into
   `jf/gptel-sessions-directory`; write `part-of` for every
   `branches/*/session.org` in it; re-stamp a fresh `:ID:` on every
   non-`main` branch (pre-existing branches share the parent's ID).
3. For each `~/.gptel/sessions/<id>/` (the old root, when different):
   `rename-file` into the new root; re-stamp non-`main` branch IDs.
4. Rename every `branches/*/agents/` to `.agents/`.
5. Delete `<home>/home.org` and `<home>/sessions/` (only if now empty).
6. Rewrite persisted layout blobs: any `workspace-buffer` whose filename
   is a former `home.org` is re-pointed at the project note's file.
7. `org-graph/configure-sync` (full scan) and
   `org-graph/seed-org-id-locations`.

Run once per machine after the code lands. Rollback: `git` for code; the
moved files are plain directories (`mv` back), and the vault notes are
inert if unused.

### D10 — Testing approach

Buttercup, co-located per package, run with `./bin/run-tests.sh -d <dir>`:

- `config/workspaces/test/`: `data-model-spec.el` (node-id slot, payload
  shape, no `:sessions-dir`); `persistence` specs (round-trip, absent →
  nil, setter flushes); `display-name-spec.el` rewritten for the function
  variable and fallback; `scaffold` / anchoring specs (pipeline stops at
  `git init`; `fresh`/`anchored` contexts; birth offer for both);
  `gptel-integration-spec.el` and `workspace-routing-spec.el` deleted
  (they asserted the removed consult).
- `config/gptel/sessions/test/`: `filesystem/` (single root; `.agents/`
  paths); `commands/` (keywords emitted; hook fires with plist; failing
  hook contained); `branching/` (fresh `:ID:`, heading IDs stripped,
  drawer/keywords/EDGES preserved, branch hook after rewrite);
  `workspace-integration-spec.el` deleted.
- `config/org-graph/test/`: `discovery-spec.el` (single root; hardening
  advice binds `enable-local-variables`; sync error contained);
  `authoring-spec.el` (edge-writer: creates drawer after property drawer,
  idempotent, heading node, lock acquired via spy); `dblock-spec.el`
  (grouping and lineage from stubbed query rows); `tools-spec.el` (agent
  directory default, outside-vault refusal); `module-load-spec.el`
  (ten-module order); `extractor-spec.el` (scope gate is vault root).
- `config/integrations/test/`: `boundary-lint-spec.el` (grep contract from
  D1); `workspaces-org-graph-spec.el` (on-create creates note and sets
  node-id, skips when resolvable; display name/home builder; finder set
  construction from stubbed edges including path-expansion; new-in-
  workspace writes the edge; adopt command); `workspaces-gptel-spec.el`
  (birth session called with `project-root`, preset tools populated);
  `gptel-org-graph-spec.el` (hook subscribers write the two edges with the
  expected triples; no edge off-workspace); `migration-spec.el` (in a
  temp tree: notes created, sessions moved, IDs re-stamped, agents
  renamed, idempotent second run is a no-op).

Mocks follow the existing pattern: `cl-letf` on vulpea / org-id / process
entry points, spies for cross-module calls, temp directories for
filesystem behaviour. Scenario mapping: each spec `it` names the spec
scenario it covers in a leading comment.

## Risks / Trade-offs

- [Indexing chats makes long sessions produce many heading notes] → only
  ID'd headings become notes; the user opts in per topic. If it grows
  noisy, `vulpea-db-index-heading-level` accepts a path predicate.
- [Dynamic-block regeneration edits the project note on every home
  build] → regenerate only when the note buffer is unmodified, and the
  output is deterministic text, so a no-change regeneration leaves the
  file byte-identical.
- [`#+title:` on sessions surfaces in `vulpea-find` alongside notes] →
  intended; the `session` type and the `gptel_session` filetag let
  finders include or exclude them.
- [Hook subscribers write to a file gptel just created, racing the
  chat-mode buffer] → the hook runs before the buffer is displayed and
  the edge writer goes through the coordinator lock; the chat buffer is
  opened from disk afterwards. Verified by an integration spec ordering
  assertion.
- [Migration touches the persistence file's layout blobs] → the file is
  already corruption-safe (temp + rename, read round-trip); the command
  backs it up to `workspaces.eld.pre-vault` first.
- [`org-graph-roam-root` removal changes the agent write default's name]
  → the value is unchanged (`~/org/roam/`); only the variable is renamed,
  and the tool description is regenerated from it.
- [Advice on vulpea internals] → two private-ish functions; pinned
  version; `discovery-spec.el` asserts the advice is installed and effective
  so a vulpea bump that renames them fails loudly.
- [Birth offer now runs for adopted repos] → the offer itself writes
  nothing; the worktree command still writes only under `:home`.

## Migration Plan

1. Land package-internal changes first, each independently green:
   workspaces (D2–D4), gptel sessions (D5), org-graph (D6). At this
   point workspaces no longer scaffold notes and gptel no longer consults
   workspaces; nothing links them yet.
2. Land the integration layer (D7–D8) and its lint; enable
   `integrations/integrations` in `init.org`; set
   `jf/gptel-sessions-directory` to `~/org/sessions/`.
3. Land and run `workspace-migrate-to-vault` (D9) on this machine; commit
   the resulting vault notes in `~/org`; re-run the org-graph runbook's
   Discovery checks with the single root.
4. Sync the seven delta specs at archive; delete the stale activities
   requirement.
5. Rollback: revert the commits (the packages are independent, so partial
   rollback is possible per package); `mv` session trees back; vault notes
   are inert.

## Open Questions

- **OQ-A:** Should stamping an `:ID:` on a topic heading be a gptel
  command (a wrapper around `gptel-org-utils-new-topic` that also calls
  `org-id-get-create`) in this change, or left to `M-x org-id-get-create`?
  Leaning: small pure-gptel addition, add as a task if cheap.
- **OQ-B:** `find-in-workspace` path-expansion admits *every* ID'd heading
  in a linked session, including ones the user ID'd for unrelated
  reasons. Acceptable for now; a `part-of` edge on the heading itself
  could later narrow it.
- **OQ-C:** Does the `session` finder in the org-graph menu (spec'd in the
  Find group) belong there when no integration layer registered the
  `session` schema? It then offers nothing; harmless, but the menu could
  hide it via the same extra-groups mechanism.
- **OQ-D:** Cross-machine flow beyond `workspace-adopt-project-note` — for
  example, auto-suggesting project notes with no local workspace. Deferred.
