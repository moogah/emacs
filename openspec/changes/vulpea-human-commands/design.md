# Design: vulpea-human-commands

## Context

The org-graph spike (change `org-graph-spike`, still open) built the
vulpea-backed graph substrate: discovery over bounded roots, note-type
schemas and finders, typed edges, and the agent tool surface. The eval
window exposed the missing half: humans have no native way to create or
link notes on the vulpea stack. All `org-graph/find-*` finders pass
`:require-match t`; creation happens through `org-roam-node-find` muscle
memory, which feeds the wrong index. Since the end goal is vulpea
*replacing* org-roam, this change adds the human interaction surface —
two authoring commands, a `SPC v` transient menu — and re-anchors the
vault at `~/org/` (index root + default note placement) so created
notes live inside the re-scan boundary.

Already landed on the branch: `vulpea-default-notes-directory` pinned to
`~/org/` in `config/org-graph/discovery.org` § Default notes directory.

Relevant verified facts about installed vulpea 2.4:

- `vulpea-find` with `:require-match nil` offers create-on-miss via
  `vulpea-find-default-create-fn` → `vulpea-create`.
- `vulpea-insert` (with `vulpea-insert-default-create-fn` nil, the
  default) natively: uses active-region text as link description and
  replaces the region, creates missing notes via `vulpea-create`, and
  inserts the `id:` link — all inside an `atomic-change-group`.
- `vulpea-create` is fully synchronous: writes the file, calls
  `org-id-add-location`, calls `vulpea-db-update-file`, and errors if
  the note is not queryable afterwards. This satisfies every
  "immediately findable / immediately id-resolvable" scenario with no
  extra work on our side.

## Goals / Non-Goals

**Goals:**

- `org-graph/find-or-create` and `org-graph/insert-link` commands
  (spec: `org-graph-note-commands`).
- `org-graph-menu` transient on `SPC v` covering find / author / edges /
  maintain (spec: `org-graph-menu`).
- Vault root `~/org/` for `org-graph/index-roots` and note placement
  (spec: `org-graph` delta).
- Sync configuration actually running at boot, so the Discovery
  requirement's auto-pickup scenarios hold on a normal launch (the eval
  showed nothing enables autosync today).

**Non-Goals:**

- org-roam retirement (unloading it, migrating dailies/capture) — future
  change.
- Any org-roam interop (bidirectional index sync) — explicit non-goal in
  the proposal.
- Widening typed-edge extraction or the agent write-node target beyond
  `~/org/roam/` — see Open Questions.
- Note-type selection at creation time (created notes are untyped until
  the user adds a filetag) — see Open Questions.

## Decisions

### D1 — Separate `org-graph-vault-root`; `org-graph-roam-root` keeps its meaning

Introduce a new defcustom `org-graph-vault-root` (default `~/org/`) in
the loader (`org-graph.org`), consumed by `org-graph/index-roots` and
the note-placement config. `org-graph-roam-root` (default `~/org/roam/`)
is NOT repointed: it continues to gate typed-edge extraction
(`org-graph-extractor--roam-note-p`), edge-type registry scanning
(`edge-type.el`), and the agent write-node target (`tools.el`).

*Why:* the spike spec's Typed Semantic Edges requirement literally
excludes notes "outside `~/org/roam/`" from the `typed_edges` index, and
this change's spec delta does not touch that requirement. Repointing
`org-graph-roam-root` wholesale would silently widen extraction and move
agent drafts — spec-visible changes this change did not propose.
*Alternative considered:* one variable repointed to `~/org/` —
rejected for the above; revisit at org-roam retirement (OQ-A).

### D2 — Note placement: directory + filename template in discovery.org

`vulpea-default-notes-directory` stays pinned (landed), switched to
derive from `org-graph-vault-root` rather than a literal. Additionally
set `vulpea-create-default-template` `:file-name` to
`"${timestamp}-${slug}.org"` — vulpea's default uses an underscore
(`${timestamp}_${slug}.org`); the dash form matches the existing corpus
convention (`20260813104944-vulpea_org_graph_spike.org`), keeping one
filename style across old and new notes. Both settings live in
`discovery.org` § Default notes directory (plain `setq`; both variables
are vulpea defcustoms, and `defcustom` never clobbers an existing
binding, so load order is irrelevant).

### D3 — Authoring commands are thin wrappers over vulpea built-ins

- `org-graph/find-or-create` → `(vulpea-find :require-match nil)`
- `org-graph/insert-link` → `(vulpea-insert)`

No custom completion, creation, or link-insertion logic. The wrappers
exist to give the surface stable org-graph names (menu targets, future
filtering hooks, tests) — behavior is vulpea's, verified above to meet
every spec scenario. *Alternative considered:* bespoke
`completing-read` + `vulpea-create` plumbing — rejected as pure
duplication of `vulpea-find`/`vulpea-insert` internals.

### D4 — Two new modules: `authoring` and `menu`

- `config/org-graph/authoring.org` → `authoring.el`: the two commands.
  Requires `vulpea` (hard, same as `finders.el`).
- `config/org-graph/menu.org` → `menu.el`: note-at-point edge-query
  commands (D7/D8), the `org-graph-menu` transient prefix, and the
  `SPC v` binding (D6). Requires `transient` (loaded early in
  `jf/enabled-modules`, well before org-graph).

Loader (`org-graph.org`) canonical order gains two entries: `authoring`
after `finders` (it completes over what finders see), `menu` last (it
references commands from finders, authoring, query, discovery). The
loader-order invariant
(`register/invariant/org-graph-loader-ordered-sequence`) and
`module-load-spec.el` update from nine to eleven submodules; cold load
stays DB-free (neither new module touches the DB at load).

*Why not extend `finders.org`:* finders is scoped to schema-aware
type finders; authoring and menu are different concerns, and separate
modules keep the cold-load invariant auditable per file.

### D5 — Third deferred startup op: configure-sync

Add `org-graph--configure-sync-deferred` to `emacs-startup-hook` in the
loader, wrapping `org-graph/configure-sync` in the same resilient
error-guard pattern as the existing two deferred ops (failures
`display-warning`, never abort startup). Hook order: extractor
registration → configure-sync → org-id seed (sync config before the
seed so a first-ever boot seeds from a DB that is at least being
populated; the seed itself still reads whatever rows exist).

*Why:* the eval demonstrated that on a normal boot autosync is never
enabled and the DB stays frozen at the last manual re-index — while the
spike spec's Discovery requirement already promises save-time pickup and
external-change detection. This lands implementation where the spec
already is, and makes the widened `~/org/` root actually watched from
boot. The menu's Maintain → re-index entry remains the manual recovery
path. *Note:* directories added to the registry mid-session (new
workspaces) still get only the `:on-create` one-shot; live watching of
later-added roots is unchanged (runbook "ongoing pickup" finding, out of
scope here).

### D6 — `SPC v` installed by the menu module, guarded on evil

`menu.org` installs the binding via
`(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "<SPC> v") #'org-graph-menu))`.
`config/core/evil.org` is not touched. *Why:* keeps org-graph
self-contained (one module to delete on rollback, matching the spike's
cheap-rollback posture) and degrades gracefully to `M-x org-graph-menu`
when evil is absent, as the spec requires. Verified `SPC v` is unbound
today. *Alternative:* central binding in `evil.org` alongside the other
SPC keys — rejected while org-graph is still a spike; revisit if the
module graduates.

### D7 — Note-at-point resolution

Edge-query commands resolve their subject via
`(org-entry-get nil "ID" t)` (inherited: enclosing heading first, then
file level) — the same idiom vulpea itself uses in
`vulpea-find-backlink`. On nil, signal
`(user-error "No note with an :ID: at point")`.

### D8 — Edge results render in a dedicated org buffer

`org-graph-query/outgoing|incoming|connected` return edge rows; the
menu commands render them into a read-only org-mode buffer
(`*org-graph-edges*`): one section per direction, one list item per
edge — `- <rel-type> :: [[id:<uuid>][<title>]]` — titles resolved via
`vulpea-db-get-by-id` (falling back to the raw id when unresolvable).
`id:` links are clickable since the startup seed keeps
`org-id-locations` populated. *Alternative:* echo-area `message` —
rejected: multi-edge results need navigation, and org links give free
follow-through.

### D9 — Testing approach

Buttercup specs in `config/org-graph/test/`, following the existing
patterns (`helpers-spec.el` fixtures; DB stubbed via `cl-letf` on
vulpea functions, as in `discovery-spec.el` / `finders-spec.el`):

- `authoring-spec.el` — wrappers delegate to `vulpea-find` /
  `vulpea-insert` with the right arguments (spy on the vulpea entry
  points; behavioral creation scenarios are covered by vulpea itself and
  by the runbook's live checks).
- `menu-spec.el` — prefix exists with all four groups' entries bound;
  note-at-point resolution (heading, file-level, none → `user-error`);
  edge-buffer rendering from stubbed query rows; binding installed when
  evil present, absent-evil load is clean.
- `discovery-spec.el` — index-roots assertions repointed to
  `org-graph-vault-root`; placement/template assertions.
- `module-load-spec.el` — eleven-module canonical order; the third
  deferred op is on `emacs-startup-hook` and drives
  `org-graph/configure-sync` when fired with the DB stubbed.

Run: `./bin/run-tests.sh -d config/org-graph`.

## Risks / Trade-offs

- [Widened root indexes stray top-level files] → surveyed: only ~7 org
  files outside `roam/`, and vulpea skips files without `:ID:`. `.git`,
  `.stfolder`, `agenda/`, `data/` contents are non-org or ID-less noise
  at worst.
- [Boot-time full scan of ~1100 files could stall startup] → vulpea 2.4
  sync is async with smart change detection; the deferred op runs
  post-init. Runbook OQ1 already watches for visible stall; keep it in
  the eval.
- [Vault-top-level notes get no typed-edge extraction (gate stays
  roam-only per D1)] → surprising for anyone authoring `EDGES` drawers
  on a note created at `~/org/` top level: nothing extracts. Near-term
  guidance: concept notes carrying edges belong under `roam/`.
  Tracked as OQ-A.
- [Two deferred-op patterns become three; only wiring-tested] → same
  mitigation as the existing two: runbook live-boot checks; the resilient
  guard reports via `*Warnings*`.
- [Menu duplicates the workspaces Integrations "G" re-index entry] →
  intentional; both dispatch `org-graph/configure-sync`, and the spec
  requires menu entries to be behaviorally identical to their commands.

## Migration Plan

1. Implement per tasks; tangle each touched `.org`
   (`./bin/tangle-org.sh`), run `./bin/run-tests.sh -d config/org-graph`.
2. Fresh-boot verification piggybacks on the spike runbook: re-run the
   Discovery section checks with the widened root (expectations change:
   boot now enables autosync; note the runbook edits under its
   Findings), plus `SPC v` menu smoke test.
3. Rollback: remove `authoring` / `menu` from the loader list and revert
   `discovery.org` / `org-graph.org` — same cheap-rollback posture as
   the spike; authored notes and links are inert data.

## Open Questions

- **OQ-A:** When org-roam retires, do typed-edge extraction and the
  agent write-node target widen from `org-graph-roam-root` to
  `org-graph-vault-root`? Requires a spec delta on Typed Semantic Edges
  (its exclusion scenario names `~/org/roam/` literally). Defer to the
  retirement change.
- **OQ-B:** Should `org-graph/find-or-create` offer note-type selection
  (filetag stamping) at creation? Deferred: created notes are untyped;
  the felt need feeds the spike's RE-3/OQ3 findings.
- **OQ-C:** Live watching of workspace roots added mid-session (the
  runbook's "ongoing pickup" sub-check) — unchanged here; resolve with
  the spike's own findings.
