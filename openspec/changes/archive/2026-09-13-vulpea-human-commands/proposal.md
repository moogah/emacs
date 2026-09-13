# Proposal: vulpea-human-commands

## Why

The org-graph spike eval surfaced a gap: org-graph has no human-side
creation path. Every `org-graph/find-*` finder passes `:require-match t`,
so the only way to author an indexed note interactively today is
`org-roam-node-find` — which writes to org-roam's index, not vulpea's,
leaving the new note invisible to org-graph until a manual re-index
(observed directly during the eval on 2026-08-13). The end goal is for
vulpea to **replace** org-roam, not live beside it, so the human
interaction surface (find-or-create, insert-link) must exist natively on
the vulpea stack. Two supporting defaults are wrong for that goal:
new-note placement depends on session boot order (falls back to
`org-directory` until `org-graph/configure-sync` has run), and the index
root is the org-roam-shaped `~/org/roam/` rather than the whole `~/org/`
vault.

## What Changes

- **New interactive command: find-or-create.** An org-graph command with
  `org-roam-node-find`-equivalent behavior: complete over all indexed
  notes; selecting a non-existent title creates the note (file written,
  `:ID:` assigned, org-id location registered, vulpea DB updated
  synchronously — the existing `vulpea-create` birth-index path) and
  visits it.
- **New interactive command: insert-link-with-create.** An org-graph
  command with `org-roam-node-insert`-equivalent behavior: select or
  create a note, then insert an `[[id:...]]` link to it at point
  (active-region text becomes the link description, matching org-roam
  ergonomics via vulpea's built-in support).
- **Default notes directory pinned to `~/org/`.**
  `vulpea-default-notes-directory` is set in org-graph config so new-note
  placement is boot-order-independent. (Already landed on the branch in
  `config/org-graph/discovery.org` § Default notes directory; this change
  formalizes it with spec coverage.)
- **Index root widens from `~/org/roam/` to `~/org/`.**
  `org-graph/index-roots` returns `~/org/` (plus active workspace homes,
  unchanged). This keeps the bounded-roots invariant (still one explicit
  vault root, never a `~/work` walk) while making the notes directory and
  the index root the same directory — without this, notes created in
  `~/org/` are indexed at birth but silently dropped on any full re-scan
  or DB rebuild, which only walks the sync directories. Survey: `~/org/`
  holds 1106 org files, 1099 of them already under `roam/`; the widening
  adds only a handful of files, and vulpea ignores those without an
  `:ID:`.
- **New Transient menu on `SPC v`.** A transient prefix (pattern:
  `workspaces-transient.el`) exposing the full human interaction surface
  for the graph in one place, bound to `SPC v` in evil normal state
  (`SPC v` is currently unbound). Contents (exact layout settled in
  specs/design): find-or-create and insert-link (the new commands above);
  the per-type finders (`topic` / `debug` / `log` / `reference` /
  `project` / `any` / agent drafts); typed-edge queries for the note at
  point (outgoing / incoming / connected); and maintenance operations
  (re-index via `org-graph/configure-sync`, note-type validation,
  `vulpea-doctor`).
- **Non-goal — org-roam interop.** vulpea-created notes are NOT synced
  into org-roam's DB, and org-roam-created notes get no special
  vulpea-side pickup beyond living under the widened root. The two
  systems are not meant to coexist long-term; actually retiring org-roam
  (unloading it, migrating dailies/capture) is a separate future change.

## Capabilities

### New Capabilities
- `org-graph-note-commands`: Human-side interactive command surface for
  the vulpea-backed graph — find-or-create a note, and insert a link to
  a (possibly newly created) note. Covers create-on-miss behavior,
  immediate indexing/id-resolution of created notes, and default
  placement in the vault root.
- `org-graph-menu`: Transient menu on `SPC v` aggregating the full
  human-side graph interaction surface — note commands, per-type
  finders, typed-edge queries for the note at point, and maintenance
  (re-index, validation, doctor). Covers the binding, menu structure,
  and context behavior (e.g. edge queries needing a note at point).

### Modified Capabilities
- `org-graph`: Discovery requirements change — the vault index root
  becomes `~/org/` (was `~/org/roam/`), and new-note placement
  (`vulpea-default-notes-directory`) is pinned to the vault root
  independent of sync configuration. Delta against the org-graph-spike
  change's spec (`openspec/changes/org-graph-spike/specs/org-graph/spec.md`;
  not yet archived to `openspec/specs/`).

## Impact

- **Modules:** `config/org-graph/discovery.org` (index roots, default
  notes directory — partially landed), new or extended module for the
  interactive commands (likely `finders.org` or a new `capture.org`),
  a new transient-menu module (pattern: `config/workspaces/workspaces-transient.el`),
  `config/org-graph/org-graph.el` loader for any new modules.
- **Keybinding:** `SPC v` → menu prefix, evil normal state. Global SPC
  bindings live in `config/core/evil.org` today; design decides whether
  the binding lands there or is installed by the org-graph menu module
  itself (guarded on evil being loaded, keeping org-graph self-contained).
  Load-order note: `transient` must load before the menu module, same
  constraint as the other transient-based menus.
- **`org-graph-roam-root` blast radius (design decision needed):** the
  defcustom currently anchors index roots, typed-edge extractor gating
  (`org-graph-extractor--roam-note-p`, D2 roam-only), edge-type scanning
  (`edge-type.el`), and agent write-node targeting (`tools.el`). Design
  must decide whether the widened root repoints `org-graph-roam-root`
  itself or introduces a separate vault-root variable, and whether
  typed-edge extraction stays gated to `~/org/roam/` or widens with it.
- **Spike eval:** the runbook's Discovery and Coexistence (D8) checks in
  `config/org-graph/docs/spike-eval.org` assume roam-root-only indexing
  and side-by-side operation; findings should record that this change
  supersedes the D8 coexistence framing.
- **Tests:** existing discovery specs in `config/org-graph/test/` assert
  the `~/org/roam/` root and will need updating; new command surface
  needs Buttercup specs.
- **Dependencies:** no new packages — builds on installed vulpea 2.4
  (`vulpea-find`, `vulpea-insert`, `vulpea-create`).
