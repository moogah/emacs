---
status: speculated
status_set_at: 2026-09-20T00:00:00Z
status_set_by: architect
status_note: |
  Set at cycle-1789935943 plan (the change's first orchestrated cycle). The
  forward chain has not yet executed against this proposal, so `speculated` is
  the only defensible value. Promotion to `confirmed` requires two cycles of
  clean forward chain with no goal-drift signal; this change's dependency
  graph is six levels deep, so that is cycle 3 at the earliest.

  cycle-1790453714 plan (2026-09-26, cycle 2 of this change): STAYS
  speculated. Cycle 1 closed clean (goal-drift query fired=false,
  recommendation 'continue'; 16 design decisions narrowed behaviour
  INSIDE the stated outcome, none changed it). That is one of the two
  clean cycles promotion requires. The remaining outcome chain is
  jump-seam -> missing-repo-choice / off-workspace-restore -> docs-readme,
  all in the cycle-2 batch.
---

## Why

Emacs bookmarks store absolute paths, so a bookmark set inside one workspace's
git worktree points at *that* worktree forever — and at nothing at all once the
workspace is purged. In a workflow where every workspace holds its own worktree
of the same repo, the bookmark records "this file in this one checkout" when the
user means "this file in the emacs repo". The result is that bookmarks are
effectively unusable: the live bookmark files on this machine hold one real
bookmark each.

Worse than being useless, the current behaviour is quietly wrong. Jumping from
workspace B to a bookmark made in workspace A opens A's copy of the file, on A's
branch, with no indication that you are editing the wrong checkout.

This change makes a bookmark resolve to *the current workspace's* checkout of the
repo it belongs to, on this machine. Cross-machine sharing, layout/entry-point
bookmarks, and workspace templates are deliberately deferred (see Out of scope):
this slice establishes the encoding and the resolution contract that all of them
build on.

## What Changes

- **New module** `config/workspaces/portable-bookmarks.org` — a consumer-side
  module that names `bookmark`, the workspace registry, and `git`; it does NOT
  require magit, and workspaces core does not name it.

- **Stamp portable coordinates at bookmark creation.** A record for a file
  inside a git repo gains two extra properties: the repo's *identity*
  (normalized remote URL, e.g. `github.com/moogah/emacs`) and the file's
  repo-relative path. The absolute `filename` is left untouched, so the record
  stays valid for stock Emacs and for a session where this module is absent.

- **Resolve at jump time, on a copy.** The bookmark's target is recomputed for
  the current workspace and substituted into a *copy* of the record, which is
  then handed to the record's own native handler (file, dired, org, …). The
  stored record is never modified.

- **A closed set of resolution outcomes**, none of which silently opens the
  wrong file:
  - `resolved` — the current workspace holds a checkout of the repo and the file
    exists there.
  - `not-on-branch` — the checkout exists but the file does not (renamed,
    deleted, or absent on this branch). Reported; **never** visited as an empty
    new buffer.
  - `missing-repo` — the workspace holds no checkout of the repo. The user is
    offered: add a worktree here, view the canonical clone read-only, or cancel.
  - `no-clone` — no checkout of the repo is in view on this machine.
  - `non-portable` — the record carries no portable coordinates (no repo, no
    remote, or a shape this slice does not understand). Falls through to
    today's behaviour: the stored absolute path.

- **Off-workspace jumps restore a workspace.** When the current tab is not a
  workspace, the user picks a workspace from the *registry* (not merely from
  live tabs) and it is restored via the existing `workspace-restore`, then
  resolution proceeds. This makes bookmarks work immediately after a restart,
  which is when they are most wanted. Broken workspaces remain refused.

- **Checkout classification.** Discovery distinguishes a repo's canonical
  checkout (its main worktree) from linked worktrees, and refuses to offer a
  *workspace-owned* worktree — or a stale sibling worktree — as the canonical
  clone. Without this rule, "view read-only" could land in another workspace and
  reproduce the very bug this change fixes.

- **Git-derived discovery, no configuration.** Identity and sibling checkouts
  are obtained by asking git about checkouts already in view (the current
  workspace's worktrees, other registered workspaces' worktrees, the config repo
  Emacs is running from). No directory scanning, no cache, no machine-local
  configuration in this slice.

- **BREAKING (spec-level only, no behaviour removed):** the
  `workspace-git-worktrees` repo-source requirement currently names
  `projectile-known-projects` as its minimum candidate source. Projectile is
  being retired in this config because it only sees repos that have been
  registered — a per-machine chore and a source of machine-to-machine drift.
  Git-derived discovery becomes the primary source; projectile becomes one
  optional legacy source among others. The `read-directory-name` floor is
  unchanged.

## Capabilities

### New Capabilities

- `portable-bookmarks`: bookmark records carry repo identity plus a
  repo-relative path; jumps resolve that pair against the current workspace
  through a closed set of outcomes, establishing a workspace context first when
  there is none. Covers the creation seam, the jump seam, checkout
  classification, and the degradation rules for records that cannot be made
  portable.

### Modified Capabilities

- `workspace-git-worktrees`: the "Soft repo-source selection" requirement is
  amended so git-derived discovery is primary and projectile is optional legacy
  rather than the named minimum source. No change to worktree creation,
  teardown, or the derived-not-stored rule.

## Impact

**New code**
- `config/workspaces/portable-bookmarks.org` → `portable-bookmarks.el`
- Registered in `jf/enabled-modules` in `init.org`, after `workspaces`.
- Tests: `config/workspaces/test/portable-bookmarks/` (Buttercup, `*-spec.el`).

**Modified code**
- `init.org` — module list entry.
- `config/workspaces/git-worktrees.org` — only if the repo-candidate helper is
  re-ordered to put git-derived candidates first; the magit-dependent worktree
  operations are untouched.

**Emacs integration points (both advice, both single choke points)**
- `bookmark-make-record` — `:filter-return`, to stamp coordinates.
- `bookmark-handle-bookmark` (`bookmark.el:1339`) — called by
  `bookmark--jump-via`, so it covers `bookmark-jump`,
  `bookmark-jump-other-window`, and `consult-bookmark`.

**Dependencies**
- `git` (subprocess calls only; no magit).
- The workspace registry, read from the consumer side: `workspace--registry`
  accessors, `workspace--current-name`, `workspace-restore`,
  `workspace--broken-p`.

**Not affected**
- `bookmark-default-file` location and format (records gain properties; stock
  Emacs still reads the file).
- Workspace layout records in `workspaces.eld` (a separate bookmark system;
  untouched by this slice).
- Existing bookmarks: unstamped records keep working via `non-portable`.

**Out of scope (deliberate, each a later slice)**
- Cross-machine sharing and the storage decision it requires (1b).
- Entry-point bookmarks that restore a workspace at a named layout (1c).
- Portable layouts (2) and workspace templates (3).
- Records with nested records or window trees, and per-handler path properties
  beyond `filename` (Tier 2/3).
- Any UI marking which bookmarks are portable.
