---
status: confirmed
status_set_at: 2026-09-28T14:02:40+00:00
status_set_by: architect
confirmed_after_cycle: cycle-1790453714
confirmed_basis: |
  Two consecutive clean cycles of the forward chain against this proposal:
  cycle-1789935943 (handshake `user_resolved_goal_drift` query_fired=false,
  recommendation `continue`) and cycle-1790453714 (same; 13/13 batch tasks
  done, 0 divergent register entries, the end-of-cycle audit's goal-drift
  verdict "NO goal drift — the outcome is reached at 45df6dfd": every
  proposal outcome has a live code path and a behavioural spec). The 16
  cycle-1 and 4 cycle-2 user decisions narrowed behaviour INSIDE the stated
  outcome; the one design reversal (D9, ask-cycle-1790453714-3) is design,
  not a proposal outcome. Set at cycle-1790604160 plan per the template's
  `speculated -> confirmed` lifecycle hook. Scope note carried, not drift:
  the change also hardened core workspaces (proposal says core is "not
  affected"); doc-sync-cycle2 records it in the body and two new delta
  specs (ask-cycle-1790453714-8).
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

  cycle-1790604160 plan (2026-09-28, cycle 3): speculated -> CONFIRMED (see
  confirmed_basis). Cycle 3 is doc-sync plus three follow-ups from the
  cycle-2 end-of-cycle audit; then verify / sync / archive.
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

- **Stamp portable coordinates at bookmark creation.** A record for a single
  file or directory that git tracks, inside a repository with a usable remote,
  gains three extra properties, stamped all together or not at all: the repo's
  *identity* (normalized remote URL, e.g. `github.com/moogah/emacs`), the
  location's repo-relative path as git spells it (`./` for the root), and a
  stamp-format version (`1`). Untracked or ignored locations, and repositories
  with no usable remote, stay unstamped. The absolute `filename` is left
  untouched, so the record stays valid for stock Emacs and for a session where
  this module is absent.

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
  - `missing-repo` — the workspace holds no checkout and exactly one canonical
    checkout is in view. The user is offered: add a worktree here, view the
    canonical checkout read-only (only when the file exists there), or cancel.
    When no one can be asked (batch, or a layout-restore leaf), it is reported
    and nothing opens.
  - `ambiguous` — the workspace holds two or more checkouts of the repo, or it
    holds none and two or more canonical checkouts are in view; reported,
    nothing opened.
  - `no-clone` — no checkout of the repo is in view on this machine; reported,
    nothing opened.
  - `non-portable` — the record carries no portable coordinates (no repo, no
    remote, or a shape this slice does not understand). Falls through to
    today's behaviour: the stored absolute path.

- **Off-workspace jumps restore a workspace.** When the current tab is not a
  workspace, the user picks a workspace from the *registry* (not merely from
  live tabs) and it is restored via the existing `workspace-restore`, then
  resolution proceeds. This makes bookmarks work immediately after a restart,
  which is when they are most wanted. Broken workspaces are not offered.
  Workspaces holding a checkout of the repo are listed first. With none
  offerable, or an empty answer, the jump resolves without a workspace: for
  `missing-repo` it offers the read-only canonical view or cancel; any other
  outcome is reported. It never opens the stored path.

- **Checkout classification.** Discovery distinguishes a repo's canonical
  checkout (its main worktree) from linked worktrees, and refuses to offer a
  *workspace-owned* worktree — or a stale sibling worktree — as the canonical
  clone. Without this rule, "view read-only" could land in another workspace and
  reproduce the very bug this change fixes.

- **Git-derived discovery, no configuration.** Identity and sibling checkouts
  are obtained by asking git about checkouts already in view (the current
  workspace's worktrees, other registered workspaces' worktrees, the config repo
  Emacs is running from). No recursive directory scanning (only each registered
  home's immediate children are listed) and no machine-local configuration. Two
  in-memory memos bound the git cost and nothing is persisted: repo identity per
  checkout root, revalidated against the repository's config mtimes (DD-7), and
  a per-call discovery memo.

- **BREAKING (spec-level only, no behaviour removed):** the
  `workspace-git-worktrees` repo-source requirement currently names
  `projectile-known-projects` as its minimum candidate source. Projectile is
  being retired in this config because it only sees repos that have been
  registered — a per-machine chore and a source of machine-to-machine drift.
  Git-derived discovery becomes the primary source and `magit-list-repos` (when
  magit is loaded) the only other one; projectile and project.el are no longer
  consulted (DD-12). Every candidate passes the single canonical-admission
  predicate. The `read-directory-name` floor is unchanged.

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
  amended so git-derived discovery (plus magit's list) is the whole candidate
  set; projectile and project.el are dropped. Worktree creation takes a
  caller-supplied `:source-repo` without prompting.
- `workspaces`: the layout-restore reincarnation chain gates on bookmark
  declines; remote homes are refused, or loaded broken; name and purge-scope
  guards.
- `workspace-integrations`: payload context `bookmark-jump`, optional
  `:source-repo`, a menu command invoked by a consumer.

## Impact

**New code**
- `config/workspaces/portable-bookmarks.org` → `portable-bookmarks.el`
- Loaded by a `jf/load-module` line in the package loader
  `config/workspaces/workspaces.org`, after `workspaces-mode.el` and before
  `workspaces-transient.el` (D10); `init.org` is not touched.
- Tests: flat `config/workspaces/test/portable-bookmarks-<topic>-spec.el` files
  plus `portable-bookmarks-fixtures.el` (Buttercup).

**Modified code**
- `config/workspaces/git-worktrees.org` — repo candidates are git-derived plus
  magit only, built in one discovery scope; `jf/workspace--add-worktree`
  honours `:source-repo`; `--worktree-children` compares with core's identity
  predicate.
- workspaces core: `data-model.org` (`workspace--same-file-p`,
  `workspace--path-within-p`), `layouts.org` (the decline gate),
  `integrations.org` (the menu-command accessor), and `tabs.org` /
  `workspaces.org` / `persistence.org` (remote-home and registration guards).

**Emacs integration points (both advice, both single choke points)**
- `bookmark-make-record` — `:filter-return`, to stamp coordinates.
- `bookmark-handle-bookmark` (`bookmark.el:1339`) — called by
  `bookmark--jump-via`, so it covers `bookmark-jump`,
  `bookmark-jump-other-window`, `consult-bookmark`, and workspaces' own layout
  restore (layouts.el calls `bookmark-jump` per leaf). bufferlo's native tab
  and frame restore calls handlers directly and does NOT pass this seam.

**Dependencies**
- `git` (subprocess calls only; no magit).
- The workspace registry, read from the consumer side: `workspace--registry`
  accessors, `workspace--current-name`, `workspace-restore`,
  `workspace--broken-p`, plus `workspace--registered-names`, `workspace--home`,
  `workspace--same-file-p`, `workspace--path-within-p`,
  `workspace--integration-menu-command-p`, `workspace--integration-payload`,
  `workspace--restoring-layout-leaf` (read only when bound), and git-worktrees'
  `jf/workspace--add-worktree` (a quoted, fboundp-guarded soft call, mirroring
  DD-10).

**Workspace layouts**
- The `workspaces.eld` format is unchanged. Each layout leaf's embedded
  `bookmark-make-record` result is stamped like any other record, and its
  restore passes through the jump seam. A leaf whose bookmark declines lands on
  a reason buffer, reported once per restore (asks 1790453714-1/2), never on
  the stored filename.

**Not affected**
- `bookmark-default-file` location and format (records gain properties; stock
  Emacs still reads the file).
- Existing bookmarks: unstamped records keep working via `non-portable`.

**Out of scope (deliberate, each a later slice)**
- Cross-machine sharing and the storage decision it requires (1b).
- Entry-point bookmarks that restore a workspace at a named layout (1c).
- Portable layouts (2) and workspace templates (3).
- Records with nested records or window trees, and per-handler path properties
  beyond `filename` (Tier 2/3).
- Any UI marking which bookmarks are portable.
