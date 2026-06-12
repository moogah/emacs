# Design — add-workspace-git-worktrees

## Context

A workspace is a containing directory (`:home`) anchored as a tab. The cycle-6
integration registry (`config/workspaces/integrations.el`) lets peripheral
subsystems attach to the creation flow via `:on-create` (non-interactive, run
at birth) and `:menu` (interactive, on-demand against the current workspace);
gptel is the first consumer. The registry today covers only the *post-anchor*
middle of the lifecycle.

This change makes git worktrees a first-class, mostly-automatic part of the
workspace: the user picks an already-cloned repo and creates a worktree off
main *inside* the workspace home, often for several repos sharing one feature
branch. Because a worktree is a *child* of the home (not the home itself), it
lands on the same post-anchor side of the timeline as gptel's session — so it
fits the existing `:menu` surface and needs only one new lifecycle surface,
`:on-purge`, for clean removal at teardown.

Reference: exploration in this change's conversation; existing patterns in
`config/gptel/sessions/workspace-integration.org` (registry consumer) and
`config/workspaces/workspaces-transient.org` (registry-driven menu).

## Goals / Non-Goals

**Goals:**
- A droppable first-party module `config/workspaces/git-worktrees.org` that
  registers a `:menu` "add worktree" command and an `:on-purge` teardown
  handler against the workspace integration registry.
- Worktree-off-main creation: pick repo → detect main → new branch (default =
  workspace name) → worktree at `<home>/<repo-basename>/`.
- Soft repo-source selection (projectile ∪ magit ∪ project.el ∪
  `read-directory-name`).
- Clean teardown on purge, with *guarded* branch deletion (merged only).
- Birth-time worktree creation by reusing the existing `C-x w` transient.
- One new generic registry surface (`:on-purge`) + purge-time dispatch in
  `workspace-purge`.

**Non-Goals:**
- Cloning repos (sources must already be cloned locally).
- Storing repo/branch in the workspace plist — all git facts are derived.
- Force-deleting unmerged branches, or any destructive branch cleanup.
- Worktree move/rename, multi-remote main detection beyond `magit-main-branch`.
- Making git mandatory: a zero-worktree workspace stays valid.

## Decisions

### D1 — Git operations go through magit (real dep), gated on its presence
**Choice:** Use magit's worktree plumbing directly — `magit-worktree-branch`
(create on new branch), `magit-worktree-delete` (remove), `magit-list-worktrees`
(enumerate), `magit-main-branch` (main detection), `magit-branch-merged-p` +
`magit-branch-delete` (guarded branch cleanup). Operations that target a
specific repo bind `default-directory` to that repo/worktree before calling.
The integration registers inside `(with-eval-after-load 'workspaces
(with-eval-after-load 'magit ...))`, so absent magit ⇒ no git integration
(not an error).
**Rationale:** magit is already a loaded core module here; reusing its tested
plumbing and error handling beats hand-rolled subprocess parsing. Gating on
magit keeps the module honestly droppable.
**Alternatives:** Shell `git worktree` via `process-file` (mirrors
`workspace-new`'s `git init`/`commit`). Rejected: more parsing glue, and we'd
re-implement main-branch and merged-branch detection magit already provides.
**Spec impact:** dependency reframed from "git only, soft magit" to "magit +
git, registered only when magit present" (already updated in the delta spec).

### D2 — Repo-source selection stays soft and list-only
**Choice:** A `jf/workspace--worktree-repo-candidates` helper merges, when each
is loaded, `projectile-known-projects`, `(magit-list-repos)`, and
`project-known-project-roots`; de-dupes; filters to dirs that are git repos
(`locate-dominating-file ... ".git"` / `magit-toplevel`); then `completing-read`
with a `read-directory-name` fallback for anything not listed.
**Rationale:** This is the user's explicit "leverage where valuable without a
hard dependency" seam. We consume only candidate *lists*, never these
packages' workflows, and the floor prompt means none of them is required.
**Alternatives:** Hard-depend on projectile. Rejected per user intent.

### D3 — One new generic registry surface: `:on-purge`
**Choice:** Extend `workspace-register-integration` to accept `:on-purge`
(handler `(payload) -> ok|skipped|(failed . reason)`), counting it toward the
"at least one surface" rule. Add `workspace--dispatch-purge-integrations`
mirroring the existing `workspace--dispatch-create-integrations` (registration
order, error-guarded, `message` on failure, never signals). `workspace-purge`
calls it *after* confirmation and *before* `delete-directory`.
**Rationale:** Teardown is a genuine, reusable lifecycle event (gptel could
later archive sessions on it). Reusing the existing dispatch/runner shape keeps
the registry uniform. The birth case needs *no* new surface (see D4), so
`:on-purge` is the only contract growth.
**Alternatives:** `advice-add` on `workspace-purge` from the git module.
Rejected: advice is opaquer than a published surface and re-introduces the
directionality coupling the registry exists to avoid.

### D4 — Birth reuses the existing transient, not a new surface
**Choice:** After `workspace-new` finishes `:on-create` dispatch, for `fresh`
and `anchored-scaffolded` contexts only, it pops `workspace-menu` (the `C-x w`
transient) so the user can invoke add-worktree (and any other `:menu` item)
repeatedly, then dismiss. The new workspace's tab is already current, so the
transient's `:menu` payload targets it. `workspace-new` names no integration —
it just raises the generic menu.
**Rationale:** The interactive add-worktree command already exists as the
`:menu` entry; "offer it at birth" = show the menu. No second interactive
surface, naturally supports adding several repos, consistent with the menu the
user already knows.
**Alternatives:** A bespoke `y-or-n-p` loop over registered `:menu` commands.
Rejected: reimplements what the transient does and diverges from the menu UX.
**Trade-off:** Adopted (`anchored-existing`) workspaces are skipped — we never
auto-open a menu against a directory the user already owned.

### D5 — Guarded branch deletion on teardown
**Choice:** In the `:on-purge` handler, after removing each worktree, offer to
delete its branch but call `magit-branch-delete` only when
`magit-branch-merged-p` reports the branch safely merged; unmerged branches are
kept and never force-deleted. A worktree magit refuses to remove (dirty tree)
is recorded in the `failed` reason; teardown continues for the rest and never
aborts the purge.
**Rationale:** Honors "clean teardown" without ever destroying possibly-
unfinished work. `magit-branch-merged-p` is the same safety check magit's own
branch UI uses.
**Alternatives:** Leave all branches (original spec) — less tidy; or force-
delete — unsafe. Guarded deletion is the middle the user chose.

### D6 — No persisted git state; everything derived
**Choice:** The workspace plist gains no `:repo`/`:branch` slots. Worktrees are
enumerated by listing child dirs of `:home` (or `magit-list-worktrees` bound
into a child), and each worktree's branch/repo is read from git on demand.
**Rationale:** Matches the codebase's treatment of `:home`/broken-state as
disk-derived; eliminates save-file drift.

### D7 — Testing approach (repo convention, no new decision needed)
**Framework:** Buttercup (`describe`/`it`/`expect`), the repo default for new
suites. **Location/naming:** `config/workspaces/test/*-spec.el`, co-located with
the existing workspace specs (`integration-registry-spec.el`,
`transient-menu-spec.el`, `gptel-integration-spec.el` are the direct models).
**Level:** behavioral — exercise real module code, mock only at the boundary to
magit and to interactive prompts, scoped with `cl-letf` (never global), per the
CLAUDE.md behavioral-test contract. **Running:**
`./bin/run-tests.sh -d config/workspaces`. **Scenario mapping:** each `####
Scenario` in the three delta specs becomes one `it`; new git specs live in
`config/workspaces/test/git-worktrees-spec.el`, the `:on-purge` registry
behavior extends `integration-registry-spec.el`, and the birth-offer/purge-
dispatch behavior extends the workspaces/transient specs. Spies stub
`magit-worktree-branch`/`-delete`, `magit-list-worktrees`, `magit-main-branch`,
`magit-branch-merged-p`/`-delete`, and the `completing-read`/`transient` calls.

## Risks / Trade-offs

- **magit API coupling** → We call several `magit-*` functions that are not a
  formal public API. *Mitigation:* confine all magit calls to the one module;
  behavioral tests spy them, so a magit signature change surfaces as a single
  module's failing specs.
- **`default-directory` correctness** → magit functions act on
  `default-directory`; a wrong binding could operate on the wrong repo.
  *Mitigation:* every magit call is wrapped in an explicit
  `(let ((default-directory ...)))`; tested via spy argument assertions.
- **Branch-merged check across diverged main** → `magit-branch-merged-p`
  semantics depend on the target ref. *Mitigation:* check against the detected
  main branch; when in doubt, keep the branch (fail safe).
- **Popping a transient at birth feels heavy if the user wanted an empty
  workspace** → *Mitigation:* the transient is dismissable with one key and the
  offer is skipped for adopted workspaces; declining leaves a valid workspace.
- **Child-dir naming collisions** (two source repos share a basename) → the
  command refuses rather than overwrite; user can rename. Documented in spec.

## Migration Plan

Additive; no data migration. Steps: (1) add `:on-purge` to the registry +
`workspace--dispatch-purge-integrations`; (2) wire dispatch into
`workspace-purge`; (3) add the birth transient pop to `workspace-new`; (4) new
`git-worktrees.org` module registering `:menu` + `:on-purge`; (5) add it to
`jf/enabled-modules` in `init.org`. Rollback = drop the module from
`jf/enabled-modules` (registry + workspaces changes are inert without a
consumer, since `:on-purge` dispatch over an empty registry is a no-op).

## Open Questions

- Should the birth transient also appear after `workspace-re-anchor` of a
  broken workspace, or only on `workspace-new`? (Leaning: only `workspace-new`,
  to match the spec's `fresh`/`anchored-scaffolded` scope.)
- Default branch name when the workspace name isn't a valid git ref (spaces,
  slashes): sanitize silently vs. prompt? (Leaning: sanitize, show the result
  as the editable default.)
