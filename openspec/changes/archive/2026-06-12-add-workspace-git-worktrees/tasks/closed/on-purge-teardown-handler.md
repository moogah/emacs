---
name: on-purge-teardown-handler
description: :on-purge handler — remove worktrees with guarded (merged-only) branch deletion
change: add-workspace-git-worktrees
status: blocked
relations:
  - blocked-by:git-worktrees-module-scaffold
  - blocked-by:registry-on-purge-surface
---

## Files to modify
- config/workspaces/git-worktrees.org (modify) — add the handler, and tangled .el
- config/workspaces/test/git-worktrees-spec.el (modify)

## Implementation steps
1. Implement `jf/workspace--worktree-on-purge` taking the anchor PAYLOAD. It
   runs before `workspace-purge` deletes `:home`, so the worktrees still exist.
   Steps:
   a. Enumerate the git worktrees living under `(plist-get payload :home)`.
      Identify child dirs of `:home` that are git worktrees (each has a `.git`
      FILE — not dir — pointing into a source repo's gitdir; `magit-toplevel`
      from inside the child resolves it). Build the list of (worktree-dir,
      its branch, its source-repo main worktree).
   b. For each worktree, remove it via magit: bind `default-directory` into the
      worktree's source repo and call `(magit-worktree-delete dir)`. Wrap in
      `condition-case`; if magit refuses (dirty tree), accumulate the path into
      a failure reason and CONTINUE with the rest — never abort.
   c. After a successful removal, offer guarded branch deletion of that
      worktree's branch: only when `(magit-branch-merged-p branch <main>)` is
      non-nil, call `(magit-branch-delete (list branch))` WITHOUT force.
      Unmerged branches are kept and never force-deleted.
   d. Return `'ok` when everything removed cleanly; `'skipped` when there were
      no worktrees under `:home`; `(cons 'failed REASON)` naming any
      worktree(s) that could not be removed.
2. Confirm the scaffold task's registration references this symbol
   (`:on-purge #'jf/workspace--worktree-on-purge`).
3. Tangle + validate: `./bin/tangle-org.sh config/workspaces/git-worktrees.org`.
4. Extend `git-worktrees-spec.el`: spy `magit-list-worktrees`/enumeration,
   `magit-worktree-delete`, `magit-branch-merged-p`, `magit-branch-delete`.
   Assert: each worktree under `:home` is removed; a merged branch is deleted
   when offer accepted; an unmerged branch is kept and `magit-branch-delete` is
   never called with force; a dirty worktree (delete refuses) yields `failed`
   naming it while the others are still removed; no worktrees ⇒ `skipped`.

## Design rationale
Purging the container with a raw delete would leave each SOURCE repo holding a
stale/prunable worktree entry pointing at a deleted directory — the worktree
registry lives in the source repo, outside `:home`. So teardown must remove
each worktree from its source repo while the dir still exists. Branch deletion
is guarded by `magit-branch-merged-p` (the same safety check magit's own branch
UI uses) so possibly-unfinished work is never destroyed; a dirty worktree is
reported, not forced, and never aborts the purge (additive-failure contract).

## Design pattern
Return protocol (`ok`/`skipped`/`(failed . reason)`) matches the integration
result protocol in specs/workspace-integrations. Per-call `default-directory`
binding matches the add-worktree command. The "continue past one failure,
surface it, never abort" shape mirrors `workspace--run-one-integration`.

## Verification
- `./bin/tangle-org.sh config/workspaces/git-worktrees.org` validates
- `./bin/run-tests.sh -d config/workspaces` — teardown specs pass
- Acceptance (spec scenarios): purge removes worktrees from their source repos;
  merged branch cleaned, unmerged kept; a dirty worktree does not abort purge.

## Context
design.md § Decisions 'D3', 'D5 — Guarded branch deletion on teardown'
specs/workspace-git-worktrees/spec.md (Requirement: Worktree teardown on purge)
