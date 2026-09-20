---
name: checkout-discovery
description: Enumerate the checkouts in view via git and classify each as canonical, workspace-owned, or other
change: workspace-portable-bookmarks
status: blocked
task_class: feature
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
relations:
  - blocked-by:repo-identity
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-discovery-spec.el` (new)

## Implementation steps

1. Add `workspace-bookmark--checkouts-of (dir)` returning every checkout of the
   repository containing DIR, by parsing `git worktree list --porcelain` with
   `default-directory` bound to DIR. Parse the porcelain form (one `worktree
   <path>` line per entry, blank-line separated) rather than the human-readable
   output, which is column-aligned and lossy. Return a list of expanded directory
   paths. Nil on any git failure.

2. Add `workspace-bookmark--canonical-checkout (dir)` returning the repository's
   main worktree. Two independent signals, and they must agree:
   - `git rev-parse --git-common-dir` → the parent of that `.git` directory;
   - the first entry of `git worktree list --porcelain`, which git documents as
     the main worktree.
   Prefer `--git-common-dir`; it is the direct question. A linked worktree's
   `.git` is a *file* (54 bytes on `~/.emacs.d`), a main worktree's `.git` is a
   *directory* — use that as the classification test in step 3.

3. Add `workspace-bookmark--classify (checkout)` returning one of the symbols
   `canonical`, `workspace-owned`, or `other`:
   - `canonical` — `(file-directory-p (expand-file-name ".git" checkout))`.
   - `workspace-owned` — the checkout's root is inside some registered
     workspace's `:home`. Walk `(workspace--registered-names)` and compare with
     a containment test built on `file-name-absolute-p` + `file-equal-p` on
     successive parents, or `file-in-directory-p`. Do NOT use plain
     `string-prefix-p`: trailing slashes and `~` forms produce false results.
   - `other` — anything else (e.g. the legacy `~/emacs-*` sibling worktrees).

4. Add `workspace-bookmark--workspace-checkout (identity workspace-name)`
   returning the checkout of IDENTITY that lives inside WORKSPACE-NAME's `:home`,
   or nil. Implementation: enumerate the immediate children of `:home` that are
   git checkouts (a child whose `.git` exists), take each child's identity via
   the `repo-identity` helpers, and return the first whose identity matches.
   This is the workhorse the resolver calls.

5. Add `workspace-bookmark--canonical-for-identity (identity)` implementing the
   safety rule: find a checkout of IDENTITY among the checkouts *in view*, ask
   git for that repository's canonical checkout, and return it **only if** it
   classifies as `canonical`. Never return a `workspace-owned` or `other`
   checkout. Return nil when no checkout of the identity is in view.

   Checkouts in view, in order: the current workspace's children, then the
   children of every other registered workspace, then the directory the running
   configuration was loaded from (`jf/emacs-dir`).

6. Write `portable-bookmarks-discovery-spec.el`:
   - **Behavioral** (real git): build a temp repo with two `git worktree add`
     children; assert `--checkouts-of` finds all three from any of them; assert
     the main worktree classifies `canonical` and the children do not; register a
     fake workspace whose `:home` contains one of the children and assert that
     child classifies `workspace-owned`; assert
     `--canonical-for-identity` returns the main worktree and never a child.
   - **Behavioral**: `--workspace-checkout` finds the checkout inside a
     registered workspace's home and returns nil for an identity absent from it.
   - **Unit** (mocked git): `--checkouts-of` returns nil when the git call fails;
     classification of a path under a workspace home whose name differs in
     trailing slash / `~` form still reports `workspace-owned`.

7. Tangle and validate.

## Design rationale

The index cannot be `identity → path`, because one identity routinely has many
checkouts: the `emacs` repository on this machine has seven live ones (`~/emacs`
main, `~/.emacs.d` locked production, three legacy `~/emacs-*` feature worktrees,
and two workspace-owned worktrees). So discovery must *classify*, not merely
list.

The safety rule is the crux of the whole change: offering a workspace-owned
worktree as the canonical checkout would open one workspace's files while the
user is in another — exactly the defect being fixed, reintroduced through the
fallback path. `other` checkouts are excluded too; they are stale branches, and
offering them would make the read-only view a lottery.

Classification via `.git` being a file versus a directory is chosen because it is
a cheap filesystem test with no subprocess, and it is precisely how git
distinguishes the two.

No scanning and no cache in this slice: discovery starts from checkouts already
in view, which git can expand into the full sibling set. Declared clone roots
(the cold path, needed to answer "no clone anywhere on this machine" precisely,
and to populate candidates when *creating* a worktree) are deferred to the
git-worktrees candidate seam.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — discovery spec passes.
- `grep -n "string-prefix-p" config/workspaces/portable-bookmarks.el` returns
  nothing for containment checks (use `file-in-directory-p`/`file-equal-p`).
- `grep -n "magit" config/workspaces/portable-bookmarks.el` returns nothing.
- Manual: in this worktree, the canonical checkout for
  `github.com/moogah/emacs` resolves to `~/emacs`, never to a directory under
  `~/emacs-workspaces/`.

## Context
- specs/portable-bookmarks/spec.md § 'Checkout classification and canonical-checkout safety'
- specs/portable-bookmarks/spec.md § 'Discovery without configuration'
- design.md § D5, § D6
