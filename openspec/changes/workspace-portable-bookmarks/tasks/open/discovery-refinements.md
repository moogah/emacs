---
name: discovery-refinements
description: Discovery fixes before the resolver consumes it — .git-named external git dir, anchored workspace homes, sibling-worktree ambiguity, directory-only symlink ownership
change: workspace-portable-bookmarks
status: done
merge_commit: 2ca7faeb
task_class: bug
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
relations:
  - blocks:resolver-outcomes
discovered_from: [wave3-review-fixes, checkout-discovery]
discovered_by: [reviewer, user]
discovered_class: invariant-gap
reconciled_into: null
---

## Problem

When a repository's real git directory is named `.git` but lives OUTSIDE its
work tree, `workspace-bookmark--canonical-checkout` returns the directory that
CONTAINS that git dir rather than the main worktree. This happens with
`git init --separate-git-dir=<elsewhere>/.git`, or with a main worktree whose
`.git` is a symlink to such a directory. `--canonical-for-identity` then offers
a directory holding only `.git`, even when the real main worktree is
`jf/emacs-dir` itself.

The cause is in `--canonical-checkout` (portable-bookmarks.el ~519-529): the
`--git-common-dir` parent, the first porcelain entry, and the `--show-toplevel`
check all agree on the wrong directory, because git run inside the parent of a
bare-looking `.git` dir treats that parent as a work tree. The logic predates
wave3-review-fixes; the reviewer of that fix re-found it while re-attacking
the safety rule.

It usually degrades to a missing file, but it is the "not the repository's
main worktree" outcome the canonical-checkout safety rule forbids, and
`resolver-outcomes` is about to consume this function.

## Scope (user decisions 2026-09-24 folded in)

This task began as the `.git`-named git dir bug (A, below, unchanged). The user's
decisions on asks 7 and 8 add three small changes to the SAME `* Checkout
discovery` section, so they ship together:

- **B. Anchored workspaces (ask 7a, agreed).** A workspace created by
  anchoring to an existing repository has that repository AS its `:home`, not
  as a child of it. `--home-checkouts` must include the home itself when the
  home is a checkout, so such a workspace can resolve bookmarks into its own
  repository. Add a behavioral spec using the real anchor constructor
  (`workspace--new-anchor-existing` or whatever data-model path it uses;
  verify in source).
- **C. Sibling worktrees of one repository in one home (ask 7b).** Stop
  silently returning the alphabetically-first. Add
  `workspace-bookmark--workspace-checkouts (identity workspace-name)` returning
  EVERY matching checkout. `--workspace-checkout` returns the match only when it
  is unique, and nil when there are none OR several. The resolver
  (`resolver-outcomes`) decides how ambiguity maps onto the outcome vocabulary;
  do NOT add an outcome here. Spec both counts. (The user's longer-term
  direction, a checkout-path hint in the stamp, is a separate backlog item. Do
  not change the stamp.)
- **D. Ignore symlinks entirely (ask 8).** Only real directories count as a
  workspace's checkouts.
  Amended 2026-09-24 by user decision: ignore symlinks entirely (supersedes directory-links-only).
  1. A child of a workspace `:home` that is a symlink (to anything) is not one
     of that workspace's checkouts. `--home-checkouts` skips it, so
     `--workspace-checkout(s)` never returns it or its target.
  2. Symlinks confer NO ownership. Remove the symlink-ownership logic that
     wave3-review-fixes added (`--home-symlinks` and the symlink branch of
     `--home-owns-p`). A checkout is `workspace-owned` only when it really lies
     inside a home (compared with `--same-file-p` on truenames, as now), or
     when it IS a home (an anchored home, part B).
  3. Accepted consequence, DOCUMENTED rather than tested away: if a user
     symlinks a main clone into a workspace home, other workspaces may be
     offered that main clone as canonical. Nothing in the workspaces tooling
     creates such links; the guidance is "don't do that". State this plainly
     in the org prose next to the classification, as a known limitation.
  4. Specs: replace the symlink-ownership specs with specs pinning the new
     behaviour (a symlinked child is not a workspace checkout and does not make
     its target `workspace-owned`). Remove the dead code paths rather than
     leaving them unreachable. Keep every non-symlink safety spec green,
     including the timestamp-race and case-variant home specs.

## Implementation steps (A: the .git-named git dir)

1. Reproduce both layouts in a fixture repo (separate git dir named `.git`,
   and a symlinked `.git`), and pin the current wrong answer in a failing spec.
2. Make `--canonical-checkout` return a directory only if git, run THERE,
   reports that directory as its top level AND that directory's git dir IS the
   repository's common dir (compare with `workspace-bookmark--same-file-p`),
   AND the directory contains something besides the git dir. Choose the
   smallest rule that rejects both layouts. Prefer nil to a guess.
3. Implement B, C and D above, each with its own specs.
4. Keep every existing discovery spec green. Show that each new spec fails
   with the fix reverted.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` all green.
- Full-suite set comparison: NO-REGRESSION.

## Context
- `.orchestrator/cycles/cycle-1789935943/reviews/wave3-review-fixes.md` (advisory 1, spec-signal 1)
- `.orchestrator/cycles/cycle-1789935943/reviews/checkout-discovery.md` (spec-signal: :home-children model)

## Observations

- **Revert proofs.** Each fix was reverted on its own. The mutation was made in the `.org`, then tangled, then `./bin/run-tests.sh -d config/workspaces` was run, then the final `.org` was restored and re-tangled. Every new behaviour spec failed under its mutant:
  - **A.** Dropping the `--holds-tracked-file-p` conjunct from `--canonical-checkout` makes 3 specs fail: the `--separate-git-dir=<dir>/.git` layout, the symlinked-`.git` layout, and the husk-with-`.DS_Store` spec. "still returns an ordinary main worktree" is a control, and it passes both with and without the fix.
  - **B.** Removing `(cons home …)` from `--home-checkouts` makes 3 specs fail: the anchored-workspace spec (which drives the real `workspace-new` with a prefix argument, i.e. `workspace--new-anchor-existing` case 1), the home-plus-child ambiguity spec, and the rewritten assertion in "a checkout whose directory name ends in a space".
  - **C.** Two mutants, each making the same 2 specs fail: the sibling spec and the home-plus-child ambiguity spec. The first mutant reverts `--workspace-checkout` to returning the first match. The second makes `--workspace-checkouts` return only the first match. "lists exactly that one checkout" pins the unique count. It passes under both mutants, because a single match is still a single match. It fails only if `--workspace-checkouts` is absent, which is the pre-fix state.
  - **D.** Two mutants:
    - Keeping symlinked children in `--home-checkouts` makes 3 specs fail: "does not list a symlinked child", the known-limitation spec, and "still owns a real clone child next to any links".
    - Restoring the wave-3 two-way symlink ownership makes 4 specs fail: the links to the clone root, to a directory inside it, to a file inside it, and to a directory above it.
- **D safety. These inputs move from `nil`/`workspace-owned` to a non-`nil` canonical offer.** Each one needs a symbolic link that the user created. `grep make-symbolic-link` / `ln -s` over the non-test `config/` and `bin/` finds nothing, so no workspace tooling creates one.
  1. A home child that links to a main clone's root (`ws/A/x -> ~/src/main`). Since wave 3 the clone was `workspace-owned`. It is now `canonical` and offered to other workspaces. This is the documented known limitation, and its spec pins it.
  2. A home child that links to a directory inside a main clone (`ws/A/foo -> ~/src/mono/packages/foo`).
  3. A home child that links to a file inside a main clone.
  4. A home child that links to a directory above a main clone (`ws/A/src -> ~/src`).
  5. Relative and chained spellings of 1–4.

  In each case, the checkout that is now offered is still the repository's main worktree. It passes all four `--canonical-checkout` checks and the `.git`-is-a-directory test. It is not inside any home (`--owning-workspace` is `nil`), and it is none of any workspace's `--home-checkouts`. The specs assert all three of these conditions through `--expect-offered-main-only`. A linked worktree is still never canonical. A real clone or worktree child of a home is still owned: "still owns a real clone child" pins that links do not remove ownership either.

  D also moves some answers toward `nil`. Symlinked children leave `--home-checkouts`, and therefore leave the in-view set. An identity reachable only through such a link is no longer found. `--workspace-checkout` never answers with a link, or with a link's target.
- **B also moves one input from `nil` to non-`nil`, and it needs no symlink.** Take a workspace anchored at a LINKED worktree of repository R, whose main is outside every home, when nothing else in view has R's identity. The anchored home is now in view, so `--canonical-for-identity` returns R's main. That is the intended canonical: it is exactly what a worktree child of any home already yields. A home is never offered itself, because `--path-within-p home home` is true and the home therefore classifies `workspace-owned`. The anchored-workspace spec asserts that `nil` answer.
- **A: two departures from the prescribed rule. Both are recorded as a Discovery.**
  - The "candidate's git dir IS the common dir" check was omitted. It cannot reject anything, because the candidate is by construction the parent of a directory named `.git` that IS the common dir. Measured on git 2.49: in the husk, `rev-parse --absolute-git-dir` equals the common dir in both layouts.
  - The prescribed "contains something besides the git dir" test was strengthened to "at least one path `git ls-files -z` lists exists under it". A husk that Finder or a sync client has visited holds a `.DS_Store` and would pass the weaker test. A spec covers that case.
  - Costs of the strengthened test:
    - One extra subprocess, paid only after checks 1–3 pass.
    - A main worktree that tracks no files, or whose tracked files have all been deleted, gets no canonical checkout. Such a repository cannot have stamped bookmarks.
    - A husk that holds a stale copy of the tree still passes. No git-derived signal can tell it apart from the real work tree.
- **A: the real main worktree W in those two layouts is not recovered. The answer is `nil`**, even when W is `jf/emacs-dir`. The reviewer's second suggestion was: when the in-view checkout's own `--absolute-git-dir` equals the common dir, answer its `--show-toplevel`. That would recover W in the symlinked-`.git` layout, where W's `.git` resolves to a directory and so classifies `canonical`. It would not help the `--separate-git-dir` layout, where W's `.git` is a gitfile and W classifies `other`. It was left out because the task asked for the smallest rejecting rule. It is a candidate follow-up if the user wants that layout to resolve.
- **An existing spec assertion changed on purpose.** "A checkout whose directory name ends in a space" asserted `--workspace-checkout home-repo` = `nil`. That encoded "the home is never a checkout", which part B reverses. It now asserts that the home's identity lists exactly the home. The F2 regression it guards (a trimmed `proj ` answering with the enclosing home's identity) is still pinned by its `--identity proj` and `--workspace-checkout <proj identity>` assertions. Under the F2 bug the new assertion would also fail, because PROJ would list too.
- **Wave-3 symlink specs replaced.**
  - "owns the clone a home child links to", "owns every checkout under a directory a home child links to" and "owns the clone when a home child links to something inside it" pinned behaviour that the user decision removes. Each is replaced by its inverse.
  - The "dangling link or link elsewhere" spec is kept unchanged.
  - The timestamp-race specs, the case-flip home spec, the path-spelling specs and the GIT_* environment spec are untouched and green.
- **B and C together.** A workspace anchored at repository X that also holds a worktree of X in its home now gets `nil` from `--workspace-checkout` for X: the home and the child are both checkouts of X. That is deliberate ambiguity, and `--workspace-checkouts` exposes both. resolver-outcomes decides the outcome. No outcome was added here.
- **Cost (adjacent to checkout-discovery review finding 4).**
  - `--workspace-checkout` no longer stops at the first match, because uniqueness needs every child's identity.
  - Every scaffolded home is a git repository (`git init`), so it is now enumerated too. It usually has no remote, which means its identity costs 4 git calls in each `--canonical-for-identity` walk.
  - Both costs are linear. The per-common-dir grouping follow-up suggested in that review would absorb them.
- **The parallel consumer (worktree-candidates-git-first) sees a changed in-view set.** The signatures of `--checkouts-in-view` and `--classify` are unchanged. The set now includes each home that is itself a checkout, and excludes symlinked children. Homes always classify `workspace-owned`, so a canonical-only filter never offers one.
- The B spec stubs `tab-bar-new-tab`, `tab-bar-rename-tab` and `workspace--dispatch-create-integrations`, and binds `workspace-home-builder` to `nil`. Registration itself goes through the real `workspace--new-anchor-existing` → `workspace--make` path, against the spec file's private registry.

## Discoveries

- discovery_id: disc-discovery-refinements-1
  class: invariant-gap
  description: |
    The canonical member needed a fourth check. When the real git dir is
    named `.git` but sits outside its work tree
    (`--separate-git-dir=<P>/.git`, or a `.git` symlink), git's common-dir
    parent, its first porcelain entry, `--show-toplevel` run there and
    `--absolute-git-dir` run there all name the husk P. Only the disk
    distinguishes it: none of the paths in git's index exist under P.
    `--canonical-checkout` now also requires that at least one path
    `git ls-files -z` lists exists under the candidate
    (`workspace-bookmark--holds-tracked-file-p`).
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Reconcile the canonical member to record FOUR checks: common-dir
    parent named .git; first porcelain entry, non-bare, same file;
    `--show-toplevel` in the candidate names it; a tracked path exists
    under it. Resolve the SPECULATED note: the signals disagree for bare,
    D/.git-bare, --separate-git-dir (non-.git name) and submodules; they
    agree and are all wrong for core.worktree (caught by check 3) and for
    the two `.git`-named layouts (caught by check 4).

- discovery_id: disc-discovery-refinements-2
  class: deviation
  description: |
    Two parts of task step 2 were not implemented as written. The check
    "the candidate's git dir IS the common dir" was omitted, because it
    is true by construction and rejects nothing (measured, git 2.49).
    The check "contains something besides the git dir" was strengthened
    to "a tracked path exists", because a `.DS_Store` defeats the
    weaker check.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    No register change beyond disc-1. Note in the entry that a husk
    holding a stale copy of the tree still passes, and that no
    git-derived signal can tell it from the real work tree.

- discovery_id: disc-discovery-refinements-3
  class: invariant-gap
  description: |
    The workspace-owned member's text is stale. Containment walks the
    truenamed checkout's ancestors and compares each to the home with
    `--same-file-p` (device and inode). It does not use
    `file-in-directory-p` or `file-equal-p`. The home itself counts, which
    is the anchored case. By user decision on 2026-09-24, symbolic links
    confer NO ownership. The wave-3 `--home-symlinks` and `--home-owns-p`
    are removed. The accepted, documented limitation is that a main clone
    symlinked into a home is `canonical`, and may be offered to other
    workspaces.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Reconcile workspace-owned to say: "truename of the checkout is, or
    lies inside, a registered :home, by --same-file-p on each ancestor;
    symbolic links are ignored in both directions". Add a
    known_limitations field recording the symlinked-main-clone case and
    the user's guidance (add a worktree, do not link a clone in). Update
    validator 2 to grep for `file-equal-p` as well as `string-prefix-p`.

- discovery_id: disc-discovery-refinements-4
  class: interface-drift
  description: |
    CHECKOUTS IN VIEW changed meaning, though no signature changed. A
    workspace's checkouts are now its :home when the home is a checkout
    (anchored workspaces), followed by its immediate children that are
    real directories. Symlinked children are excluded. `--home-checkouts`,
    `--checkouts-in-view` and every consumer see the new set, including
    the parallel git-worktrees candidate source.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Amend the CHECKOUTS IN VIEW paragraph: "the current workspace's
    checkouts (its :home if that is a checkout, then its real-directory
    immediate children; symlinks skipped), then every other registered
    workspace's, then jf/emacs-dir".

- discovery_id: disc-discovery-refinements-5
  class: spec-signal
  description: |
    `--workspace-checkout` now returns nil for two cases: no checkout of
    the identity, and SEVERAL (sibling worktrees, or an anchored home that
    also holds a worktree of itself). The new
    `workspace-bookmark--workspace-checkouts (identity workspace-name)`
    returns every match, so a consumer can tell the two cases apart. No
    resolution outcome was added.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    resolver-outcomes must decide how "several checkouts in the current
    workspace" maps onto the outcome vocabulary, by calling
    `--workspace-checkouts` when `--workspace-checkout` is nil. Record
    `--workspace-checkouts` in the discovery boundary entry's function
    list.
