---
name: ownership-and-picker-polish
description: Small follow-ups from the wave-4 reviews — remote magit entries never touch the network, and prose drift
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: false
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/workspace-git-worktrees-consumer
relations:
discovered_from: [worktree-candidates-git-first, canonical-predicate-consolidation]
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
---

## Decided (implement)

1. **Remote magit entries must never touch the network** (worktree-candidates review,
   advisory). The git-repo filter probes a TRAMP entry that magit returns BEFORE
   classification. A failed connection raises `file-error` out of
   `jf/workspace--worktree-repo-candidates`, so the "Source git repo:" prompt and
   its `read-directory-name` floor never appear. Drop remote entries in
   `jf/workspace--worktree-dedupe-dirs` (they could never classify as canonical,
   because discovery is local-only). Correct the prose "building the list touches
   no network" (git-worktrees.org:112-118). Spec: a stubbed `magit-list-repos`
   returning `/ssh:nohost.invalid:/a/` still reaches the prompt.
2. **Prose drift** (consolidation review): portable-bookmarks.org ~1822 still says
   classification "runs no subprocess". Correct it.

## Deferred by the user (NOT in scope)

Ownership by tracked content (ask 12) and anchored repos in the picker (ask 14)
were deferred on 2026-09-26 until the user has used the feature. See
`.tasks/portable-bookmarks-ownership-refinements-after-use.md`. Do not implement
them here.

## Verification
- Tangle both .org files; no .el drift.
- `./bin/run-tests.sh -d config/workspaces` all green; full-suite NO-REGRESSION.

## Observations

- **Reproduced first.**  A batch probe (portable-bookmarks NOT loaded, so
  magit's entries pass through classification unchanged) with
  `magit-list-repos` stubbed to `("/ssh:nohost.invalid:/a/")` and
  `file-name-handler-alist` bound to a single fake "/ssh:" handler (answers
  `file-remote-p` from the name, signals `file-error` for anything else)
  raised `(file-error "Simulated network access" "expand-file-name")` out of
  `jf/workspace--worktree-read-repo`; neither prompt was reached.  After the
  fix the same probe reaches the prompt (with no candidates) and the
  `read-directory-name` floor, and the handler is asked only `file-remote-p`.
  Real TRAMP was never invoked (no DNS lookup, no ssh).
- **Where the probe really happens.**  With portable-bookmarks loaded, the
  real classifier already drops a remote entry after one `file-remote-p`
  (`workspace-bookmark--git` refuses remote names), so the network exposure
  was only on the pass-through path (classifier unbound, or a classifier that
  returns a remote name).  The first probe on that path was
  `expand-file-name` in `--dedupe-dirs` (harmless under real TRAMP for a
  non-`~` localname), then `magit-toplevel` / `locate-dominating-file` in the
  git-repo filter, which connects.  The fix drops remote entries in
  `--dedupe-dirs` on the RAW name, before `expand-file-name`, and once more
  after expansion (a relative entry under a remote `default-directory`).
  The specs hold the stricter invariant: `file-remote-p` is the ONLY
  operation a remote entry is asked.
- **Revert proofs.**  With `config/workspaces/git-worktrees.el` restored to
  base (`git show HEAD:…`) and the new specs kept, all three new specs in
  `git-worktrees-spec.el` § "remote (TRAMP) magit entries" FAIL (the
  `file-error` from `expand-file-name` escapes / is raised); restoring the
  fixed `.el` makes them pass.  A fourth spec (the same prompt/floor check
  with the REAL classifier) was drafted and dropped: it passes on base
  because the classifier already filters remote names, so it could not be
  shown to fail.
- **Prose correction beyond the cited line (same file, owned).**  The
  "Repo-source seam" overview in `git-worktrees.org` still said
  `magit-repository-directories` "is unset in this config, so today the
  candidate list is git-derived only".  Since b32523b8 it is set to
  `(("~/src" . 1))`; corrected to say so.  Prose only.
- **portable-bookmarks.org prose.**  "The classification is asked first
  because it runs no subprocess" was false: `--classify` →
  `--owning-workspace` runs `--same-repository-worktree-p` (one or two git
  subprocesses) for each registered home that lies INSIDE the candidate.
  Rewritten to say it is the cheaper half and when it does run git.  Only
  that paragraph was touched; `portable-bookmarks.el` does not change.
- The spec's fake handler replaces `file-name-handler-alist` wholesale for
  the body; that is deliberate (TRAMP can never be reached) but means those
  specs would not notice a handler-dependent local operation.  None exists
  on these paths.

## Discoveries

- discovery_id: disc-ownership-and-picker-polish-1
  class: invariant-gap
  description: |
    Pin 3 of the confirmed boundary entry ("soft repo-source selection ...
    with a read-directory-name floor so none is required") did not hold for
    a remote (TRAMP) entry in magit's list when classification passed it
    through: the git-repo filter probed it, an unreachable host raised
    file-error, and the floor was unreachable.  Remote entries are now
    dropped in jf/workspace--worktree-dedupe-dirs after only a
    file-remote-p question.  No interface changed (same functions, same
    signatures, same result shapes); only the de-dupe's contract narrowed
    from "remote entries keyed by expanded name" to "remote entries dropped".
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Add to pin 3: "remote (TRAMP) entries are dropped unprobed (only
    file-remote-p is asked), so building the candidate list never touches
    the network and the floor is always reachable."
- discovery_id: disc-ownership-and-picker-polish-2
  class: spec-signal
  description: |
    The classification half of workspace-bookmark--offerable-canonical-p is
    not subprocess-free: --owning-workspace calls
    --same-repository-worktree-p (1-2 git subprocesses) for every registered
    home contained in the candidate.  The entry's "workspace-owned" member
    describes containment as pure file-identity tests, which is no longer the
    whole story since canonical-predicate-consolidation's fix commit.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    When reconciling, note that workspace-owned for a CONTAINED home (home
    inside the checkout) additionally requires that the home is not a
    worktree of the checkout's own repository, decided by git.
