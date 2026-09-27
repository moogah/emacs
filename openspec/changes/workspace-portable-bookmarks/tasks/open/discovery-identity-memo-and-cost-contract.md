---
name: discovery-identity-memo-and-cost-contract
description: Route discovery through the identity memo, memoise the canonical test per common dir within a call, and pin a spawn budget for the resolve and picker paths
change: workspace-portable-bookmarks
status: needs_review
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
merge_commit: 6736b02a
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
relations:
  - discovered-from:arch-cycle-1789935943-eoc-06
  - relates-to:jump-seam
discovered_from: arch-cycle-1789935943-eoc-06
discovered_by: architect
discovered_class: invariant-gap
created_at: 2026-09-26T14:00:00-04:00
---

## Summary

The stamp path is memoised (`--memoised-identity`, `.el:239-251`) but the discovery path is
not: `--workspace-checkouts` (`.el:711-714`) and `--canonical-checkouts-for-identity`
(`.el:796-797`) run `--repo-root` + identity per candidate. Measured on the audit fixture:
`missing-repo` = 10 spawns / ~160 ms, the picker = 21 spawns / ~600 ms with three magit
entries, about four spawns per entry. DD-15 (`magit-repository-directories` = `~/src`, ~51
repos) made the picker cost live: ~200 spawns before the "Source git repo:" prompt.

## Cited register entries

- `register/boundary/git-derived-checkout-discovery` (reconciled this cycle; the audit drafted
  a `cost_contract`): the memo is validated by the git config mtime, so the resolver inherits
  the stamp path's invalidation rule. Pressure-test that rule under a worktree whose config
  is the common dir's.

## Implementation steps

1. `--checkout-position` already returns ROOT and GIT-COMMON-DIR from one `rev-parse`; have
   both plural discovery functions take identity through `--memoised-identity` so a warm memo
   costs one spawn per candidate.
2. In `--canonical-checkouts-among`, memoise `--canonical-checkout` per common dir within one
   call, so N linked worktrees of one repository pay once.
3. Specs: extend the spawn-budget pattern of `stamp-spec.el:539-687` to `--resolve` (warm
   memo: `resolved` ≤ 2 spawns, `missing-repo` ≤ 1 + in-view count) and to
   `jf/workspace--worktree-repo-candidates` (≤ 2 spawns per magit entry on a warm memo).
   Each must fail with the memo bypassed.
4. Tangle and validate.

## Verification
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set.

## Out-of-scope
Async or cached discovery across calls beyond the existing memo. Changing outcomes.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/end-of-cycle-1789935943.md` § Finding 6
- DD-7 (memoise repo-root → identity), DD-15

## Observations

- **Measured spawn counts, before (e6974126) and after, same fixture** (`workspace-bookmark--git`
  call-through count; fixture: canonical clone A in view as `jf/emacs-dir`; workspace a's home holds
  a linked worktree of A and one of OTHER; workspace b's home holds one of OTHER; picker list =
  five magit entries [W1 A W2 B C], W1/W2 linked worktrees of A, three distinct repositories):

  | path                                   | before | after, warm memo | after, identity memo bypassed | after, per-call memo bypassed |
  |----------------------------------------|-------:|-----------------:|------------------------------:|------------------------------:|
  | `resolved` in a (2 checkouts in home)  |   4    |  **2** (~32 ms)  |               4               |               2               |
  | `missing-repo` in b (4 in view)        |  16    |  **7** (~120 ms) |              12               |              12               |
  | picker, 5 magit entries / 3 repos      |  17    | **11** (~220 ms) |              11               |              19               |
  | canonical test of one main clone       |   5    |  **3**           |               3               |               5               |

  Before, warm and cold were identical (16/16, 17/17): discovery did not touch the memo at all.
  Outcomes are unchanged in every cell (`resolved`/`missing-repo` with the same `:path` and
  `:checkout`; the picker returns the same three clones). Existing 655 specs pass unchanged
  against the new module before any new spec was added.

- **The task's numeric targets are not reachable without an outcome change, and step 2 as
  written is a no-op.** `--canonical-checkouts-among` already de-duplicated candidates by file
  identity BEFORE the predicate, so `--canonical-checkout` was already called once per distinct
  main worktree; memoising it "per common dir within one call" saves nothing. The redundancy was
  elsewhere: `--checkouts-of` (one `worktree list` per checkout) plus `--canonical-checkout`'s own
  `rev-parse --git-common-dir` and second `worktree list`, plus a `--show-toplevel` in the
  candidate that the position already answers. Sharing those within one call gives the floor:
  a canonical main clone costs THREE subprocesses (position, worktree listing, tracked-file
  check — three different git commands, none droppable without removing one of the four signals
  the "signals disagree" / husk specs pin). Hence the picker is 3 per canonical entry (+1 per
  further checkout of a seen repository), not "<= 2", and `missing-repo` is (workspace's own
  checkouts) + (checkouts in view) + 2, not "<= 1 + in-view count". The specs pin the derived
  numbers exactly; the register push-back is in `## Discoveries`.

- **How each budget fails with the memo bypassed** (all as companion specs, so the evidence is
  executable): identity memo bypassed = `cl-letf` `--memoised-identity` to call
  `--root-identity` every time → `resolved` 2→4, `missing-repo` 7→12; per-call memo bypassed =
  `cl-letf` `--memoised-in-call` to apply without memoising → `missing-repo` 7→12, picker 11→19.
  The picker does NOT depend on the identity memo: it computes no identity at all (pinned:
  `--memoised-identity` and `--remote-url` are never called on the picker path). The task's
  premise that the picker's cost was identity-driven was wrong; it is the canonical test.

- **Speculation settled empirically (git 2.49), then by spec.** `git remote get-url` DOES read
  the worktree's `config.worktree` under `extensions.worktreeConfig`: a `url.*.insteadOf` set
  with `git config --worktree` rewrites the URL in that worktree only; a remote defined only
  there is that worktree's remote; a `remote.<name>.url` set there is APPENDED to the shared one
  (multi-valued key), so `get-url` still reports the shared URL first — but only while the shared
  one exists. None of these writes touch the shared `config`'s mtime. With the old stamp rule
  and discovery sharing the memo, `--workspace-checkouts` returned the stale identity's checkout
  (evidence run: the two "changes the identity ... without the shared config changing" specs fail
  with `--config-stamp` reverted to the mtime-only rule; the pre-task module passes them only
  because pre-task discovery never used the memo). Fixed: the stamp is now the PAIR (shared
  `config` mtime, checkout's `config.worktree` mtime-or-nil); the checkout's git dir comes from
  `<root>/.git` (directory, or `gitdir:` file) with no subprocess. Residual blind spot, unchanged
  and documented: global/XDG/system gitconfig and `include`/`includeIf` targets. A `core.worktree`
  layout (root has no `.git`) has no readable git dir and so no `config.worktree` in its stamp;
  such layouts are already nil-canonical everywhere.

- **Memoised nil + `git remote add`**: confirmed and pinned — `remote add` writes the shared
  config; the next discovery lookup recomputes and finds the checkout (spec pins mtime with
  `set-file-times` +10 s, like the stamp-spec, so it does not depend on timestamp granularity).

- **Scope of edits vs. the brief's "ONLY memo / position / discovery-plural sections".** The
  position section is untouched. Beyond the memo and plural sections I edited § "Every checkout
  of a repository" (extracted `--usable-checkout` from `--checkouts-of`'s lambda, same semantics)
  and § "The canonical checkout" (`--canonical-checkout` takes its common dir from the shared
  position when the directory is in a work tree, falls back to the previous `--git-common-dir`
  spawn otherwise, and skips the `--show-toplevel` spawn when DIR's own top level IS the
  candidate). Both are required for any saving beyond identity, and neither changes an answer
  (existing canonical/husk/signals-disagree specs unchanged and green). A new § "One discovery
  call asks git each question once" holds the per-call memo plumbing. Seam 2 and the two
  singulars were not touched.

- **Git-version floor moves for the resolver.** `--workspace-checkouts` and
  `--canonical-checkouts-for-identity` now position candidates with `--path-format=absolute`
  (git >= 2.31), as the stamp path and `--canonical-checkout` already did; on an older git the
  resolver now degrades to `no-clone` where it previously could still `resolve`. Stamps could
  not have been produced on such a git anyway; noting it because the register's "older git ->
  nil -> no-clone" clause now covers the resolver too.

- **Adjacent, out of scope:** `--same-repository-worktree-p` (classification) runs its own
  `rev-parse --show-toplevel --git-common-dir` in a contained home and, when the checkout's
  `.git` is a file, a `--git-common-dir` in the checkout; both could come from
  `--shared-position` within a discovery call (saves 1-2 per anchored home inside a candidate).
  `jf/workspace--worktree-git-repo-p` re-verifies every picker candidate through
  `magit-toplevel` (magit's own git spawn, uncounted here) although git-derived candidates are
  already main worktrees. Neither touched.

- **Cross-call caching was not added** (task's out-of-scope). The per-call memo lives only inside
  one plural call and is dropped on return; the two `--canonical-checkouts-in-view` calls the
  picker makes (git-derived, then magit) do not share, and `--resolve-coordinates`' two plural
  calls re-position the current workspace's checkouts (the 1 in "1 + 4 + 2"). Binding the memo
  once in `--resolve` would save that 1; left alone because the Resolution section is outside
  this task.

## Discoveries

- discovery_id: disc-discovery-identity-memo-and-cost-contract-1
  class: invariant-gap
  description: |
    The identity memo's invalidation rule (shared config mtime, keyed per root) was
    INCOMPLETE once discovery shares the memo: `git remote get-url` honours the checkout's own
    `config.worktree` (extensions.worktreeConfig) — an insteadOf rule or a worktree-only remote
    there changes the identity of that worktree, main or linked, with no write to the shared
    config. Measured on git 2.49; pinned by portable-bookmarks-cost-spec.el § "Identity memo
    invalidation seen through discovery" (fails with the old rule). Fix: the stamp is the pair
    of mtimes (shared `config`, `<git-dir>/config.worktree` or nil); the git dir is read from
    `<root>/.git` without a subprocess (`--checkout-git-dir`). The "memoised nil is invalidated
    by `remote add`" half of the speculation is CONFIRMED and pinned.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Resolve the SPECULATED paragraph: get-url does NOT ignore config.worktree; the stamp now
    folds the worktree git dir's config.worktree mtime in. Update the purpose/functions text
    that describes the memo as "keyed on the shared config's mtime" to "keyed per root, stamped
    with the shared config's and the checkout's config.worktree mtimes". The remaining accepted
    blind spot is global/XDG/system config and include targets (clear-identity-cache covers it).

- discovery_id: disc-discovery-identity-memo-and-cost-contract-2
  class: spec-signal
  description: |
    The cost_contract's target line ("<= 2 per candidate on a warm memo") and the task's
    budgets ("missing-repo <= 1 + in-view count", "picker <= 2 per magit entry") are not
    reachable without an outcome change. The canonical test needs three different git commands
    (position, worktree listing, tracked-file check); 3 per canonical main clone is the floor.
    Measured on one fixture: resolved 4 -> 2, missing-repo 16 -> 7 (1 own checkout + 4 in view
    + 2), picker 17 -> 11 for five magit entries of three repositories (3 per repository + 1 per
    further checkout). The picker's cost is not identity-driven at all — it never computes an
    identity — so the identity memo cannot help it; the per-call sharing is what does.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Rewrite cost_contract with the measured numbers: --workspace-checkouts 1 per checkout of
    the home (identity from the memo); --canonical-checkouts-for-identity 1 per checkout in
    view + per matching repository 1 worktree listing + 1 ls-files; --canonical-checkout 3 on a
    main clone / 4 on a linked worktree, shared within one --canonical-checkouts-among call;
    picker 3 per canonical magit entry, +1 per further checkout of a seen repository. Mark
    validator 6 satisfied by git-worktrees-spec.el § "repo picker subprocess budget" and
    portable-bookmarks-cost-spec.el § "Discovery subprocess budget on a warm identity memo".

- discovery_id: disc-discovery-identity-memo-and-cost-contract-3
  class: interface-drift
  description: |
    New private helpers in the discovery boundary: --checkout-git-dir (memo section),
    --usable-checkout, --with-discovery-memo (macro), --memoised-in-call,
    --discovery-memo-key, --shared-position, --shared-worktree-entries, --main-worktree-of;
    plus the variable --discovery-memo (nil outside a discovery call). --canonical-checkout's
    signals are unchanged but its first spawn is the position (with a --git-common-dir fallback
    when DIR is in no work tree). --config-stamp gained an optional ROOT argument.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Add the helpers to the `functions` list (the cleanup task owns that list's removal of the
    singulars; this is the addition side). Note the per-call memo explicitly as NOT a cache
    across calls, so the "no cache" architecture statement stays true as written.
