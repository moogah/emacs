---
name: discovery-memo-dead-helpers-and-picker-scope
description: Remove or re-home the three discovery helpers left without a production caller by the memo work, and let the worktree picker build its candidates in one discovery-memo scope
change: workspace-portable-bookmarks
status: done
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
merge_commit: 4dec801c
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
  - register/boundary/workspace-git-worktrees-consumer
relations:
  - discovered-from:arch-on-touch-1790453714-23
  - discovered-from:arch-on-touch-1790453714-25
  - discovered-from:arch-on-touch-1790453714-26
  - discovered-from:disc-missing-repo-choice-2
  - blocked-by:discovery-identity-memo-and-cost-contract
discovered_from: [arch-on-touch-1790453714-23, arch-on-touch-1790453714-25, arch-on-touch-1790453714-26]
discovered_by: architect
discovered_class: dead-branch
created_at: 2026-09-27T11:30:00+00:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute from the on-touch audit of
> `discovery-identity-memo-and-cost-contract` (merge 6736b02a). Wave 4, after that task's fix pass lands.

## Summary

The per-call discovery memo left `workspace-bookmark--checkouts-of`, `--identity` and `--repo-root` with no production caller
(second instance of the class end-of-cycle finding 2 named last cycle: helpers kept alive by tests only). Separately, the
git-worktrees repo picker builds its candidate list in TWO memo scopes because the stable public entry
`workspace-bookmark-canonical-checkouts-in-view` is called twice (once for the view, once for magit's list), so a repository
present in both pays its three git calls twice. This task closes both without changing any outcome.

## Cited register entries

- `register/boundary/git-derived-checkout-discovery` (reconciled, load-bearing): its `functions` list must end up naming only
  live functions; its cost_contract (re-stated at integrate to the measured floor) should hold with the picker in one scope.
  Pressure-test: is `--checkouts-of` worth keeping as the memo-aware home for `--main-worktree-of`, or is deleting it and
  inlining cleaner? Decide from the call graph, report in `## Discoveries`.
- `register/boundary/workspace-git-worktrees-consumer` (confirmed, load-bearing): pins 2/3 (sources git-derived + magit; one
  admission predicate; DD-10 soft `fboundp` call). A one-scope public entry is a change to the stable surface both modules
  share — keep the old entry point working (or alias it) and add the scope-wrapping form; say which in `## Discoveries`.

## Implementation steps

1. Route `--main-worktree-of` through a memo-aware `--checkouts-of`, or delete `--checkouts-of`; delete `--identity` and
   `--repo-root` if nothing but tests calls them (move any still-valuable spec onto the live path). `git grep` in `config/`
   excluding tests must show each surviving helper with a production caller.
2. Give the picker one discovery-memo scope: either a public `workspace-bookmark-with-discovery-memo` macro/function that
   git-worktrees.el wraps around its whole candidate build (guarded by `fboundp` per DD-10), or a single public entry taking
   both candidate sources. Re-pin the picker budget (currently 3 per repo + 1 per extra checkout, with a repo in both sources
   paying twice) so a repo present in both view and magit list pays once.
3. In the picker budget spec header, name the `magit-toplevel` post-filter spawn as knowingly uncounted (spy is at
   `workspace-bookmark--git`).
4. (added 2026-09-27 from missing-repo-choice discovery disc-missing-repo-choice-2, class interface-drift) `jf/workspace--add-worktree`
   ignores the anchor payload's `:source-repo` and re-prompts through `--worktree-read-repo` (a discovery re-run with the free-form
   floor). Make it honour `:source-repo` when present (skip the prompt; ~3 lines in `git-worktrees.org`), keep the prompt when absent,
   and add a spec. This is what lets the bookmark chooser's "add worktree here" target the canonical checkout it was handed.
   Reconcile `register/boundary/workspace-git-worktrees-consumer` / `register/shape/workspace-integration-anchor-payload` at integrate.
5. Tangle `portable-bookmarks.org` and `git-worktrees.org`; validate.

## Verification
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set (compare-baseline.py NO-REGRESSION).
- `git grep -n 'workspace-bookmark--checkouts-of\|workspace-bookmark--identity\b\|workspace-bookmark--repo-root' -- 'config/**/*.el' ':!config/**/test/**'`
  shows only definitions WITH a production caller (or nothing for deleted ones).
- Picker budget spec passes with the re-pinned number and its bypass companion pins the higher one.

## Out-of-scope
Any change to classification or resolution outcomes. Replacing the worktree-list signal (the design change that would reach a
2-spawn floor) — that is a user decision at integrate.

## Context
- `.orchestrator/cycles/cycle-1790453714/findings/on-touch-identity-memo.md` findings 23, 25, 26
- DD-10 (soft fboundp call), DD-12/13 (picker sources)

## Observations

- **Built on 30a8bba4.** Mid-task the integration branch moved (off-workspace-restore fix pass). The tree was reset to
  the base, fast-forwarded to 30a8bba4, the `.org` + spec edits re-applied (`git apply -3`, clean), and both `.org`
  files re-tangled; a second tangle is byte-identical. No `.el` was merged by hand. `(require 'view)` was still
  mid-file after that merge, as the coordinator said, and is moved here.
- **Step 1, `--checkouts-of` KEPT and routed, not deleted.** It is now the memo-aware function
  (`--shared-worktree-entries` keyed by the `--shared-position`'s common dir) and `--main-worktree-of` is
  `(car (--checkouts-of checkout))`. Answer identical: `(car (delq nil (mapcar usable entries)))` equals
  `(seq-some usable entries)`; the extra `--usable-checkout` calls on later entries are stats, no spawn. Cost inside a
  discovery call is unchanged (every budget spec keeps its number). Outside one it rises from 1 to 2 subprocesses
  (position + listing); only specs call it there. Reason to keep it: the full-list porcelain specs
  (discovery-spec :94-154, :289, :319, :861, :941) and the register's "`--checkouts-of` is NOT reflexive" clause stay
  live on the production path without edits, and the org's porcelain table documents the function that uses it.
- **`--identity` and `--repo-root` deleted.** Their spec sites (identity-spec 26, discovery-spec 6) moved onto two
  fixture helpers, `workspace-bookmark-fixtures-root-of` / `-identity-of`, which compose exactly what the live paths
  compose: `--checkout-position`, then `--root-identity` on its root. The identity memo is bypassed on purpose, so
  specs stay independent. The mocked "composed question" describe became a `--root-identity` unit describe (2 specs).
  Its third spec ("no repository -> no remote asked") was dropped: on the live path that guard is each caller's
  `(and position ...)`, and the behavioural "gives a directory outside any repository no identity" still covers it.
  The org § "The composed question" became § "The identity of a checkout root" and says why no root finder exists.
- **A spec was passing for the wrong reason, and now isn't.** discovery-spec "`--canonical-checkouts-for-identity`
  never lists a checkout that is not canonical" stubbed `--identity` and `--checkouts-of`. Since the memo task,
  neither is on the live path. The real `--shared-position` of the fixture's empty `.git` failed, so nothing reached
  classification and the spec was green whatever the classification said. It now stubs the live path
  (`--shared-position`, `--memoised-identity`, `--checkouts-of`, `--canonical-checkout`, `--classify`). A `canonical`
  control proves the stubs reach the predicate.
- **Step 2 needed a third memoised question.** With only one scope, a repository reached from both picker sources still
  paid its `ls-files` twice: `--holds-tracked-file-p` was not memoised, and the plural calls get away with that only
  because they de-duplicate candidates before the predicate runs. Added `--shared-tracked-file-p`, which is per call and
  keyed by truename like a position, and `--canonical-checkout` now asks through it. No existing budget number moves.
  Without this memo the new pin would be 11, not 10.
- **Step 2 form: a public FUNCTION scope, not the brief's macro, and not a merged single entry.** A macro cannot cross
  the DD-10 soft edge: git-worktrees loads first and is read before portable-bookmarks defines anything, so eager
  macroexpansion would treat the form as a plain function call. A single "view + these" entry would put both sources
  behind ONE error barrier. The existing spec "warns once and still takes candidates from magit" (discovery signals
  -> magit's list still offered) would then lose magit's candidates, which is an outcome change on the error path.
  `workspace-bookmark-call-with-discovery-memo (FUNCTION)` keeps both calls, their barriers and their unbound
  fallbacks exactly as they were. Unbound, git-worktrees simply calls the build.
- **Step 4: a `:source-repo` that is present but not an existing LOCAL directory returns `(failed . REASON)` and does
  not prompt.** This departs from the on-touch finding's suggestion to fall back to the prompt. From the bookmark
  chooser that fallback would bring back the exact defect: a second, free-form source prompt after the user already
  chose. The chooser declines a `failed` result with its reason. A remote spelling is refused on `file-remote-p`
  without being probed. The source is given a trailing slash before it is bound as `default-directory`.
- **Latent issue, out of scope:** `jf/workspace--worktree-read-repo` returns `(expand-file-name choice)`. A typed path
  without a trailing slash then becomes a `default-directory` that names the PARENT for `magit-main-branch` and
  `magit-worktree-branch` (Emacs reads a non-slash `default-directory` as a file name). The completion candidates are
  slash-ended, so only free-form typing hits this. It sits next to `.tasks/worktree-read-repo-empty-input.md`.
- **Finding 33:** the payload now comes from `workspace--integration-payload` (declared, `fboundp`-guarded, never
  required), with `:source-repo` appended. Signature as the brief said, `(name home context)`; it emits `:name :home
  :sessions-dir :context`. When it is unbound, the fallback lists the same keys without `:sessions-dir`, because this
  module cannot derive that key without core.
- **Finding 36:** `(require 'view)` moved to `* Requires`. The finding's "while there" was also taken: the duplicate
  `(require 'seq)` at the head of § Registry access was dropped.
- **Step 3:** the picker budget describe's header names the `magit-toplevel` post-filter spawn as knowingly uncounted,
  outside the one-runner spy by design.
- **The task's verification grep** (`workspace-bookmark--identity\b` in basic regex) also matches
  `workspace-bookmark--identity-memo` (the memo variable), because `\b` falls between `y` and `-`. Excluding that
  variable, the only surviving helper in production `.el` is `--checkouts-of`, and its production caller is
  `--main-worktree-of` (portable-bookmarks.el:1532).
- **Each new or moved spec fails with its production change reverted.** Method: the one `defun` in the TANGLED `.el`
  is replaced by its HEAD version with a scratch script, then git-worktrees-spec, missing-repo-spec, discovery-spec and
  cost-spec are run under the harness (control: 195 specs, 0 failed), then the `.el` is restored and `cmp`-verified.
  - `jf/workspace--add-worktree` reverted: 4 fail. These are the three `:source-repo` specs and missing-repo-spec's
    real-command integration spec (the stubbed `completing-read` errors, the chooser declines). "prompts for the
    source as before when the payload has no `:source-repo`" passes under the revert, as it should: it pins the
    unchanged arm.
  - `jf/workspace--worktree-repo-candidates` reverted (two scopes): 2 fail. They are the unit "classifies both sources
    inside ONE discovery call" and the budget "pays ... ten" (13 measured).
  - `--canonical-checkout` reverted (tracked check not shared): 1 fails, the budget "ten" (11 measured).
  - `--add-worktree-payload` reverted: 1 fails, "is core's anchor payload ... plus `:source-repo`".
  - `--main-worktree-of` reverted: 1 fails, the re-homed "never lists a checkout that is not canonical" (its
    `canonical` control).
  - `--checkouts-of` reverted to un-memoised: 11 budget specs fail (cost-spec and picker).

## Discoveries

- discovery_id: disc-discovery-memo-dead-helpers-and-picker-scope-1
  class: dead-branch
  description: |
    Finding -23 closed. `workspace-bookmark--identity` and `workspace-bookmark--repo-root` are DELETED (no production
    caller; their specs moved onto the live composition through two fixture helpers). `workspace-bookmark--checkouts-of`
    is KEPT and LIVE. It is the memo-aware home of `--main-worktree-of` (= its car), so the porcelain specs and the
    "not reflexive" clause describe production again. Decided from the call graph: deleting it would have left
    `--main-worktree-of` re-implementing it and the full-list specs pinning nothing live. New private function
    `workspace-bookmark--shared-tracked-file-p` (per-call memo of the canonical test's `ls-files`).
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    functions list: drop `--repo-root` and `--identity`; keep `--checkouts-of` annotated "memo-aware; sole
    production caller `--main-worktree-of`"; add `--shared-tracked-file-p` beside `--shared-position` /
    `--shared-worktree-entries` (and those two, `--with-discovery-memo`, `--memoised-in-call`, `--main-worktree-of`
    if the list does not yet name them); add the public `workspace-bookmark-call-with-discovery-memo`. The purpose
    table's "which repository?" row is answered at a POSITION's root (`--root-identity` via `--memoised-identity`),
    never by a separate `rev-parse --show-toplevel`.

- discovery_id: disc-discovery-memo-dead-helpers-and-picker-scope-2
  class: interface-drift
  description: |
    Finding -25 closed by ADDING a scope-wrapping public function beside the unchanged entry point:
    `(workspace-bookmark-call-with-discovery-memo FUNCTION)` calls FUNCTION inside one discovery memo and returns
    its value. `workspace-bookmark-canonical-checkouts-in-view` keeps its name, calling convention and meaning (not
    aliased, not extended). git-worktrees' new `jf/workspace--worktree-in-one-discovery-call` wraps the two source
    calls in `jf/workspace--worktree-repo-candidates` (only the source calls: de-dupe, the git-repo filter and the
    prompt are outside), and it is `fboundp`-guarded at call time with plain `funcall` when unbound. The tripwire
    (git-worktrees-spec "names no private portable-bookmarks function") now permits exactly these TWO public names.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Pin 3 / directionality.permitted: "MAY name exactly two portable-bookmarks symbols, the public entry
    `workspace-bookmark-canonical-checkouts-in-view` and the public scope `workspace-bookmark-call-with-discovery-memo`,
    each quoted and fboundp-guarded at call time; both sources are classified inside one scope". producer_side gains
    `jf/workspace--worktree-in-one-discovery-call`. Same change in git-derived-checkout-discovery.cross_module_surface
    ("ONE public function" -> one public function + one public scope, same STABILITY clause and tripwire).

- discovery_id: disc-discovery-memo-dead-helpers-and-picker-scope-3
  class: spec-signal
  description: |
    Picker budget re-pinned for the overlap shape (a repository in view through a workspace worktree AND in magit's
    list). With one scope and the tracked-file memo it costs 10 on the fixture: that worktree's position and the
    listing, then the main clone's position and tracked check, then 0 for magit's copy of it and 3 each for two other
    clones. With the scope split it is 13. The existing numbers do not move (9 / 11 / 19; cost-spec 2 / 6 / 11 / 12 /
    3). The `magit-toplevel` post-filter spawn is documented in the spec as knowingly uncounted.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    cost_contract: the picker is ONE discovery call per candidate build (not per public call). A repository reached from
    both sources pays its canonical test once (pinned 10, companion 13 with the scope split,
    git-worktrees-spec § "with a repository both in view and in magit's list"). The unit costs gain "the tracked-file
    check is shared per candidate within a call". Drop the "(finding -25)" double-count sentence.

- discovery_id: disc-discovery-memo-dead-helpers-and-picker-scope-4
  class: interface-drift
  description: |
    Finding -31 (blocking on missing-repo-choice) closed. `jf/workspace--add-worktree` honours the payload's
    `:source-repo`. When the key is present and names an existing local directory, the command shows no source prompt
    and runs no discovery (`--worktree-read-repo` and the candidate build are not called), and the worktree is made
    from that directory, slash-ended, as `default-directory`. When the key is present but unusable, the command
    returns `(failed . "source repository is not a directory: ...")` with no prompt and no magit call. When the key
    is absent, the command prompts exactly as before. Pinned on both arms in git-worktrees-spec, and end to end in
    missing-repo-spec: the bookmark chooser's add-worktree path through the REAL command (only magit and the branch
    prompt stood in for) makes no source prompt, never calls the public discovery entry, and creates
    `<home>/<basename of :checkout>` from `:checkout`.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Pin 3: "`jf/workspace--worktree-read-repo` is the SOLE prompting fn — skipped when the payload carries
    `:source-repo`; a present-but-unusable `:source-repo` is `(failed . REASON)`, never a prompt". Lift the DIVERGENT
    note drafted in the missing-repo-choice on-touch findings. The nested `add-worktree` member text in
    register/vocabulary/bookmark-resolution-outcome can now read "targets :checkout and never re-runs discovery"
    without the caveat. test_corpus: git-worktrees-spec § `jf/workspace--add-worktree` "with a :source-repo in the
    payload", and portable-bookmarks-missing-repo-spec "through the REAL git-worktrees command".

- discovery_id: disc-discovery-memo-dead-helpers-and-picker-scope-5
  class: shape-fragmentation
  description: |
    Finding -33 closed. `workspace-bookmark--add-worktree-payload` builds the anchor payload through the ONE
    constructor: `(append (workspace--integration-payload WORKSPACE HOME 'bookmark-jump) (list :source-repo
    CHECKOUT))`. The signature matches `(name home context)` and the constructor emits `:name :home :sessions-dir
    :context`, so the payload now carries `:sessions-dir` too. The constructor is reached declared + `fboundp`-guarded,
    never required, and the hand-built list (without `:sessions-dir`) is only the unbound fallback. Pinned in
    missing-repo-spec § "the add-worktree payload". NEW core symbol named by portable-bookmarks:
    `workspace--integration-payload`.
  affected_register_entry: register/shape/workspace-integration-anchor-payload
  recommendation: |
    Shape entry: add `optional: :source-repo` (string, absolute directory; "the source repository a caller has already
    decided; git-worktrees' :menu command then skips its source prompt"), and a constructed_by note: "portable-bookmarks'
    missing-repo chooser appends :source-repo to the canonical constructor's output". consumed_by gains
    git-worktrees.org § Menu Handler (reads :source-repo). register/boundary/portable-bookmarks-consumer: permitted list
    gains `workspace--integration-payload` (same class as `workspace--home`). register/vocabulary/workspace-creation-context:
    `bookmark-jump` is now passed as the constructor's CONTEXT argument (the fifth member, as the missing-repo-choice
    on-touch draft said).
