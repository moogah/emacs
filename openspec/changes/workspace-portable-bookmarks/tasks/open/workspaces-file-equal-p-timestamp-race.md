---
name: workspaces-file-equal-p-timestamp-race
description: "workspaces core (tabs.el, git-worktrees.el) uses file-equal-p for path identity; file-equal-p compares full file-attributes (timestamps included), so a directory modified between the two reads compares unequal. Replace with a device+inode comparison."
source: wave3-review-fixes (cycle-1789935943, workspace-portable-bookmarks)
status: needs_review
task_class: bug
change: workspace-portable-bookmarks
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/invariant/directory-identity-by-device-inode
  - register/boundary/portable-bookmarks-consumer
discovered_from: checkout-discovery
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
relations:
  - promoted-from:.tasks (2026-09-27, plan ask P2, user decision)
  - discovered-from:checkout-discovery
  - related-to:wave3-review-fixes
promoted_at: 2026-09-27T00:30:00+00:00
---

## Problem

`file-equal-p` (a C subr in Emacs 30) treats two paths as the same file only if
their complete `file-attributes` lists are `equal`, including access and
modification times. Any write into a directory between the two attribute reads
makes the SAME directory compare unequal. The author-blind review of
checkout-discovery demonstrated this failing OPEN in the portable-bookmarks
ownership test (4,817 of 20,000 calls under a concurrent writer). That module now
uses `workspace-bookmark--same-file-p` (device + inode of the truenames), merged in
9c8b2309.

`config/workspaces/tabs.el` and `config/workspaces/git-worktrees.el` still use
`file-equal-p` for path identity. The implementor of wave3-review-fixes judged
that neither use can cause a wrong canonical-checkout offer. Both are still
spuriously-false comparisons under concurrent writes.

## Decision (user, 2026-09-27)

Fix now, in cycle-1790453714, as the last code task (wave 4). The user's reading: the
check is flawed because it compares attribute lists that include volatile fields
(modification / access times), so two fresh reads of the same directory straddling a
write compare unequal — a false negative.

## Cited register entries

- `register/invariant/directory-identity-by-device-inode` (NEW, speculated, load_bearing):
  ONE device+inode predicate decides identity; strings are hash keys / dedup pre-filters
  only; `file-equal-p` is rejected. Its `known_violations` are exactly the two sites below.
  This task is its enforcement. Pressure-test: is a single core predicate sufficient for
  every caller, including remote (TRAMP) paths, which the bookmarks copy refuses (nil)?
- `register/boundary/portable-bookmarks-consumer` (confirmed): core never names a
  `workspace-bookmark-*` symbol, so the shared predicate's home MUST be core and the
  bookmarks module routes to it (not the reverse). The permitted-core-surface list gains
  the new core symbol at integrate.

## Implementation steps

1. Add ONE core predicate in `config/workspaces/workspaces.org` (or the core file that
   `tabs.org` and `git-worktrees.org` both already load; say which and why), e.g.
   `workspace--same-file-p (a b)`: device+inode of the two truenames; nil when either is
   not a string, is remote, or does not exist; never signals. Port the body and docstring
   of `workspace-bookmark--same-file-p` (portable-bookmarks.el:96-118 at b56f385a).
2. Route the two core sites to it: `tabs.org:296` (tangled tabs.el:147, home comparison)
   and `git-worktrees.org:454` (tangled git-worktrees.el:208, worktree-child match).
   Update the prose at tabs.org:272, :285, :347 that says "by `file-equal-p`".
3. In `portable-bookmarks.org`, DELETE `workspace-bookmark--same-file-p` and route its 6
   call sites (grep at HEAD) to the core predicate. A second tangled body is duplication,
   which this project's overlay treats as blocking. Keep the spy-based spawn/identity specs
   green: rename any `spy-on 'workspace-bookmark--same-file-p` to the core symbol.
4. Specs: move the existing device+inode / concurrent-writer spec for the bookmarks copy
   onto the core predicate (a core `*-spec.el`, buttercup); add one spec per re-routed core
   site proving a directory modified between two stats still compares equal to itself
   (the failing form under `file-equal-p`: touch a file inside DIR between the two reads).
5. Tangle all three `.org` files; validate.

## Verification
- `grep -rn "file-equal-p" config/workspaces/*.el config/workspaces/*.org` returns only
  prose that says it is rejected (or nothing).
- `grep -rn "workspace-bookmark--same-file-p" config/` returns nothing.
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set.

## Out-of-scope
Any other identity comparison outside `config/workspaces/`. Changing what counts as
"in view" or "owned".

## Context
- `register/invariant/directory-identity-by-device-inode` (interfaces.org, seeded at
  cycle-1790453714 plan from meta-discovery "path-identity-by-device-inode")
- architect forward-mode finding arch-cycle-1790453714-plan-04 (why the body was rewritten)
- Wave placement: wave 4 beside `docs-readme` (touches no .org); every earlier code wave
  edits `portable-bookmarks.org`.
- Never bulk-delete shared temp prefixes (no `rm -rf $TMPDIR/wsbm-*`).

## Observations

- Where the predicate lives: `config/workspaces/data-model.org` § File identity, as
  `workspace--same-file-p` (tangled `data-model.el:264`). I did not use `workspaces.org`.
  `workspaces.el` is the package loader, and loading it runs every `jf/load-module`
  line. No spec loads it on its own except `workspace-new-anchor-spec`, so a predicate
  defined there would be missing from every spec that loads `tabs.el` or
  `git-worktrees.el` directly. `data-model.el` is the first line in the loader
  (workspaces.org:130), and `tabs.el` already `(require 'workspace-data-model)`.
  `git-worktrees.el` loads after it (:135). `portable-bookmarks.el` loads after it (:148).
  Every spec in scope already loads `../data-model.el`, or gets it through the
  fixtures. I did not reorder the loader. The first paragraph of `data-model.org` said
  "Pure, side-effect-free", so I changed it to name this predicate as the one function
  in the file that reads the filesystem. It writes nothing.
- `git-worktrees.el` calls `workspace--same-file-p` with no `require` and no
  `declare-function`. It already calls core (`workspace-register-integration`) the same
  way and relies on the package load order. `portable-bookmarks.org` gains
  `(declare-function workspace--same-file-p "data-model" (a b))`, the same form it uses
  for `workspace--home` and `workspace--broken-p`.
- The task body is wrong in two small places:
  (a) It says `file-equal-p` is "a C subr in Emacs 30". It is Lisp: in Emacs 30.2,
  `(symbol-file 'file-equal-p)` gives `files.elc`. That is why a `cl-letf` on
  `file-attributes` catches it. A probe under a timestamp-drift spy gave `(file-equal-p d d)` → nil.
  (b) It says there are "6 call sites" in portable-bookmarks. At base 43350791 there are
  11 code call sites plus 2 docstring mentions in `portable-bookmarks.el`, 1 in
  discovery-spec and 11 in missing-repo-spec. I routed all of them.
- tabs.el `workspace--registered-for-home-p` no longer does its own
  `file-name-as-directory`/`expand-file-name` on both sides, because the predicate
  expands and truenames its inputs. For a nil `:home`, the result is nil, as before. A
  missing home gives nil, as before, because `file-equal-p` also needed attributes for
  both sides. What changed is remote homes: see disc-2.
- New spec `config/workspaces/test/file-identity-spec.el` has 14 specs:
  - 7 contract specs: spellings, symlink, different directories, missing path,
    non-string, remote (with no `file-attributes` or `file-truename` call), never signals.
  - 3 race specs on the predicate:
    - a deterministic model where timestamps drift on every read;
    - a real write into DIR after every `file-attributes` read, which is the form that
      fails under `file-equal-p`;
    - a bounded stress test with a real concurrent writer: 2 s, 1000 calls at most.
    The first two include a control that asserts `file-equal-p` answers nil under the
    same spy.
  - 3 specs for core site 1, `workspace--registered-for-home-p`, including the
    write-between-reads race. They use `workspace-bookmark-fixtures-with-empty-registry`,
    the one registry-scope construct, so the file never touches `workspace--registry` by hand.
  - 1 spec for core site 2, `jf/workspace--worktree-children`. It stubs
    `magit-list-worktrees` with `cl-letf`, uses a real child directory with a gitfile, and
    writes between reads.
- How I showed the new specs fail without the fix. Each run loaded only the new spec
  under the harness.
  - Run A: `tabs.el` and `git-worktrees.el` restored from 43350791. 2 of 14 failed, and
    they were exactly the two site race specs: `registered-for-home-p` gave nil, and
    `:branch` was nil instead of "feature".
  - Run B: the current `.el` files, with `workspace--same-file-p` redefined after load as
    `(and (stringp a) (stringp b) (file-equal-p a b))`. 7 of 14 failed:
    - remote: TRAMP tried to connect, "Tramp failed to connect";
    - never-signals;
    - timestamp drift;
    - write between reads;
    - stress, with 262 wrong answers;
    - both site specs.
  - Under both runs the spelling, symlink, different-directory, missing and non-string
    contract specs pass. They pin the contract; they do not guard against regressions.
  - After both runs the restored `.el` files matched the tangled output again (git diff
    checked).
- The module's own ownership-under-write specs stay in portable-bookmarks-discovery-spec
  § "Canonical safety while the workspace home is being written to". They test
  `--classify` behaviour, not the predicate. The predicate's own coverage now lives in the
  core spec. I read "move the coverage" as moving the predicate-level coverage.
- Latent test hygiene issue, not mine to fix: in the full directory run, an earlier spec
  file (buffer-reincarnation-spec, prefix `ws-br-spec-`) leaves the current buffer's
  `default-directory` pointing at a directory it has deleted. `make-process` then fails
  with "Setting current directory". The new stress spec binds `default-directory` to its
  own temp dir. The discovery-spec stress spec happens to run in a good buffer. This
  belongs with `.tasks/test-suite-shared-global-state-isolation`.
- Test-side `file-equal-p` assertions: I switched the three that the register names as
  advisory to the core predicate: integration-dispatch-spec:118, discovery-spec:520 (an
  `assume` precondition) and :924. I also updated the comment in
  workspace-new-anchor-spec:15. I left 14 in portable-bookmarks-jump-spec and 6 in
  portable-bookmarks-off-workspace-spec. They compare `buffer-file-name` against static
  fixture files, they are not production identity decisions, and changing them would widen
  the diff for no behaviour change.
- `config/workspaces/docs/README.org:98` still says the anchor guard compares "by
  `file-equal-p`", and "false positives" should read "false negatives". The file is outside
  this task's verification grep (`config/workspaces/*.org`) and belongs to the docs task
  running in this wave, so I left it. The docs task should update it.

## Discoveries

- discovery_id: disc-workspaces-file-equal-p-timestamp-race-1
  class: interface-drift
  description: |
    The ONE identity predicate now lives in workspaces core as
    `workspace--same-file-p` (config/workspaces/data-model.org § File
    identity; tangled data-model.el:264). `workspace-bookmark--same-file-p`
    is deleted; every module call site (11 code sites in
    portable-bookmarks.el) and every spec reference names the core symbol.
    Core's two known violations are fixed:
    tabs.el `workspace--registered-for-home-p` and git-worktrees.el
    `jf/workspace--worktree-children`. Validator 1: grep file-equal-p over
    config/workspaces/*.el and *.org → only prose and docstrings rejecting it
    (data-model.el:268 docstring; data-model.org, tabs.org, git-worktrees.org,
    portable-bookmarks.org prose). Validator 2: exactly one
    `defun .*same-file-p` body (data-model.el:264). Validator 5 now has
    its test: config/workspaces/test/file-identity-spec.el (both core
    sites under a real write between reads, plus the predicate's
    timestamp-drift, write-between and bounded-writer specs).
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Promote speculated → confirmed. Rewrite statement "today
    =workspace-bookmark--same-file-p= (portable-bookmarks.el:96)" as
    "=workspace--same-file-p= (data-model.el:264; data-model.org § File
    identity)". Empty known_violations. Change the enforcement location
    to name file-identity-spec.el as the predicate-level proof, with the
    discovery-spec § "Canonical safety while the workspace home is being
    written to" as the module-level behavioural proof. Rewrite corollary
    3 in the past tense (the move happened; the module has NO defun, only
    a declare-function). Re-derive the line numbers in corollary 2:
    portable-bookmarks.el lost 22 lines above them.

- discovery_id: disc-workspaces-file-equal-p-timestamp-race-2
  class: scope-question
  description: |
    Pressure test: is one predicate enough for every caller, including
    remote paths? For the callers that exist today, yes. Nothing in the
    workspaces package supports a remote :home: scaffold, persistence and
    the broken-home check all assume a local directory. git-worktrees
    and portable-bookmarks already drop remote names before touching them.
    But the change is not neutral for remote paths at the tabs site.
    `file-equal-p` has a TRAMP handler, so the old duplicate-home guard
    compared two remote homes by opening a connection (run B showed
    "Tramp failed to connect" for an unreachable host). The core
    predicate answers nil for any remote name without a connection. So a
    remote home already in the registry would no longer block registering
    the same remote directory again. That is fail-open for
    `register/invariant/registry-name-equals-basename` and
    home-required-no-floating-workspaces, but only for remote homes,
    which the package does not otherwise support. For git-worktrees a
    remote home's child would get :branch nil, which is the same outcome
    as today's failure shape. No caller needs a DIFFERENT remote answer
    today. If remote homes are ever supported, the fix belongs in the ONE
    predicate: a remote arm that compares `file-remote-p` identification
    plus the remote truename's device and inode through the handler.
    It does not belong in a second predicate.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Add to the statement: "Remote (TRAMP) names are never the same file
    as anything (nil, no connection). Workspace homes are local by
    construction. Supporting remote homes means adding a remote arm to
    this predicate, never a second predicate." Record the tabs
    duplicate-home guard as the one caller for which nil is fail-open.

- discovery_id: disc-workspaces-file-equal-p-timestamp-race-3
  class: interface-drift
  description: |
    portable-bookmarks now names one more core symbol:
    `workspace--same-file-p`, declared with
    `(declare-function workspace--same-file-p "data-model" (a b))` in
    portable-bookmarks.org § Same file, the same form as its
    `workspace--home` and `workspace--broken-p` declarations. The
    direction is the permitted one (consumer → core). Core names no
    `workspace-bookmark-*` symbol: grep over data-model, tabs,
    integrations, workspaces, persistence, layouts, workspaces-mode and
    workspaces-transient .el gives zero hits.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Add `workspace--same-file-p` (data-model) to directionality.permitted's
    list of core symbols the module may name.

- discovery_id: disc-workspaces-file-equal-p-timestamp-race-4
  class: deviation
  description: |
    The brief suggested workspaces.org as the home. The predicate went to
    data-model.org instead, because workspaces.el is the loader and specs
    do not load it. data-model.el is the first module loaded, is required
    by tabs.el, and is loaded by every spec that loads tabs.el or
    git-worktrees.el. data-model.org described itself as a "pure,
    side-effect-free data layer". It now names this predicate as its one
    filesystem READ, which has no writes and no state.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    Record data-model.el as the predicate's home. If any register entry
    describes data-model as filesystem-free, note the one read-only
    exception.
