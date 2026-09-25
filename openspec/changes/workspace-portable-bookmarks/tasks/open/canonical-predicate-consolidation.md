---
name: canonical-predicate-consolidation
description: One shared "real canonical checkout" predicate and one public in-view entry point for both portable-bookmarks and git-worktrees; close the OS-artifact husk gap
change: workspace-portable-bookmarks
status: needs_review
merge_commit: e433c5c9
task_class: refactor
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
  - register/boundary/workspace-git-worktrees-consumer
relations:
  - blocked-by:worktree-candidates-git-first
  - blocks:resolver-outcomes
discovered_from: [discovery-refinements, worktree-candidates-git-first]
discovered_by: [architect, reviewer, user]
discovered_class: duplication
reconciled_into: null
---

## Why

Three independent findings converge on one shape problem:

1. **OS-artifact husk (architect F1 on discovery-refinements, re-found independently by
   its author-blind reviewer).** `--canonical-checkout`'s fourth check ("a
   tracked path exists under the candidate") is fooled when the repository TRACKS
   an OS artifact such as `.DS_Store` that Finder or a sync client also writes into
   the directory holding only a `.git`-named external git dir. Reproduced:
   `--canonical-for-identity` returns that directory. The org prose claims the
   check defeats exactly these artifacts.
2. **Second route to the husk (architect F2 on discovery-refinements).**
   git-worktrees filters `--checkouts-of` through `--classify` alone. That never runs
   `--canonical-checkout`'s checks, so it offers the husk as a worktree source and
   omits the real main worktree.
3. **Undeclared cross-module coupling (architect F2 on worktree-candidates-git-first).**
   git-worktrees makes fboundp-guarded calls to three `--`-private
   portable-bookmarks functions. Renaming one silently empties the git-derived
   source.

## Implementation steps

1. Add ONE predicate that answers "is DIR a real canonical checkout", composing
   `--canonical-checkout`'s checks with `--classify` = `canonical`. Every "offer as
   canonical" path in BOTH modules goes through it.
2. Harden the tracked-path check against OS and sync artifacts. Either ignore a short,
   documented list of artifact names (`.DS_Store`, `Thumbs.db`, `desktop.ini`,
   `Icon\r`, `.localized`, ...), or require a meaningful fraction of tracked
   top-level entries to exist. Pick one, justify it, and fix the org prose. Spec: a repo
   that tracks `.DS_Store`, with a husk that holds `.git` + `.DS_Store`, returns nil.
3. Add a PUBLIC `workspace-bookmark-canonical-checkouts-in-view` (no `--`) that
   returns the canonical checkouts in view, in order, de-duplicated by truename.
   git-worktrees calls only that, behind its existing call-time `fboundp` guard
   and `condition-case`. Remove the three private calls.
4. Guard (reviewer note): a child of an anchored home whose `.git` is an EMPTY
   directory is not a checkout root. It already fails closed; add the one-line
   guard and a spec.
5. **A home inside a checkout makes that checkout workspace-owned** (user
   decision 2026-09-25, ask 10). A workspace anchored at a SUBDIRECTORY of a main
   clone (e.g. `~/emacs/config`) is working in that clone, so the clone
   classifies `workspace-owned` and is never offered as canonical. Add the
   ownership test alongside the existing "checkout inside a home" walk: owned if
   the checkout is inside a home, IS a home, OR CONTAINS a home. Still compare
   with `--same-file-p`, and still ignore symlinks (DD-11). Spec it with the real
   anchor constructor.
6. **Two separate main clones of one identity are AMBIGUOUS** (user decision
   2026-09-25, ask 11). `--canonical-for-identity` must not pick silently when the
   checkouts in view yield more than one distinct canonical checkout for an
   identity, whether by current-workspace order or by registry order. Add
   `workspace-bookmark--canonical-checkouts-for-identity` (plural, every
   distinct canonical checkout). The singular returns the checkout only when it
   is unique, and nil when there are none or several, mirroring
   `--workspace-checkout(s)` from discovery-refinements part C. Do NOT add a
   resolution outcome: resolver-outcomes maps ambiguity.
7. Keep every existing spec in both suites green. Show that each new spec fails with its
   change reverted.

## Verification
- Tangle both `.org` files; no `.el` drift.
- `./bin/run-tests.sh -d config/workspaces` all green; full-suite NO-REGRESSION.
- `git grep -n "workspace-bookmark--" config/workspaces/git-worktrees.el` →
  no calls, only prose if any.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/on-touch-discovery-refinements.md` (F1, F2)
- `.orchestrator/cycles/cycle-1789935943/findings/on-touch-worktree-candidates-git-first.md` (F2)
- `.orchestrator/cycles/cycle-1789935943/reviews/discovery-refinements.md` (advisory; note on empty .git)

## Observations

- **How each new spec was shown to fail without its change.** I copied
  both module `.org`/`.el` files aside. For each step, a script applied ONE
  mutation to the `.org` (a revert of that step, or a deliberately wrong
  variant for guard specs), re-tangled both modules, ran
  `./bin/run-tests.sh -d config/workspaces` (567 specs), and then restored
  the four files byte-for-byte (`diff -q` clean). Results:
  - step 1, predicate without the `--classify` half: 11 failed. They include
    the owned-clone predicate spec, the public entry point's "never returns
    a checkout in view" spec, the anchored-subdirectory spec, and the
    existing clone-safety specs.
  - step 1, predicate without the `--canonical-checkout` half: 6 failed.
    They include both git-worktrees husk specs, the husk predicate spec, the
    tracked-`.DS_Store` husk spec, and the two existing husk specs.
  - step 2, the artifact skip removed from `--holds-tracked-file-p`: 2
    failed, the tracked-`.DS_Store` husk spec and the all-artifacts spec.
  - step 3, `git-worktrees.org` restored to its base version (it calls
    `--checkouts-in-view` / `--checkouts-of` / `--classify` directly): 10
    failed. They include both husk specs, where the old picker offered the
    husk, the structural "names no private portable-bookmarks function"
    spec, and the three "unbound / not loaded" specs.
  - step 3, the public function ignoring its CHECKOUTS argument: 8 failed.
    Examples are the explicit-empty-list spec and the magit-classification
    specs.
  - step 4, the root guard removed from `--workspace-checkouts`: 1 failed,
    the empty-`.git` child spec.
  - step 5, the reverse containment clause removed: 1 failed, the
    anchored-subdirectory spec, which uses the real `workspace-new`
    constructor.
  - step 5 guard specs pass whether or not the change is there, by design.
    They are shown by wrong implementations instead. A `string-prefix-p`
    reverse clause fails the "string prefix of a home" spec (1 failed). A
    clause that follows symlinks in the checkout fails the "holds a symbolic
    link to a home" spec (1 failed).
  - step 6, the singular returning the first match again: 2 failed, both
    two-clone ambiguity specs. With candidate de-duplication removed: 4
    failed. These are "not ambiguous for two worktrees of ONE clone" and
    three public-entry-point specs.

  Other specs pass with their change reverted, by design: "still returns a
  main worktree that tracks .DS_Store beside real files", the predicate's
  "admits a main worktree" and nil-input specs, "is nil with nothing in
  view", and "is nil for a nil identity". They are controls.
- **The public function takes an optional CHECKOUTS list. The task names
  only "the canonical checkouts in view".** git-worktrees also offers
  `magit-list-repos` entries, and those must pass the same predicate. The
  public function is the only surface git-worktrees may call, so it has to
  accept them. `cl-defun` with a supplied-p flag makes an explicit nil mean
  "none", so `(f nil)` can never silently turn into "everything in view".
- **Magit entries are now resolved to their repository's main worktree.**
  Before, each entry was tested as it stood. Now a magit-listed linked
  worktree contributes its repository's main worktree, when the predicate
  admits it. This follows from one definition ("the canonical checkouts of
  the repositories of these checkouts"), and it never widens what is
  offered: every output passed the predicate. `magit-repository-directories`
  is unset in this config, so nothing changes today.
- **Warn once per candidate collection.** git-worktrees now makes two
  barrier-wrapped calls: the in-view set, then magit's list. This keeps the
  existing spec where in-view discovery fails and magit's repository is
  still offered. It also keeps "warns once" when the classifier signals and
  both calls fail. A dynamically bound `jf/workspace--worktree-warning-state`
  suppresses the second warning inside one `jf/workspace--worktree-repo-candidates`
  call. Direct calls outside a collection still warn every time.
- **The predicate has no `condition-case` of its own.** It is built only
  from functions that already return nil or `other` instead of signalling,
  and `--canonical-for-identity` already worked that way. An unexpected
  error (a bug, or a spy in a spec) therefore reaches git-worktrees' barrier
  and is reported, not silently turned into nil.
- **Existing specs whose mechanism named the removed surface were
  updated.** Their assertions are unchanged.
  - `git-worktrees-spec.el`: the unit block's default spy now fakes the
    public function instead of `--classify`, and so does the "offers only
    the candidates the classifier calls canonical" fake.
  - `git-worktrees-spec.el`: the three "classifier unbound" / "not loaded"
    specs now unbind the public function. Unbinding only the private
    helpers no longer means "not loaded": git-worktrees consults the public
    name only.
  - The "warns once and still takes candidates from magit" and "classifier
    signals" specs are unchanged. They still spy the private
    `--checkouts-in-view` / `--classify`, which now fail INSIDE the public
    function.
  - `portable-bookmarks-discovery-spec.el`: the stubbed unit spec
    "canonical-for-identity never returns a checkout that is not canonical"
    would have passed vacuously, because the candidate now comes from
    `--checkouts-of`. It now also stubs `--checkouts-of` and uses a real
    directory, so only the classification can refuse it. The step 1
    no-classify mutant fails it.
- **Where the candidate comes from.** Each checkout's candidate is the first
  usable entry of `--checkouts-of`, one subprocess. The predicate then runs
  `--classify` first, which starts no subprocess, and only then
  `--canonical-checkout`, which starts four. Candidates are de-duplicated
  before the predicate runs. This is cheaper than calling
  `--canonical-checkout` on every in-view checkout and then again inside the
  predicate. Only the main worktree can pass, because the predicate
  requires DIR to BE the main worktree git names. `--canonical-checkouts-for-identity`
  gives up the old early stop: uniqueness needs every in-view checkout of
  the identity.
- **Step 2: a denylist, not a fraction.** The reasons are in the org:
  - a fraction must read the whole index on every call;
  - it refuses sparse checkouts and trees with many deletions in progress;
  - any threshold would be arbitrary.

  The residuals are documented. A stale copy of the tree still passes, and
  so does a tracked file some other program writes into the husk under a
  name not on the list. A main worktree whose ONLY tracked files are
  artifacts is now refused (nil, meaning `no-clone`).
- **Step 4: where the guard sits.** It is in `--workspace-checkouts`, not in
  `--home-checkouts` / `--checkout-dir-p`. That avoids a subprocess per home
  child on every in-view walk. The in-view set can still list such a child.
  Its candidate is then the enclosing repository's main worktree, and the
  predicate decides whether that is offered. The guard reuses the root it
  computes to read the identity, so it costs no extra subprocess.
- **Step 5 widens ownership only to clones that really contain a home.**
  The new clause is a disjunction, so a checkout's classification changes
  only when some registered home's TRUENAME is the checkout or lies below
  it. Nothing else changes. Consequences worth knowing:
  - A repository whose work tree is `$HOME`, with a real `~/.git`, contains
    every home under `~`. It is never offered.
  - Homes are compared by truename. So a home that is itself a symlink
    whose target lies inside a clone makes that clone owned. This matches
    the existing treatment of a home that is a symlink to a clone.
  - A symlink INSIDE the clone that points at a home confers nothing, and
    this is pinned by a spec.
- **Adjacent latent issue, out of scope.** `jf/workspace--worktree-children`
  in `git-worktrees.org` still compares with `file-equal-p`. That is the
  timestamp-sensitive comparison portable-bookmarks replaced with
  `--same-file-p`. Under a concurrent writer, `self` can come back nil, so
  the branch is nil and no branch deletion happens. That fails safe, but a
  merged branch may be left behind.
- The anchored-subdirectory spec runs `workspace--new-anchor-existing`
  case 3 for real, which does `git init` plus a commit inside a tracked
  subdirectory of a temp fixture clone. That is the hazard already
  externalised as finding 4 of discovery-refinements. The spec only uses it
  to build the layout the user decision is about.

### Revision 1

Fix scope from the architect's on-touch audit of 532e9bb0
(`findings/on-touch-canonical-predicate-consolidation.md`).

- **Finding 1 (spec-signal).** This corrects step 5 to the user's stated
  intent. The rationale was "working in that clone". A home that is
  itself ANOTHER WORKTREE OF THE SAME REPOSITORY, nested inside the main
  clone (`<main>/.worktrees/feat`, `<main>/.claude/worktrees/x`), works
  in its own tree. The reverse-containment clause in `--owning-workspace`
  therefore now also requires
  `(not (--same-repository-worktree-p home checkout))`. That helper holds
  when two things are true:
  - the home is its own top level;
  - its `--git-common-dir` is `--same-file-p` to the checkout's.

  The checkout's common dir is taken as its `.git` directory when it has
  one, so no subprocess is needed. Git is asked only when the checkout's
  `.git` is a file. The whole check runs only when spatial containment
  already holds: one `rev-parse` in the home, and one more only for a
  checkout whose `.git` is a file. Two cases still own the clone:
  - A plain subdirectory home: git's top level for it is the enclosing
    clone, not the home.
  - The nested repository that anchor case 3 `git init`s: its common dir
    is different.

  The worktree spec registers the home with the fixtures, not with
  `workspace-new`. `workspace--new-anchor-existing` misreads a linked
  worktree as a non-repository and scaffolds it: `git init`, add and
  commit on its branch. That is the anchor bug already externalised as
  discovery-refinements finding 4.
- **Finding 2 (advisory).** `--path-within-p` now requires PATH to exist,
  as well as DIR, so a deleted (broken) workspace home owns nothing. The
  org sentence at the containment walk now matches the code and explains
  why.
- **Finding 3 (advisory).** The magit linked-worktree → main-worktree
  replacement is KEPT. `git-worktrees.org` § "Cross-module interface" now
  says so plainly. A new picker-level spec isolates it: magit lists only a
  linked worktree whose main is not in view, and the picker offers that
  main.
- **Revert proofs.** For each change I applied one mutation to the `.org`,
  re-tangled, and ran `./bin/run-tests.sh -d config/workspaces` (571
  specs). I then restored the files byte-for-byte.
  - The same-repository exemption removed: 2 failed. One is the
    nested-worktree spec. The other is the plain-subdirectory control,
    because the nested worktree's workspace became the owner instead.
  - The top-level check in `--same-repository-worktree-p` removed: 1
    failed, the plain-subdirectory control. Git answers for the enclosing
    clone, so without the check a plain subdirectory home would stop
    owning it.
  - The PATH existence test removed: 1 failed, the deleted-home spec.
  - magit entries kept only when they are themselves canonical (the old
    semantics, no replacement): 1 failed, the new picker-level
    replacement spec.

## Discoveries

- discovery_id: disc-canonical-predicate-consolidation-1
  class: invariant-gap
  description: |
    The canonical-safety rule now has ONE home:
    `workspace-bookmark--offerable-canonical-p` (DIR classifies
    `canonical` AND is `--same-file-p` to `--canonical-checkout` of DIR).
    Every offer-as-canonical path goes through it: `--canonical-for-identity`
    (via its plural), `workspace-bookmark-canonical-checkouts-in-view`, and
    git-worktrees' picker. `--classify` = `canonical` is necessary, not
    sufficient; its docstring now says so. A home INSIDE a checkout makes
    the checkout workspace-owned (user decision 2026-09-25, ask 10). Check
    4 skips tracked OS / sync-client artifacts (`.DS_Store`, `._*`,
    `.localized`, `Icon\r`, `Thumbs.db`, `ehthumbs.db`, `desktop.ini`,
    `.directory`; case-insensitive).
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Reconcile. `canonical_safety_rule`: name `--offerable-canonical-p` as
    the only admission test, and add "classify=canonical is NECESSARY, NOT
    SUFFICIENT"; drop the "(Open: git-worktrees uses --classify alone)"
    note, which is now closed. `members[workspace-owned]`: "is, lies
    inside, or CONTAINS a registered :home (by truename / --same-file-p);
    links confer nothing." `known_limitations`: replace "a home inside a
    clone does not own it" with its opposite; change the husk residual to
    "a stale copy of the tree, or a tracked file under a non-denylisted
    name written by another program". Add
    `canonical-predicate-consolidation` to `cited_by_tasks`.

- discovery_id: disc-canonical-predicate-consolidation-2
  class: interface-drift
  description: |
    The three-function private surface git-worktrees used
    (`--checkouts-in-view`, `--checkouts-of`, `--classify`) is gone. The
    cross-module surface is now ONE public function:
    `(workspace-bookmark-canonical-checkouts-in-view &optional CHECKOUTS)`.
    - With no argument, it returns the canonical checkouts of the
      repositories in view.
    - With CHECKOUTS, it returns those of CHECKOUTS instead, and an
      explicit nil yields nil.
    - Each result passed the predicate; results are in order of first
      appearance, de-duplicated by file identity, absolute and slash-ended.
    - It never signals in normal operation.
    Both modules' org prose names it as a cross-module interface with a
    stability promise. `git grep -n "workspace-bookmark--"
    config/workspaces/git-worktrees.el` finds nothing, and a spec reads the
    tangled file to pin that.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Reconcile `cross_module_surface` (as drafted in the on-touch audit of
    worktree-candidates-git-first) to name only the public function, its
    optional CHECKOUTS argument and the explicit-nil semantics.
    `consumer_filter` becomes "none: every element already passed
    --offerable-canonical-p". `dependency_direction`: unchanged (call-time
    fboundp, no require, no declare-function). Add the list form
    `--canonical-checkouts-for-identity` (plural) and
    `--offerable-canonical-p` to the functions list.

- discovery_id: disc-canonical-predicate-consolidation-3
  class: deviation
  description: |
    CONFIRMED entry, deviation documented. Pin 3's picker now works like
    this:
    - The git-derived source is already canonical when it arrives (from
      the public function).
    - magit's entries are classified by the SAME public function, and are
      resolved to their repository's main worktree instead of tested as
      they stand.
    - Two error barriers show at most ONE warning per candidate collection.
    Unchanged: the call-time fboundp guard; no require; passthrough of
    magit when portable-bookmarks is absent; truename de-dupe;
    git-repo-p filter; the read-directory-name floor and free-form
    entry; projectile and project.el ignored.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Amend pin 3: "candidates from every source are admitted by
    workspace-bookmark-canonical-checkouts-in-view (the single public
    portable-bookmarks function); a source that signals is warned about
    (once per collection) and contributes nothing." Keep status confirmed.

- discovery_id: disc-canonical-predicate-consolidation-4
  class: spec-signal
  description: |
    User decision 2026-09-25 (ask 11) is implemented: two separate main
    clones of one identity are AMBIGUOUS.
    `workspace-bookmark--canonical-checkouts-for-identity` returns every
    distinct canonical checkout, with the current workspace's first and
    then registry order. `--canonical-for-identity` is non-nil only when
    that list has exactly one element. No resolution outcome was added.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Record the ambiguity rule next to canonical_safety_rule. resolver-outcomes
    must call the plural to tell "none" from "several", as it must for
    `--workspace-checkouts`.

- discovery_id: disc-canonical-predicate-consolidation-5
  class: interface-drift
  description: |
    Supersedes the pin 3 recommendation in disc-3. The magit
    linked-worktree → main-worktree replacement is KEPT (architect on-touch
    finding 3), and is now stated in git-worktrees.org and pinned by a
    picker-level spec.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Replace pin 3 with the architect's drafted wording; status stays
    confirmed:
      3. =SOFT REPO-SOURCE SELECTION, ONE ADMISSION TEST=:
      =jf/workspace--worktree-repo-candidates= merges two sources, in order:
      - the git-derived source, =(workspace-bookmark-canonical-checkouts-in-view)=;
      - =(magit-list-repos)= when magit is loaded, passed through
        =(workspace-bookmark-canonical-checkouts-in-view DIRS)=.
      Projectile and project.el are not consulted. portable-bookmarks'
      single predicate admits every candidate. A magit-listed LINKED
      worktree contributes its repository's MAIN worktree, which magit need
      not list. The new worktree is created off main either way, and only
      the default directory name follows the main worktree.
      The public function is the ONLY portable-bookmarks symbol named. It
      is called under a call-time =fboundp= guard and never =require=d.
      When it is unbound, the git-derived source contributes nothing and
      magit's list passes through unclassified. When it signals, a warning
      is shown (at most one per candidate collection) and that source
      contributes nothing.
      The merged list is de-duped by truename (first spelling kept) and
      filtered to real git repositories. The =read-directory-name= floor
      and free-form entry (=jf/workspace--worktree-read-repo=) are
      unchanged.
    Add canonical-predicate-consolidation to cited_by_tasks.

- discovery_id: disc-canonical-predicate-consolidation-6
  class: invariant-gap
  description: |
    Two changes to ownership:
    - Reverse ownership (a home inside a checkout) is narrowed. A home
      that is a checkout root of the SAME repository (same git common
      dir, and the home is its own top level) confers no ownership on the
      checkout containing it (architect finding 1).
    - A registered home that no longer exists confers no ownership in
      either direction, because --path-within-p now requires both
      arguments to exist (architect finding 2).
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Amend `members[workspace-owned]`:
      "... or CONTAINS an existing registered :home that is not itself a
      worktree of the checkout's repository ..."
    Do NOT add the drafted known limitation "a linked worktree nested in
    main, anchored as a home, owns main"; this revision removes it. Keep
    the limitation "a home under an untracked/ignored subdirectory owns
    the clone"; that is the decision as worded.

