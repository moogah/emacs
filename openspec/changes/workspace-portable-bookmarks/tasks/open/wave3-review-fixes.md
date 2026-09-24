---
name: wave3-review-fixes
description: Close wave-3 review/architect findings — fail-closed same-file identity, symlinked workspace children, and a hardened git runner
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/checkout-classification
  - register/boundary/git-derived-checkout-discovery
  - register/shape/portable-bookmark-coordinates
relations:
  - blocks:resolver-outcomes
  - blocks:worktree-candidates-git-first
discovered_from: [checkout-discovery, stamp-seam]
discovered_by: [reviewer, architect]
discovered_class: invariant-gap
reconciled_into: null
---

## Why

The canonical-checkout safety rule (never offer a checkout that belongs to a
workspace) can currently fail OPEN, and the shared git runner can be steered
to the wrong repository by the process environment or by whitespace in a path.
Both are silent wrong-file risks: `register/invariant/no-silent-wrong-file-open`.

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify) + tangled `.el`
- `config/workspaces/test/portable-bookmarks-discovery-spec.el` (modify)
- `config/workspaces/test/portable-bookmarks-identity-spec.el` and/or
  `portable-bookmarks-stamp-spec.el` (modify) — runner and memo specs

## Implementation steps

1. **One same-file predicate, by identity, not by attributes.** `file-equal-p`
   compares the two paths' FULL attribute lists, timestamps included, so a
   directory modified between the two reads compares unequal. Used in the
   ownership test, that makes a workspace-owned checkout classify `canonical`
   (demonstrated: 4,817 of 20,000 `--classify` calls under a concurrent writer).
   Add one private predicate that compares device and inode numbers of the two
   paths' truenames (nil when either does not exist), and route EVERY same-file
   comparison in the module through it. No module code may call `file-equal-p`
   afterwards.

2. **Symlinked workspace children are owned.** A child of a workspace `:home`
   that is a symlink to a checkout elsewhere (`ws/A/x -> ~/src/main`) makes that
   checkout workspace A's. Today the path is resolved before the containment
   walk, so the target classifies `canonical` and is offered to other
   workspaces. A checkout is `workspace-owned` if it is inside a home OR it is
   the same file (step 1) as a child of a home. Ownership is still tested before
   structure.

3. **Harden the shared runner `workspace-bookmark--git`:**
   - Remove the repository-locating variables from the subprocess environment:
     at least `GIT_DIR`, `GIT_WORK_TREE`, `GIT_COMMON_DIR`, `GIT_INDEX_FILE`,
     `GIT_OBJECT_DIRECTORY`, `GIT_ALTERNATE_OBJECT_DIRECTORIES`,
     `GIT_NAMESPACE`, `GIT_CEILING_DIRECTORIES`, `GIT_DISCOVERY_ACROSS_FILESYSTEM`.
     `default-directory` must be the only thing that decides which repository
     git answers about.
   - Strip only git's trailing line terminator, not surrounding whitespace, so
     a path that begins or ends with a space survives intact.
   - A DIR argument that does not exist returns nil. It must not fall back to
     querying the parent directory; only an EXISTING file resolves to its
     directory.

4. **Absolute common dir everywhere.** Every `rev-parse --git-common-dir` call
   passes `--path-format=absolute`. The identity memo's key is currently
   computed from a relative answer resolved against a symlinked path, so it
   becomes nil and never invalidates (demonstrated: stale identity stamped
   after `git remote set-url`).

5. **Specs**, each demonstrated to FAIL without its fix:
   - ownership holds under a concurrent writer in the home (a bounded loop is
     fine; assert zero `canonical` results);
   - a symlinked workspace child makes its target `workspace-owned`, and
     `--canonical-for-identity` never returns it;
   - with `GIT_DIR` / `GIT_WORK_TREE` set in `process-environment`, identity,
     stamping and `--workspace-checkout` still answer about the right repo;
   - a checkout whose directory name ends in a space gets its own identity,
     not the enclosing repository's;
   - a missing DIR returns nil;
   - stamping through a symlink to a repo subdirectory picks up a changed
     remote (memo invalidates).

6. Tangle and validate.

## Out of scope
- Jump-path cost / grouping discovery by common dir (reviewer advisory A2) —
  tracked separately.
- The "immediate children of :home" model gaps (reviewer spec-signal) and the
  `bookmark-relocate` stale-stamp rule — pending user decisions.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` all green.
- `grep -n "file-equal-p" config/workspaces/portable-bookmarks.el` — no calls
  (docstring mentions explaining why it is avoided are fine).
- `grep -n "git-common-dir" config/workspaces/portable-bookmarks.el` — every
  call site also passes `--path-format=absolute`.

## Context
- `.orchestrator/cycles/cycle-1789935943/reviews/checkout-discovery.md` (B1, B2, A1)
- `.orchestrator/cycles/cycle-1789935943/reviews/stamp-seam.md` (A1, A2)
- `.orchestrator/cycles/cycle-1789935943/findings/on-touch-checkout-discovery.md` (F2, F3)

## Observations

- **How each spec was shown to fail without its fix.** `final.org` was
  copied aside. For each fix, a script wrote a copy of it with ONLY that
  fix reverted to the base code, re-tangled, and ran
  `./bin/run-tests.sh -d config/workspaces` (514 specs). It then restored the
  file byte-for-byte (`diff` clean) and re-tangled. Results:
  - step 1, `--same-file-p` body back to `file-equal-p`: 2 failures. The
    deterministic timestamp-spy spec got `canonical`. The bounded
    real-writer stress spec got 153 of 1000 calls `canonical`.
  - step 2, ownership back to home containment only: 3 failures, all three
    symlinked-child specs.
  - step 3, environment passed through: 3 failures, one each for identity
    (runner spec), `--workspace-checkout` (discovery) and stamping (stamp spec).
  - step 3, `string-trim` restored (with the old 2-or-3-line position
    parser it needed): 4 failures. These were the `proj ` identity and
    workspace-checkout spec, the terminator unit spec, the trailing-space root
    spec and the leading-space stamp spec.
  - step 3, parent fallback restored for a missing DIR: 1 failure, the
    missing-path spec.
  - step 4, `--path-format=absolute` removed from `--checkout-position`
    (relative answer expanded against DIR again): 1 failure, the
    symlink-to-subdirectory memo spec.
  - memo nil-stamp guard removed: 1 failure, its unit spec.
  In every run the failures were exactly the new specs; no existing spec
  failed. Two new specs are guards that pass either way by design: "does
  not change the caller's process-environment" and the dangling/elsewhere
  link control.
- **Step 2 is wider than the task's wording.** The task says a checkout is
  owned when it is the same file as a child of a home. I also treat it as
  owned when:
  - it lies INSIDE a symlinked child's target (`ws/A/src -> ~/src`, the
    reviewer's recommendation (b)); or
  - a symlinked child's target lies inside the checkout
    (`ws/A/foo -> ~/src/mono/packages/foo`). The workspace is then editing
    that checkout's files.

  Both only move a verdict from `canonical` to `workspace-owned`, which is
  the nil direction. The cost: a workspace that links broadly (to `~/src`)
  makes every checkout under the link non-canonical, and the resolver says
  `no-clone`. Only immediate children that are symlinks are examined. A
  real child directory is already covered by the home walk, and deeper
  links are not followed.
- **Step 3 unsets more variables than the task's nine.** The list is the
  task's nine plus the rest of `git rev-parse --local-env-vars` on git
  2.49: `GIT_CONFIG`, `GIT_CONFIG_PARAMETERS`, `GIT_CONFIG_COUNT`,
  `GIT_IMPLICIT_WORK_TREE`, `GIT_PREFIX`, `GIT_GRAFT_FILE`,
  `GIT_NO_REPLACE_OBJECTS`, `GIT_REPLACE_REF_BASE` and `GIT_SHALLOW_FILE`.
  `git -c` sets `GIT_CONFIG_PARAMETERS`/`COUNT` for a child editor or hook,
  and they can inject `remote.origin.url`. The list is static rather than
  queried at runtime, which would cost a subprocess per call.
  `GIT_CONFIG_GLOBAL`/`SYSTEM`/`NOSYSTEM` are deliberately left in place:
  they are the user's own configuration choice, like any gitconfig. The
  variables are removed by PREPENDING bare names (no `=`) to a let-bound
  `process-environment`, which Emacs treats as unset. That wins over
  duplicates later in the list and leaves the caller's list untouched (spec).
- **Parser change forced by removing the trim.** At a repository root,
  `--checkout-position` now receives three lines, the last one empty,
  where it used to receive two. The parser now requires exactly 3 lines
  (4 with COMMITTED) and absolute root and common-dir lines. Anything else
  is nil.
- **Supplementary hardening (not a numbered step).**
  `workspace-bookmark--memoised-identity` never treats a nil config stamp
  as a hit. A nil stamp matching a nil stamp is the mechanism that made
  the stale-identity bug permanent, and this closes it for any other way
  the common dir could come back wrong or missing. It is one condition and
  has its own spec.
- **Side effect in the stamp seam.** A tracked file at the repository root
  whose name begins with whitespace is now stamped. The stamp-seam review
  called it a "safe miss": the runner's trim mangled `ls-files -z`. It has a
  new spec.
- **Stress spec bounds.** At most 1000 calls or 3 s. The `sh` writer is
  deleted in `unwind-protect` and its exit is awaited for at most 5 s.
  With the fix the spec cannot fail, because ownership no longer reads
  timestamps. The deterministic spy spec is the primary proof.
- **Latent, out of scope.** Two other modules still use `file-equal-p`:
  - `tabs.el` (`tabs.org:296`, duplicate-home check at registration);
  - `git-worktrees.el:121` (`git-worktrees.org:256`).

  Both have the same timestamp race. There it yields a missed dedupe or a
  missed match, not a canonical offer. They could adopt the same
  device/inode comparison in a follow-up.
- **Known ownership gap left open.** A `:home` that itself lies INSIDE a
  checkout (for example `~/src/mono/.ws/A`) does not make that checkout
  owned. Neither do symlinks below the home's immediate children. Both
  belong to the "immediate children of :home" model question that is
  pending a user decision.
- **Errors in the containment walk.** `--path-within-p` still maps an
  internal error to nil, which is the not-owned direction. The only error
  reachable in practice is `file-truename` on a circular symlink, and such
  a path has no `.git` directory, so `--classify` still answers `other`.
  Noted, not changed.
- Full-suite report was written to the session scratchpad rather than into
  the worktree, to keep the worktree free of untracked files.

## Discoveries

- discovery_id: disc-wave3-review-fixes-1
  class: interface-drift
  description: |
    The workspace-owned member says containment is tested with
    file-in-directory-p "or an equivalent built on file-equal-p". Both are
    wrong. file-in-directory-p compares strings after truename and does not
    fold case. file-equal-p compares whole attribute lists, timestamps
    included, so it fails OPEN under a concurrent writer. Containment is now
    by device and inode of truenames through one predicate,
    workspace-bookmark--same-file-p, used at every same-file site in the
    module. Ownership also covers symlinked immediate children of a home, in
    both directions.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Replace the containment sentence: "Containment is by FILE IDENTITY
    (device + inode of truenames, workspace-bookmark--same-file-p). Neither
    string comparison nor file-in-directory-p nor file-equal-p is used; the
    last compares timestamps and fails open while the home is written to."
    Add: "A checkout is also workspace-owned when a symbolic link among a
    home's immediate children resolves to it, to a directory containing
    it, or to something inside it." Add validators: (a) no file-equal-p in
    portable-bookmarks.el; (b) a symlinked-child spec; (c) a timestamp-spy
    spec on file-attributes.

- discovery_id: disc-wave3-review-fixes-2
  class: interface-drift
  description: |
    subprocess_contract assumed that binding default-directory pins the
    repository, and said nothing about output or missing paths. Implemented:
    - repository-locating GIT_* variables are unset for every call;
    - only git's trailing "\n" is removed;
    - a missing DIR returns nil, and only an EXISTING file is resolved to
      its directory.
    Every --git-common-dir query, including the one in
    --checkout-position (stamp path), passes --path-format=absolute.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Add to subprocess_contract:
    - OUTPUT: only git's trailing line terminator is removed.
    - MISSING DIR -> nil; an existing FILE resolves to its directory.
    - ENVIRONMENT: workspace-bookmark--git-unset-environment (the task's
      nine plus the rest of `git rev-parse --local-env-vars`) is removed
      for every call.
    In the FOUR QUESTIONS, spell the canonical question
    `rev-parse --path-format=absolute --git-common-dir` (git >= 2.31) and
    note that the stamp path's position query uses the same flag.

- discovery_id: disc-wave3-review-fixes-3
  class: invariant-gap
  description: |
    The identity memo (stamp-seam DD-7) treated a nil config stamp as a
    valid key, so an entry whose config file could not be found matched
    forever and never revalidated. It now recomputes whenever the stamp is
    nil. A root-level relpath may now begin with whitespace, persisted
    verbatim: previously the runner trimmed it and the record was left
    unstamped.
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Note on the memo: "a nil config stamp is never a hit". Note on relpath:
    "whitespace at either end is part of the relpath and is persisted
    verbatim."

- discovery_id: disc-wave3-review-fixes-4
  class: scope-question
  description: |
    Ownership now extends through symlinked immediate children in both
    directions, which is wider than this task's wording. Two layouts are
    still not owned: a :home nested inside a checkout, and symlinks below
    the home's immediate children.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Fold these into the pending user decision on the "immediate children of
    :home" model (checkout-discovery review finding 5). Confirm or narrow
    the reverse direction (a link into a checkout makes it owned).
