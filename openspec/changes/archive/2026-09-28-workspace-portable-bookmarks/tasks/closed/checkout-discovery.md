---
name: checkout-discovery
description: Enumerate the checkouts in view via git and classify each as canonical, workspace-owned, or other
change: workspace-portable-bookmarks
status: done
merge_commit: 5170d7ef
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

## Observations

- **I changed the classification order: ownership is checked before structure.** Step 3 lists `canonical` (`.git` is a directory) first. I test `workspace-owned` first. A workspace can hold a FULL CLONE instead of a linked worktree. That clone's `.git` is a directory, so a structure-first classifier calls it `canonical`, and `--canonical-for-identity` would then hand workspace A's clone to a user in workspace B. That is exactly the defect this change exists to remove. The spec "Canonical safety with a full clone inside a workspace home" pins the behavior. In it, git itself says the clone is the main worktree (`--canonical-checkout` returns it), the classifier says `workspace-owned`, and `--canonical-for-identity` returns nil. The cost is conservative: a main worktree that sits under any registered home is never offered, so the resolver says `no-clone`. A workspace whose `:home` is a very broad directory (e.g. `~`) would therefore stop every canonical fallback under it. Registry homes are one directory per workspace in practice, so I accepted that.
- **Pressure point 1: `--git-common-dir` is relative.** Measured on git 2.49. It returns `.git` at the main worktree root, `../.git` one directory down, and an absolute path from a linked worktree. I pass `--path-format=absolute`, which also returns the truename (`/private/var/…`). This needs git ≥ 2.31. On an older git the call exits non-zero, so the result is nil and the resolver degrades to `no-clone`. There is no silent mis-expansion. I did not re-expand the relative answer myself, because the runner turns a FILE argument into its parent directory before running git. Expanding against the caller's DIR would be wrong whenever DIR is a file. A unit spec asserts that a relative common dir is REFUSED. A behavioral spec asserts an absolute answer from the root, from a subdirectory and from a file.
- **Pressure point 2: porcelain entries that are not usable checkouts.** `--checkouts-of` returns an entry only when it is a local, existing directory that contains `.git`, and is neither `bare` nor `prunable`. Detached worktrees are kept. Locked worktrees that exist are kept (the `~/.emacs.d` shape). Locked worktrees whose directory is missing are dropped. Git NEVER marks a locked entry `prunable`, so the existence check is the only guard there, and a spec proves both halves. Probing turned up something the task did not anticipate: for a submodule, and for a `git init --separate-git-dir G W` repository, the FIRST porcelain entry is the GIT DIRECTORY (`S/.git/modules/m`, `G`), not a working tree. That path exists and is a directory, so "never a directory that does not exist" alone would still hand callers a non-checkout. The `.git`-inside test catches it. Paths with spaces arrive intact (spec). I deliberately did NOT use `-z`, because it needs git ≥ 2.36. Ubuntu 22.04 ships 2.34, where `-z` would make every call fail and silently turn the feature off. Without `-z`, a worktree path containing a newline is read cut off at the newline, and the existence test then drops it. That checkout goes missing; it is never replaced by a different directory.
- **Pressure point 3: the two canonical signals disagree.** They do disagree in practice, and each case resolves to nil. The rule: the common dir must be absolute, must be a directory literally named `.git`, and its parent must be `file-equal-p` to the first porcelain entry, which must not be `bare`. Cases measured on git 2.49, all with specs except `--separate-git-dir`:
  - A bare `R.git` with worktrees. The parent of the common dir is the directory ABOVE `R.git`.
  - A bare repository stored as `D/.git` (the "bare-in-.git" worktree workflow). The parent is `D`, which has no working tree, yet `D` PASSES the classifier's cheap `.git`-is-a-directory test (spec asserts `classify D` → `canonical`). This is why the safety rule has to go through `--canonical-checkout` and must never trust `--classify` on its own.
  - `--separate-git-dir`. Both signals point at `G`.
  - A submodule. Both signals point at `S/.git/modules/m`.
- **Pressure point 4: submodules.** A submodule classifies as `other`, and `--canonical-checkout` returns nil. I think that is the right answer for this capability. The superproject chooses which commit a submodule checkout shows, and git cannot name a main worktree for the submodule's repository (see above). Offering the submodule as "the canonical checkout" of its own repository would show content picked by another repository. That is the "lottery" the safety rule forbids. One side effect: `--checkouts-of` called from inside a submodule returns nil, because git lists only the submodule's git dir. Nothing in this slice asks that question. A future caller that needs it should add the checkout's own `--show-toplevel`.
- **Pressure point 5: containment across path spellings.** I use `file-in-directory-p`, which applies `file-truename` to both sides and compares whole path components. Proven with real fixture paths:
  - a home registered under the RAW `/var/…` spelling (`make-temp-file` output, which `workspace--make` does not truename) against a truenamed `/private/var/…` checkout;
  - with and without a trailing slash;
  - a `~/…` spelling of the checkout, with HOME rebound to a real directory;
  - a `ws-old` sibling that shares the home's name prefix, which is NOT contained.

  A unit spec covers `~` and slash-less HOMES. Those can only come from a hand-built or stale plist, since `workspace--make` normalizes `:home`. Its directories are real because `file-equal-p` compares file attributes, so fully synthetic paths cannot be tested. One caveat: `file-in-directory-p` compares with `string-equal`, which is case-sensitive, so on case-insensitive APFS a home registered as `~/Work/x` does not contain `~/work/x/y`. That is documented in the `.org`, not fixed. Remote homes and remote checkouts are rejected before any filesystem call, so no TRAMP connection is opened.
- **Pressure point 6: cost.** Measured with a fixture of 4 workspaces × 3 checkouts, each a worktree of a different repository, 12 in view, on this machine. One git subprocess costs about 9.5 ms.

  | call | ms | subprocesses |
  |---|---|---|
  | `--identity` of one child | 19 | 2 |
  | `--workspace-checkout`, match on the 1st child | 19 | 2 |
  | `--workspace-checkout`, miss over 3 children | 58 | 6 |
  | `--canonical-for-identity`, match in the current workspace | 42 | 4 |
  | `--canonical-for-identity`, match in the last workspace | 250 | 26 |
  | `--canonical-for-identity`, miss over 12 in view | 238 | 24 |
  | `--classify` | 0.8 | 0 |

  On the jump path the `missing-repo` / `no-clone` outcomes pay a `--workspace-checkout` miss plus a `--canonical-for-identity` scan, about 300 ms in this layout. `off-workspace-restore` orders candidates by calling `--workspace-checkout` once per registered workspace, which cost another 250 ms (24 subprocesses) on this fixture.

  **The callers WOULD benefit from the DD-7 memo, but only half as much as they could.** A memo keyed on the repository root still runs `rev-parse --show-toplevel` on every child just to find that key, so it saves `remote get-url` only: 12 of the 24 subprocesses. Every child this module passes in is already a checkout ROOT (it contains `.git`). A memo that accepts a known root directly, or that is keyed on the queried directory, would remove all of them on a warm cache. I did not add a memo, per the brief.
- **Manual verification (read-only, from this worktree).** With `jf/emacs-dir` set to this worktree and no registry loaded, `--canonical-for-identity "github.com/moogah/emacs"` returns `/Users/jefffarr/emacs/` using 4 subprocesses. `--classify "~/.emacs.d"` returns `other` and `--classify "~/emacs"` returns `canonical`. `--checkouts-of` lists 9 checkouts. I scanned no other directories.
- **Requires and declarations.** In line with the file-ownership rule, `(require 'seq)` plus `defvar` / `declare-function` for `workspace--registry`, `jf/emacs-dir`, `workspace--registered-names`, `workspace--current-name` and `workspace--home` sit at the top of the new section's first block (`** Registry access`). The `* Requires` section is untouched. Whoever integrates may move `seq` up. The registry calls are `fboundp`-guarded, so the queries still return normally (no registry, so nothing is workspace-owned) when tabs.el has not been loaded.
- **Scope choices I made rather than was told.** Only the IMMEDIATE children of `:home` count; grandchildren and the home itself do not. If `--canonical-for-identity` refuses one in-view checkout's answer, it goes on to the next, because two in-view checkouts of one identity can be independent clones. It does not skip repeated checkouts of the SAME repository, so a refused repository gets asked again, 2 subprocesses per duplicate. I left that simple.
- **Spec hygiene.** Every spec swaps in a fresh `workspace--registry` and binds `jf/emacs-dir` to nil. The harness loads init.el and the live registry, and without the swap the developer's real workspaces and this configuration checkout would be in view. The spec file also defines `workspace--registered-names` only when it is unbound (guarded by `unless fboundp`), so the file runs standalone. tabs.el cannot be loaded alone. Under the harness the real definition is already bound and the shim is skipped. Standalone: 46/46 pass.
- **Latent fixture hazard, found by leaking a temp dir.** `workspace-bookmark-fixtures-teardown` deletes its temp dirs behind `(when (file-directory-p dir) …)`. Buttercup spies stay active through `after-each`, so a spec that spies on `file-directory-p` makes teardown silently skip every deletion. My remote-path spec did this. I moved it into its own `describe` with no fixture scope and confirmed zero leftovers with a private TMPDIR. A one-line hardening of the fixture (delete without the guard, inside `ignore-errors`) would make this impossible. Out of scope here.
- **Process note (possible interference with the parallel task).** While finding the leak I ran `rm -rf $TMPDIR/wsbm-*` once, at about 14:54 local time. If the parallel `stamp-seam` run had fixture directories live at that moment, they were deleted, and a spurious fixture failure in its log around that time would come from this. After that I attributed leftovers only with a private TMPDIR.

### Revision 1 (architect on-touch findings 1 and 4)

- **Finding 1 (blocking): containment now compares file identity, not strings.** `--path-within-p` truenames the checkout, then walks it and each parent directory up to the filesystem root, testing `file-equal-p` against the home. `file-equal-p` compares inode and device, so letter case, `~`, a trailing slash and symlinked prefixes cannot matter. This replaces `file-in-directory-p`, which truenames both sides but then compares path components with `string-equal`. On this case-insensitive Mac, a home registered with a different letter case therefore missed its own full clone. Ownership-first ordering then turned that miss into a `canonical` verdict, and the safety rule was broken. That supersedes the case-sensitivity caveat in the pressure-point-5 bullet above.
  - New spec: the home is registered as the `upcase` of the fixture's real, truenamed path. The spec asserts that both the in-view spelling and git's spelling of the clone classify `workspace-owned`, and that `--canonical-for-identity` returns nil. It is guarded by `assume`, which reports it as pending with a message if the temp filesystem is case-sensitive. It ran (not pending) here.
  - Mutation check: with the old `file-in-directory-p` body swapped back in, this spec fails.
  - Behaviour I kept: the path is truenamed, so a symlink inside a home that points to a clone elsewhere is still not owned by that home, exactly as before.
- **Finding 4 (advisory): `--canonical-checkout` has a third condition.** `git rev-parse --show-toplevel`, run in the candidate, must be `file-equal-p` to the candidate. Otherwise the answer is nil. This catches `core.worktree`, where both signals agree on `D` while the work tree lives elsewhere. I added a row to the `.org` table.
  - New spec: `core.worktree` is set on a fixture repository. The spec checks that both signals agree on `D` and that `D/.git` is a directory, then asserts nil from both the linked worktree and `D`. A linked worktree of such a repository ignores `core.worktree` (measured).
  - Mutation check: with `--show-toplevel` forced to agree, this spec fails.
  - Cost: one more subprocess, paid only after the two signals agree. A `--canonical-for-identity` match is now 5 subprocesses (it was 4).
- Findings 2 and 3 (they live in the shared runner) are not addressed here, as instructed. Spec count is 46 → 48. No temp leftovers, checked with a private TMPDIR.

## Discoveries

- discovery_id: disc-checkout-discovery-1
  class: invariant-gap
  description: |
    The vocabulary does not say what to do when a checkout is BOTH
    workspace-owned (it lies under a registered :home) AND structurally a main
    worktree (its .git is a directory). That happens whenever a workspace
    holds a full clone instead of a linked worktree. Testing structure first
    (the task body's listing order) classifies it canonical, and the safety
    rule then offers another workspace's files. That is the defect, coming
    back through the fallback path. I test ownership first. A behavioral spec
    pins this: a clone inside a workspace home returns nil from
    --canonical-for-identity, even though git itself names it the main
    worktree.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Add a precedence clause to the entry: "Classification tests ownership
    before structure; a main worktree under any registered :home is
    workspace-owned, never canonical." Add validator item 4: a full clone
    inside a registered home is not returned by --canonical-for-identity.

- discovery_id: disc-checkout-discovery-2
  class: spec-signal
  description: |
    This answers the entry's SPECULATED status_note: the canonical signals DO
    disagree in practice, in four layouts (measured, git 2.49):
      - bare R.git with worktrees: parent-of-common is the directory above R.git
      - bare repo stored as D/.git: D has no working tree, yet D passes the
        .git-is-a-directory test
      - --separate-git-dir: git lists the git dir as the "main worktree"
      - submodule: same as --separate-git-dir
    Implemented rule: the common dir must be absolute and a directory named
    ".git"; the first porcelain entry must be non-bare and file-equal-p to
    its parent; otherwise nil. The D/.git case shows that the cheap
    .git-is-a-directory test is NOT sufficient on its own to identify a
    canonical checkout. It is only a filter applied after git's answer.
  affected_register_entry: register/vocabulary/checkout-classification
  recommendation: |
    Reconcile the `canonical` member description: replace "the classifier
    prefers the direct question" with the agreement rule above, and state
    that disagreement yields nil. State explicitly that submodule and
    --separate-git-dir checkouts are never canonical, and that this is
    deliberate. Mark the status_note's question answered.

- discovery_id: disc-checkout-discovery-3
  class: interface-drift
  description: |
    The boundary's command table says `git rev-parse --git-common-dir`. The
    bare command answers RELATIVE to git's working directory (".git" at the
    root, "../.git" in a subdirectory). The implementation uses
    `--path-format=absolute`, which sets a git >= 2.31 floor for this
    boundary. The porcelain_parsing clause says only "one worktree <path>
    line per entry". The implementation also has to handle the bare,
    prunable, locked and detached flag lines, and a first entry that is a
    git directory rather than a working tree. `-z` (git >= 2.36) is
    deliberately NOT used, because Ubuntu 22.04's git 2.34 would lose the
    whole feature.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Amend the FOUR QUESTIONS table to
    `git rev-parse --path-format=absolute --git-common-dir`, and record the
    git >= 2.31 floor (older git degrades to no-clone and never signals).
    Extend porcelain_parsing: a usable checkout is non-bare, non-prunable,
    exists, and contains .git; locked and detached entries are kept.

- discovery_id: disc-checkout-discovery-4
  class: spec-signal
  description: |
    Cost on the jump path (4 workspaces x 3 checkouts, about 9.5 ms per git
    subprocess): a --canonical-for-identity miss over 12 in-view checkouts
    takes 24 subprocesses, about 240 ms. off-workspace-restore's ordering
    pass (--workspace-checkout for every registered workspace) costs about the
    same again. The DD-7 per-ROOT identity memo (stamp-seam) still pays
    `rev-parse --show-toplevel` per child just to compute its key, so it
    saves only half. Every child this module queries is already a checkout
    root.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    When stamp-seam's memo lands, let --identity skip --show-toplevel for a
    directory the caller already knows is a root. That can be a
    `workspace-bookmark--identity-of-root` entry point, or a memo keyed on
    the queried directory. Discovery's warm-cache cost then drops to about
    zero subprocesses per child. Architect call; this task added no memo.

- discovery_id: disc-checkout-discovery-5
  class: deviation
  description: |
    --checkouts-of returns only USABLE checkouts (see disc-3). It is not
    reflexive: called from inside a submodule, or from a --separate-git-dir
    checkout, it can return nil or leave out the checkout it was asked about,
    because git lists the git directory in that slot. No caller in this slice
    depends on reflexivity. worktree-candidates-git-first, which expands
    in-view checkouts to their siblings, is the likely first consumer that
    would notice.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Record the non-reflexivity in the entry. If worktree-candidates-git-first
    needs "this checkout plus its siblings", it should union in the checkout's
    own --show-toplevel rather than change --checkouts-of.
