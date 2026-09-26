---
name: resolver-outcomes
description: Implement the closed set of resolution outcomes as a pure decision function over coordinates plus workspace context
change: workspace-portable-bookmarks
status: ready
task_class: feature
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/invariant/no-silent-wrong-file-open
relations:
  - blocked-by:checkout-discovery
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-resolver-spec.el` (new)

## Implementation steps

1. Declare the closed set as a defconst, mirroring the existing
   `workspace--valid-reincarnation-steps` pattern in `layouts.el`, plus a
   predicate:
   ```elisp
   (defconst workspace-bookmark--valid-outcomes
     '(resolved not-on-branch missing-repo no-clone non-portable))
   ```
   Adding a sixth outcome must then be a deliberate two-place change (the
   defconst and the resolver), not an accident.

2. Add `workspace-bookmark--resolve (record workspace-name)` returning a plist
   `(:outcome SYMBOL :path PATH-OR-NIL :identity ID :relpath REL)`. Evaluation
   order, first match wins:
   - no coordinates on the record → `non-portable`, `:path` = the record's stored
     absolute filename.
   - WORKSPACE-NAME holds a checkout of the identity
     (`--workspace-checkout`), and the relpath exists inside it → `resolved`
     with that absolute path.
   - WORKSPACE-NAME holds a checkout but the relpath does not exist there →
     `not-on-branch`, `:path` nil.
   - no checkout in that workspace, but a canonical checkout is in view
     (`--canonical-for-identity`) → `missing-repo`, `:path` = the canonical
     checkout's copy of the relpath (the *candidate* for a read-only view; the
     caller decides whether to use it).
   - nothing in view → `no-clone`, `:path` nil.

3. The resolver must be a decision function only: it performs git queries through
   the discovery helpers but MUST NOT open buffers, prompt, or message. That
   keeps it unit-testable and keeps the interactive policy in the seam and prompt
   tasks.

4. Any error raised by a discovery helper must be caught and mapped to
   `no-clone` (when an identity is known) or `non-portable` (when it is not),
   never propagated. A jump must not break because git misbehaved.

5. Write `portable-bookmarks-resolver-spec.el`, one `describe` per outcome:
   - **Behavioral** (real git + fixtures): build a repo, add two worktrees, put
     one inside a registered fake workspace's `:home`; assert `resolved` for a
     file present there; delete that file and assert `not-on-branch` with
     `:path` nil; register a second workspace with no checkout and assert
     `missing-repo` with a `:path` under the canonical checkout.
   - **Unit** (mock discovery helpers with `cl-letf`, scoped to the function
     under test): `no-clone` when nothing is in view; `non-portable` for a record
     with no coordinates; a discovery helper that signals produces `no-clone`
     rather than propagating; a malformed relpath produces a degraded outcome.
   - **Unit**: assert every returned `:outcome` satisfies the closed-set
     predicate — the guard against a stray symbol.

6. Tangle and validate.

## Design rationale

The outcome set exists so that no code path can silently open a file the user did
not ask for. `not-on-branch` is the subtle one: Emacs would otherwise visit a
nonexistent path as a fresh empty buffer, which is its own quiet wrong-file trap
— you would start typing into a file that does not exist on this branch.

`missing-repo` returns a candidate path but does not act on it: the read-only
view is a choice the user makes, never a silent fallback to the main checkout.
Editing `main` unnoticed is exactly what worktrees exist to prevent, so the
canonical checkout is never opened without an explicit decision.

Keeping the resolver free of I/O and prompts is what makes the awkward cases
testable. Forcing `no-clone` or a failing git invocation against real repositories
is fiddly; against a mocked helper it is one `cl-letf`. That is why the test
strategy is split: real git for the behavioural paths, mocks for the edges.

Errors map to degraded outcomes because this code runs inside a core Emacs
interaction. A peripheral feature must not be able to break bookmark jumps.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — resolver spec passes, every
  outcome in the closed set covered by at least one test.
- `grep -n "find-file\|switch-to-buffer\|completing-read\|message"
  config/workspaces/portable-bookmarks.el` shows no such call inside the
  resolver function body (policy belongs to the seam/prompt tasks).

## Context
- specs/portable-bookmarks/spec.md § 'Closed set of resolution outcomes'
- design.md § D7 'Outcomes are a closed set, evaluated in order'
- design.md § D11 'Testing approach'

## Amendments (2026-09-26, after waves 3–4; these SUPERSEDE the steps above where they conflict)

A. **Six outcomes, not five (user decision, ask 13).** Add `ambiguous` to the
   closed set: `(resolved not-on-branch missing-repo no-clone non-portable
   ambiguous)`. It means that discovery found MORE THAN ONE candidate and will
   not choose. `:path` is nil, and a new `:candidates` key lists them. The
   meaning matches `not-on-branch`: it MUST be reported and NOTHING is
   opened. The plist becomes `(:outcome :path :identity :relpath :candidates)`,
   with `:candidates` nil except for `ambiguous`.
B. **Use the PLURAL discovery functions, so ambiguity is never lost.** They are
   `workspace-bookmark--workspace-checkouts (identity workspace-name)` and
   `workspace-bookmark--canonical-checkouts-for-identity (identity)`. Order, first match wins:
   1. no coordinates, fewer than all three stamped properties, or a format
      version this module does not know → `non-portable`, with `:path` = the
      stored absolute filename. (The stamp is ALL THREE OR NONE: repo, relpath,
      format. Format version 1 is the only one known.)
   2. the workspace holds exactly ONE checkout of the identity: relpath exists → `resolved`;
      relpath missing → `not-on-branch`.
   3. the workspace holds TWO OR MORE → `ambiguous` (`:candidates` = those checkouts).
   4. the workspace holds none, and exactly ONE canonical checkout is in view →
      `missing-repo`, with `:path` = the candidate under it.
   5. the workspace holds none, and TWO OR MORE canonical checkouts → `ambiguous`.
   6. nothing in view → `no-clone`.
C. **Relpath forms.** The repository root is stored as `"./"`
   (`workspace-bookmark--root-relpath`). Directory relpaths come from
   `git rev-parse --show-prefix`, with a trailing slash. A relpath may begin or
   end with whitespace and is stored verbatim. Re-anchor each form correctly,
   and never let a relpath escape its checkout (for example `..`, or an absolute
   path in a hand-edited record) → degrade to `non-portable`.
D. **Stale stamps are a documented limitation, NOT checked here (user decision,
   ask 5).** `bookmark-relocate` changes `filename` without re-stamping. Do not
   add a consistency check. Say so in the org prose, and point to
   `.tasks/portable-bookmarks-stale-stamp-detection.md`.
E. **The ONLY outcome that uses the stored absolute filename is `non-portable`**
   (`register/invariant/no-silent-wrong-file-open`). `missing-repo`'s `:path` is a
   candidate for the caller to offer, never something the resolver acts on.
F. **Placement.** New `* Resolution` section in portable-bookmarks.org,
   between `* Checkout discovery` and `* Advice installation`. Seam 2 stays
   pass-through; wiring it is `jump-seam`, deferred to cycle 2.

## Fix scope (2026-09-26, on-touch architect blocking finding + user decision DD-16)

These SUPERSEDE Amendment C's confinement rule where they conflict. Findings:
`.orchestrator/cycles/cycle-1789935943/findings/on-touch-resolver-outcomes.md`.

1. **BLOCKING: a valid stamp must never degrade to `non-portable`.** A symlink on
   disk that leads out of the checkout (a tracked link `link-rel ->
   ../shared/x.txt`, or `sub/f.txt` where `sub` is a link on the checked-out
   branch) currently sends a VALID stamp to `non-portable`, which returns the
   stored absolute path: another workspace's file. The cause is the `_` catch-all
   arm in `--resolve-anchored`.
2. **User decision DD-16: resolve by spelling.** Confinement is LEXICAL only:
   refuse `..` components, absolute paths, NUL and non-string values (a TAMPERED
   stamp, which stays `non-portable`), and nothing else. A relpath that is
   lexically inside the checkout resolves to `<checkout>/<relpath>` whatever its
   links point at, exactly as visiting that path by hand would. Remove the
   file-identity escape check.
3. The catch-all arm in `--resolve-anchored` must SIGNAL (a programming error the
   outer barrier maps to `no-clone`), never return a location.
4. **Advisory: add a `:checkout` key** to the plist, naming the chosen checkout
   for `resolved` / `not-on-branch` and the canonical checkout for `missing-repo`,
   so the deferred `missing-repo-choice` task can offer add-worktree without
   re-running discovery. It is nil otherwise. The closed-set guard and plist-shape
   spec get updated to match.
5. **Specs.** Turn the two architect reproductions into specs that assert
   `resolved` via the in-checkout spelling, never the stored path, from a
   workspace other than the one that stamped the bookmark. Update the existing
   symlink specs (resolver-spec ~438, ~452). Revert-prove each. Fix the prose and
   docstrings.

## Observations

- **How each new spec was shown to fail without its change.** Deleting
  the new `* Resolution` section fails all 42 specs with `void-function`,
  which proves nothing specific. So each step was reverted or broken on
  its own instead. A script took the tangled `portable-bookmarks.el`,
  applied ONE textual mutation to a copy in the scratchpad (a string
  replacement that must match exactly once, or an appended redefinition),
  loaded the spec file, loaded the mutant over it, and ran buttercup. The
  worktree `.el` was never touched. There were 31 mutants. Every one was
  killed, and every one of the 42 specs failed under at least one mutant.
  Per outcome:
  - `non-portable`: the format-version check dropped kills 2 specs
    ("unknown format version", "two of three properties", the second
    through the format-less record). A dropped empty-identity check kills
    "malformed identity or relpath". The `..`, absolute and NUL checks
    dropped kill, in turn, the two `..` specs, the absolute spec (through
    its "no discovery ran" assertion) and the malformed spec. The symlink
    containment check dropped kills both symlink-escape specs. Looking up
    a string name in `bookmark-alist` kills "not a record". Dropping the
    stored `:path` kills 6 specs.
  - `resolved`: disabling the single-checkout branch kills 17. Leaking the
    stored filename into `:path` kills 8, including "used by no outcome
    but non-portable". Calling `find-file-noselect` in the present branch
    kills 12 through the interaction trap every resolve call runs under.
    Asking canonical discovery FIRST kills 17, including "wins over a
    canonical checkout in view (first match wins)".
  - `not-on-branch`: dropping the existence check kills 6, the kind check
    kills "file relpath that names a directory", and treating an absent
    path as resolved kills 7.
  - `missing-repo`: never asking canonical discovery kills 7. Mapping an
    absent canonical path to `not-on-branch` kills "nil :path when the
    canonical checkout lacks the relpath".
  - `ambiguous`: taking the first workspace checkout, taking the first
    canonical checkout, or using either SINGULAR discovery function each
    kills 2 specs, one behavioral and one unit. Removing `ambiguous` from
    the defconst kills 5, including "declares exactly six outcomes".
  - `no-clone`: making the final branch `non-portable` kills 3, mapping a
    known-identity error to `non-portable` kills the 3 "signals, instead
    of propagating" specs, and removing both error barriers kills 4.
  - Relpath forms: `string-trim` on the relpath kills both whitespace
    specs. Anchoring by `concat` kills the `"./"` root spec.
  - Closed set: dropping the constructor guard AND returning a stray
    `nothing-found` kills "is a member of the closed set". A `(message ..)`
    call in the non-portable path kills 12.
- **The "directory relpath that names a file" spec is also pinned by the
  OS.** On macOS `stat("file/")` fails, so `file-exists-p` already says
  nil. That is why dropping the kind check kills only the reverse case.
  The explicit kind check is still needed for "file relpath that names a
  directory".
- **Departures and choices the task body left open:**
  - The resolver takes a RECORD (a cons) only. A string is
    `non-portable` and is NOT looked up in `bookmark-alist`, because a
    lookup would be a decision over a different value than the one
    passed. The seam already normalizes with `bookmark-get-bookmark`.
  - For `non-portable`, `:identity` and `:relpath` are nil even when the
    record carried some (a malformed stamp is nothing a caller should
    use).
  - Any component that is exactly `..` is refused, including one that
    stays inside (`sub/../README.md`). git never writes one. A leading
    `~` that `file-name-absolute-p` rejects (`~foo`) is kept as an
    ordinary name, so it anchors exactly as the stamp's own verification
    anchored it.
  - The trailing slash is treated as the record's KIND. A kind mismatch
    on this branch gives `not-on-branch`.
  - A symlink escape is detected by file identity
    (`workspace-bookmark--path-within-p`, which is already the module's
    containment walk). A dangling link is `not-on-branch` because nothing
    exists to open. A tracked file that is itself a link leaving the
    checkout also escapes, and so resolves `non-portable`.
  - `missing-repo` whose relpath is absent under the canonical checkout
    keeps the outcome but sets `:path` to nil, so a caller can never
    offer a read-only view of a path that does not exist (see Discoveries).
- **The task's verification grep** (`find-file|switch-to-buffer|
  completing-read|message` over the whole `.el`) finds one line, and it is
  in the pre-existing stamp-seam docstring ("never messages"), not in the
  resolver. The resolver section has zero hits. Its docstrings avoid the
  words on purpose, so the grep stays usable.
- **Spec-file order dependence.** The last `describe` asserts over every
  result recorded earlier in the file (closed-set membership and
  coverage of all six members). Run focused on its own, it fails its
  "results non-nil" precondition. This is intended, but a reviewer should
  know.
- **The Overview "Scaffold status" paragraph was updated** to say the
  resolver exists and the jump seam still does not call it. The discovery
  section was not edited, and Seam 2 is still pass-through.

- **Fix pass (2026-09-26, on-touch finding 1 + DD-16).** Four code
  changes, all in `* Resolution`: (1) `--anchor` no longer follows links
  — the `--path-within-p` file-identity test and its `escapes` answer are
  gone, and it answers `present` or `absent` by spelling; (2) the `_` arm
  of `--resolve-anchored` signals (`Unknown anchoring answer`) and never
  returns a location, so the barrier in `--resolve` maps it to
  `no-clone`; `record` was dropped from `--resolve-anchored` and
  `--resolve-coordinates`; (3) `--outcome` builds a SIX-key plist with
  `:checkout`, filled by `--resolve-anchored` for `resolved`,
  `not-on-branch` and `missing-repo` (present or absent path), nil
  elsewhere; (4) prose and docstrings no longer say a link escape
  "resolves non-portable to the stored path". A valid stamp can no
  longer reach `non-portable` by any route.
- **How each changed or new spec was shown to fail with its change
  reverted (fix pass).** Same method as before, simplified: the spec
  file loads the tangled module, then a small mutant `.el` of
  REDEFINITIONS is loaded over it, then buttercup runs. The worktree
  `.el` was never edited. Ten mutants, every one killed:
  M1 = pre-fix `--anchor` (identity test, `escapes`) AND a catch-all
  returning `non-portable`; M2 = the identity test alone (`escapes` now
  signals); M3 = catch-all returns a `resolved` location; M3b =
  catch-all returns `non-portable`; M4 = `--outcome` drops `:checkout`;
  M5 = `:checkout` exists but is never set; M6 = `:checkout` set only
  for a present path; M7 = `--anchor` without its outside-the-checkout
  assertion; M7b = the assertion rejects the checkout itself; M9 = the
  inner barrier catches only `quit`. Per spec:
  - the two architect reproductions ("resolves a tracked link that
    leaves the checkout to the second workspace's spelling", "resolves a
    file whose parent is a link on the second workspace's branch") and
    "spells a valid stamp under whichever checkout the asking workspace
    holds": killed by M1, M2, M4 and M5.
  - the rewritten symlink specs ("resolves a symbolic link out of the
    workspace's checkout by its spelling", "offers a symbolic link out
    of the canonical checkout by its spelling"): killed by M1, M2, M4,
    M5. The `..`, absolute and NUL specs are unchanged and still pass.
  - "answers for an anchoring answer nobody wrote code for, never a
    location" (new; feeds `(escapes)`, a stray `present-elsewhere` cons
    and nil into the dispatch): killed by M3, M3b and M9.
  - "answers when re-anchoring signals" (its `cl-letf` repointed from
    `--path-within-p` to `--anchor`): killed by M9.
  - the `:checkout` describe (three specs): killed by M4, M5 and M6.
  - closed set: "is a member of the closed set, in a six-key plist"
    killed by M4; "names a checkout exactly for resolved, not-on-branch
    and missing-repo" (new) killed by M4, M5, M6. The three full-plist
    `:to-equal` assertions (no-clone, non-portable, ambiguous) killed by
    M4.
  - the anchoring helper (new, unit): "signals for a relpath that
    anchors outside the checkout" killed by M7; "anchors a hand-edited
    `.` to the checkout itself" killed by M7b.
- **A choice the fix scope left open: `--anchor`'s lexical prefix
  test.** It also produced `escapes`, and with the file-identity test
  gone `escapes` had no home. Under DD-16 confinement lives in
  `--confined-relpath-p` only, so `--anchor` now ASSERTS (signals)
  instead of answering, and the barrier maps that to `no-clone` — an
  outcome, never a location. A confined relpath cannot trip it except
  the hand-edit `.`, which `expand-file-name` anchors to `<checkout>`
  without the trailing slash; the assertion accepts the checkout
  itself, and `.` then resolves `not-on-branch` by the kind check,
  consistent with `sub/.` (the architect's informational note — it was
  `non-portable` before). Git never writes either spelling.
- **The barrier hides a signal from the anchoring dispatch** exactly
  as it hides the closed-set guard's: a future stray anchoring answer
  surfaces as `no-clone`, not as a visible error. The new unit spec
  pins that it is `no-clone` and not a location, which is the
  invariant's concern; whether such errors should be logged is
  unchanged from the first pass.
- **Fixture note for the second-workspace describe.** The canonical
  clone lives in its own temp directory with no `shared/` sibling, so
  `link-rel` DANGLES under it and answers `missing-repo` with `:path`
  nil — correct (a dangling link is absent), but not the point of that
  describe, so its loop spec covers the two workspaces only. The
  canonical-side link case is "offers a symbolic link out of the
  canonical checkout by its spelling".
- **The task's verification grep** is unchanged after the fix: one hit,
  the stamp-seam docstring; zero in the resolver section.

## Discoveries

- discovery_id: disc-resolver-outcomes-1
  class: vocabulary-mismatch
  description: |
    The cited vocabulary lists FIVE ordered members. The user added a
    sixth, `ambiguous` (ask 13), and the implemented closed set is
    (resolved not-on-branch missing-repo no-clone non-portable ambiguous).
    `ambiguous` fires at TWO points in the order: after the workspace
    check (2+ workspace checkouts) and after the canonical check (2+
    canonical checkouts). The result carries `:candidates`. The
    `non-portable` member now also covers an unknown format version, a
    relpath that escapes by `..`, an absolute spelling or a symlink, and
    any value that is not a record. The entry's validator item 1 says
    "exactly the five members".
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    Reconcile: six members. The order is non-portable(1), resolved or
    not-on-branch(2), ambiguous-in-workspace(3), missing-repo(4),
    ambiguous-canonical(5), no-clone(6). The plist shape is (:outcome
    :path :identity :relpath :candidates), with :candidates nil except
    for ambiguous and :identity/:relpath nil for non-portable. Update
    validator 1 to "exactly the six members", and add "ambiguous opens
    nothing" to the ambiguous member's description.
- discovery_id: disc-resolver-outcomes-2
  class: spec-signal
  description: |
    `missing-repo`'s :path can be nil: the repository is missing from the
    workspace, but the canonical checkout does not hold the relpath (for
    example, a file that exists only on the workspace branch where it was
    bookmarked). The plist also does not name the canonical checkout
    itself, since :candidates is reserved for ambiguous. The
    missing-repo-choice consumer's `add-worktree` action needs that
    checkout, and `view-canonical-read-only` must be withheld when :path
    is nil.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    missing-repo-choice: offer view-canonical-read-only only when :path
    is non-nil. Get the repository for add-worktree from
    `workspace-bookmark--canonical-for-identity` (:identity), or decide at
    integrate whether missing-repo should carry the canonical checkout
    (for example in :candidates as a one-element list). The latter
    changes the plist contract.
- discovery_id: disc-resolver-outcomes-3
  class: shape-fragmentation
  description: |
    The cited shape entry describes TWO flat properties ("BOTH OR
    NEITHER") named by defvars. The code has THREE defconst properties
    (repo, relpath, format), ALL OR NONE, and format version 1 is the only
    one read. The relpath forms are "./" for the root, a trailing slash
    for directories, and whitespace kept verbatim. The resolver adds a
    read-side rule the entry lacks: a relpath must stay inside the
    checkout both lexically (no absolute path, no `..` component, no NUL)
    and on disk (file identity through symlinks). Otherwise the record is
    non-portable.
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Reconcile the entry to three properties
    (workspace-bookmark-repo-property / -relpath-property /
    -format-property, all defconst), ALL OR NONE, and the known-version
    rule. Add the relpath forms and the confinement rule. Validator 4
    becomes "any record missing one of the three resolves non-portable".
- discovery_id: disc-resolver-outcomes-4
  class: invariant-gap
  description: |
    Two accepted routes by which the resolver's answer can still be a
    file the user did not mean, both inside the invariant's letter:
    (a) the stale-stamp limitation (ask 5). After `bookmark-relocate`
    or a hand edit, `resolved` / `missing-repo` follow the OLD stamped
    relpath. This is documented in the org prose and deferred to
    .tasks/portable-bookmarks-stale-stamp-detection.md, with no check
    added. (b) A record whose relpath escapes degrades to `non-portable`,
    which by definition returns the stored absolute filename. That
    filename may be another workspace's checkout, which is today's
    behaviour and the one outcome the invariant allows to use it.
  affected_register_entry: register/invariant/no-silent-wrong-file-open
  recommendation: |
    Record (a) and (b) as known residuals of prohibition 4 in the
    invariant entry. When stale-stamp detection is taken up, the same
    read-time check could route an escaping or inconsistent stamp to a
    reported outcome instead of to the stored path.
- discovery_id: disc-resolver-outcomes-5
  class: invariant-gap
  description: |
    (Fix pass, 2026-09-26; supersedes residual (b) of
    disc-resolver-outcomes-4 and the on-disk clause of
    disc-resolver-outcomes-3.) The confinement clause is now LEXICAL
    ONLY (user decision DD-16). `workspace-bookmark--confined-relpath-p`
    refuses an absolute name, any `..` component, NUL and a non-string;
    that is the whole test. A relpath that passes resolves to
    `<checkout>/<relpath>` by spelling, whatever its links point at —
    the same thing visiting that spelling by hand opens. Consequences
    for the invariant: (1) a VALID stamp never resolves `non-portable`
    (only absent / partial / unknown-version / lexically-escaping
    stamps do), which closes the on-touch finding 1 route to the stored
    filename; (2) `:path` for `resolved` and `missing-repo` is now
    guaranteed to be SPELLED inside `:checkout`, not to be inside it by
    file identity; (3) the anchoring dispatch has no catch-all that
    returns a location — its `_` arm signals and the barrier maps the
    signal to `no-clone`. The remaining residual is only the tampered
    stamp (lexical escape → stored filename, as stock Emacs would).
  affected_register_entry: register/invariant/no-silent-wrong-file-open
  recommendation: |
    Reword the confinement clause: "the :path of resolved / missing-repo
    is spelled inside :checkout (lexical confinement, DD-16); where a
    link on that branch points is the branch's business". Prohibition
    4: "non-portable is reached only by a stamp that is absent, partial,
    of an unknown version, or lexically escaping; a valid stamp never
    degrades to it". Validator 1 gains the concrete site
    (`--resolve-anchored`'s default arm signals) and validator 5 the
    second-workspace spec (a valid stamp resolved in another workspace
    through an escaping link asserts :path = that workspace's spelling,
    never the stored filename). Known residuals: (a) stale stamps
    (unchanged), (b) a TAMPERED stamp opens the stored filename.
- discovery_id: disc-resolver-outcomes-6
  class: shape-fragmentation
  description: |
    (Fix pass, 2026-09-26; resolves the open question in
    disc-resolver-outcomes-2.) The result plist gained a sixth key,
    `:checkout`: the workspace's checkout for `resolved` and
    `not-on-branch`, the canonical checkout for `missing-repo` (whether
    or not :path is nil), nil for `ambiguous`, `no-clone` and
    `non-portable`. `:candidates` is NOT overloaded and stays nil
    outside `ambiguous`. The shape is
    (:outcome :path :identity :relpath :candidates :checkout), built
    only by `workspace-bookmark--outcome`.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    result_shape: the six keys above. missing-repo member: ":checkout
    names the canonical checkout; :path is the candidate under it or
    nil". Nested action set: add-worktree targets :checkout and never
    re-runs discovery; view-canonical-read-only is offered only when
    :path is non-nil. non-portable member: "absent, partial,
    unknown-version or lexically-escaping stamp; a link escape is NOT
    non-portable". Validator 1: "exactly the six members".
