---
name: jump-seam
description: Substitute a resolved copy of the record at bookmark-handle-bookmark, never mutating the stored record
change: workspace-portable-bookmarks
status: ready
task_class: feature
on_critical_path: true
cycle: cycle-1790453714
cites_register_entries:
  - register/boundary/bookmark-advice-seams
  - register/invariant/stored-record-never-mutated-by-jump
  - register/vocabulary/bookmark-resolution-outcome
relations:
  - blocked-by:stamp-seam
  - blocked-by:resolver-outcomes
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-jump-spec.el` (new)

## Implementation steps

> Cycle 1 (cycle-1789935943): step 2 corrected — the resolver returns SIX outcomes
> (DD-14 added `ambiguous`) in a six-key plist; see `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`.

1. Install advice on `bookmark-handle-bookmark` (`bookmark.el:1339`). It is the
   choke point called by `bookmark--jump-via`, so it covers `bookmark-jump`,
   `bookmark-jump-other-window` and completion front-ends such as
   `consult-bookmark`. Do NOT advise `bookmark-jump` — that misses the others.

2. The advice receives `BOOKMARK-NAME-OR-RECORD`, which may be a string name or a
   record. Normalize with `bookmark-get-bookmark` so both forms are handled, then:
   - call `(workspace-bookmark--resolve record (workspace--current-name))`; it returns
     `(:outcome :path :identity :relpath :candidates :checkout)` and never signals
     (only `quit` propagates) and it ALWAYS returns that six-key plist — it is
     never nil (portable-bookmarks.el:992-1042 at b56f385a; corrected at cycle-2
     plan, finding arch-cycle-1790453714-plan-01). The off-workspace hand-off is
     therefore decided BEFORE calling the resolver: when
     `(workspace--current-name)` is nil AND the record is stamped
     (`workspace-bookmark--stamped-coordinates` non-nil), hand off to the
     off-workspace flow (task `off-workspace-restore`) — until that task lands,
     pass through unchanged in that case. Unstamped records off-workspace are
     ordinary `non-portable` pass-throughs.
   - `non-portable` → call through with the original argument untouched.
   - `resolved` → build a **copy** of the record with `copy-tree`, substitute the
     resolved absolute path into its `filename` property with
     `bookmark-prop-set`, and call through with the copy.
   - `missing-repo` → hand off to the prompt (task `missing-repo-choice`); until
     that lands, report and open nothing.
   - `ambiguous` → report the `:candidates` (2+ checkouts of the identity in this
     workspace, or 2+ canonical clones) and open nothing; nothing is chosen for the user.
   - `not-on-branch` / `no-clone` → report via `message` (or `user-error` if the
     jump must not continue) and do NOT call through. The key rule: never let
     Emacs visit a nonexistent path as a new empty buffer.

3. **Never mutate the stored record.** Use `copy-tree` on the record before
   substituting, so the copy shares no structure with the stored alist entry.
   `bookmark-save-flag` is `1`, so an in-place edit would be written to disk and
   the absolute fallback — the thing that makes every degraded outcome safe —
   would be destroyed on first use.

4. Preserve the native handler: the copy keeps its own `handler` property, so
   dired, org, pdf-view and third-party handlers continue to do their own
   restoration. Do not set a handler of our own.

5. Wrap the advice body in `condition-case`: on any internal error, call through
   with the original argument. A broken resolution must degrade to today's
   behaviour, never break the jump. One bad record must not affect other
   bookmarks or the bookmark file.

6. Write `portable-bookmarks-jump-spec.el`:
   - **Behavioral** (real git, two worktrees, fake registry, temp bookmark file):
     stamp a bookmark from worktree A, make worktree B the current workspace,
     jump, and assert the opened buffer's file is B's copy. Then assert the
     stored record's `filename` is **still** A's path — the regression guard for
     the copy rule.
   - **Behavioral**: jumping an unstamped record opens its stored absolute path
     unchanged.
   - **Behavioral**: a dired (directory) record resolves into the current
     workspace's checkout and is opened by the dired handler (assert the buffer's
     major mode).
   - **Behavioral**: `bookmark-jump-other-window` goes through the same
     resolution (proves the choke point is right).
   - **Behavioral**: `not-on-branch` creates no buffer — assert no buffer visits
     the missing path afterwards.
   - **Unit**: force an error inside the resolver with `cl-letf`; the jump still
     opens the stored absolute location.

7. Tangle and validate.

## Design rationale

Three assumptions were verified in `emacs -Q --batch` before this design, and this
task is where the third one pays off: a modified *copy* handed to the jump
machinery opened a different file at the recorded position while the stored
record's `filename` stayed unchanged. That is the whole mechanism.

A custom bookmark handler was rejected: it would *replace* dired's, org's or
pdf-view's handler and we would have to reimplement their restoration behaviour.
Substituting one property inside the record and letting the native handler run is
strictly less invasive — every handler still receives the bag it expects.

Refusing to visit a nonexistent path is a deliberate departure from Emacs's
default helpfulness. Normally visiting a missing file is how you create one; here
it would mean silently starting a new empty buffer for a path that exists on a
different branch, which is precisely the class of quiet wrongness this change
exists to remove.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — jump spec passes.
- `grep -n "bookmark-handle-bookmark\|copy-tree"
  config/workspaces/portable-bookmarks.el` shows the advice and the copy.
- `grep -n "advice-add 'bookmark-jump" config/workspaces/portable-bookmarks.el`
  returns nothing (wrong choke point).
- Manual: jump to a bookmark made in another workspace's worktree; the file opens
  from the current workspace, and re-inspecting the stored record shows the
  original absolute path.

## Context
- specs/portable-bookmarks/spec.md § 'Jump-time resolution on a copy'
- specs/portable-bookmarks/spec.md § 'Degradation and isolation'
- design.md § D1, § D2

## Cycle 1 updates (cycle-1789935943)

### Cited register entries
- `register/boundary/bookmark-advice-seams`: speculated → reconciled text, entry stays `speculated`. Seam 1 (stamp) is confirmed live; seam 2 is verified pass-through at 20944160 (`.el:398-405`) and is THIS task's to wire. See `.orchestrator/cycles/cycle-1789935943/reconciliations/boundary-bookmark-advice-seams.md`.
- `register/invariant/stored-record-never-mutated-by-jump`: unexercised this cycle (status unchanged); this task owns its enforcement (`portable-bookmarks-jump-spec.el`, the `copy-tree` validator). Audit finding eoc-03 asks that jump-seam be the FIRST task of its cycle-2 wave and that the on-touch for this invariant fire on it.
- `register/vocabulary/bookmark-resolution-outcome`: speculated → reconciled. Six members; `non-portable` is the only outcome that uses the stored `filename`; `resolved` is the only one that opens without asking. See `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`.

### User-resolved decisions
- DD-14 (ask 13): `ambiguous` with `:candidates` — report, open nothing. **Implication**: one more non-visiting arm in step 2; add a behavioural spec (two worktrees of one repo inside the current workspace's home → no buffer).
- DD-16 (ask 15): confinement is lexical; a relpath spelled inside the checkout resolves there whatever its links point at. **Implication**: none for the seam; substitute `:path` verbatim. Do not add a symlink check here.
- Ask 5 (stale stamps after `bookmark-relocate`): documented limitation, not checked. **Implication**: the seam does not compare `filename` against the stamp; `.tasks/portable-bookmarks-stale-stamp-detection.md` owns that.

### Open asks
- ask 16 (`missing-repo` may carry nil `:path`): does not touch this task's arms; the hand-off passes the whole plist to `missing-repo-choice`.

### Already-shipped inline fixes / related work
- The resolver's own `condition-case` maps discovery errors to `no-clone`; the seam's step-5 barrier is the SECOND barrier (for the seam's own code), not the first.
- Follow-up `discovery-identity-memo-and-cost-contract` (audit eoc-06) will change spawn counts on the resolve path; the jump spec should not pin spawn counts.

## Cycle 2 plan (cycle-1790453714)

- Step 2 amended (finding arch-cycle-1790453714-plan-01, blocking interface-drift against `register/vocabulary/bookmark-resolution-outcome`): the resolver never returns nil; the off-workspace trigger is a nil `workspace--current-name` on a STAMPED record, decided before `--resolve`.
- Wave 1 merge order: `consolidate-portable-bookmarks-test-fixtures` merges FIRST. Write `portable-bookmarks-jump-spec.el` against its registry-scope macro `workspace-bookmark-fixtures-with-empty-registry` and its `workspace-bookmark-fixtures-commit-file` helper (rebase onto the integration branch once it has merged). Do NOT copy the per-file `workspace--registry` swap idiom: `register/invariant/spec-registry-isolation-via-fixtures` (new, speculated) forbids it and on-touch fires on it.
- On-touch fires on this task for `register/boundary/bookmark-advice-seams`, `register/invariant/stored-record-never-mutated-by-jump` and `register/invariant/no-silent-wrong-file-open`; each entry's cycle-2 forward-mode paragraph names what the seam must settle (arms per outcome, copy-tree on the `(NAME . ALIST)` cons, second-barrier `condition-case` with `quit` propagating, `bookmark-jump-other-window` through the real jump path, "filename unchanged" asserted after a `bookmark-save` round trip).
- Never bulk-delete shared temp prefixes (no `rm -rf $TMPDIR/wsbm-*`); delete only the directories this task created.
