---
name: workspaces-file-equal-p-timestamp-race
description: "workspaces core (tabs.el, git-worktrees.el) uses file-equal-p for path identity; file-equal-p compares full file-attributes (timestamps included), so a directory modified between the two reads compares unequal. Replace with a device+inode comparison."
source: wave3-review-fixes (cycle-1789935943, workspace-portable-bookmarks)
status: ready
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
