---
name: workspaces-file-equal-p-timestamp-race
description: "workspaces core (tabs.el, git-worktrees.el) uses file-equal-p for path identity; file-equal-p compares full file-attributes (timestamps included), so a directory modified between the two reads compares unequal. Replace with a device+inode comparison."
source: wave3-review-fixes (cycle-1789935943, workspace-portable-bookmarks)
status: ready
task_class: bug
discovered_from: checkout-discovery
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
relations:
  - discovered-from:checkout-discovery
  - related-to:wave3-review-fixes
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

## Suggested fix

Give workspaces core its own device+inode same-file helper. Core must not call the
portable-bookmarks module (`register/boundary/portable-bookmarks-consumer`: core
never names a `workspace-bookmark-*` symbol). Consider whether portable-bookmarks
should then use the core helper instead of its own copy; the overlay treats
duplication as blocking.
