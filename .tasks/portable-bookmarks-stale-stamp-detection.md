---
name: portable-bookmarks-stale-stamp-detection
description: "Detect broken/stale portable-bookmark stamps on read (e.g. after bookmark-relocate or a hand edit) and manage them, rather than trusting them blindly."
source: workspace-portable-bookmarks cycle-1789935943 (ask-cycle-1789935943-5)
status: backlog
task_class: feature
discovered_from: stamp-seam
discovered_by: [architect, reviewer]
discovered_class: invariant-gap
reconciled_into: null
relations:
  - discovered-from:stamp-seam
---

## Problem

`bookmark-relocate` (and any hand edit of the bookmark file) rewrites a record's
`filename` but leaves the stamped coordinates (`workspace-bookmark-repo`,
`-relpath`, `-format`) pointing at the OLD location. An author-blind reviewer
demonstrated this: after relocating, `filename` pointed at README.md while the
stamped relpath still said `sub/deep/f.el`. A resolver that trusts the stamp
would undo the user's relocation.

## Decision so far

USER 2026-09-24: document and defer. Do not add complexity now. KNOWN
LIMITATION for slice 1a: a relocated bookmark may jump to its pre-relocation
file when used from another checkout.

The user's direction for later: **check for broken stamps ON READ** as the way
of managing them. Candidate rule, not yet adopted: honour a stamp only if
`filename` ends with the stamped relpath; otherwise treat the record as
non-portable and optionally offer to re-stamp or strip. A read-time check covers
relocate, hand edits and synced bookmark files, and needs no new advice seam.
