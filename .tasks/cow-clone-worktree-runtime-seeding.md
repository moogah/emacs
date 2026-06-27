---
name: cow-clone-worktree-runtime-seeding
description: Replace the 922MB rsync seed in init-worktree-runtime.sh with APFS copy-on-write clones (cp -c / clonefile) for near-instant, space-shared worktree runtimes; add a cheap reset-to-snapshot
status: open
source: opsx-explore session comparing Emacs MCP-server projects (emcp, emacs-jail-mcp, llm-test)
relations:
  discovered-from: connect-external-llm-clients-mcp-broker
priority: medium
---

## Problem
`bin/init-worktree-runtime.sh` seeds a new worktree's `runtime/` by `rsync`-ing
~922MB of `straight/` (repos + build) plus the tree-sitter grammars from a source
worktree. This is slow (~minute) and costs full disk per worktree, so N worktrees
cost N×922MB.

The `emacs-jail-mcp` project (Podman-based disposable Emacs sandbox) gets fresh
environments essentially for free via a copy-on-write overlay filesystem — zero
byte-copy until first write, instant seed, shared blocks. That technique ports to
this repo because we are on APFS (darwin), where `cp -c` uses `clonefile(2)` —
filesystem-level COW: instant clone, blocks shared until a file diverges.

## Why this is external to the MCP-broker change
This was discovered while surveying isolation models for the MCP-server work, but
it is a standalone optimization of the existing worktree/runtime infrastructure
(`bin/init-worktree-runtime.sh`, `bin/invalidate-runtime.sh`). It stands on its
own regardless of whether the MCP broker is built, so it lives here rather than
inside that change.

## Proposed approaches
1. **COW seed.** In `init-worktree-runtime.sh`, when source and target are on the
   same APFS volume, use `cp -c -R` (clonefile) instead of `rsync`/`cp -R`. Fall
   back to `rsync` across volumes or on non-APFS. Turns the ~60s/922MB copy into a
   sub-second, ~0-disk clone.
2. **Cheap reset-to-snapshot (disposability).** The jail's real value is "restart =
   known-good clean slate, instantly." Today the equivalent is
   `invalidate-runtime.sh` → re-clone + rebuild (slow, destructive). With COW we
   could keep a pristine `runtime/` snapshot and restore it via clone in ~1s — the
   jail's disposability without a container. Useful for both testing and for the
   isolated-daemon targets the MCP broker would spawn.

## Verification
- On an APFS volume, seeding a new worktree with the COW path completes in <2s and
  adds negligible disk (verify with `du` / `diskutil`), while a subsequent edit to
  one worktree's package does not affect the source (COW divergence works).
- Cross-volume / non-APFS still works via the rsync fallback.

## Context
- `bin/init-worktree-runtime.sh` (the rsync seed), `bin/invalidate-runtime.sh`
  (the slow destructive reset).
- Prior art: `emacs-jail-mcp` (https://github.com/gavv/emacs-jail-mcp) COW overlay.
- Note: macOS `clonefile` is same-volume APFS only — keep rsync as the portable
  fallback.
