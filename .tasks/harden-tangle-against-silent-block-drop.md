---
name: harden-tangle-against-silent-block-drop
description: bin/tangle-org.sh should detect a src block silently dropped because a body line starts with `*`
status: open
source: openspec/changes/add-workspace-git-worktrees (cycle-20260612-164043 integrate)
relations:
  discovered-from: add-worktree-command
priority: medium
---

## Problem
org-babel treats a line beginning with `*` (column 0) inside a `#+begin_src`
emacs-lisp body as an org HEADLINE boundary and silently truncates the block at
that point — the rest of the block (and any following defun) never reaches the
tangled `.el`. `./bin/tangle-org.sh` reports SUCCESS (its only gate is
`check-parens`, which sees the now-shorter-but-balanced output as fine).

This is a latent footgun for EVERY literate module in the repo, not specific to
the git-worktrees change. It cost real debugging time on `add-worktree-command`
(a docstring enumerating git-illegal punctuation — `* [ \\ ...` — began a line
with `*`, and the entire `jf/workspace--worktree-sanitize-branch` defun vanished
from the `.el` while tangle reported success).

## Proposed approaches (pick one)
1. In `bin/tangle-org.sh`, after tangling, count `#+begin_src emacs-lisp` blocks
   in the `.org` vs `;;; ...` / top-level form boundaries in the `.el`, and warn
   on a mismatch.
2. Pre-tangle lint: scan each `#+begin_src emacs-lisp` body for a line matching
   `^\*` and emit a warning (most legitimate code never starts a line with `*`;
   docstrings can be reworded, as the on-purge task did).
3. Cheapest: document the hazard prominently in the literate-programming skill /
   CLAUDE.md and the implementor overlay (a forewarned `on-purge-teardown-handler`
   implementor avoided it cleanly), and defer the tooling change.

## Verification
- A deliberately-crafted `.org` with a `*`-leading docstring line is caught
  (warning or non-zero exit) by `./bin/tangle-org.sh`.

## Context
arch-cycle-20260612-164043-eoc-02 (advisory, invariant-gap).
