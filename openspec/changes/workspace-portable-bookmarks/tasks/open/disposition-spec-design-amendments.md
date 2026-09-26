---
name: disposition-spec-design-amendments
description: Amend the two delta specs and design.md to the decided design (DD-9, DD-12, DD-14, DD-16) before any sync or archive; user confirms the ready-to-apply edits
change: workspace-portable-bookmarks
status: blocked
task_class: doc
on_critical_path: false
cycle: cycle-2
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/shape/portable-bookmark-coordinates
  - register/boundary/workspace-git-worktrees-consumer
  - register/boundary/git-derived-checkout-discovery
relations:
  - discovered-from:arch-cycle-1789935943-eoc-01
  - discovered-from:ask-cycle-1789935943-17
  - relates-to:docs-readme
discovered_from: arch-cycle-1789935943-eoc-01
discovered_by: architect
discovered_class: interface-drift
blocker_note: user confirmation of the edits below (ask 17); blocks `opsx-sync` / `opsx-archive` of this change, NOT the cycle-2 code tasks
created_at: 2026-09-26T14:00:00-04:00
---

> Disposition task created by cycle-1789935943 integrate from the end-of-cycle architect
> finding eoc-01 (blocking, interface-drift against the change's own delta specs and
> design.md). The code is right; the documents describe the pre-decision design. Syncing
> them as they stand would propagate "two properties", "five outcomes" and "projectile
> merged" into the main specs. Confirm the edits (or amend them) and the task becomes
> `ready` for a doc-class implementor.

## Summary

Bring `specs/portable-bookmarks/spec.md`, `specs/workspace-git-worktrees/spec.md` and
`design.md` into line with user decisions DD-9 (three-property stamp, all or none),
DD-12 (worktree sources are git-derived + magit only), DD-14 (sixth outcome `ambiguous`)
and DD-16 (lexical confinement; links resolve by spelling), and with the merged code at
`20944160`. No code change.

## Ready-to-apply edits (confirm each)

1. `specs/portable-bookmarks/spec.md` § "Portable coordinates" (line ~5): "two additional
   properties" → THREE (repo identity, repo-relative path, format version = 1), stamped all
   or none; untracked locations are left unstamped (DD-9; `portable-bookmarks.el:366-369`).
2. `specs/portable-bookmarks/spec.md` § "Closed set of resolution outcomes" (lines ~98-112):
   six outcomes. Add `ambiguous` (2+ workspace checkouts, or none and 2+ canonical
   checkouts; `:candidates`; reported, nothing opened). State the result keys
   `(:outcome :path :identity :relpath :candidates :checkout)`. State DD-16: a relpath that
   is absolute, holds `..` or NUL is refused (`non-portable`); anything else resolves to
   `<checkout>/<relpath>` by spelling, whatever its links point at. State that `missing-repo`
   may carry a nil `:path` (ask 16, pending).
3. `specs/workspace-git-worktrees/spec.md` § "Soft repo-source selection" (lines ~6-13, 32-36,
   45-48): sources are git-derived discovery, then `magit-list-repos`; every candidate passes
   ONE admission predicate; projectile and project.el are not consulted. Replace scenario
   "Candidates merge from loaded sources" with "A magit-listed linked worktree contributes
   its main worktree" and "A workspace-owned checkout is never offered"; add "Remote magit
   entries are dropped without a connection". (`git-worktrees-spec.el:128` already pins the
   opposite of the current scenario.)
4. `design.md` D5 (line ~128): command table is `git remote get-url origin`,
   `rev-parse --path-format=absolute --show-toplevel --git-common-dir`, `ls-files -z`;
   D6 (~147-160): the offer predicate is `workspace-bookmark--offerable-canonical-p`; the
   `.git`-is-a-directory test is structural only; D7 (~162-172): six outcomes, DD-16.
5. `.tasks/` bookkeeping note only: all 14 task files sit in `tasks/open/` with
   `status: done`; `opsx-archive` reads them. No action here.

## Verification
- `grep -n "two additional properties\|five outcomes\|projectile" openspec/changes/workspace-portable-bookmarks/specs/*/spec.md openspec/changes/workspace-portable-bookmarks/design.md` returns nothing.
- `grep -n "ambiguous" openspec/changes/workspace-portable-bookmarks/specs/portable-bookmarks/spec.md` shows the sixth outcome.

## Out-of-scope
Syncing to `openspec/specs/` (that is `opsx-sync`, after this lands). Any code change.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/end-of-cycle-1789935943.md` § Finding 1
- design decisions DD-9, DD-12, DD-14, DD-16 (`state.json` → `design_decisions`)
