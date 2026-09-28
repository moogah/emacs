---
name: add-worktree-broken-workspace-gate
description: Withhold the bookmark chooser's "add worktree here" when the current workspace is broken or its home is not an existing local directory
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: true
cycle: cycle-1790453714
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/boundary/portable-bookmarks-consumer
relations:
  - discovered-from:eoc-1790453714-01
discovered_from: eoc-1790453714-01
discovered_by: architect
discovered_class: vocabulary-mismatch
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714 from the end-of-cycle audit (BLOCKING), `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § eoc-1790453714-01.

## Problem
`workspace-bookmark--missing-repo-withheld` withholds add-worktree only for no workspace / command not registered / nil `:home`. Its
runnable check (`workspace--integration-menu-command-p`) repeats the menu's registry walk but not the menu's healthy-only `:if` gate, so
in a BROKEN current workspace (tab carrying a broken workspace's name, or home deleted mid-session) the chooser offers add-worktree and
`git worktree add` recreates the missing home as a bare directory holding only the worktree; the next startup loads it healthy with no
`home.org`.

## Steps
1. Withhold add-worktree (with its reason in the prompt) when the current workspace is broken (`workspace--broken-p`) or its `:home` is
   not an existing local directory (remote homes are unsupported). Mirror the menu's gate; do not invent a second health notion.
2. Specs in the offer matrix: broken current workspace → withheld with reason; home deleted mid-session → withheld; healthy → offered.
3. Tangle; commit `.org` + `.el`.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green; full suite NO-REGRESSION; each new spec fails with the gate reverted.
