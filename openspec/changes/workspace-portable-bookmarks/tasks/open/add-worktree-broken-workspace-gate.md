---
name: add-worktree-broken-workspace-gate
description: Withhold the bookmark chooser's "add worktree here" when the current workspace is broken or its home is not an existing local directory
change: workspace-portable-bookmarks
status: ready
task_class: bug
on_critical_path: true
cycle: cycle-1790604160
cites_register_entries:
  - register/invariant/add-worktree-offer-shares-workspace-health-gate
  - register/vocabulary/workspace-broken-disposition
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
> Pressure-tested at plan of cycle-1790604160 against HEAD c65efb86 (finding arch-cycle-1790604160-plan-02). Wave 1, FIRST merge of the cycle.

## Problem
`workspace-bookmark--missing-repo-withheld` (portable-bookmarks.el:618-643; .org:1496) withholds add-worktree only for no workspace /
command not registered / nil `:home`. Its "runnable" check (`--add-worktree-runnable-p`, :605-616) asks core's
`workspace--integration-menu-command-p` (integrations.el:41-51), which answers REGISTRATION ("is this some integration's :menu
command"), NOT menu VISIBILITY. The menu's health gate is `workspace--menu-healthy-p` (workspaces-transient.el:11-14, applied at :98)
— a module this consumer may not name (it loads after portable-bookmarks, deferred). So in a BROKEN current workspace (a tab carrying
a broken workspace's name, or a home deleted mid-session) the chooser offers add-worktree, `jf/workspace--add-worktree`
(git-worktrees.el:~190-235) runs `git worktree add`, and git recreates the missing home as a bare directory holding only the worktree;
the next startup loads it healthy with no `home.org` (probes P1/P2 in the audit).

The same module already applies the broken disposition to its OTHER workspace-facing prompt (`--offerable-workspaces`, :903-915,
via `--broken-workspace-p` :892-901): two health notions in one file. Note that `workspace--menu-healthy-p` itself is
`broken-p`-only, i.e. core's menu has the mid-session-deletion half of this gap too — that is core's, NOT this task's
(finding arch-cycle-1790604160-plan-01, routed to `.tasks/workspaces-registration-hardening-leftovers`).

## Files
- `config/workspaces/portable-bookmarks.org` (+ tangled `.el`): § Missing repo › "What is offered, and why not" (.org:1429-1535,
  `--add-worktree-runnable-p` :1483, `--missing-repo-withheld` :1496; .el:605-643); § Off-workspace, `--broken-workspace-p`
  (.org:2086; .el:892-901) and `--offerable-workspaces` (.org:2097; .el:903-915); `--workspace-home` (.org:2318; .el:1088-1095).
- `config/workspaces/test/portable-bookmarks-missing-repo-spec.el` § "missing-repo choice: the offer" (:243-365).
- `config/workspaces/test/portable-bookmarks-off-workspace-spec.el` § "off-workspace jump: candidates" (:518-560).
- `config/workspaces/test/portable-bookmarks-fixtures.el` ONLY if a new helper is needed (see step 5).
- NOT touched: any core `.org` (tabs, workspaces, persistence, workspaces-transient, integrations), `git-worktrees.org`.

## Steps
1. ONE module health predicate, next to `--broken-workspace-p` (.el:892): registered AND not `workspace--broken-p` AND `:home` a
   string that is not `file-remote-p` AND `file-directory-p` — remote tested FIRST so a remote home is never stat'ed
   (`register/invariant/directory-identity-by-device-inode`: remote homes unsupported). Suggested shape: return nil when usable,
   else a REASON the prompt can print (`broken` / `home-missing` / `home-remote` / `no-home`), so both call sites share one function
   and one wording. Do not invent a second notion: this is the consumer's mirror of the menu's gate PLUS the on-disk check the
   load-time broken tag cannot carry (`register/invariant/broken-tag-runtime-only`). "Runnable" stays a separate, orthogonal
   condition (registration).
2. In `--missing-repo-withheld` (.org:1496): withhold `add-worktree` with that reason when the current workspace is unusable
   (this arm subsumes the existing nil-`:home` arm; keep its wording as the `no-home` reason). Keep the arm order: no workspace →
   not runnable → not usable. The prompt (`--missing-repo-prompt`, .el:657-674) already prints every withheld reason.
3. Route `--offerable-workspaces` (.org:2097) through the SAME predicate (replacing the bare `--broken-workspace-p` call, which may
   become the predicate's helper or be folded into it), so the off-workspace picker also drops a workspace whose home vanished
   mid-session. One spec row for that (step 4).
4. Specs, each RED with the gate reverted, each asserting NO call of `jf/workspace--add-worktree` (spy) and the reason in the prompt:
   - missing-repo-spec § "the offer" (:243): (a) current workspace TAGGED BROKEN (mark it the way off-workspace-spec.el:526-536 does:
     `workspace--mark-broken` + `puthash` into the isolated table) → add-worktree withheld, reason names "broken"; (b) HOME DELETED
     after registration (`delete-directory` the fixture home, tab still current) → withheld, reason names the missing home; the
     existing healthy row (:250) stays green. This is the audit's probe P1 as a spec.
   - off-workspace-spec § "candidates" (:518): "excludes a workspace whose home vanished mid-session" beside the tagged-broken row
     (:526).
   Use the fixtures' describe-level macro; do not rebind `workspace--registry`.
5. Stand-ins: any NEW helper (e.g. "mark this fixture workspace broken") goes into `portable-bookmarks-fixtures.el`, never inline in
   the spec — `test-stand-in-consolidation` (wave 3) adds a counting validator that fails the suite on an inline copy.
6. Tangle (`./bin/tangle-org.sh config/workspaces/portable-bookmarks.org`); commit `.org` + `.el` together.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` exit 0; full suite `compare-baseline.py` → NO-REGRESSION.
- Each new spec fails with the gate reverted (state which assertion goes red).
- `grep -n 'workspace--broken-p\|file-directory-p' config/workspaces/portable-bookmarks.el` → the one predicate (plus
  `--checkout-dir-p` :1097, which is about checkouts); `grep -n '<predicate-name>' config/workspaces/portable-bookmarks.el` → called
  from `--missing-repo-withheld` AND `--offerable-workspaces`.
- `grep -n 'workspace-bookmark' config/workspaces/{tabs,workspaces,persistence,workspaces-transient,integrations,layouts,data-model}.el`
  → zero (core still names no consumer symbol; `register/boundary/portable-bookmarks-consumer` validator 2).

## Register
On-touch fires on `register/invariant/add-worktree-offer-shares-workspace-health-gate` (owner: this task; settles the predicate's
name, the off-workspace routing and the prompt wording), `register/vocabulary/workspace-broken-disposition` (sixth consumer row),
`register/boundary/portable-bookmarks-consumer` (registration-vs-visibility correction) and
`register/vocabulary/bookmark-resolution-outcome` (the add-worktree member's withheld clause). Neighbours are listed in
`.orchestrator/cycles/cycle-1790604160/briefs/entries-add-worktree-broken-workspace-gate.txt`.
