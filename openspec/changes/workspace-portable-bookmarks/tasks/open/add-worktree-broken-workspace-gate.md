---
name: add-worktree-broken-workspace-gate
description: Withhold the bookmark chooser's "add worktree here" when the current workspace is broken or its home is not an existing local directory
change: workspace-portable-bookmarks
status: done
reviewed_at: 2026-09-28T17:55:00+00:00
findings_path: .orchestrator/cycles/cycle-1790604160/reviews/add-worktree-broken-workspace-gate.md
merge_commit: 360ad60c
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

## Observations

- Predicate settled as `workspace-bookmark--workspace-unusable (name)` (portable-bookmarks.org § Off-workspace, beside
  `--offerable-workspaces`): nil when usable, else the first applying reason symbol — `unregistered` / `broken` / `no-home` /
  `home-remote` / `home-missing`. Not a `-p` name: the return shape is a reason, not a boolean. `file-remote-p` is asked before
  `file-directory-p`; a remote home is never stat'ed (pinned by the unit row that fails the spec on a `file-directory-p` of it).
- A FIFTH reason, `unregistered`, beyond the task's four: "registered" is part of the predicate's definition, and a tab carrying a name
  the registry does not hold is a real case (desktop-restored / hand-renamed tab). Before, that case fell through to the "has no home"
  arm with a misleading phrase; now the prompt says `workspace "X" is not registered`. The `no-home` phrase is the old one, unchanged.
- `--broken-workspace-p` is DELETED, not kept as a helper: it had one caller (`--offerable-workspaces`), and keeping it would leave
  two defuns testing `workspace--broken-p`. `workspace--broken-p` now appears in the `.el` only at the `declare-function` and inside
  the predicate.
- Prompt phrases (`workspace-bookmark--unusable-phrase`, one `pcase` next to `--missing-repo-withheld`): `broken` → `workspace "X" is
  broken (its home was missing when workspaces were loaded); re-anchor or purge it first`; `home-missing` → `the home of workspace
  "X", ~/path/, no longer exists; re-anchor or purge it first`; `home-remote` → `the home of workspace "X", /ssh:…, is remote` (the
  raw home, not `abbreviate-file-name`, which on a TRAMP path consults tramp's connection cache); `unregistered` → `workspace "X" is
  not registered`; `no-home` → unchanged. The re-anchor/purge remedy mirrors core's `:refused` user-errors.
- `--missing-repo-withheld` evaluates the predicate once, up front (`(and workspace (--workspace-unusable workspace))`), so the arm
  order no workspace → not runnable → unusable is kept and the reason is computed at most once; the cost is one `file-directory-p`
  even when the not-runnable arm wins. `--offerable-workspaces` calls the same predicate per registered name.
- The task's Verification grep 1 (`workspace--broken-p\|file-directory-p`) was imprecise: besides the predicate and `--checkout-dir-p`
  there are ten PRE-EXISTING `file-directory-p` sites in the `.el` that are not about workspace health (git cwd resolution :82/:86,
  `.git` resolution :211, stamping :328/:355, `--common-dir` :1378, :1414, :1456, `--home-checkouts` :1472 "only a home that exists
  confers ownership", :1744). The register's validator 1 as actually worded ("exactly ONE defun tests BOTH `workspace--broken-p` and
  `file-directory-p` of a workspace home") holds. Suggest the lint be `grep -n 'workspace--broken-p'` → declare-function + one defun.
- Existing off-workspace row "excludes a broken workspace" (off-workspace-spec) now tags through the new fixtures helper
  `workspace-bookmark-fixtures-mark-broken` instead of its inline `workspace--mark-broken` + `puthash` — a one-line touch outside the
  task's named rows, so wave 3's stand-in counting validator does not find an inline copy. The helper tags only; the caller deletes the
  home when it wants the load-time picture.
- Specs beyond the three rows the task names: a unit `describe "missing-repo choice: the workspace health gate"` (7 rows: healthy → nil;
  unregistered/nil; tagged with and without the home on disk → `broken`; plist without `:home` → `no-home`; remote → `home-remote`
  with `file-remote-p`/`file-directory-p` stood in at the Emacs-primitive boundary for the ONE call, no TRAMP; deleted → `home-missing`;
  and the withheld phrase after the no-workspace / not-runnable arms). The offer rows use a prompt stand-in that takes add-worktree
  WHENEVER it is offered (`--preferring ?a ?c`) so the spy assertion is itself load-bearing.
- Revert check. Variant A (gate arms reverted to HEAD's — nil-`:home` arm and bare broken-p filter — predicate kept): 806 specs, 4
  failed: "tagged broken … never calls the command" and "home was deleted mid-session … never calls the command" both go red on
  `Expected 'jf/workspace--add-worktree :not :to-have-been-called` (the command RAN — probes P1/P2 reproduced as specs);
  "withholds add-worktree with the reason's phrase" goes red on `(cdr (assq 'add-worktree …)) :to-match "no longer exists"`
  (nil: still offered); "excludes a workspace whose home vanished mid-session" goes red on the candidates `:to-equal`, showing
  `("wsbm-off-a" "wsbm-off-b" "wsbm-off-gone")`. Variant B (HEAD's `.el`): 806 specs, 10 failed — the four above plus the six
  predicate unit rows on `void-function workspace-bookmark--workspace-unusable`.
- Latent, out of scope: persistence tags a REMOTE home broken at load (persistence.el:136-155), so `home-remote` can only fire for a
  remote home registered mid-session; the order (broken first) makes that harmless. `--home-checkouts` (:1472) also stats the home
  during discovery — a second stat of the same directory per jump, but a checkout-discovery question, not a health one.
- Core's menu gap (`workspace--menu-healthy-p` is broken-p only; finding arch-cycle-1790604160-plan-01) is untouched, as instructed.
  No core `.org` changed; validator 2 grep over the seven core `.el` files is empty; the `jf/` surface is unchanged from HEAD
  (4 × `jf/emacs-dir`, 8 × `jf/workspace--add-worktree`).

## Discoveries

- discovery_id: disc-add-worktree-broken-workspace-gate-1
  class: vocabulary-mismatch
  description: |
    The speculated predicate was named `workspace-bookmark--workspace-usable-p
    (name)` with three new withheld reasons (broken / home missing / home
    remote). The code wanted a reason-returning function, so the name is
    `workspace-bookmark--workspace-unusable (name)` (nil = usable, else a
    symbol), and the reason set is FIVE: `unregistered`, `broken`, `no-home`,
    `home-remote`, `home-missing` — `unregistered` because "registered" is in
    the predicate's own definition and an unregistered current name is a real
    case that previously fell into the nil-:home arm with a wrong phrase;
    `no-home` because the existing arm folds in. Phrases live in one `pcase`,
    `workspace-bookmark--unusable-phrase`. `--broken-workspace-p` is gone.
  affected_register_entry: register/invariant/add-worktree-offer-shares-workspace-health-gate
  recommendation: |
    Reconcile: name = workspace-bookmark--workspace-unusable; return shape =
    nil | unregistered | broken | no-home | home-remote | home-missing (first
    applying); phrase helper = workspace-bookmark--unusable-phrase; callers =
    --missing-repo-withheld (third arm) and --offerable-workspaces. Narrow
    validator 1's lint to `grep -n 'workspace--broken-p'` (declare-function +
    the one defun) — `file-directory-p` has ten unrelated sites in the module.
    Add validator 4's spec anchor: unit row "names a remote home `home-remote'
    without ever asking the disk about it".

- discovery_id: disc-add-worktree-broken-workspace-gate-2
  class: interface-drift
  description: |
    The boundary entry's guard-site list (interfaces.org ~7709 and ~7794) names
    `--broken-workspace-p` among the permitted-surface guards. That defun no
    longer exists; its role is inside `--workspace-unusable`. The permitted
    core surface did NOT grow: the predicate reads `workspace--broken-p` and
    `workspace--home` (already listed) plus the Emacs primitives
    `file-remote-p` / `file-directory-p`; the transient module and its gate are
    still not named anywhere in the module (prose refers to "a health gate
    that lives in the transient module").
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Replace `--broken-workspace-p` with `--workspace-unusable` in the guard
    lists; re-run validators 1, 2, 5 (all hold at this commit: zero
    magit/projectile; core names no workspace-bookmark symbol; jf/ surface
    unchanged).

- discovery_id: disc-add-worktree-broken-workspace-gate-3
  class: spec-signal
  description: |
    The sixth consumer row (the add-worktree offer, `:refused`) is confirmed
    as speculated, with one refinement: the guard is the `unusable` arm of
    `--missing-repo-withheld`, decided by `--workspace-unusable`, whose reason
    set is the five above rather than the row's three; the prompt phrases for
    `broken` and `home-missing` carry the same remedy as core's `:refused`
    user-errors ("re-anchor or purge it first").
  affected_register_entry: register/vocabulary/workspace-broken-disposition
  recommendation: |
    Confirm the sixth row; list the five reasons in guard_location. The five
    core guards are unchanged.

- discovery_id: disc-add-worktree-broken-workspace-gate-4
  class: spec-signal
  description: |
    The `add-worktree` member's withheld clause now reads: no current
    workspace; the command not runnable (registration); the workspace
    unusable — unregistered / broken / no home / remote home / home gone from
    disk. The six members, the result shape and the chooser's plan variants
    are untouched (owner: outcome-action-mapping).
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    Re-state the add-worktree member's withheld clause at integrate with the
    text above; no member added or removed.

## Fix pass (cycle-1790604160)

Fixes the review's two advisory findings
(`.orchestrator/cycles/cycle-1790604160/reviews/add-worktree-broken-workspace-gate.md`,
Findings 1 and 2). Finding 3 (the register's lint text) is integrate's.

- Finding 1: the `broken` phrase of `workspace-bookmark--unusable-phrase`
  now reads "workspace %S is broken (tagged at load: its home was missing
  or remote); re-anchor or purge it first". That is true for both causes
  persistence tags (persistence.el: a remote `:home` and a non-directory
  `:home`). The order `broken` before `home-remote` is kept, so a tagged
  remote home is still never stat'ed. Updated to match: the tagged-broken
  spec row's `:to-match` (now the whole phrase, regexp-quoted), the withheld
  table row, and the `.org` glosses that said "missing at load" (the
  offer-gate prose, the predicate docstring, the off-workspace prose).
- Finding 2: `workspace-bookmark--add-worktree-and-replan` asks
  `workspace-bookmark--workspace-unusable` again immediately before the
  `jf/workspace--add-worktree` funcall. A reason becomes
  `(failed . PHRASE)`, PHRASE being what `--unusable-phrase` gives the prompt,
  and goes through the existing `failed` arm. The decline therefore reads
  "Bookmark %S: no worktree added (PHRASE); nothing opened": the command is
  not called, nothing opens, and no directory is created. New spec row in
  `portable-bookmarks-missing-repo-spec.el` § "the offer": "asks the gate
  again at the call ...". Its prompt stand-in deletes the fixture home and
  then answers `?a`. The new stand-in helper
  `workspace-bookmark-fixtures-answering-after` lives in
  `portable-bookmarks-fixtures.el`. The `.org` prose next to the withheld
  table and in "Add worktree here" now says the gate runs at offer AND call.
- Fail-on-revert (both checked in one scratch run against a hand-edited `.el`,
  then restored by re-tangling, which gave a byte-identical result): with the
  second check removed (`unusable` bound to nil), the new row goes red on
  `(expect 'jf/workspace--add-worktree :not :to-have-been-called)`. With the
  old wording restored, the tagged-broken row goes red on its prompt
  `:to-match`. Exactly 2 of 812 fail. Nothing else goes red.

### Observations (fix pass)

- The pre-call decline reuses the `(failed . REASON)` arm rather than adding
  a new plan shape. A second message format would have been a second
  spelling of "no worktree added ...; nothing opened".
- `'home-remote` stays reachable only for an UNTAGGED remote home (a stale
  or hand-built registry plist, or a direct call). Every loaded remote home
  is tagged `broken` first. The arm is kept for the predicate's
  total-function contract, not for a live prompt.

### Discoveries (fix pass)

- discovery_id: disc-add-worktree-broken-workspace-gate-fix-1
  class: invariant-gap
  description: |
    The entry places enforcement "at the OFFER — before any
    directory-creating call". The offer alone left a window as long as the
    modal prompt: a home deleted while the prompt was up reached
    `git worktree add`. The consumer now gates at the offer AND at the call,
    with the same predicate and the same phrase.
  affected_register_entry: register/invariant/add-worktree-offer-shares-workspace-health-gate
  recommendation: |
    Reconcile enforcement_locus_check to "in the consumer, at the offer
    (`--missing-repo-withheld`) AND immediately before the call
    (`--add-worktree-and-replan`)". Validator 1's caller list becomes
    `--missing-repo-withheld`, `--add-worktree-and-replan` and
    `--offerable-workspaces`. Validator 3 gains the row "home deleted while
    the prompt is up -> declines, command never called".
- discovery_id: disc-add-worktree-broken-workspace-gate-fix-2
  class: spec-signal
  description: |
    The `broken` reason is the load-time tag, and persistence sets it for a
    missing home AND for a remote home. The reason's gloss (prompt phrase,
    docstring, register prose) must not name only "missing".
  affected_register_entry: register/invariant/add-worktree-offer-shares-workspace-health-gate
  recommendation: |
    Where the entry or `register/vocabulary/workspace-broken-disposition`
    glosses the broken tag as "home missing at load", read "home missing or
    remote at load".
