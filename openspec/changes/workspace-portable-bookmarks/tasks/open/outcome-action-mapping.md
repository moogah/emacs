---
name: outcome-action-mapping
description: One outcome-to-action mapping for the jump seam, chooser and off-workspace flow; the seam declines (not falls through) for stamped records; drop the unreachable fallback payload builder
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: true
cycle: cycle-1790604160
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/invariant/no-silent-wrong-file-open
  - register/boundary/bookmark-advice-seams
  - register/shape/workspace-integration-anchor-payload
relations:
  - discovered-from:eoc-1790453714-04
  - discovered-from:eoc-1790453714-05
discovered_from: [eoc-1790453714-04, eoc-1790453714-05]
discovered_by: architect
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714 from `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § eoc-04 / eoc-05.
> Pressure-tested at plan of cycle-1790604160 against HEAD c65efb86 (finding arch-cycle-1790604160-plan-03). Wave 2, alone: it edits
> the same § Missing repo region of `portable-bookmarks.org` and the same `missing-repo-spec.el` as the wave-1 gate. `on_critical_path`
> set true at plan (step 1 closes a route to the silent wrong-file open the proposal forbids).

## Files
- `config/workspaces/portable-bookmarks.org` (+ tangled `.el`): § Seam 2 › "What the seam does with each outcome" (.org:1066),
  "The barrier" (.org:1186; `--handle-bookmark-advice` .org:1353 / .el:542-587, barrier :568-570), `--jump-plan` (.org:1308 /
  .el:499-540), `--decline-message` (.org:1263 / .el:458-485); § Missing repo › "The choice" (`--offer-missing-repo` .org:1632 /
  .el:690-726) and "Add worktree here" (`--add-worktree-payload` .org:1824 / .el:806-817, `--after-add-message` .org:1837 /
  .el:819-826, `--add-worktree-and-replan` .org:1846 / .el:828-861); § Off-workspace › "The fallback never opens the stored path"
  (`--no-workspace-plan` .org:2209 / .el:1009-1026; `--off-workspace-plan` .org:2228 / .el:1028-1070); § Resolution › "The closed
  set of outcomes" (.org:3529; `--valid-outcomes` .el:1597).
- `config/workspaces/test/portable-bookmarks-jump-spec.el` § "barrier and host contract" (:505-545) and a new structural describe.
- `config/workspaces/test/portable-bookmarks-missing-repo-spec.el` § "the add-worktree payload" (:773-797; delete :789-795).
- `config/workspaces/test/portable-bookmarks-off-workspace-spec.el` § "layout reincarnation chain" (:762) — one new leaf spec.
- `openspec/changes/workspace-portable-bookmarks/design.md` § Risks › "Advice on core bookmark functions" (:272-275) — correction
  D-j, moved here from doc-sync-cycle2 because THIS task changes the behaviour it describes.
- NOT touched: `layouts.org` (core consumes the seam's decline unchanged), `git-worktrees.org`, `integrations.org`.

## Callers of the advised function (enumerated at plan; every one must keep working, none may be missed)
`bookmark-handle-bookmark` (Emacs 30.2 bookmark.el:1339) is reached:
- INSIDE the seam (they pass seam 2 and are covered by this change):
  - bookmark.el:1267 `bookmark--jump-via` — behind `bookmark-jump` :1319, `bookmark-jump-other-window` :1328,
    `bookmark-jump-other-frame`, and the bmenu commands :2283 / :2296 / :2303 / :2310 / :2326;
  - bookmark.el:1534 `bookmark-insert` — a DIRECT call (a decline now aborts the insert with the `user-error`; acceptable, say so in
    the seam docstring);
  - `config/workspaces/layouts.el:246` `workspace--bookmark-buffer` → `bookmark-jump` with `workspace--restoring-layout-leaf` bound;
    a `user-error` is a DECLINE (:248-250) → reason buffer, one report per restore (`workspace--deserialize-buffer` :288-329);
  - bufferlo.el:2559 `bufferlo--bookmark-jump` → `bookmark-jump`; bufferlo.el:1223 ALSO advises `bookmark--jump-via` `:around`
    (`bufferlo--bookmark--jump-via-advice` :5230) ABOVE our seam and hands OLDFN the RECORD (recorded in the register: the host's
    string-gated tail is skipped under bufferlo-mode);
  - consult.el:4331 `consult--bookmark-action` and :4370 `consult-bookmark` → `bookmark-jump`;
  - embark.el:4568-4569 → `bookmark-jump-other-window` / `-other-frame`;
  - org.el:19092 and org-refile.el:647 → `bookmark-jump`.
- OUTSIDE the seam (they call handlers directly; unchanged by this task, recorded as residuals):
  - bufferlo.el:3021-3022 (nested buffer records of a bufferlo tab/frame bookmark: `(funcall (or (bookmark-get-handler record)
    'bookmark-default-handler) record)`) and :4961 (`(funcall bookmark-handler bookmark-record 'not-jump)`) — residual (f);
  - consult.el:4322-4327 bookmark PREVIEW opens `bookmark-get-filename` of a default-handler record in a temporary buffer — a
    stamped record previews its STORED path (new residual (g), informational finding arch-cycle-1790604160-plan-06);
  - marginalia.el:848 reads the handler for annotation only (no call).
- NOT loaded by this config (present in runtime/straight/repos only): activities.el:835. No other package under
  `runtime/straight/repos` that the config loads names `bookmark-handle-bookmark` or `bookmark--jump-via`.

## Steps
1. (05) ONE mapping. The outcome → action mapping is spelled at five sites — `--jump-plan` :529-540, `--no-workspace-plan`
   :1017-1026, `--add-worktree-and-replan` :846-851, `--after-add-message` :822 (memq over four members), `--decline-message`
   :471-485 (pcase over the same four). Introduce `workspace-bookmark--plan-for-outcome (record resolution context original)` with
   context ∈ {`in-workspace`, `none`, `after-add`} and a `workspace-bookmark--declining-outcomes` constant
   `(not-on-branch ambiguous missing-repo no-clone)` that `--decline-message` and `--after-add-message` read. The matrix (also in
   `register/vocabulary/bookmark-resolution-outcome` → `canonical_mapping_function`):
   | outcome | in-workspace | none | after-add |
   |---|---|---|---|
   | non-portable | `(call-through . ORIGINAL)` | decline | decline (after-add report) |
   | resolved | `--plan-to-open` call-through | decline | `--plan-to-open` call-through |
   | missing-repo | `--offer-missing-repo` | `--offer-missing-repo` | decline (after-add report) |
   | not-on-branch / ambiguous / no-clone | decline (`--decline-message`) | decline | decline (after-add report) |
   | anything else | SIGNAL | SIGNAL | SIGNAL |
   `(call-through . ORIGINAL)` is produced by exactly one cell. The three planners call it; `--valid-outcomes`' docstring (:1597-1602)
   then names ONE dispatch site to update when a member is added.
2. (05) The seam's barrier (:568-570) DECLINES for a STAMPED record: when `workspace-bookmark--stamped-coordinates` (:1642, never
   signals) is non-nil for the (normalised) record, an error inside the decision becomes `(decline . "Bookmark %S: the jump decision
   failed (%s); nothing opened")` — a `user-error` before ORIG-FN; an UNSTAMPED record (or a non-record argument) keeps the stock
   fall-through with the ORIGINAL argument. `quit` still passes. The off-workspace plan's own barrier and its "call-through with
   ORIGINAL → decline" conversion (:1061-1065) stay (belt and braces).
3. Specs:
   - jump-spec § "barrier and host contract": the two pins "still opens the stored location when the resolver signals" (:512) and
     "... when the seam's own copy step signals" (:523) stamp their record through the real seam 1, so they will go RED — rewrite
     them as "an error inside the seam's decision for a STAMPED record opens nothing" (decline `user-error`, no buffer visits the
     stored path nor the resolved one, stored record unchanged) and add an UNSTAMPED twin (a record `--set` with the stamp properties
     stripped, resolver signalling) that keeps the stock assertion. "lets quit propagate" (:534) stays.
   - jump-spec, new describe "one outcome-to-action mapping": for every member of `workspace-bookmark--valid-outcomes` × every
     context, with `--offer-missing-repo` and `--plan-to-open` stubbed to return markers, the mapper returns a plan whose car is one
     of `call-through` / `decline` / `view` / `cancel`, and `(call-through . ORIGINAL)` only for (non-portable, in-workspace); an
     unknown outcome signals in every context.
   - off-workspace-spec § "layout reincarnation chain" (:762): a leaf whose STAMPED bookmark meets an internal seam error (resolver
     stubbed to signal) lands on the reason buffer, one report, the stored path never visited — the fourth actor's consequence
     (asks 1790453714-1/2 already chose reason-buffer over stored path).
4. (04) Delete the unreachable hand-built fallback in `--add-worktree-payload` (:813-817): call `workspace--integration-payload`
   unconditionally (its `declare-function` is at :803; the offer gate at :605-616 already requires the function defined in the same
   file, integrations.el:41 / :53). Delete the spec that keeps it alive by unbinding the constructor (missing-repo-spec.el:789-795).
   Delete the unreachable `(unless (fboundp 'jf/workspace--add-worktree) (error ...))` at :839-840 (same gate). KEEP the `skipped`
   arm (:856-859) and its spec (:760-767): `register/vocabulary/workspace-integration-outcome` lists `skipped` as a legitimate
   handler result, and the consumer reads the vocabulary STRICTLY (unknown → signal → the chooser's barrier declines) where core's
   `--run-one-integration` (integrations.el:82-86) reads it leniently — state that reading in the arm's comment.
5. Docs: amend design.md § Risks › "Advice on core bookmark functions" (:272-275) to: "... An error in the seam's own decision
   declines for a STAMPED record (reported, nothing opened) and falls through to today's behaviour only for an unstamped one. An error
   inside the missing-repo chooser or the off-workspace flow declines likewise, so no path falls back to the stored path for a stamped
   record." (This replaces correction D-j of the cycle-2 audit, which doc-sync-cycle2 deliberately skips.)
6. Stand-ins: any NEW test helper (e.g. "strip the stamp from a record", "resolver that signals") goes into
   `portable-bookmarks-fixtures.el`, never inline — `test-stand-in-consolidation` (wave 3) counts inline copies and fails the suite.
7. Tangle; commit `.org` + `.el`.

## Verification
- Isolated suite exit 0; full suite `compare-baseline.py` → NO-REGRESSION.
- The stamped-record barrier spec and the leaf spec fail with step 2 reverted; the mapping spec fails when one planner is given a
  private `pcase` again (revert step 1 for one site).
- `grep -n "pcase (plist-get" config/workspaces/portable-bookmarks.el` in the seam / missing-repo / off-workspace sections → the
  mapping function only; `grep -n "declining-outcomes" config/workspaces/portable-bookmarks.el` → the constant, `--decline-message`,
  `--after-add-message`.
- `grep -n "fboundp 'workspace--integration-payload\|fboundp 'jf/workspace--add-worktree" config/workspaces/portable-bookmarks.el`
  → only the offer gate (:613), not the payload builder or the replan.
- `grep -n "declines\|STAMPED" openspec/changes/workspace-portable-bookmarks/design.md` → the amended Risks paragraph.

## Register
On-touch fires on `register/invariant/no-silent-wrong-file-open` (validator 1'; residual (c) closed for stamped records),
`register/boundary/bookmark-advice-seams` (degradation_contract narrowing; `plan_shape`; validator 3 re-armed) and
`register/vocabulary/bookmark-resolution-outcome` (`canonical_mapping_function`; validator 6). `workspace-integration-anchor-payload`
(load_bearing false) is re-stated at integrate once the fallback is gone. Neighbours:
`.orchestrator/cycles/cycle-1790604160/briefs/entries-outcome-action-mapping.txt`.
