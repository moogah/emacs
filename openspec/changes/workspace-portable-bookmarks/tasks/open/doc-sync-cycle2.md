---
name: doc-sync-cycle2
description: Bring proposal.md, design.md and the delta specs in line with the code and this cycle's user decisions; add the two missing delta specs (workspaces, workspace-integrations)
change: workspace-portable-bookmarks
status: ready
task_class: doc
on_critical_path: true
cycle: cycle-1790604160
cites_register_entries:
  - register/vocabulary/buffer-reincarnation-fallback-chain
  - register/shape/workspace-integration-anchor-payload
  - register/vocabulary/workspace-creation-context
  - register/invariant/bookmark-jump-not-captured-as-working-state
relations:
  - discovered-from:eoc-1790453714-02
  - discovered-from:meta-discovery/doc-sync-lag
discovered_from: [eoc-1790453714-02, meta-discovery/doc-sync-lag]
discovered_by: architect
discovered_class: interface-drift
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714. Blocks `opsx-sync` / `opsx-archive`. USER RULE 2026-09-26: doc corrections where the code is
> right are applied and listed, not asked.
> Pressure-tested at plan of cycle-1790604160 against HEAD c65efb86 (finding arch-cycle-1790604160-plan-05): all 31 corrections
> re-verified — every "current" phrase is still in the named file (proposal :48/:58-70/:76/:87-88/:95-96/:112-115/:121-122/:125-128/
> :130-134/:138-140/:145-146; design :61-62/:66-70/:98/:126-133/:139/:159/:187-191/:200-218/:266-268/:272-275/:277-279/:307-309/
> :313-317; portable-bookmarks spec :110-115/:158-160/:185-188+:203-206/:252-254+:266-269/:212-215; git-worktrees delta :5-9 and
> main spec :93). D9 is unchanged at c65efb86. Wave 1, docs only, merges AFTER `add-worktree-broken-workspace-gate`.

## Files
- `openspec/changes/workspace-portable-bookmarks/proposal.md` (body only — the YAML status header is the architect's/PM's)
- `openspec/changes/workspace-portable-bookmarks/design.md`
- `openspec/changes/workspace-portable-bookmarks/specs/portable-bookmarks/spec.md`
- `openspec/changes/workspace-portable-bookmarks/specs/workspace-git-worktrees/spec.md`
- NEW `openspec/changes/workspace-portable-bookmarks/specs/workspaces/spec.md`
- NEW `openspec/changes/workspace-portable-bookmarks/specs/workspace-integrations/spec.md`
- optionally `config/workspaces/docs/README.org` (step 6)
- No code, no tests, no `.org`/`.el` under `config/`.

## Steps
1. Apply 30 of the 31 corrections listed in `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § Doc corrections
   (proposal P1-P11, design D-a..D-i + D-k..D-m, specs/portable-bookmarks S1-S5, specs/workspace-git-worktrees G1-G2). Before applying
   each one, grep its "current" phrase in the named file (they were all present at c65efb86; if one is gone, say so in
   ## Observations and skip it). NOT this task: design D-j (§ Risks › "Advice on core bookmark functions", :272-275) — its behaviour
   changes in wave 2 (`outcome-action-mapping`: the seam DECLINES on an internal error for a stamped record), so that task amends D-j to
   the final wording. Leave D-j's text as it is.
2. design.md D9 (:200-218), per USER ask-cycle-1790453714-3 (b), correction D-h verbatim: a jump-triggered restore IS remembered
   like any find-file; the anti-save predicate was removed; what remains is paint-then-show ordering and outer-jump-only target scoping
   (the reconciled `register/invariant/bookmark-jump-not-captured-as-working-state` is the source: title, statement 1-3). Delete the
   capture-rejection alternative and the anti-save implementation note; keep the "open in a split" alternative.
3. S1 (spec § "Closed set of resolution outcomes" item 3, :110-115) must ALSO carry the two withheld reasons the gate task adds this
   cycle (it merges before this task): append "The add-worktree choice SHALL NOT be offered when no workspace is current, when the
   current workspace is broken or its home is not an existing local directory, when the current workspace has no home, or when the
   git-worktrees add command is not registered; the prompt names each withheld choice and why. When no one can answer (non-interactive,
   `inhibit-interaction`, or during a layout-restore leaf), the capability SHALL report the outcome and open nothing instead of
   prompting." Add the two scenarios (broken current workspace; home vanished mid-session) beside the existing offer scenarios.
4. Add `specs/workspaces/spec.md` (MODIFIED: Buffer reincarnation across restart — a handler's `user-error` is a DECLINE: steps 2-3
   skipped, the leaf lands on a reason buffer, ONE aggregated report per restore, `workspace--restoring-layout-leaf` bound around the
   leaf jump; workspace-new / anchoring — name must equal basename, a registered name and a remote home are refused; persistence —
   a remote `:home` is loaded broken and never contacted; workspace-purge — scope STRICTLY below the default parent (the parent
   itself needs `C-u`), scope re-checked after the prompt, a remote home is unregister-only with no teardown) and
   `specs/workspace-integrations/spec.md` (MODIFIED: Anchor payload contract — USER ask-cycle-1790453714-5, NARROW the clause:
   workspaces itself adds no domain keys; a caller invoking a `:menu` command directly MAY add keys that command documents, e.g.
   `:source-repo`; a consumer (portable-bookmarks) may invoke a `:menu` command directly with context `bookmark-jump`; Creation-time
   dispatch context vocabulary — the six members of `register/vocabulary/workspace-creation-context`: fresh, anchored-scaffolded,
   anchored-existing, menu-invoke, purge, bookmark-jump). Follow the main specs' requirement/scenario style; the register entries
   named in `cites_register_entries` are the source text — do not contradict them.
5. proposal § Capabilities › Modified Capabilities (P6) names `workspaces` and `workspace-integrations`; § Impact records the core
   hardening (P8) and the layout chain / bufferlo note (P9, P11); ask-cycle-1790453714-8 (scope note) is thereby recorded.
6. Also correct the README.org Restore / Save-triggers drift only if it falls out naturally (otherwise it stays in
   `.tasks/workspaces-readme-restore-and-save-drift.md`).

## Verification
- `openspec validate workspace-portable-bookmarks` passes (binary at `$(which openspec)`, present on this machine).
- Every binding decision of cycles 1-2 is greppable in the file it names — run and paste these:
  - `grep -n "remembered like any\|anti-save" openspec/changes/workspace-portable-bookmarks/design.md` → D9 says remembered; zero
    `anti-save` hits.
  - `grep -n "ambiguous" openspec/changes/workspace-portable-bookmarks/proposal.md` → the sixth outcome is listed (P2).
  - `grep -n "layouts.el\|bufferlo" openspec/changes/workspace-portable-bookmarks/proposal.md` → P9/P11 present.
  - `grep -n "broken or its home\|not an existing local directory" openspec/changes/workspace-portable-bookmarks/specs/portable-bookmarks/spec.md` → S1 carries the gate.
  - `grep -n "source-repo" openspec/changes/workspace-portable-bookmarks/specs/workspace-git-worktrees/spec.md openspec/changes/workspace-portable-bookmarks/specs/workspace-integrations/spec.md` → G2 and the narrowed clause.
  - `grep -n "user-error\|DECLINE\|decline" openspec/changes/workspace-portable-bookmarks/specs/workspaces/spec.md` → the gate is specified.
  - `grep -n "bookmark-jump\|menu-invoke\|purge" openspec/changes/workspace-portable-bookmarks/specs/workspace-integrations/spec.md` → six contexts.
- `git diff --stat -- config/` is empty for this task.
