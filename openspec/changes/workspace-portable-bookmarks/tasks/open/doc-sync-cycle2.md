---
name: doc-sync-cycle2
description: Bring proposal.md, design.md and the delta specs in line with the code and this cycle's user decisions; add the two missing delta specs (workspaces, workspace-integrations)
change: workspace-portable-bookmarks
status: needs_review
merge_commit: 24496974
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

## Observations

- Applied P1 (proposal § Stamp: three properties, all-or-nothing, tracked-only, `./` root; current phrase "gains two extra" verified present before edit).
- Applied P2 (proposal § outcomes: six outcomes, `ambiguous` added; `missing-repo` narrowed to exactly-one-canonical, read-only only when the file exists there, reported when no one can be asked; `no-clone` reported, nothing opened).
- Applied P3 (proposal § Off-workspace: broken not offered, holders first, resolves without a workspace, never opens the stored path).
- Applied P4 (proposal § Discovery: no recursive scan, two in-memory memos, config-mtime revalidation, DD-7).
- Applied P5 (proposal § BREAKING: magit list the only other source, projectile/project.el dropped, DD-12, single admission predicate).
- Applied P6 (proposal § Modified Capabilities: git-worktrees reworded; `workspaces` and `workspace-integrations` added as modified capabilities — rendered as two sibling bullets rather than one run-on sentence, same content).
- Applied P7 (proposal § New code: loaded from `workspaces.org` after `workspaces-mode.el`, before `workspaces-transient.el`; flat per-topic spec files + fixtures; verified against `config/workspaces/workspaces.org:142-152` and `ls config/workspaces/test/`).
- Applied P8 (proposal § Modified code: init.org line dropped; git-worktrees line rewritten; workspaces-core files listed).
- Applied P9 (proposal § Integration points: layout restore passes the seam per leaf; bufferlo does not).
- Applied P10 (proposal § Dependencies: the extra core symbols and the soft `jf/workspace--add-worktree` call; each symbol verified present in `portable-bookmarks.el` by grep).
- Applied P11 (proposal: `workspaces.eld` item moved out of "Not affected" into a new **Workspace layouts** paragraph with the reworded text).
- Applied D-a (design Non-Goals: projectile/project.el stop being repo sources, DD-12).
- Applied D-b (design D1: core binds `workspace--restoring-layout-leaf`, `user-error` is a DECLINE, module declines instead of prompting per leaf).
- Applied D-c (design D3: origin preferred, single-remote fallback, `git remote get-url` honours `insteadOf`, userinfo/port/slash runs dropped; verified at `portable-bookmarks.el:108-116,142`).
- Applied D-d (design D5 table: `--show-prefix` + `HEAD:./` on the rev-parse row, and rows for `git remote`, `ls-files -z` in the candidate, `rev-parse --show-toplevel` in the candidate, `symbolic-ref --short --quiet HEAD`; verified at `portable-bookmarks.el:296-299,736,1282,1346`).
- Applied D-e (design D5 prose: same text as P4).
- Applied D-f (design D6: workspace-owned by file identity in either direction; same-repo contained home confers nothing, DD-13; symlinks confer nothing, DD-11).
- Applied D-g (design D8: broken filtered before the prompt; declines without prompting when no one can be asked). Kept the existing clause describing `workspace-restore`'s own refusal, since the corrected text refers back to it ("the restore's refusal is never reached from here").
- Applied D-h (design D9 rewritten per ask-cycle-1790453714-3 (b): title without "and is not remembered"; paint-then-show, remembered like any find-file; outer-jump-only target scoping added from the reconciled entry's statement 2; capture-rejection alternative and the autosave-suppression implementation note deleted; "open in a split" alternative kept). DEVIATION from the audit's verbatim parenthetical: it reads "the anti-save flag was inert and was removed", but the task's own verification requires zero `anti-save` hits in design.md, so the parenthetical says "the autosave-suppression flag was inert and was removed" — same fact, different spelling.
- Applied D-i (design D11: seven requirements, per-topic spec files; the delta spec has seven `### Requirement:` headings).
- Applied D-k (design Risks › latency: identity memo + per-call discovery memo; floor of 3 spawns per canonical repo and 1 per further checkout, matching `register/boundary/git-derived-checkout-discovery` cost_contract).
- Applied D-l (design Migration: rollback is the `jf/load-module` line in `workspaces.org` or `M-x workspace-bookmark-remove-advice`; the command exists at `portable-bookmarks.el:1823`).
- Applied D-m (design Open Questions: resolved, prompts stay separate).
- SKIPPED D-j by instruction (design § Risks › "Advice on core bookmark functions"); its text is unchanged and its "current" phrase is still present for `outcome-action-mapping` to amend.
- Applied S1 (portable-bookmarks spec item 3: gate wording from task step 3 verbatim, including the two withheld reasons the gate task adds; two new scenarios "Add-worktree is withheld for a broken current workspace" and "Add-worktree is withheld when the home vanished mid-session" placed beside "Missing repo offers a bounded choice").
- Applied S2 (requirement "Workspace context is established before resolution" and scenario "Empty registry falls back, not through").
- Applied S3 (scenario "Cancel opens nothing": ends with `quit`).
- Applied S4 (requirement "Discovery without configuration" and scenario "Git failure degrades rather than signalling": `user-error` naming the bookmark).
- Applied S5 (requirement "Checkout classification": workspace-owned by file identity).
- Applied G1 (git-worktrees delta "Soft repo-source selection": each checkout contributes its repository's main worktree).
- Applied G2 (git-worktrees delta: new MODIFIED "Worktree creation off main" carrying the main spec's full text with step 1 rewritten for `:source-repo`, plus scenarios "A payload naming the source repo skips the source prompt" and "A source repo that is not a local directory is refused"; verified against `git-worktrees.el` `jf/workspace--add-worktree`).
- NEW `specs/workspaces/spec.md`: MODIFIED "Buffer reincarnation across restart" (three-answer step 1, decline gate skips steps 2-3, reason buffer, one aggregated report, `workspace--restoring-layout-leaf` bound around the jump, closed answer set; four new scenarios), "workspace-new default scaffolding" (single-directory-name, taken-name and remote-home refusals), "Anchoring an existing directory via prefix arg" (remote, same-home-by-identity and taken-basename refusals), "workspace-purge as the destructive deletion command" (STRICTLY below the default parent — the parent itself needs `C-u`; broken home judged by nearest existing ancestor; scope re-checked after the prompt; remote home unregister-only with no teardown), "Broken home directory tolerated on restore" (a remote `:home` is loaded broken with one warning and never contacted). Choice: the task's "persistence — a remote :home is loaded broken and never contacted" is recorded under "Broken home directory tolerated on restore" rather than by copying the ~100-line "Per-machine persistence and restoration" requirement; that requirement already defers broken handling to this one.
- NEW `specs/workspace-integrations/spec.md`: MODIFIED "Anchor payload contract" (one constructor; workspaces itself adds no domain keys; a consumer calling a `:menu` command directly MAY append keys that command documents, `:source-repo`; bound by the not-broken current-workspace condition) and "Creation-time dispatch" (the six-member closed `:context` vocabulary: fresh, anchored-scaffolded, anchored-existing, menu-invoke, purge, bookmark-jump; three new scenarios).
- Step 6 (README.org Restore / Save-triggers drift) did NOT fall out naturally — none of the corrections touch `config/workspaces/docs/README.org`; it stays in `.tasks/workspaces-readme-restore-and-save-drift.md`. `git diff --stat -- config/` is empty.
- Latent, out of scope (not in the audit list): design D5's last sentence still says the scanning cold path would be "replacing projectile for *creating* worktrees", although projectile is already dropped as a source (DD-12, D-a). Left as is; a one-clause follow-up for whoever next touches D5.
- Latent, out of scope: design D6's canonical bullet still names `workspace-bookmark--offerable-canonical-p` as the shared predicate; not verified here and not in the audit list.

## Discoveries

- discovery_id: disc-doc-sync-cycle2-1
  class: spec-signal
  description: |
    The main spec openspec/specs/workspaces/spec.md § "Buffer reincarnation
    across restart" step 1 says the leaf jump is `bookmark-handle-bookmark`
    on the saved record; the code (layouts.el:246, `workspace--bookmark-buffer`)
    calls `bookmark-jump`, which is what makes every leaf pass a consumer's
    seam on the handler dispatch. The new delta spec states `bookmark-jump`
    (dispatching through `bookmark-handle-bookmark`) so that opsx-sync
    corrects the main spec; no register entry is contradicted (the entry
    already says `bookmark-jump`).
  affected_register_entry: register/vocabulary/buffer-reincarnation-fallback-chain
  recommendation: |
    No entry change. At integrate, re-state the entry's delta-spec target
    from "NEW specs/workspaces/spec.md" to the merged file, and let opsx-sync
    carry the step-1 wording into the main spec.
- discovery_id: disc-doc-sync-cycle2-2
  class: spec-signal
  description: |
    The main spec openspec/specs/workspace-integrations/spec.md scenario
    "payload carries no integration-domain data" is stated for "any
    integration handler", which the direct consumer call with `:source-repo`
    now contradicts. The delta narrows the scenario to registry dispatch and
    the transient, and adds the direct-call scenario, per ask-cycle-1790453714-5.
    The register entry's `optional: :source-repo` note ("absent from every
    registry-dispatched payload") is what the delta encodes.
  affected_register_entry: register/shape/workspace-integration-anchor-payload
  recommendation: |
    No entry change. Re-state the entry's doc target to the merged delta at
    integrate; load_bearing stays false.
