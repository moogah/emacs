---
name: off-workspace-restore
description: When not in a workspace, prompt from the registry and restore the chosen workspace before resolving, without capturing the perturbation
change: workspace-portable-bookmarks
status: needs_review
task_class: feature
on_critical_path: true
cycle: cycle-1790453714
merge_commit: 83a022c6
cites_register_entries:
  - register/invariant/bookmark-jump-not-captured-as-working-state
  - register/boundary/portable-bookmarks-consumer
  - register/vocabulary/bookmark-resolution-outcome
relations:
  - blocked-by:jump-seam
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-off-workspace-spec.el` (new)

## Implementation steps

> Cycle 1 (cycle-1789935943): step 1 corrected — the singular discovery helper is being
> deleted (audit eoc-02); use the plural. See `.orchestrator/cycles/cycle-1789935943/reconciliations/boundary-git-derived-checkout-discovery.md`.

1. Add `workspace-bookmark--establish-workspace (identity)` called by the jump
   seam when `(workspace--current-name)` returns nil for a *stamped* record:
   - build candidates from `(workspace--registered-names)` — the registry, NOT
     live tabs, so a saved workspace with no tab in this session is offerable;
   - exclude broken workspaces: skip any whose registry plist satisfies
     `workspace--broken-p` (the package refuses to switch to or restore them and
     signals a `user-error` naming `workspace-re-anchor` / `workspace-purge`);
   - order candidates so workspaces that already hold a checkout of IDENTITY come
     first (use `workspace-bookmark--workspace-checkouts`: non-empty means the workspace
     holds one or more), since those resolve
     immediately while the rest lead straight into `missing-repo`. Annotate or
     group them so the ordering is legible, not accidental;
   - prompt with `completing-read` (`require-match`), then call
     `(workspace-restore name)` and return the chosen name;
   - return nil if the user quits.

2. `workspace-restore` already does the right thing: it creates-or-selects the
   tab, applies `:working-state` else `:saved-state`, falls back to
   `workspace-home-builder` when a workspace has no saved layout, and refuses
   broken workspaces. Call it; do not reimplement any of that.

3. Empty registry, or user quit: fall back to the same bounded choice as
   `missing-repo` — read-only canonical checkout (offered ONLY when the outcome
   carries a non-nil `:path`; ask 16 default, cycle-2 plan finding
   arch-cycle-1790453714-plan-05), or cancel. Do NOT open the
   record's stored absolute path as though it were a workspace copy; that is the
   silent wrong-file behaviour the spec forbids.

4. After a successful restore, resolve against the chosen workspace and open the
   file in the **selected window** of the restored layout (plain
   `find-file`-style display, not `pop-to-buffer` into a split).

5. Suppress working-state capture for a jump-triggered restore. The perturbation
   (one window of the restored layout replaced by the bookmark's file) must NOT
   become the workspace's `:working-state`:
   - the autosave path already consults `workspace-anti-save-predicates` before
     mutating, so add a dynamic flag this module binds during the
     restore-then-jump sequence and a predicate that returns non-nil while the
     flag is set;
   - keep the binding tight — it must cover the restore and the open, and be
     released afterwards, so ordinary later autosaves in that workspace still
     work;
   - note the deferred display in `layouts.el` (`window-state-put` runs via
     `run-at-time` to avoid racing `bookmark--jump-via`'s own display call): the
     suppression must still be in effect when that deferred restore runs, so use
     a flag with an explicit reset rather than a `let` that may unwind first.

6. Write `portable-bookmarks-off-workspace-spec.el`:
   - **Behavioral** (real git + fake registry, `completing-read` stubbed via
     `cl-letf`): from a non-workspace tab, jumping a stamped bookmark prompts,
     restores the chosen workspace, and opens the file from that workspace's
     checkout.
   - **Behavioral**: a registered workspace with no live tab appears among the
     candidates and is materialized when chosen.
   - **Unit**: a broken workspace (its `:home` deleted) is absent from the
     candidate list.
   - **Unit**: candidate ordering puts a workspace holding the identity's
     checkout ahead of one that does not.
   - **Unit**: empty registry → the bounded fallback choice is offered and the
     stored absolute path is never opened.
   - **Behavioral**: after a jump-triggered restore, the workspace's
     `:working-state` is unchanged from what it was before the jump (the
     regression guard for step 5), while `:saved-state` is untouched as always.

7. Tangle and validate.

## Design rationale

Restore beats switch because the moment bookmarks are most useful is right after a
restart, when the target workspace has no live tab yet. Prompting from live tabs
only would make bookmarks useless exactly then. The registry is the source of
truth, and `workspace-restore` is already written for lazy materialization.

Entering the workspace (rather than opening the file in the current
non-workspace tab) is required for coherence: membership is per-tab via bufferlo,
so opening the file outside the workspace would leave it not a member of the
workspace it belongs to — a half-state where the buffer is nowhere.

Not capturing the perturbation is the deliberate call: a bookmark jump is
navigation, not a statement about how the workspace should look. If it were
captured, a workspace's remembered arrangement would drift toward "whatever I last
jumped to". `:saved-state` was never at risk — autosaves never write it — but
working-state drift is still noise the user did not ask for. Rejected
alternatives: capturing it like any other window change (silent redefinition of
"where I was"), and opening in a split (an extra window on every jump, cramped on
a small frame).

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — off-workspace spec passes, and the
  existing `anti-save-spec.el` still passes (the new predicate must not disturb
  it).
- `grep -n "workspace-restore\|workspace-anti-save-predicates\|workspace--broken-p"
  config/workspaces/portable-bookmarks.el` shows all three used.
- `grep -n "workspace-switch" config/workspaces/portable-bookmarks.el` returns
  nothing (restore, not switch).
- Manual: from a fresh Emacs on a non-workspace tab, jump to a stamped bookmark;
  the prompt lists saved workspaces, the chosen one is restored, the file opens in
  the selected window, and switching tabs afterwards does not alter that
  workspace's remembered layout.

## Context
- specs/portable-bookmarks/spec.md § 'Workspace context is established before resolution'
- design.md § D8 'Off-workspace jumps restore, they do not merely switch'
- design.md § D9 'Restore-then-jump replaces the selected window and is not remembered'

## Cycle 1 updates (cycle-1789935943)

### Cited register entries
- `register/invariant/bookmark-jump-not-captured-as-working-state`: unexercised this cycle (status unchanged); this task owns its enforcement (`portable-bookmarks-off-workspace-spec.el`, the anti-save predicate). Audit finding eoc-03 records it as deferred to cycle 2.
- `register/boundary/portable-bookmarks-consumer`: speculated → confirmed. The permitted core surface now lists `workspace--home` and `jf/emacs-dir` besides registered-names / current-name / registry / broken-p / restore / anti-save-predicates; the module's ONE public symbol reachable from git-worktrees is `workspace-bookmark-canonical-checkouts-in-view` (fboundp at call time, DD-10). Stay inside that list. See `.orchestrator/cycles/cycle-1789935943/reconciliations/boundary-portable-bookmarks-consumer.md`.
- `register/vocabulary/bookmark-resolution-outcome`: speculated → reconciled (six members). **Implication**: after restore, `ambiguous` is possible; report `:candidates` and open nothing, like the seam.

### User-resolved decisions
- DD-1 (unified switch/restore semantics) and DD-2 (FILE bookmarks resolve within the current workspace; WORKSPACE/LAYOUT bookmarks switch): unchanged; this task is the FILE-kind off-workspace path.
- DD-4: layout-level commands notify ONCE; per-buffer lookups stay silent. **Implication**: one prompt, one message.

### Related follow-ups
- `discovery-identity-memo-and-cost-contract` (audit eoc-06): candidate ordering calls discovery once per registered workspace; do not add a second pass, and do not pin spawn counts in this task's specs.

## Cycle 2 plan (cycle-1790453714)

- Step 3 inherits the ask-16 narrowing: read-only view is withheld when `missing-repo` carries a nil `:path`; `add-worktree` targets `:checkout`. Whether this fallback REUSES `missing-repo-choice`'s chooser is a speculation `register/invariant/no-silent-wrong-file-open` names for this wave; if merged into one chooser, the nested action vocabulary in `register/vocabulary/bookmark-resolution-outcome` is re-stated at integrate.
- On-touch fires for `register/invariant/bookmark-jump-not-captured-as-working-state`: the entry's cycle-2 paragraph asks which event clears the explicit-reset flag, that it is ordered after the deferred `window-state-put`, and that it resets on error/quit paths.
- Never bulk-delete shared temp prefixes.

### Hard requirement from the jump-seam on-touch audit (arch-on-touch-1790453714-2, 2026-09-27)
`config/workspaces/layouts.el:203-212` (the layout reincarnation chain) catches the seam's decline (`user-error`) with `(error nil)` and
its step 2 (`:214-222`) then opens the leaf's STORED `filename` — exactly the route `register/invariant/no-silent-wrong-file-open`
prohibition 4 forbids. Behaviour is unchanged from before the seam (it was pass-through), so this is not a jump-seam regression, but
the restore flow this task builds MUST treat a seam decline as "skip this leaf's filename" (leave the window on its fallback, report
once per DD-4), never as "fall back to the stored path". Add a behavioural spec: a layout leaf whose bookmark resolves `not-on-branch`
is restored WITHOUT visiting the stored path. Coordinate with `missing-repo-choice` if the chain reaches its prompt (a restore must not
prompt per leaf; aggregate).

## Observations

- **Seam 2 was touched beyond the one arm, minimally.** `workspace-bookmark--jump-plan` gained an `&optional workspace` parameter: the off-workspace flow chooses and restores, then RE-ENTERS the plan with the chosen name, so the outcome is decided by the seam's normal table (including missing-repo-choice's arm) with no duplicated dispatch and no recursion into the off-workspace arm. The advice body gained one call, `(workspace-bookmark--note-jump-target origin)`, right after the host runs: the deferred finish step needs the buffer the host landed in, and capturing it there is handler-agnostic (a Dired directory record has no `buffer-file-name`, so locating the buffer by path afterwards would miss it). Everything else lives in the new `* Off-workspace` section (after `* Missing repo`, before `* Checkout discovery`).
- **layouts.org (core) WAS changed** — in scope per the brief, through standard Emacs conditions only, no boundary widening. `workspace--bookmark-buffer` binds `inhibit-interaction` around the leaf's `bookmark-jump` and returns `(declined . MESSAGE)` for a `user-error` (the handler declined) or `inhibited-interaction` (the handler needed to ask); `workspace--deserialize-buffer` gates on that: steps 2 (filename) AND 3 (name) are skipped and the leaf lands on the error buffer carrying the reason (a live same-name buffer is, in the same situations, the same wrong file already open). Declines are collected in `workspace--reincarnation-declines` (bound by `workspace--restore-frameset`) and reported ONCE per restore by `workspace--report-reincarnation-declines`, before the deferred paint. `workspace--error-buffer` gained an optional DECLINED argument. The four-step vocabulary is unchanged (a gate in front of the chain, not a fifth step). Core names no `workspace-bookmark-*` symbol; the module names no new core symbol — the gate keys on `user-error`, which the seam already chose, and the module honours `inhibit-interaction`, a standard variable.
- **`read-multiple-choice` cannot be left to honour `inhibit-interaction`** (measured, Emacs 30.2): rmc.el wraps its `read-event` in `(condition-case nil ... (error nil))`, swallows the `inhibited-interaction` signal and loops on the prompt forever (a probe hung; `read-from-minibuffer`, `read-string`, `read-event`, `read-char-choice` and `completing-read` all signal correctly). Coordination with missing-repo-choice, applied in its section: `workspace-bookmark--can-prompt-p` is now `(and (not noninteractive) (not inhibit-interaction))`, so the chooser declines with the seam's missing-repo report during a layout restore instead of prompting per leaf (or hanging); its prose bullet "No prompt where nobody can answer" was rewritten because it described the chain falling back to the leaf's filename, which the gate ends. Consequence for the coordinator's question about `quit`: no prompt is reached during a restore, so no cancel-`quit` can arise from the chooser; a user's C-g during a restore still aborts it, deliberately (the chain does not catch `quit`).
- **`save-restore-spec.el` fixture changed**: "restores a file buffer to the window it was saved in" used `init.el` of the configuration checkout — a tracked, STAMPED file — with workspace `alpha` (a scaffolded temp home) current. Its leaf resolved `missing-repo` and the chain's filename fallback opened the stored path anyway, which is exactly what the gate now forbids, so the spec failed with the gate (verified: passes with HEAD's layouts.el, fails with the gated one). The spec's intent is a plain file round trip, so it now uses a temp file outside any checkout (truename, because /var is a link to /private/var on macOS).
- **`portable-bookmarks-missing-repo-spec.el` structural spec end marker moved**: it delimited the `* Missing repo` section by the first form of `* Checkout discovery` (`(defvar workspace--registry)`), which now lies past my section; it ends at `(defvar workspace-anti-save-predicates)`, the Off-workspace section's first form. Without this it matched `pop-to-buffer-same-window` in my finish step.
- **Behavioural consequence the user should see (spec-signal, also filed as a discovery):** with the gate, ANY layout leaf whose file lives in a repository the restored workspace holds no checkout of — a file from the configuration checkout `jf/emacs-dir`, say — is no longer reincarnated from its stored filename; the window shows the error buffer with the reason, in a GUI too (the chooser declines under `inhibit-interaction`), and one message names every such leaf. Files in NO git repository are unaffected (unstamped → `non-portable` → pass-through), as are files the workspace does hold. This is what prohibition 4 and ask 1 (a) require, but it changes the day-to-day experience of restoring layouts that mix workspace files with config files; a follow-up could offer a per-restore "view canonical read-only" choice once, aggregated.
- **Timer ordering rationale, and its residual.** Both the layout paint and the finish step are `run-at-time nil nil`; `timer--activate` inserts a new timer after every strictly-earlier one and BEFORE any with an equal stamp. The two stamps are taken with the whole synchronous jump (tab creation, host `find-file`) between them, so the finish step fires after the paint in practice, and the spec pins the observable order (the flag is still up when `window-state-put` runs; afterwards the file is in the selected window of the painted layout). Residual: an identical stamp would flip the order — the paint would then cover the bookmark buffer (still open in the tab) and the flag would already be down; no autosave can run between two adjacent ordinary timers, so the invariant would hold and only D9's display would degrade.
- **Weakly asserted, stated:** the missing-repo-leaf chain spec replaces `workspace-bookmark--can-prompt-p` with `(not inhibit-interaction)` — the GUI stance — because the real predicate is nil in batch for the `noninteractive` reason alone; the real predicate's two clauses are pinned by a unit spec. The candidate-ordering spec asserts completion metadata (`display-sort-function` = `identity`) rather than a front-end's rendering.
- **Core unit specs added to `buffer-reincarnation-spec.el`** (the chain's own spec file): declined handler → error buffer with reason, filename and same-name buffer skipped, decline noted; asking handler → `inhibited-interaction` → decline; a plain failure still falls through to the filename; the report is one message or none.
- **Not done, by design:** the workspace picker and the missing-repo chooser were NOT merged into one prompt (design § Open Questions). The fallback REUSES `workspace-bookmark--offer-missing-repo` as the second question; the nested action vocabulary gains no workspace candidates.
- **Every new spec fails with the production change reverted.** Method: the tangled `portable-bookmarks.el` and `layouts.el` of the integration HEAD (492b5e88) were written over the worktree copies, the three spec files (`portable-bookmarks-off-workspace-spec.el`, `buffer-reincarnation-spec.el`, `portable-bookmarks-jump-spec.el`) were run against them, then both `.org`s were re-tangled. Result: 55 specs, 19 failed — every spec in the new file except the two deliberate controls ("still falls through to the filename when the handler merely FAILS", the pre-existing contract; "`can-prompt-p` is nil in a non-interactive Emacs", the pre-existing clause), plus the rewritten jump-spec arm and the three new gate specs in buffer-reincarnation-spec.el. Isolated gate with the change in place: `./bin/run-tests.sh -d config/workspaces` → 715 specs, 0 failed, exit 0.

## Discoveries

- discovery_id: disc-off-workspace-restore-1
  class: invariant-gap
  description: |
    The speculation is settled by code. Mechanism: an explicit-reset flag,
    `workspace-bookmark--restore-in-progress` (a module defvar, never
    `let`-bound). Set by `workspace-bookmark--restore-for-jump` before
    `workspace-restore`. CLEARING EVENT: `workspace-bookmark--finish-restore`,
    a one-shot `run-at-time nil nil` timer queued AFTER `workspace-restore`
    returns — hence after the restore's own deferred `window-state-put`
    timer (timer list ordered by stamp) — which also displays the jumped-to
    buffer in the selected window of the painted layout (D9). The "predicate's
    own first hit" alternative was rejected: the first autosave after the
    jump may be the user's next ordinary tab switch, which must proceed.
    RESET ON EVERY PATH: the finish step is queued from an `unwind-protect`
    cleanup around `workspace-restore` (normal, error and quit), and its own
    body is `unwind-protect`ed. EXACTLY ONE predicate,
    `workspace-bookmark--suppress-autosave-p`, added with `add-hook` in
    `workspace-bookmark-install-advice` and removed in
    `workspace-bookmark-remove-advice` (guarded on `boundp`, so an unbound
    defcustom is never created ahead of the package). It keys on the flag
    only. Validators 0-4: portable-bookmarks-off-workspace-spec.el exists;
    "paints the saved layout first ..." asserts the flag is up when
    `window-state-put` runs, :working-state unchanged after a suppressed
    autosave attempt, :saved-state untouched, and a later ordinary autosave
    writes :working-state again (leak guard); "continues through the seam's
    normal path ..." and "declines ... when the restore itself fails" pin the
    reset on the decline and error paths.
  affected_register_entry: register/invariant/bookmark-jump-not-captured-as-working-state
  recommendation: |
    speculated -> reconciled-recommended. Record the clearing event
    (`workspace-bookmark--finish-restore`, queued after the restore) and the
    unwind-protect queuing as the mechanism; keep validators 1-4 as written;
    note the residual that an identical timer stamp would only degrade D9's
    display, not the invariant.

- discovery_id: disc-off-workspace-restore-2
  class: invariant-gap
  description: |
    layouts.el's reincarnation chain is a FOURTH actor of the invariant and
    is now gated: `workspace--deserialize-buffer` treats a `user-error` (or
    `inhibited-interaction`) from the leaf's bookmark jump as a decline —
    steps 2 and 3 skipped, error buffer with the reason, one aggregated
    report per restore — instead of opening the stored filename. Prohibition
    4's off-workspace fallback: with no workspace chosen (empty registry or
    quit) the record is resolved with no workspace and `missing-repo` goes to
    `workspace-bookmark--offer-missing-repo` (add-worktree withheld, view only
    when :path non-nil, cancel); ambiguous/no-clone decline; nothing calls
    through. Speculation (d) settled: the chooser is REUSED, the prompts are
    NOT merged (workspace picker, then the chooser), so the nested action
    vocabulary gains no workspace candidates. New specs:
    portable-bookmarks-off-workspace-spec.el "layout reincarnation chain"
    (not-on-branch leaf and missing-repo leaf restored without visiting the
    stored path, one report, no prompt) and "empty registry" (chooser reached
    with {view read-only, cancel}; view opens read-only and marked; view
    withheld when :path is nil; cancel opens nothing).
  affected_register_entry: register/invariant/no-silent-wrong-file-open
  recommendation: |
    Add layouts.el (`workspace--deserialize-buffer`) as the fourth actor with
    a validator: "a layout leaf whose bookmark declines is restored WITHOUT
    `find-file-noselect` on the stored filename and without `get-buffer` by
    name" (buffer-reincarnation-spec.el "the gate", off-workspace-spec.el
    "layout reincarnation chain"). Record speculation (d) as: reused, not
    merged. Consider promoting the user-visible consequence (a layout leaf
    from a repository the workspace does not hold lands on an error buffer
    even in a GUI) to a user ask.

- discovery_id: disc-off-workspace-restore-3
  class: interface-drift
  description: |
    The chain vocabulary entry describes a four-step `or` chain. The steps
    and their order are unchanged (still four, `workspace--valid-reincarnation-steps`
    untouched), but a GATE now sits between step 1 and step 2: a declined
    step 1 short-circuits to step 4 (the error buffer, with the handler's
    reason). Also new: `workspace--reincarnation-declines` (per-restore
    collector) and `workspace--report-reincarnation-declines` (one message),
    both in layouts.el; `workspace--error-buffer` takes an optional DECLINED.
  affected_register_entry: register/vocabulary/buffer-reincarnation-fallback-chain
  recommendation: |
    Re-state the entry: "step 1 has three answers — a buffer, a failure
    (fall through to 2), or a decline (jump to 4 with the reason)". Not a
    fifth member.

- discovery_id: disc-off-workspace-restore-4
  class: interface-drift
  description: |
    No widening of the permitted core surface: the module now actually uses
    `workspace-restore`, `workspace--broken-p` (via the registry plist) and
    `workspace-anti-save-predicates` (add-hook/remove-hook next to the
    advice), all on the list. One NEW cross-module convention, carried by a
    standard Emacs variable rather than a symbol of either side: core binds
    `inhibit-interaction` around each leaf's bookmark jump during a layout
    restore, and the module's `workspace-bookmark--can-prompt-p` honours it
    (declining instead of prompting). The reverse check — the module never
    infers "inside a restore" from core internals or the call stack — holds.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Add to directionality: "core signals 'no interaction' to handlers only
    through `inhibit-interaction`; the module reads only that". Validator:
    grep -n 'inhibit-interaction' layouts.el (bound once) and
    portable-bookmarks.el (read in `--can-prompt-p` only).

- discovery_id: disc-off-workspace-restore-5
  class: spec-signal
  description: |
    Emacs 30.2 defect relevant to every prompt in this module:
    `read-multiple-choice` does not honour `inhibit-interaction` — rmc.el
    catches the `inhibited-interaction` error around `read-event` and loops
    on the prompt forever (measured with a probe; `completing-read`,
    `read-string`, `read-event`, `read-char-choice` signal correctly). Any
    caller that may run under `inhibit-interaction` must pre-check the
    variable, as `workspace-bookmark--can-prompt-p` now does.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    Note under the nested missing-repo action set that the chooser's guard
    is `workspace-bookmark--can-prompt-p` = interactive AND not inhibited,
    and why. Candidate for an upstream Emacs bug report.

## Fix pass (cycle-1790453714)

This fix pass covers review Findings 1, 2, 4 and 5 and architect on-touch findings 37, 38, 40, 41, 42, 43 and 45, under the binding user decisions of 2026-09-27 (asks 1, 2 and 3).

### Observations

- **Core binds no `inhibit-interaction` (review 1, arch 37/41).**
  - `layouts.el` now binds one neutral, core-owned variable around each leaf's `bookmark-jump`: `workspace--restoring-layout-leaf` (a `defvar` in layouts.org § reincarnation; docstring "Non-nil while a layout leaf is being restored; consumers must not prompt"). It suppresses nothing by itself.
  - Stock handlers behave exactly as before this task. The large-file question (`read-multiple-choice`), the unsafe-local-variables query and `vc-follow-symlinks` all ask as they would on any other visit.
  - The seam's `workspace-bookmark--can-prompt-p` now reads the variable, `boundp`-guarded. It still also checks `noninteractive` and `inhibit-interaction`. So the missing-repo chooser still declines, with the seam's own `user-error`, for every leaf of a layout restore.
  - `grep -n inhibit-interaction config/workspaces/layouts.el` is empty. Core names no `workspace-bookmark-*` symbol.
- **How core now tells "declined" from "failed".** `workspace--bookmark-buffer` has exactly three answers:
  - a buffer other than the probe → success;
  - a `user-error` out of the jump → `(declined . MESSAGE)`;
  - any other `error`, or a jump that returns without moving off the probe buffer → nil, meaning FAILED.

  `quit` propagates. The `inhibited-interaction` arm is gone: it existed only because core bound `inhibit-interaction` itself. If an outer caller binds that variable, an `inhibited-interaction` from a leaf is now an ordinary error, i.e. a failure, which was the pre-task behaviour.

  The dispatch in `workspace--deserialize-buffer` now has one arm per answer:
  - `bufferp`;
  - `declined` → the error buffer with the reason;
  - `(null jumped)` → steps 2, 3 and 4 in order;
  - a `t` arm that SIGNALS "Not a reincarnation step-1 result". That signal lands in `workspace--apply-saved-layout`'s barrier ("layout restore failed"), so an unexpected value can never reach the filename step (arch 42).

  The decline protocol is the seam's own `user-error`. It keys on the standard condition alone, so a third-party handler's `user-error` is a decline too. That is deliberate (arch 41's "accept and record") and is in the Discoveries below.
- **Jump target is the OUTER jump's only (review 2, arch 38).**
  - `workspace-bookmark--restore-for-jump` raises `workspace-bookmark--awaiting-jump-target` and clears the target only AFTER `workspace-restore` has returned. The restore's own leaf jumps pass through the seam while the flag is down and record nothing.
  - The first host call after that (the outer jump's) calls `workspace-bookmark--note-jump-target ORIGIN ARG`, which lowers the flag. It records the current buffer when `workspace-bookmark--landed-p` says the jump landed: the buffer changed, OR it visits ARG's location. The second clause covers a jump whose file is the very buffer the new tab inherited (arch 38 probe G).
  - A decline or cancel after the restore never reaches the host, so nothing is recorded and the painted layout stays as painted.
  - Residual, stated: a DIRECTORY bookmark whose Dired buffer is the inherited buffer is not recorded, because Dired has no `buffer-file-name`. The paint then covers it. This is rare and harmless: nothing wrong is opened.
- **Timer ordering by construction (arch 40).** The finish step is queued with `(run-at-time workspace-bookmark--finish-delay nil ...)`, where the delay is 0.001 s, not `nil`. The paint was stamped earlier by `run-at-time nil`, so the finish time is strictly later, and timer.el's time-sorted list fires the paint first. Equal times fire newest first, which the spec forces.
  - Remaining residual: a wall clock stepped backwards by more than 1 ms between the two calls.
  - Chaining from the paint was rejected: core would have to call back into the module or grow a hook.
- **Ask 3: the anti-save mechanism is gone.**
  - Removed: `workspace-bookmark--suppress-autosave-p`, its `add-hook`/`remove-hook` in `workspace-bookmark-install-advice`/`-remove-advice` (the advice section is now byte-identical to its pre-task text), the `(defvar workspace-anti-save-predicates)` declaration, and `workspace-bookmark--restore-in-progress`.
  - WHAT THE FLAG STILL DOES: the renamed `workspace-bookmark--awaiting-jump-target` does one thing only. It scopes the deferred display target to the outer jump, from the return of `workspace-restore` until the outer jump's host call or the finish step. It has nothing to do with autosave.
  - `grep -n suppress-autosave config/workspaces/*.el` is empty.
  - Superseded by the decision, and NOT edited here (out of remit): design D9's "is not remembered" half, the register invariant `bookmark-jump-not-captured-as-working-state`, and this task's step 5, its step-6 regression-guard bullet and its manual check ("switching tabs afterwards does not alter that workspace's remembered layout"). D9's display half stands: paint, then show in the selected window, no split.
  - A live session that loaded the old module keeps the old predicate symbol in `workspace-anti-save-predicates` until restart. It returns the old flag, which is nil, so it is harmless.
- **Off-workspace edges (arch 45).**
  - The workspace picker now sits behind `workspace-bookmark--can-prompt-p`. When workspaces are offerable but no one can answer, the jump declines: "no workspace is current, and no one can be asked which to restore; nothing opened".
  - C-g at the picker is no longer caught. `quit` ends the jump: nothing is restored, opened or asked, and there is no second prompt (DD-4).
  - An EMPTY answer is still "no workspace" and takes the fallback. That fallback may put the missing-repo chooser, which is a second question, but for a deliberate "none" answer.
  - `choose-workspace` now takes the candidate list, so offerability is computed once, in `off-workspace-plan`.
  - **`non-portable` policy: unchanged, still declines.** A record reaches the off-workspace plan only when `workspace-bookmark--stamped-coordinates` has read a usable, confined stamp. After that the resolver never answers `non-portable` except through its pre-identity error barrier, whose `:path` is the record's stored filename. Calling through would open a stamped record's stored absolute path, which is prohibition 4. That makes declining the invariant, not a defect.
- **Prose (review 4, arch 42).** Updated:
  - the seam docstring (call-through is `non-portable` only; the off-workspace case goes through the establish-then-resolve plan and never calls through with the original argument);
  - the Overview's "Scaffold status" paragraph, now "Status", describing the live seams;
  - the Boundary list (the module reads `workspace--restoring-layout-leaf` and no longer names `workspace-anti-save-predicates`);
  - the reincarnation defconst docstring and the org sentence before it (the order is load-bearing via the dispatch, gated by step 1's decline);
  - layouts.org § "A decline is not a failure" (the three answers, the signalling default, "Asking is not declining");
  - the chain section of `config/workspaces/docs/README.org` (three answers of step 1, the decline gate, and that restoring a leaf suppresses no questions).
- **Scope in `* Missing repo`.** The only touch there is `workspace-bookmark--can-prompt-p`, its `(defvar workspace--restoring-layout-leaf)` declaration and its prose bullet, as fix item 1 requires. The chooser itself, the discovery sections and `git-worktrees.org` are untouched.
- **Spec hygiene (review 5, arch 43).**
  - New `workspace-bookmark-fixtures-reset-jump-state` in the fixtures, called first in `workspace-bookmark-fixtures-teardown`. It cancels `workspace-bookmark--finish-restore` timers and clears both jump globals, all guarded so it is safe before the module loads.
  - The off-workspace spec's `--reset-frame` calls it too.
  - The rewritten jump-spec arm now lifts the prompt guard and asserts that its finish step IS queued. A following spec asserts that nothing is pending after the teardown.
  - `portable-bookmarks-missing-repo-spec.el`'s structural end marker moved to `(declare-function workspace--broken-p `, the Off-workspace section's first form now that the `defvar` it keyed on is gone.
- **Spec changes.**
  - buffer-reincarnation-spec.el, "the gate": the old "handler that tries to ask is a decline" spec (y-or-n-p under `inhibit-interaction`) is replaced by three specs:
    - the large-file regression spec: a plain file record over the threshold goes through the REAL `read-multiple-choice`, with only `read-event` stubbed. The stub answers ?y, honours `inhibit-interaction` as the C primitive does, and throws after 5 calls. The leaf is asked exactly once and restores.
    - `workspace--restoring-layout-leaf` is bound around the jump only.
    - the signalling default arm.

    Why stub `read-event` rather than rmc: an rmc stub that simply answers would pass against the old code too, because the hang is inside rmc's own loop.
  - portable-bookmarks-off-workspace-spec.el:
    - The jump helper answers the prompt guard as an interactive Emacs would: whatever `--can-prompt-p` is at call time, with `noninteractive` masked, so it still refuses during a leaf restore.
    - The "paints the saved layout first" spec is rewritten. It checks paint-then-finish order by events, that no anti-save predicate fires right after the jump, and that a REAL `tab-bar-select-tab 1` then writes `:working-state` whose leaves include the jumped-to file, with `:saved-state` untouched.
    - New specs: timers forced equal (every `run-at-time` with a nil TIME gets one fixed time); a decline after restoring a two-leaf bookmarked layout keeps the painted windows; a jump from the inherited buffer shows the file; C-g at the picker ends the jump with no second prompt (the canonical checkout is in view, so the fallback WOULD ask); no picker when no one can answer. The old "quit is no workspace" spec is replaced by the C-g spec.
    - The missing-repo leaf chain spec now uses the guard with `noninteractive` masked. The `--can-prompt-p` unit gains a "nil while core restores a layout leaf" case.
- **Every new spec fails at e42e7a99.**
  - Method: the integration HEAD's tangled `layouts.el` and `portable-bookmarks.el` were written over the worktree copies, and the four spec files were run through the harness (`buffer-reincarnation-spec.el`, `portable-bookmarks-jump-spec.el`, `portable-bookmarks-missing-repo-spec.el`, `portable-bookmarks-off-workspace-spec.el`). Then the fixed `.el`s were restored and compared with `cmp` against a fresh re-tangle.
  - Result: 85 specs, 16 failed:
    - rmc: asked 6 times, the spin;
    - leaf variable seen as nil;
    - the default arm returned the filename's buffer;
    - anti-save predicate t right after the jump;
    - the two-leaf decline painted the wrong windows;
    - the equal-times order: the selected window showed no file;
    - the inherited buffer: no file shown;
    - C-g reached the chooser;
    - the no-one-can-answer case prompted;
    - `can-prompt-p` t during a leaf restore;
    - six specs that name the renamed flag, which is void there.
  - The jump-spec "nothing pending" spec fails at e42e7a99 on the void flag. A second demonstration isolates its real content: with the FIXED module but the teardown's reset replaced by `(ignore)`, it fails on the queued finish timer.
  - With the change in place the same four files pass: 85 specs, 0 failed.
- **Gates.**
  - `./bin/tangle-org.sh` exits 0 on `layouts.org` and `portable-bookmarks.org`, and a re-tangle is byte-identical.
  - `./bin/run-tests.sh -d config/workspaces </dev/null`: 726 specs, 0 failed, exit 0.
  - The full suite's report goes through `compare-baseline.py`: NO-REGRESSION (ERT 9 failing and Buttercup 5 failing, all baseline names).

### Discoveries

- discovery_id: disc-off-workspace-restore-fix-1
  class: interface-drift
  description: |
    Core exposes ONE new symbol to consumers: `workspace--restoring-layout-leaf`
    (layouts.el defvar, bound t by `workspace--bookmark-buffer` around each
    layout leaf's `bookmark-jump`, nil otherwise). The module reads it,
    boundp-guarded, in `workspace-bookmark--can-prompt-p` only. Core no
    longer binds `inhibit-interaction`. The module no longer names
    `workspace-anti-save-predicates` (ask 3). Core protocol, for EVERY
    bookmark handler: a `user-error` out of a leaf jump is a DECLINE (reason
    buffer, no filename or name fallback, one report per restore); any other
    error is a failure (falls through as before); prompts are not suppressed.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Add `workspace--restoring-layout-leaf` to directionality.permitted (read
    only, boundp-guarded); drop or mark unused `workspace-anti-save-predicates`.
    Add a core_protocol field with the sentence above. Validator:
    `grep -n inhibit-interaction config/workspaces/layouts.el` empty;
    `grep -n restoring-layout-leaf config/workspaces/portable-bookmarks.el`
    shows only `--can-prompt-p` and its defvar.

- discovery_id: disc-off-workspace-restore-fix-2
  class: spec-signal
  description: |
    User ask 3 (2026-09-27) reverses the entry's statement: a jump-triggered
    restore IS remembered like any find-file in the workspace. The anti-save
    predicate and the flag it read are deleted. What remains of the
    restore-then-jump mechanism is display only: the layout is painted, then
    the outer jump's buffer is shown in its selected window, by
    `workspace-bookmark--finish-restore`. That step is queued
    `workspace-bookmark--finish-delay` (1 ms) after the restore returns, which
    is strictly later than the paint. The target is recorded only from the
    outer jump (`workspace-bookmark--awaiting-jump-target`, raised after
    `workspace-restore` returns).
  affected_register_entry: register/invariant/bookmark-jump-not-captured-as-working-state
  recommendation: |
    Retire the entry, or restate it as "restore-then-jump display order and
    target scoping". Proposed statement: the paint precedes the display by a
    strictly later timestamp; only the outer jump sets the target; a declined
    or cancelled jump leaves the painted layout as painted; nothing suppresses
    autosave. Validators are portable-bookmarks-off-workspace-spec.el:
    "paints the saved layout first ...", "keeps the paint-then-show order
    when the two timers read the same clock value", "leaves the painted
    layout alone when the jump declines ...", "shows the file when the jump
    starts from a buffer visiting it ...". design.md D9's "not remembered"
    half needs the same restatement.

- discovery_id: disc-off-workspace-restore-fix-3
  class: interface-drift
  description: |
    Step 1 of the chain has a closed three-member result set: a buffer; nil
    (FAILED: an `error` other than `user-error`, or no buffer change); or
    `(declined . MESSAGE)` (a `user-error`). The dispatch tests `null` for the
    failure arm and SIGNALS for any other value. Asking is not declining: the
    leaf jump runs with `workspace--restoring-layout-leaf` bound, and nothing
    else is changed.
  affected_register_entry: register/vocabulary/buffer-reincarnation-fallback-chain
  recommendation: |
    Apply arch-on-touch-1790453714-42's paragraph with these changes:
    - member `bookmark`: "runs with `workspace--restoring-layout-leaf` bound;
      a `user-error` is a decline; prompts are asked as on any visit" (NOT
      "runs with inhibit-interaction bound");
    - canonical_mapping_function: the `cond` with a `null` failure arm and a
      signalling default;
    - drop the OPEN note about `read-multiple-choice`.
    The test corpus gains three specs in buffer-reincarnation-spec.el "the
    gate": the large-file prompt asked once, the leaf variable scope, and the
    signalling default.

- discovery_id: disc-off-workspace-restore-fix-4
  class: invariant-gap
  description: |
    Three off-workspace policies are now settled in code and spec:
    - C-g at the workspace picker ends the jump with `quit`: nothing is
      restored, opened or asked, and there is no fallback prompt.
    - An empty answer is "no workspace" and takes the fallback.
    - The picker is behind `workspace-bookmark--can-prompt-p`, and declines
      when no one can answer.
    `non-portable` after a restore still declines. It is reachable only
    through the resolver's pre-identity error barrier, whose :path is the
    stored filename, so declining it is prohibition 4, not a loss.
  affected_register_entry: register/invariant/no-silent-wrong-file-open
  recommendation: |
    Cancel wording: "C-g at the off-workspace workspace prompt aborts the
    jump (quit), like cancel at the missing-repo prompt; an empty answer is
    'no workspace'". Record that off-workspace `non-portable` declines, and
    why. Validators are off-workspace-spec's "ends the jump on C-g ..." and
    "puts no workspace prompt where no one can answer ...".

- discovery_id: disc-off-workspace-restore-fix-5
  class: invariant-gap
  description: |
    Module globals outside the registry now have a fixtures-owned reset:
    `workspace-bookmark-fixtures-reset-jump-state`, run first by
    `workspace-bookmark-fixtures-teardown`. It cancels
    `workspace-bookmark--finish-restore` timers and clears
    `workspace-bookmark--awaiting-jump-target` and
    `workspace-bookmark--jump-target`. The jump spec pins it: the off-workspace
    arm leaves a finish step queued, and the next spec sees none.
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    Record the generalisation as resolved for these globals. The reset has
    one owner, the fixtures teardown.
