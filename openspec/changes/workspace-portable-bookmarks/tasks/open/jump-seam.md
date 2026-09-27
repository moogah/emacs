---
name: jump-seam
description: Substitute a resolved copy of the record at bookmark-handle-bookmark, never mutating the stored record
change: workspace-portable-bookmarks
status: needs_review
task_class: feature
on_critical_path: true
cycle: cycle-1790453714
merge_commit: 99b335fd
cites_register_entries:
  - register/boundary/bookmark-advice-seams
  - register/invariant/stored-record-never-mutated-by-jump
  - register/vocabulary/bookmark-resolution-outcome
relations:
  - blocked-by:stamp-seam
  - blocked-by:resolver-outcomes
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-jump-spec.el` (new)

## Implementation steps

> Cycle 1 (cycle-1789935943): step 2 corrected — the resolver returns SIX outcomes
> (DD-14 added `ambiguous`) in a six-key plist; see `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`.

1. Install advice on `bookmark-handle-bookmark` (`bookmark.el:1339`). It is the
   choke point called by `bookmark--jump-via`, so it covers `bookmark-jump`,
   `bookmark-jump-other-window` and completion front-ends such as
   `consult-bookmark`. Do NOT advise `bookmark-jump` — that misses the others.

2. The advice receives `BOOKMARK-NAME-OR-RECORD`, which may be a string name or a
   record. Normalize with `bookmark-get-bookmark` so both forms are handled, then:
   - call `(workspace-bookmark--resolve record (workspace--current-name))`; it returns
     `(:outcome :path :identity :relpath :candidates :checkout)` and never signals
     (only `quit` propagates) and it ALWAYS returns that six-key plist — it is
     never nil (portable-bookmarks.el:992-1042 at b56f385a; corrected at cycle-2
     plan, finding arch-cycle-1790453714-plan-01). The off-workspace hand-off is
     therefore decided BEFORE calling the resolver: when
     `(workspace--current-name)` is nil AND the record is stamped
     (`workspace-bookmark--stamped-coordinates` non-nil), hand off to the
     off-workspace flow (task `off-workspace-restore`) — until that task lands,
     pass through unchanged in that case. Unstamped records off-workspace are
     ordinary `non-portable` pass-throughs.
   - `non-portable` → call through with the original argument untouched.
   - `resolved` → build a **copy** of the record with `copy-tree`, substitute the
     resolved absolute path into its `filename` property with
     `bookmark-prop-set`, and call through with the copy.
   - `missing-repo` → hand off to the prompt (task `missing-repo-choice`); until
     that lands, report and open nothing.
   - `ambiguous` → report the `:candidates` (2+ checkouts of the identity in this
     workspace, or 2+ canonical clones) and open nothing; nothing is chosen for the user.
   - `not-on-branch` / `no-clone` → report via `message` (or `user-error` if the
     jump must not continue) and do NOT call through. The key rule: never let
     Emacs visit a nonexistent path as a new empty buffer.

3. **Never mutate the stored record.** Use `copy-tree` on the record before
   substituting, so the copy shares no structure with the stored alist entry.
   `bookmark-save-flag` is `1`, so an in-place edit would be written to disk and
   the absolute fallback — the thing that makes every degraded outcome safe —
   would be destroyed on first use.

4. Preserve the native handler: the copy keeps its own `handler` property, so
   dired, org, pdf-view and third-party handlers continue to do their own
   restoration. Do not set a handler of our own.

5. Wrap the advice body in `condition-case`: on any internal error, call through
   with the original argument. A broken resolution must degrade to today's
   behaviour, never break the jump. One bad record must not affect other
   bookmarks or the bookmark file.

6. Write `portable-bookmarks-jump-spec.el`:
   - **Behavioral** (real git, two worktrees, fake registry, temp bookmark file):
     stamp a bookmark from worktree A, make worktree B the current workspace,
     jump, and assert the opened buffer's file is B's copy. Then assert the
     stored record's `filename` is **still** A's path — the regression guard for
     the copy rule.
   - **Behavioral**: jumping an unstamped record opens its stored absolute path
     unchanged.
   - **Behavioral**: a dired (directory) record resolves into the current
     workspace's checkout and is opened by the dired handler (assert the buffer's
     major mode).
   - **Behavioral**: `bookmark-jump-other-window` goes through the same
     resolution (proves the choke point is right).
   - **Behavioral**: `not-on-branch` creates no buffer — assert no buffer visits
     the missing path afterwards.
   - **Unit**: force an error inside the resolver with `cl-letf`; the jump still
     opens the stored absolute location.

7. Tangle and validate.

## Design rationale

Three assumptions were verified in `emacs -Q --batch` before this design, and this
task is where the third one pays off: a modified *copy* handed to the jump
machinery opened a different file at the recorded position while the stored
record's `filename` stayed unchanged. That is the whole mechanism.

A custom bookmark handler was rejected: it would *replace* dired's, org's or
pdf-view's handler and we would have to reimplement their restoration behaviour.
Substituting one property inside the record and letting the native handler run is
strictly less invasive — every handler still receives the bag it expects.

Refusing to visit a nonexistent path is a deliberate departure from Emacs's
default helpfulness. Normally visiting a missing file is how you create one; here
it would mean silently starting a new empty buffer for a path that exists on a
different branch, which is precisely the class of quiet wrongness this change
exists to remove.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — jump spec passes.
- `grep -n "bookmark-handle-bookmark\|copy-tree"
  config/workspaces/portable-bookmarks.el` shows the advice and the copy.
- `grep -n "advice-add 'bookmark-jump" config/workspaces/portable-bookmarks.el`
  returns nothing (wrong choke point).
- Manual: jump to a bookmark made in another workspace's worktree; the file opens
  from the current workspace, and re-inspecting the stored record shows the
  original absolute path.

## Context
- specs/portable-bookmarks/spec.md § 'Jump-time resolution on a copy'
- specs/portable-bookmarks/spec.md § 'Degradation and isolation'
- design.md § D1, § D2

## Cycle 1 updates (cycle-1789935943)

### Cited register entries
- `register/boundary/bookmark-advice-seams`: speculated → reconciled text, entry stays `speculated`. Seam 1 (stamp) is confirmed live; seam 2 is verified pass-through at 20944160 (`.el:398-405`) and is THIS task's to wire. See `.orchestrator/cycles/cycle-1789935943/reconciliations/boundary-bookmark-advice-seams.md`.
- `register/invariant/stored-record-never-mutated-by-jump`: unexercised this cycle (status unchanged); this task owns its enforcement (`portable-bookmarks-jump-spec.el`, the `copy-tree` validator). Audit finding eoc-03 asks that jump-seam be the FIRST task of its cycle-2 wave and that the on-touch for this invariant fire on it.
- `register/vocabulary/bookmark-resolution-outcome`: speculated → reconciled. Six members; `non-portable` is the only outcome that uses the stored `filename`; `resolved` is the only one that opens without asking. See `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`.

### User-resolved decisions
- DD-14 (ask 13): `ambiguous` with `:candidates` — report, open nothing. **Implication**: one more non-visiting arm in step 2; add a behavioural spec (two worktrees of one repo inside the current workspace's home → no buffer).
- DD-16 (ask 15): confinement is lexical; a relpath spelled inside the checkout resolves there whatever its links point at. **Implication**: none for the seam; substitute `:path` verbatim. Do not add a symlink check here.
- Ask 5 (stale stamps after `bookmark-relocate`): documented limitation, not checked. **Implication**: the seam does not compare `filename` against the stamp; `.tasks/portable-bookmarks-stale-stamp-detection.md` owns that.

### Open asks
- ask 16 (`missing-repo` may carry nil `:path`): does not touch this task's arms; the hand-off passes the whole plist to `missing-repo-choice`.

### Already-shipped inline fixes / related work
- The resolver's own `condition-case` maps discovery errors to `no-clone`; the seam's step-5 barrier is the SECOND barrier (for the seam's own code), not the first.
- Follow-up `discovery-identity-memo-and-cost-contract` (audit eoc-06) will change spawn counts on the resolve path; the jump spec should not pin spawn counts.

## Cycle 2 plan (cycle-1790453714)

- Step 2 amended (finding arch-cycle-1790453714-plan-01, blocking interface-drift against `register/vocabulary/bookmark-resolution-outcome`): the resolver never returns nil; the off-workspace trigger is a nil `workspace--current-name` on a STAMPED record, decided before `--resolve`.
- Wave 1 merge order: `consolidate-portable-bookmarks-test-fixtures` merges FIRST. Write `portable-bookmarks-jump-spec.el` against its registry-scope macro `workspace-bookmark-fixtures-with-empty-registry` and its `workspace-bookmark-fixtures-commit-file` helper (rebase onto the integration branch once it has merged). Do NOT copy the per-file `workspace--registry` swap idiom: `register/invariant/spec-registry-isolation-via-fixtures` (new, speculated) forbids it and on-touch fires on it.
- On-touch fires on this task for `register/boundary/bookmark-advice-seams`, `register/invariant/stored-record-never-mutated-by-jump` and `register/invariant/no-silent-wrong-file-open`; each entry's cycle-2 forward-mode paragraph names what the seam must settle (arms per outcome, copy-tree on the `(NAME . ALIST)` cons, second-barrier `condition-case` with `quit` propagating, `bookmark-jump-other-window` through the real jump path, "filename unchanged" asserted after a `bookmark-save` round trip).
- Never bulk-delete shared temp prefixes (no `rm -rf $TMPDIR/wsbm-*`); delete only the directories this task created.

## Observations

- **Private shims for the fixtures names (remove after `consolidate-portable-bookmarks-test-fixtures` merges).** The parallel branch had no commits when this task ran, so `portable-bookmarks-jump-spec.el` carries two shims at its top, guarded by `(unless (macrop 'workspace-bookmark-fixtures-with-empty-registry) ...)` and `(unless (fboundp 'workspace-bookmark-fixtures-commit-file) ...)`. The macro shim is body-wrapping: it `let`-binds `workspace--registry` to a fresh `equal` table (dynamic binding restores on every exit), runs `workspace-bookmark-fixtures-setup`, and runs `workspace-bookmark-fixtures-teardown` in `unwind-protect`. Because a body-scoped macro cannot span `before-each` → `it` → `after-each`, the spec builds its world inside each `it` (`portable-bookmarks-jump-spec--world`) and wraps every `it` body in one local macro, `portable-bookmarks-jump-spec--scope`; if the real macro has a different shape, that one macro is the only adaptation point. Until the shims go, the shim's `let` of `workspace--registry` is a hit for validator 1 of `register/invariant/spec-registry-isolation-via-fixtures`.
- **Declining is a `user-error`, not a silent return — measured, not assumed.** Probe against bookmark.el 30.2 (`emacs -Q --batch`): when `bookmark-handle-bookmark` returns nil without changing the buffer, `bookmark--jump-via` continues — it ran `bookmark-after-jump-hook`, ran the display function on the buffer the user was already in, and dropped a bookmark fringe-mark overlay there (overlays 0 → 1). `user-error` aborted the jump cleanly at the command loop in the same probe. So "report and open nothing" is only safe as a signal, and `user-error` is not `bookmark-error-no-filename`.
- **The host swallows `bookmark-error-no-filename` for a RECORD argument.** Same probe: a record whose `filename` does not exist produced no signal and no `y-or-n-p` — the relocate arm's `(signal (car err) (cdr err))` sits inside the `(when (stringp ...))` branch, so for a record the `condition-case` returns nil. The register expected only prompt suppression. Consequence for `resolved`: a path that vanishes between the resolver's existence check and the visit opens nothing and signals nothing (and `bookmark--jump-via` then fringe-marks the current buffer, as above). Pinned by the spec "neither prompts to relocate nor signals when a resolved path vanishes before the visit".
- **bufferlo already hands the host a record for every jump in this configuration.** `bufferlo-mode` (`buffer-membership.org`, bufferlo 1.2) advises `bookmark--jump-via` (`bufferlo--bookmark--jump-via-advice`) and calls it with `(bookmark-get-bookmark name)`. Under the harness `bookmark-handle-bookmark` therefore never receives a string through `bookmark-jump`, and the host's string-gated tail — the relocate offer AND `(setq bookmark-current-bookmark name)` — is skipped for every jump here, substituted or not. Found because the first draft of the `bookmark-current-bookmark` spec drove `bookmark-jump` and saw nil even for a plain bookmark; the spec now drives `bookmark-handle-bookmark` with a string for both a plain and a stamped bookmark and asserts parity. Documented in the org prose ("The record handed to the host").
- **One step beyond "normalize, resolve, substitute, call through".** After a substituted jump by NAME the seam sets `bookmark-current-bookmark` (buffer-local, in the visited buffer) as the host does for a string argument; without it every resolved jump would drop the default that `C-x r b` / `C-x r m` offer next. Under bufferlo this is unobservable through `bookmark-jump` (see above) but correct for stock Emacs and for any caller that reaches `bookmark-handle-bookmark` with a name.
- **Emacs 30 dired records carry no `handler` property.** Probe: `bookmark-make-record` in a Dired buffer gives `(filename . "DIR/")` and no handler; "the dired handler" is `bookmark-default-handler` visiting a directory. The spec asserts `dired-mode` and B's `sub/` as `default-directory`, which is what the design's scenario means; the copy keeps whatever handler a record has (`bookmark-get-handler` equal on copy and stored).
- **Every new spec was shown to fail with the production change reverted.** Method: under the harness, load the new spec file, then `(load "<fd55f40f's portable-bookmarks.el>")` over it so `workspace-bookmark--handle-bookmark-advice` is the pass-through again, then `buttercup-run`. Result: 13 of 19 fail — all five resolved-jump specs (B's copy not opened; copy `eq` to stored), all five decline specs (no signal), `quit` propagation, and the relocate-arm spec (the stored path was visited). The six that still pass are the pass-through / degradation specs (unstamped, off-workspace, resolver-signals, copy-step-signals, handler's own error) plus the structural copy-tree spec, which reads the tangled `.el` on disk (a real revert of the `.el` fails it: 0 `copy-tree` sites).
- **Two interim behaviours are pinned on purpose** and labelled in their `it` names: a stamped record with no current workspace passes through unchanged "(until off-workspace-restore lands)" and `missing-repo` declines "(until missing-repo-choice lands)". Both wave-3 tasks will have to change those two specs; that is the intended signal.
- **The barrier's residual is stated, not hidden.** An error in the seam's own code falls through to ORIG-FN with the original argument, which for a stamped record is the stored absolute path — the route-4 shape of `no-silent-wrong-file-open`. It is reachable only by a bug in the seam section (the resolver never signals; the copy step is three forms), and the task's step 5 and the boundary entry's degradation_contract both require it. The org prose says so under "The barrier".
- **Dispatch shapes.** The outcome `pcase` has a signalling default arm (`(outcome (error ...))`), mirroring `workspace-bookmark--resolve-anchored`; the barrier turns that into stock behaviour, never into an open. The plan dispatch in the advice is a two-valued `if` (`decline` / call-through), not a `pcase` with a catch-all.
- **Spec boundary.** The only mock in the behavioral specs is `workspace--current-name` (tabs.el) — a tab-bar tab cannot be selected from a batch spec. `jf/emacs-dir` is bound to nil for the whole scope, including the real `find-file-noselect`; checked that nothing in `config/workspaces` hooks `find-file-hook` (only `kill-emacs-hook` in persistence.el), so the binding cannot disturb the visit. `bookmark-set` under `bookmark-save-flag` 1 writes the fixtures' scratch bookmark file on every set — harmless, echo noise suppressed with `inhibit-message`.
- **Spawn counts are not pinned** anywhere in the jump spec (`discovery-identity-memo-and-cost-contract` changes them).

### Fix pass (after `consolidate-portable-bookmarks-test-fixtures` merged, ae45fdd5)

- Both private shims are deleted. The real `workspace-bookmark-fixtures-with-empty-registry` is a DESCRIBE-level macro (expands to a `before-each` running `workspace-bookmark-fixtures-setup-with-empty-registry` with the body as the per-spec setup, plus an `after-each` teardown), not the body-wrapping per-`it` scope the first pass assumed. The jump spec now uses it as intended: each `describe` declares `:var (world)` and builds its world in the macro body; `portable-bookmarks-jump-spec--scope` is gone, replaced by a registry-free `portable-bookmarks-jump-spec--hermetic` that only binds `jf/emacs-dir` to nil, kills the buffers a spec leaves behind (before the `after-each` deletes their directories) and clears the identity memo. Validator 1 of `register/invariant/spec-registry-isolation-via-fixtures` returns zero hits for this file; `workspace-bookmark-fixtures-commit-file` is the real one (`git add -f`). The structural copy-tree spec moved to its own `describe` so it builds no world. disc-jump-seam-7 is answered: shape confirmed, adaptation done, the entry can drop the open question.
- The `bookmark-current-bookmark` parity spec also asserts that the stamped jump landed in B's copy: parity alone is something the stock host satisfies by itself when driven with a string, so without that assertion the spec no longer failed under the revert demonstration (12 of 19 instead of 13). With it, the revert demonstration is back to 13 of 19 failing; the six survivors are unchanged.

## Discoveries

- discovery_id: disc-jump-seam-1
  class: interface-drift
  description: |
    A THIRD piece of advice sits on the bookmark jump path in this
    configuration, and it is not ours: bufferlo 1.2
    (`bufferlo--bookmark--jump-via-advice`, installed by `bufferlo-mode`
    from buffer-membership.org) wraps `bookmark--jump-via` and calls it
    with `(bookmark-get-bookmark name)`. Every jump through
    `bookmark-jump` / `-other-window` / `-other-frame` therefore reaches
    `bookmark-handle-bookmark` with a RECORD. The entry's ARGUMENT
    NORMALIZATION clause ("may be a STRING NAME or a record") stays true
    at the seam, but its upstream_contract_risk clause ("passing a
    substituted RECORD suppresses the relocate prompt") describes a
    difference that does not exist here: the host's string-gated tail
    (relocate offer, `bookmark-current-bookmark`) is skipped for ALL
    jumps in this config. Only a caller that reaches
    `bookmark-handle-bookmark` directly with a name sees the string path.
  affected_register_entry: register/boundary/bookmark-advice-seams
  recommendation: |
    Amend upstream_contract_risk: note bufferlo's `:around` on
    `bookmark--jump-via` as a co-tenant on the jump path (bufferlo.el:1223)
    and that, with it installed, the host never sees a string from the
    jump commands. Keep the seam's string handling (it is what stock
    Emacs and direct callers see). Keep "exactly two seams" — the two
    are OURS; record bufferlo's under related third-party advice.

- discovery_id: disc-jump-seam-2
  class: invariant-gap
  description: |
    Measured against bookmark.el 30.2: for a RECORD argument the host
    does not merely skip the relocate `y-or-n-p`, it swallows
    `bookmark-error-no-filename` entirely (the re-signal is inside the
    `(when (stringp ...))` branch) and returns nil; `bookmark--jump-via`
    then continues in the buffer the user was already in (display
    function, fringe mark, after-jump hook). For the seam this means a
    `resolved` path that vanishes between the resolver's existence check
    and the visit opens nothing and signals nothing. Reached only by a
    race; the direction (never a relocate offer against the stored path)
    is the desired one. Pinned by portable-bookmarks-jump-spec.el
    "neither prompts to relocate nor signals when a resolved path
    vanishes before the visit".
  affected_register_entry: register/boundary/bookmark-advice-seams
  recommendation: |
    Amend upstream_contract_risk to "suppresses the prompt AND the
    signal"; add to `register/invariant/no-silent-wrong-file-open`
    known_residuals: (c) a resolved path that vanishes before the visit
    is a silent no-op with a stray fringe mark, host behaviour for record
    arguments, pinned by spec.

- discovery_id: disc-jump-seam-3
  class: deviation
  description: |
    Clause (a) "HOW the seam declines" is settled: `user-error`, not a
    silent return. Probe (emacs -Q --batch, bookmark.el 30.2): a nil
    return from `bookmark-handle-bookmark` lets `bookmark--jump-via` run
    the display function on the CURRENT buffer, drop a bookmark fringe
    mark in it (overlays 0 -> 1) and run `bookmark-after-jump-hook`;
    `user-error` aborts the jump cleanly. Asserted: the signal is
    `user-error` (a `bookmark-errors` handler in the spec would catch a
    `bookmark-error-no-filename` and fail the spec), `y-or-n-p` is
    trapped for every jump, and "no buffer visits the missing path" is
    asserted with `find-buffer-visiting` after the jump for
    not-on-branch, ambiguous, no-clone and missing-repo — plus "no
    buffer visits the STORED path" for each, which is what proves the
    seam did not fall through.
  affected_register_entry: register/invariant/no-silent-wrong-file-open
  recommendation: |
    Reconcile clause (a): declines are `user-error`; a silent return is
    unsafe because `bookmark--jump-via` continues (cite the probe). The
    "cancel opens nothing and signals nothing" rule stays with the
    missing-repo prompt (an answer the user gave), not with declined
    outcomes. Clause (c) and validator 4 are live in
    portable-bookmarks-jump-spec.el.

- discovery_id: disc-jump-seam-4
  class: invariant-gap
  description: |
    The barrier's fall-through is the one place a STAMPED record's stored
    `filename` can be opened: an error in the seam's own code (plan,
    copy, message) calls ORIG-FN with the ORIGINAL argument, which is
    stock behaviour = the stored absolute path = route 4 of the
    invariant. It is required by task step 5 and by the boundary entry's
    degradation_contract, and is reachable only by a programming error in
    the seam section (the resolver has its own barrier and never
    signals; the copy step is three forms). The spec exercises it
    deliberately ("still opens the stored location when the resolver /
    the copy step signals").
  affected_register_entry: register/invariant/no-silent-wrong-file-open
  recommendation: |
    Record as known_residual (d): "an internal error in the jump seam
    degrades to the stored path (degradation_contract of
    bookmark-advice-seams); reachable only by a seam bug". No code
    change: the alternative (decline on internal error) would turn a
    seam bug into a bookmark that cannot be jumped to at all.

- discovery_id: disc-jump-seam-5
  class: spec-signal
  description: |
    Clauses (a)-(d) settled for the resolved-jump clause: (a) ONE
    construction site, `workspace-bookmark--substituted-record`
    (portable-bookmarks.org § "The copy rule"); the structural spec
    counts `(copy-tree ` lines in the tangled .el = 1 and asserts no
    `copy-alist` / `copy-sequence` and no `bookmark-prop-set ... 'handler`.
    (b) `copy-tree` on the `(NAME . ALIST)` cons returned by
    `bookmark-get-bookmark`, then `bookmark-prop-set` on the copy; the
    spec asserts the handler received a record that is not `eq` to the
    stored one and shares NO cons with it (`cl-intersection ... :test
    #'eq` = nil). (c) `bookmark-save-flag` is 1 at fd55f40f
    (config/core/defaults.el:114). (d) "filename unchanged" is asserted in
    memory (`equal` to a `copy-tree` snapshot) AND after `bookmark-save`
    to the scratch `bookmark-default-file`, `bookmark-alist` emptied, and
    `bookmark-load` back — for a file record and for a directory record.
  affected_register_entry: register/invariant/stored-record-never-mutated-by-jump
  recommendation: |
    speculated -> reconciled for validators 0, 1, 2 and the resolved
    clause of validator 3; enforcement_mechanism.location =
    config/workspaces/test/portable-bookmarks-jump-spec.el (exists). The
    read-only-view and restore-then-jump clauses stay with their wave-3
    owners; the entry should require them to re-enter
    `workspace-bookmark--substituted-record` or open `:path` without a
    record, never a second copy site.

- discovery_id: disc-jump-seam-6
  class: deviation
  description: |
    The seam does one thing beyond "normalize, resolve, substitute, call
    through": after a substituted jump by NAME it sets
    `bookmark-current-bookmark` (buffer-local, in the visited buffer) as
    the host does for a string argument, so a resolved jump does not
    drop the default the next `C-x r b` / `C-x r m` offer. Asserted by
    driving `bookmark-handle-bookmark` with a string for a plain and a
    stamped bookmark and requiring parity (through `bookmark-jump` the
    string path is unreachable here — disc-jump-seam-1).
  affected_register_entry: register/boundary/bookmark-advice-seams
  recommendation: |
    Amend mitigation (a) of upstream_contract_risk to "normalize,
    resolve, substitute, call through, and mirror the host's
    `bookmark-current-bookmark` bookkeeping for a substituted jump by
    name". Validator 3's seam-2 half: the barrier wraps the DECISION only
    (`workspace-bookmark--jump-plan`); the call to ORIG-FN is outside it,
    so the host is called exactly once and its own errors propagate
    (spec "does not catch the handler's own errors, and calls the host
    exactly once"); `quit` propagates (spec). Validator 4 is live
    (`bookmark-jump-other-window` through the real jump path).

- discovery_id: disc-jump-seam-7
  class: scope-question
  description: |
    The jump spec is written against the planned macro
    `workspace-bookmark-fixtures-with-empty-registry` as a BODY-WRAPPING
    scope (fresh registry + fixtures-setup on entry, fixtures-teardown +
    restore on every exit), because that is the only shape the
    statement's "restore inside unwind-protect so a signalling spec body
    cannot leak" admits. A body-scoped macro cannot span buttercup's
    before-each / it / after-each, so world-building moved into each
    `it` behind one local macro (`portable-bookmarks-jump-spec--scope`).
    The temporary shim `let`-binds `workspace--registry` and matches
    validator 1's grep until it is deleted after the fixtures merge.
  affected_register_entry: register/invariant/spec-registry-isolation-via-fixtures
  recommendation: |
    The fixtures task confirms the macro's calling shape; if it differs
    (for example a describe-level form that installs before-each /
    after-each), `portable-bookmarks-jump-spec--scope` is the one place
    to adapt. Delete the two shims at the top of
    portable-bookmarks-jump-spec.el as part of the post-merge rebase and
    re-run validator 1.
