---
name: missing-repo-choice
description: Offer a bounded choice when the current workspace holds no checkout — add worktree, read-only view, or cancel
change: workspace-portable-bookmarks
status: reviewed
task_class: feature
on_critical_path: true
cycle: cycle-1790453714
merge_commit: 492b5e88
findings_path: .orchestrator/cycles/cycle-1790453714/reviews/missing-repo-choice.md
reviewed_at: 2026-09-27T15:40:00+00:00
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/vocabulary/checkout-classification
  - register/invariant/no-silent-wrong-file-open
relations:
  - blocked-by:jump-seam
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-missing-repo-spec.el` (new)

## Implementation steps

> Cycle 1 (cycle-1789935943): steps 2, 3 and 6 corrected for the merged resolver — the
> plist carries `:checkout`, and `:path` may be nil for `missing-repo` (ask 16, pending
> confirmation; see `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`).

1. Add `workspace-bookmark--offer-missing-repo (resolution)` invoked by the jump
   seam on the `missing-repo` outcome. Present a bounded choice — three options,
   no free-form input — naming the repository identity and the current workspace
   in the prompt so the user knows what is missing from where. Use
   `read-multiple-choice` (bounded by construction and self-documenting) rather
   than `completing-read` over action strings.

   Options:
   - **add worktree here** — add a worktree of the identity to the current
     workspace, then resolve again and open;
   - **view read-only** — open the canonical checkout's copy, read-only;
   - **cancel** — open nothing, signal nothing.

2. "Add worktree here": the actual worktree creation lives in the magit-dependent
   `git-worktrees` module (`jf/workspace--add-worktree`, which takes an anchor
   payload). This module must not require magit, so:
   - call it only when it is `fboundp`, and omit the option from the choice when
     it is not;
   - build the payload the same way `integrations.el` does (`:name`, `:home`,
     `:context`) for the current workspace; the SOURCE repository is the resolution's
     `:checkout` (the canonical checkout discovery chose) — never re-run discovery here;
   - on success, re-run the resolver and continue the jump; on a `failed`
     outcome, report the reason and open nothing.

3. "View read-only": offered ONLY when the resolution's `:path` is non-nil (the relpath
   exists under the canonical checkout). When `:path` is nil the option is omitted and the
   prompt says the file is absent there; the choice is then add-worktree or cancel. When
   offered, open `:path`, then make it unmistakable that this is not a workspace copy:
   - `view-mode` on (read-only plus the standard view keymap);
   - a visible marker — set a buffer-local prefix/suffix in
     `mode-line-buffer-identification` (or equivalent) naming it as the canonical
     checkout, so the state survives after the echo-area message is gone;
   - a `message` naming the absolute path and the branch it is on.
   The marker matters because a `message` alone disappears; the risk being
   mitigated is an unnoticed edit of the main checkout.

4. "Cancel": return without opening a buffer and without signalling an error.
   Cancelling a navigation gesture is not an error condition.

5. Ensure the option set is genuinely bounded: no path in this function may fall
   through to opening the stored absolute location, which would be the silent
   wrong-file behaviour the spec forbids.

6. Write `portable-bookmarks-missing-repo-spec.el`, driving the choice by
   stubbing `read-multiple-choice` with `cl-letf` to return each option in turn:
   - **Behavioral**: with a canonical checkout in view and no checkout in the
     current workspace, choosing read-only opens the canonical copy, the buffer
     is read-only (`buffer-read-only` non-nil, `view-mode` active), and the marker
     is present in the buffer's mode-line identification.
   - **Behavioral**: choosing cancel opens no buffer and signals nothing.
   - **Unit**: with `jf/workspace--add-worktree` unbound, the offered option set
     excludes "add worktree here".
   - **Unit**: with a stub `jf/workspace--add-worktree` returning `ok`, the
     resolver is re-run and the jump proceeds; with one returning
     `(failed . REASON)`, the reason is reported and no buffer is opened.
   - **Unit**: assert no branch of the function opens the record's stored
     absolute path.
   - **Unit**: with a resolution whose `:path` is nil and `:checkout` set, the option set
     excludes "view read-only", and add-worktree targets `:checkout`.

7. Tangle and validate.

## Design rationale

The whole point of the outcome set is that nothing opens by accident. This is the
one outcome where a useful action exists but the user must choose it: the
canonical checkout usually *has* the file, so it is tempting to open it silently —
and that would mean editing `main` without noticing, which is what worktrees exist
to prevent.

Hence read-only plus a durable marker rather than a transient message. A quick
lookup stays cheap, and turning it into an accidental edit takes a deliberate
`C-x C-q`.

"Add worktree here" is gated on `fboundp` because worktree creation is magit's
job and this module must stay magit-free: with magit absent the option simply is
not offered, which matches the git-worktrees spec's own rule that absent magit
means no git integration rather than an error.

`read-multiple-choice` is chosen over `completing-read` because the option set is
closed and each option can carry its own help text — the same "closed set"
discipline the outcomes themselves follow.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — missing-repo spec passes.
- `grep -n "view-mode\|read-multiple-choice"
  config/workspaces/portable-bookmarks.el` shows both.
- `grep -n "fboundp 'jf/workspace--add-worktree"
  config/workspaces/portable-bookmarks.el` shows the guard.
- `grep -n "require 'magit" config/workspaces/portable-bookmarks.el` returns
  nothing.
- Manual: from a workspace with no `emacs` worktree, jump to an emacs-repo
  bookmark; verify all three options behave as specified and that the read-only
  buffer is visibly marked.

## Context
- specs/portable-bookmarks/spec.md § 'Closed set of resolution outcomes'
- design.md § D7 'Outcomes are a closed set, evaluated in order'
- design.md § 'Open Questions' (whether this prompt merges with the
  off-workspace prompt)

## Cycle 1 updates (cycle-1789935943)

### Cited register entries
- `register/vocabulary/bookmark-resolution-outcome`: speculated → reconciled (six members; six keys; the nested missing-repo ACTION set stays speculated until this task lands). `add-worktree` targets `:checkout`; `view-canonical-read-only` only when `:path` is non-nil. See `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`.
- `register/vocabulary/checkout-classification`: speculated → reconciled. One admission predicate `workspace-bookmark--offerable-canonical-p`; anchored homes count as checkouts (DD-11/12/13). See `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-checkout-classification.md`.
- `register/invariant/no-silent-wrong-file-open`: reconciled text (DD-16), entry stays `speculated` until the jump-side actors exist; this task is one of them. See `.orchestrator/cycles/cycle-1789935943/reconciliations/invariant-no-silent-wrong-file-open.md`.

### User-resolved decisions
- DD-12 / DD-13: worktree sources are git-derived + magit only; a magit linked worktree stands for its main worktree. **Implication**: `jf/workspace--add-worktree`'s source list is already classified; pass `:checkout` straight through.
- Ask 5: stale stamps not checked. **Implication**: the read-only view may show a pre-relocation path; say nothing about it here.

### Open asks
- ask 16: confirm (a) nil `:path` for `missing-repo`, (b) read-only withheld when nil, (c) this task's text above. The defaults are written in; object to change them.

### Related follow-ups
- `discovery-cleanup-singulars-and-docstrings` (audit eoc-02) deletes the singular discovery helpers; if a one-arm "the unique checkout" helper is wanted here, add it in THIS task with its caller.
- design.md § Open Questions (merge this prompt with the off-workspace prompt) is still open; `off-workspace-restore` step 3 reuses this bounded choice.

## Cycle 2 plan (cycle-1790453714)

- `register/vocabulary/bookmark-resolution-outcome` now tags the nested missing-repo ACTION set with `cycle_2_owner: missing-repo-choice` and states the ask-16 default as the speculation to settle (nil `:path` with `:checkout` set; read-only withheld when nil → {add-worktree, cancel}; add-worktree never re-runs discovery). Compare `:path` / `:checkout` by device+inode (`register/invariant/directory-identity-by-device-inode`, new), never by string.
- `discovery-cleanup-singulars-and-docstrings` (wave 2) deletes the singular discovery helpers before this task runs; if a one-arm "the unique checkout" helper is wanted, add it HERE with its caller.
- Never bulk-delete shared temp prefixes.

## Observations

- **Cancel ends the jump with `quit`, not a silent return and not an error** — a departure from step 4 ("without signalling an error") as literally read, forced by the host: measured against bookmark.el 30.2, when `bookmark-handle-bookmark` returns nil `bookmark--jump-via` still runs the display function on the buffer the user was already in (`bookmark-jump-other-window` shows it in another window), drops a bookmark fringe mark on the current line there, and runs `bookmark-after-jump-hook` — the tail the jump seam already documented for declines. A `user-error` was rejected the other way: `layouts.el`'s reincarnation chain (`workspace--bookmark-buffer`, `(error nil)`) answers any caught error by opening the leaf's STORED filename, so a cancel would have become a prohibition-4 open. `quit` is not an error, is not caught by `(error …)` barriers, already passes both seam barriers, and is what C-g at the same prompt produces, so the `cancel` option and C-g do the same thing. The cancel spec asserts: quit signalled, no `message`, no fringe overlay in the origin buffer, origin still current and writable, nothing visited.
- **The seam was edited beyond the one arm.** (a) The plan vocabulary grew two members, `(view COPY . CHECKOUT)` and `(cancel)`, so the advice — the seam's actor — gained an arm each and now dispatches with a closed `pcase` (`_` signals; before, anything not `decline` was called through). (b) The `resolved` arm's body (pre-flight + copy) moved into `workspace-bookmark--plan-to-open (record resolution kind)` so the read-only view and the post-add replan use the ONE pre-flighted route to `workspace-bookmark--substituted-record` instead of a second one. (c) The landed test moved into `workspace-bookmark--landed-p` and now compares by `workspace-bookmark--same-file-p`; the jump-seam merge had introduced a production `file-equal-p` there (portable-bookmarks.el, the advice's bookkeeping), unlisted in `register/invariant/directory-identity-by-device-inode` known_violations — fixed in passing since the line was being restructured anyway (Discoveries). Parallel task `off-workspace-restore` edits the `(null workspace)` cond arm of `workspace-bookmark--jump-plan`; nothing here touches that arm.
- **`jf/workspace--add-worktree` does not yet honour the source this module hands it.** The payload carries `:source-repo` (= `:checkout`, the channel the anchor-payload shape leaves open: "flat and extensible; handlers read only the keys they need"), but today's git-worktrees command reads `:name`/`:home` only and prompts through `jf/workspace--worktree-read-repo`, whose candidates come from `workspace-bookmark-canonical-checkouts-in-view` — a discovery re-run — and whose free-form floor lets the user pick a repository other than `:checkout`. Binding decision (c) therefore holds on THIS module's side (specs pin `:source-repo` same-file to `:checkout`, and the fixture-backed stub proves the replan) but not end to end until `git-worktrees.org` skips its source prompt when `:source-repo` is present (~3 lines + one spec). That file is outside this task's file list, so it is flagged, not changed (Discoveries). Meanwhile nothing wrong-file can result — re-resolution still decides what opens — but the user sees git-worktrees' own source and branch prompts after choosing "add worktree here".
- **The chooser has its own error barrier.** The seam's barrier answers an error in its decision by calling through with the ORIGINAL argument — for a stamped record, the stored absolute path. Tolerable for a bug in the seam; not for a bug in the prompt of the very outcome that exists to keep that path shut. `workspace-bookmark--offer-missing-repo` therefore turns any error inside the choice into a decline naming the error (`quit` passes). One spec forces it.
- **Payload shape.** Hand-built `(:name :home :context :source-repo)`, per step 2's "the same way integrations.el does". `:sessions-dir` is omitted: the constructor `workspace--integration-payload` and `workspace--sessions-dir` are core-private and not on `register/boundary/portable-bookmarks-consumer`'s permitted surface; the worktree command reads neither. `:context` is `bookmark-jump`, a value outside the confirmed `workspace-creation-context` set — chosen over borrowing `menu-invoke` ("built by the transient"), which would be untrue (Discoveries; a one-symbol change if the set is kept closed).
- **Directory records.** A `missing-repo` record whose relpath names a directory gets its canonical directory in Dired with the mode-line marker but WITHOUT `view-mode` (Dired is read-only already and view-mode's keymap would shadow its own). Files opened from that listing are ordinary writable canonical files; that is stock Dired one hop away from a marked buffer, stated here as a residual.
- **Layout reincarnation now reaches a prompt.** `layouts.el`'s `workspace--bookmark-buffer` runs `bookmark-jump` inside `(error nil)`. Before this task a missing-repo leaf declined (user-error → caught → step 2 opened the stored filename, the known prohibition-4 route owned by `off-workspace-restore`'s hard requirement). After it, such a leaf PROMPTS during a restore, once per leaf; a cancel there is `quit`, which the chain does not catch, so it aborts the restore. `off-workspace-restore`'s brief already says "a restore must not prompt per leaf; aggregate" — that coordination is now concrete.
- **No prompt where nobody can answer — found by the isolated gate, not by design.** The first isolated run HUNG: `save-restore-spec.el`'s "file buffer round-trip" restores a layout whose leaf is a bookmark on `init.el` with fake workspace "alpha" current; with the configuration checkout in view that is `missing-repo`, and the real `read-multiple-choice` blocked on the batch process's stdin. Before this task the leaf declined, layouts' `(error nil)` caught it and step 2 fell back to the filename, so the spec was green. Fix: `workspace-bookmark--can-prompt-p` (`(not noninteractive)`) gates the prompt; when nil the chooser declines with the seam's own `missing-repo` report — the pre-task behaviour, verbatim. Specs bind the predicate to t (the harness is batch) and one spec runs the real predicate and asserts the stand-in prompt is never reached. Both gates are re-run with stdin from `/dev/null` so a stray prompt fails instead of hanging. Consequence worth stating: in a GUI the same layout chain now DOES reach the prompt once per missing-repo leaf (see the layout reincarnation bullet above).
- **Mocks and weak spots.** `read-multiple-choice` is bound to a stand-in in every behavioural spec (Emacs owns the real key handling); `jf/workspace--add-worktree` is bound to a stub that really adds a worktree with the fixtures (or returns a vocabulary outcome) — the magit path is git-worktrees-spec's. The one message is asserted by binding `message` for the jump's duration, because batch `current-message` is nil. The branch in the message is asserted as `main` (fixtures' default). `bookmark-jump-other-window` is not exercised for the view arm (the seam's choke point is already covered by the jump spec).
- **Revert proof.** The two spec files (plus the fixtures and data-model) were copied to a scratch directory twice, once beside HEAD's `portable-bookmarks.el` and once beside the tangled one, and run with the harness's Emacs invocation: old module 39 ran / 20 failed — the 19 new specs plus the rewritten jump-spec arm, and nothing else; new module 39 ran / 0 failed. The worktree was not touched for the probe. The byte-compiler's only new remark is "`jf/workspace--add-worktree` is not known to be defined" at the quoted `funcall` — deliberate, mirroring git-worktrees' DD-10 discipline (no `declare-function`, no `require`).
- **Merging the two prompts (design § Open Questions).** REUSE looks right: the chooser is `(record resolution)`, reads the current workspace itself, and with none current offers exactly {view read-only, cancel} — `off-workspace-restore` step 3's fallback verbatim, including the `:path`-nil narrowing. MERGING looks wrong: `read-multiple-choice` is a single-key closed set, workspace names are an open, growing set that wants `completing-read` with `require-match`; folding them into one prompt would either cap the workspaces at single keys or reintroduce free-form input into the bounded choice. Recommendation: two prompts — `completing-read` for the workspace, this chooser as the fallback — and leave the nested action vocabulary at three members. Not decided here; reported.

## Discoveries

- discovery_id: disc-missing-repo-choice-1
  class: spec-signal
  description: |
    The nested missing-repo action `cancel` is specified as "open nothing,
    signal nothing", and the invariant says "Cancelling MUST open nothing
    AND signal nothing". Inside `bookmark--jump-via` a silent nil return is
    not nothing: the host displays the originating buffer (other-window
    variants show it elsewhere), fringe-marks its current line and runs
    the after-jump hook (measured, bookmark.el 30.2; the seam's own
    "Declining is a user-error" finding). A `user-error` is caught by
    callers' `(error …)` barriers — layouts.el then opens the stored
    filename. Implemented: cancel signals `quit`, the same as C-g at the
    prompt; the cancel spec asserts no buffer, no message, no overlay.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    Re-state the nested member: "cancel: opens nothing, says nothing, and
    ends the jump with `quit` — never an error, so no `(error …)` barrier
    can turn it into a fallback open; identical to C-g at the prompt."
    Same wording for `register/invariant/no-silent-wrong-file-open`'s
    cancel clause ("signal nothing" → "signal nothing but `quit`"). The
    rest of the nested set (add-worktree targets :checkout, never
    re-runs discovery; view withheld when :path nil) is confirmed as
    drafted: speculated → reconciled-recommended.

- discovery_id: disc-missing-repo-choice-2
  class: interface-drift
  description: |
    Decision (c) — add-worktree targets :checkout and never re-runs
    discovery — needs a channel into `jf/workspace--add-worktree`, which
    reads only :name/:home and prompts through
    `jf/workspace--worktree-read-repo` (candidates = a discovery run;
    free-form floor). This module passes the source as payload key
    `:source-repo` (the shape's designed extension point) and pins it by
    spec, but git-worktrees does not read it, so end to end the user still
    gets the source prompt and a second discovery pass. git-worktrees.org
    is outside this task's file list; not changed.
  affected_register_entry: register/boundary/workspace-git-worktrees-consumer
  recommendation: |
    Small follow-up in git-worktrees.org: when the payload carries
    `:source-repo`, use it (expanded) and skip `--worktree-read-repo`;
    keep the branch prompt. Pin 3 stays true (read-repo remains the SOLE
    prompting fn — it is simply not called). Amend
    `register/shape/workspace-integration-anchor-payload` with optional
    key `:source-repo` (producer: portable-bookmarks' missing-repo prompt;
    consumer: jf/workspace--add-worktree) and record the second, 3-key
    construction site `workspace-bookmark--add-worktree-payload` — or add
    `workspace--integration-payload` to the permitted surface so there is
    one constructor again. git-worktrees-spec's "never stores :repo/:branch
    on the payload" spec is unaffected (a key passed IN is not a key added).

- discovery_id: disc-missing-repo-choice-3
  class: interface-drift
  description: |
    The boundary entry says "This module never names git-worktrees". By
    the task's own design it now names `jf/workspace--add-worktree` at two
    sites — `(fboundp 'jf/workspace--add-worktree)` and a quoted `funcall`
    — never required, never `declare-function`ed, offered only when bound
    (the mirror of DD-10). Validator 1 (no magit/projectile symbol) still
    holds: grep magit → 0 hits.
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Amend sibling_consumer to a two-way soft coupling: git-worktrees names
    ONE public portable-bookmarks symbol; portable-bookmarks names ONE
    git-worktrees command, `jf/workspace--add-worktree`, quoted and
    fboundp-guarded at call time. Add validator: grep "jf/workspace--" in
    portable-bookmarks.el → hits only that symbol.

- discovery_id: disc-missing-repo-choice-4
  class: vocabulary-mismatch
  description: |
    The payload's :context is `bookmark-jump`, a fifth value outside the
    confirmed closed set {fresh, anchored-scaffolded, anchored-existing,
    menu-invoke}. `menu-invoke` is defined as "built by the transient when
    a user invokes an integration's :menu command"; a worktree added from
    the missing-repo prompt is neither a birth nor a transient invocation.
    No handler branches on it today (jf/workspace--add-worktree ignores
    :context).
  affected_register_entry: register/vocabulary/workspace-creation-context
  recommendation: |
    Either add member `bookmark-jump` ("NOT a birth context — built by
    portable-bookmarks when the missing-repo prompt adds a worktree of the
    canonical checkout to the current workspace") or rule that on-demand
    invocations share `menu-invoke`; the latter is a one-symbol change in
    `workspace-bookmark--add-worktree-payload` and its spec.

- discovery_id: disc-missing-repo-choice-5
  class: invariant-gap
  description: |
    The jump-seam merge introduced a production `file-equal-p` in
    portable-bookmarks.el (the advice's "did the jump land" test against
    `bookmark-get-filename`), not listed in known_violations (validator 1
    expected hits only in the docstring and tabs.el / git-worktrees.el).
    Replaced here by `workspace-bookmark--same-file-p` inside the extracted
    `workspace-bookmark--landed-p`; portable-bookmarks.el is back to the
    docstring-only hit.
  affected_register_entry: register/invariant/directory-identity-by-device-inode
  recommendation: |
    No register change beyond noting the transient violation and its
    closure at this commit; the validator's expected hit list is again
    accurate for portable-bookmarks.el.

- discovery_id: disc-missing-repo-choice-6
  class: scope-question
  description: |
    design.md § Open Questions asks whether this prompt and the
    off-workspace workspace-picker should be one "where should this open?"
    question. The chooser is reusable as-is by `off-workspace-restore`
    (function of (record resolution); with no current workspace it offers
    {view read-only, cancel}, exactly its step-3 fallback), but merging the
    two into one `read-multiple-choice` would put an open, growing set
    (workspace names) behind single keys or reintroduce free-form input.
    Separately, layouts.el's reincarnation chain now reaches this prompt
    once per missing-repo leaf in an interactive Emacs (before: a caught
    decline → stored path). In batch (`noninteractive`) the chooser
    declines as before — that guard is what keeps save-restore-spec's
    layout round-trip from blocking on stdin.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    Recommend REUSE without MERGE: completing-read for the workspace, this
    chooser as the fallback; the nested action set stays at three members
    and gains no workspace candidates. Route the per-leaf prompt during
    layout restore to off-workspace-restore's existing hard requirement
    ("a restore must not prompt per leaf; aggregate") — a cancel there is
    now `quit`, which the chain's `(error nil)` does not catch.
