---
name: test-stand-in-consolidation
description: Move per-spec stand-ins (fake TRAMP handlers, registered-names stand-ins, the bookmark-jump harness) into the shared fixtures module
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790604160
cites_register_entries:
  - register/invariant/spec-registry-isolation-via-fixtures
relations:
  - discovered-from:eoc-1790453714-03
  - discovered-from:meta-discovery/fixture-regrowth
discovered_from: [eoc-1790453714-03, meta-discovery/fixture-regrowth]
discovered_by: architect
discovered_class: duplication
reconciled_into: null
created_at: 2026-09-28T00:00:00+00:00
---

> Created at integrate of cycle-1790453714 from `.orchestrator/cycles/cycle-1790453714/findings/eoc-audit.md` § eoc-1790453714-03.
> Pressure-tested at plan of cycle-1790604160 against HEAD c65efb86 (finding arch-cycle-1790604160-plan-04). Wave 3, LAST: it edits
> spec files that waves 1-2 also edit (missing-repo, jump, off-workspace), and its counting validator must land on the FINAL spec set.

## Counts at c65efb86 (re-verified)
- Fake-remote file-name handler: FIVE, two dialects. "Name ops pass" (nine name operations pass through, everything else records and
  signals): `broken-home-load-spec.el:151`, `workspace-delete-purge-spec.el:579`, `workspace-new-anchor-spec.el:363`. "Only
  `file-remote-p`" (everything else signals `file-error`): `git-worktrees-spec.el:209`, `portable-bookmarks-cost-spec.el:366`.
- `workspace--registered-names` stand-in `(unless (fboundp ...) (defun ...))`: SIX — `portable-bookmarks-cost-spec.el:44`,
  `git-worktrees-spec.el:53`, `portable-bookmarks-discovery-spec.el:49`, `portable-bookmarks-jump-spec.el:47`,
  `portable-bookmarks-missing-repo-spec.el:64`, `portable-bookmarks-resolver-spec.el:44`. (The fixtures module already has a
  `(defvar workspace--registry ...)` fallback at :47; the stand-in belongs beside it.)
- Bookmark-jump harness (`--hermetic`, `--world`, `--set`, `--jump`, `--decline`, `--visits-p`): THREE —
  `portable-bookmarks-jump-spec.el:69-170`, `portable-bookmarks-missing-repo-spec.el:103-215`,
  `portable-bookmarks-off-workspace-spec.el:103-280`. Divergence: off-workspace's `--visits-p` (:273) lacks the Dired arm that
  jump-spec's (:161-170) and missing-repo's (:204) have. `portable-bookmarks-discovery-spec.el:64` has a fourth `--hermetic` macro
  (no jump helpers) — fold it in if its body is the same shape; otherwise leave it and say why.

## Files
- `config/workspaces/test/portable-bookmarks-fixtures.el` (registered-names stand-in; jump harness).
- NEW core test helper, e.g. `config/workspaces/test/workspaces-test-helpers.el` (the fake-remote handler) — see step 1.
- The five handler files, the six stand-in files, the three harness files (above); `config/workspaces/test/portable-bookmarks-spec.el`
  (the counting validator, beside the existing fixture specs :99-256).
- No production code.

## Steps
1. ONE fake-remote file-name handler. HOME DECIDED: a CORE test-helper file that both core specs and the fixtures module load —
   `register/invariant/spec-registry-isolation-via-fixtures` › known_violations records the accepted rule "a core spec must not depend
   on a droppable consumer's fixtures", and three of the five copies are core specs (broken-home-load, workspace-delete-purge,
   workspace-new-anchor). `portable-bookmarks-fixtures.el` `load`s the helper (the specs already `load` the fixtures by path,
   jump-spec.el:38, so the same pattern works for the helper). Shape: a `with-fake-remote` macro parameterised by PREFIX, recording
   touches, with the STRICT permitted set by default (only `file-remote-p` is free — matching the register's "never contacted"
   wording) and an explicit `:allow` list of name operations for a spec that needs one (the three "name ops pass" specs list the
   nine they use today; the implementor checks each still passes with only the operations it actually needs). Replace the five copies.
2. ONE `workspace--registered-names` stand-in in `portable-bookmarks-fixtures.el` next to the `workspace--registry` fallback (:47);
   replace the six copies.
3. ONE bookmark-jump harness in `portable-bookmarks-fixtures.el` based on jump-spec's Dired-aware `--visits-p` (:161-170), with
   `--hermetic` / `--world` / `--set` / `--jump` / `--decline` parameterised where the three copies differ (missing-repo's `--jump`
   takes an ANSWER; off-workspace's takes an answer and a chooser). Replace the three copies; the off-workspace specs about directory
   bookmarks stop passing vacuously — note any that go red and fix the SPEC, not the harness.
4. Counting validator in `portable-bookmarks-spec.el`: a spec that reads every `config/workspaces/test/*-spec.el` and asserts ZERO
   matches of `(defun workspace--registered-names`, `remote-handler (operation`, `--visits-p (path)` and `defmacro [a-z-]*--hermetic`
   outside the fixtures module and the core helper (register validator 6). It runs in the normal suite, so a regrown copy fails the
   run, not only a review.
5. No inline stand-in of any kind is added by this task; every helper it creates lives in the fixtures module or the core helper.

## Cycle-3 on-touch additions (arch-on-touch-1790604160-08, from the wave-1 gate merge 360ad60c)
- `portable-bookmarks-missing-repo-spec.el` gained an inline `--preferring` answer stand-in (~:164-176) that subsumes the pre-existing
  inline `--answering` (~:154-162): fold both into ONE fixtures helper (the harness's answer parameter, step 3) and delete the inline pair.
- The same file fakes a remote home via `cl-letf` on `file-remote-p` / `file-directory-p` (~:460-477): a SIXTH remote stand-in of a
  different shape than the five `*-remote-handler` handlers. Route it through the single fake-remote helper (step 1) and make the
  counting validator (step 4) also match `cl-letf.*file-remote-p` outside the fixtures module and the core helper.
- Re-verify all counts at your base commit (the wave-1 and wave-2 merges shift lines and may add stand-ins).

## Cycle-3 on-touch additions (outcome-action-mapping merge 755483a5)
- (arch-on-touch-1790604160-14) The new fixtures helper `workspace-bookmark-fixtures-signalling` has FIVE pre-existing inline
  equivalents (a `cl-letf` that makes a function signal): `portable-bookmarks-missing-repo-spec.el` ~:942-943,
  `portable-bookmarks-stamp-spec.el` ~:444-445 and ~:456-457, `portable-bookmarks-resolver-spec.el` ~:570-571 (re-locate at your
  base). Route them through the helper, and extend the counting validator to this shape. The fixtures comment near the helper
  (~:408-413) cites the counting validator: make it name the spec you add in step 4.
- (arch-on-touch-1790604160-11, test half) Add ONE spec to `portable-bookmarks-jump-spec.el`: an UNSTAMPED record jumped with NO
  current workspace opens its stored path (stock behaviour). Today `--jump-plan` passes context `in-workspace` for that case, which is
  correct but unpinned; a later tidy that passes `none` would make every unstamped bookmark decline silently. The spec must go red if
  `--jump-plan` is changed to pass `'none` for an unstamped record with no workspace (show it on a scratch copy of the tangled `.el`).
  This is a test-only addition; do not edit production code.

## Verification
- Isolated suite exit 0; full suite `compare-baseline.py` → NO-REGRESSION; spec count unchanged except the validator (+1) and any
  off-workspace directory spec that the Dired-aware `--visits-p` turns from vacuous into real (list them).
- `grep -c '(defun workspace--registered-names\|remote-handler (operation\|--visits-p (path)\|defmacro [a-z-]*--hermetic' config/workspaces/test/*-spec.el`
  → 0 for every file (the validator's own grep).
- Sample 3 replaced specs (one per family) and show each still fails with its production change reverted.
- `grep -n "require\|load" config/workspaces/test/broken-home-load-spec.el config/workspaces/test/workspace-delete-purge-spec.el config/workspaces/test/workspace-new-anchor-spec.el`
  → they load the core helper, NOT `portable-bookmarks-fixtures.el`.

## Register
On-touch fires on `register/invariant/spec-registry-isolation-via-fixtures` (validator 6, the counting validator; the helper's home).
Neighbours: `.orchestrator/cycles/cycle-1790604160/briefs/entries-test-stand-in-consolidation.txt`.

## Observations

- Recounted at base 24ce67b8 (the body's lines were from c65efb86): fake-remote handlers 5 (unchanged files), plus the sixth
  (`cl-letf` of `file-remote-p` / `file-directory-p`, missing-repo "names a remote home `home-remote'"); registered-names stand-ins 6;
  jump harness 3 (+ discovery's `--hermetic`); inline `--answering` / `--preferring` pair in missing-repo. The signalling
  collaborator had NINE sole-purpose `cl-letf` equivalents, not five: missing-repo 1, stamp 2, resolver 4 (one inside a two-binding
  `cl-letf`, "Must not be asked"), off-workspace 1, portable-bookmarks-spec.el 1. All nine now go through
  `workspace-bookmark-fixtures-signalling`. Resolver's `(signal 'file-error ...)` stand-in and jump-spec's call-counting "Handler
  boom" stand-in are different shapes (a specific condition / a counter) and stay.
- Homes: NEW core helper `config/workspaces/test/workspaces-test-helpers.el` (`workspaces-test-with-fake-remote`,
  `workspaces-test-fake-remote-asked` / `-touches` / `-operations`), loaded by path by the three core specs and by
  `portable-bookmarks-fixtures.el`. The fixtures module gained the registered-names stand-in (beside the `workspace--registry`
  fallback) and the jump harness: `-hermetic` (options `:integrations`, `:current-workspace`), `-jump-world`, `-set-bookmark`,
  `-jump` (`:workspace` / `:choose` / `:pick` / `:jumper`), `-decline`, `-visits-p` (jump-spec's Dired-aware one),
  `-answering` (KEYS &optional BEFORE), `-offered-keys`, `-prompt-text`, `-prompted!`, and the jump constants `-jump-contents` /
  `-jump-position` (identical in all three files). It now requires `dired` and `buttercup` (the decline helper reports through
  `expect`).
- Minimal remote allow lists, found by running each spec strict first: broken-home-load NONE, workspace-delete-purge NONE,
  workspace-new-anchor FOUR (`expand-file-name`, `file-name-as-directory`, `directory-file-name`, `file-name-nondirectory`: the anchor
  canonicalises the home and takes its basename before the refusal). The other five of the old nine were never asked. The
  missing-repo remote-home spec now registers a `/wsbm-mr-remote:` home and asserts, beyond the verdict, no touch and
  operations = `(file-remote-p)`; before it only failed on a `file-directory-p`.
- Harness semantics that changed in unifying (each file's specs all still pass): missing-repo's prompt guard stub `(lambda () t)`
  became the off-workspace shape (the real guard asked with `noninteractive` masked) for every jump that answers a prompt; both prompt
  records reset at the start of every jump (off-workspace used to reset per spec only); jump-spec's jumps now bind `inhibit-message`;
  every `--hermetic` body (jump, missing-repo, discovery too) now gets the persistence file redirected to a fixtures temp dir (made
  lazily), so an autosave in those files can no longer reach the real workspaces.eld.
- `workspace-bookmark-fixtures-answering-after` (added by the wave-1 gate task) was a third variant of the same answer stand-in; it is
  folded into `workspace-bookmark-fixtures-answering` (its BEFORE argument) and removed. The gate task's file
  (tasks/open/add-worktree-broken-workspace-gate.md:239) still names it; not edited here.
- Inline prompt stand-ins that remain and that the validator does not count: jump-spec's two `cl-letf`s that lift the guard and stub
  `completing-read` (hand-off spec, which records the prompt/restore/resolve ORDER) and `read-multiple-choice` (missing-repo hand-off
  spec); off-workspace's four inline chooser lambdas passed to `:choose`; two message-recorder macros that are near-duplicates
  (missing-repo `--collecting-messages`, off-workspace `--recording-messages`). Candidates for a later pass, not in this task's list.
- The registered-names stand-in is dead code under the harness (init.el loads tabs.el first), so no fail-on-revert can reach it; its
  sample family for the revert check is therefore the jump harness and the fake remote handler, plus the signalling collaborator.
- git-worktrees and cost specs used to REPLACE `file-name-handler-alist` with the fake; the one helper PREPENDS it (the three core
  specs' shape). The fake still serves "/ssh:" and "/fake:" names (it is first; `find-file-name-handler` keeps the first of equal
  match positions) and every operation it is asked is still pinned to `(file-remote-p)`; local names keep their usual handlers.
- Spec count 812 -> 814: +1 the counting validator, +1 the unstamped/no-workspace jump spec (arch-on-touch-1790604160-11). Zero specs
  went from vacuous to real (see disc-2).
- Pre-existing, untouched: workspace-delete-purge-spec.el has two describes after its `(provide ...)` / "ends here" trailer.

## Discoveries

- discovery_id: disc-test-stand-in-consolidation-1
  class: interface-drift
  description: |
    Validator 6 as written (a one-line grep of four literal shapes, plus the body's `cl-letf.*file-remote-p`) is narrower than the
    classes it guards: a handler not named `*remote-handler`, a `file-remote-p` rebinding split across lines, a `spy-on` of it, or a
    `--visits-p` / `--hermetic` with other argument spellings would all pass it. The implemented counting spec
    (portable-bookmarks-spec.el "portable-bookmarks fixtures: one home per stand-in" / "finds no inline stand-in in any spec file")
    reads all 39 `*-spec.el` and counts, per file with line numbers: registered-names defun; `remote-handler (operation` and any
    `(defun X (operation &rest`; `cl-letf.*file-remote-p`, `(symbol-function 'file-remote-p)`, `spy-on 'file-remote-p`;
    `--visits-p (path)` and any `(defun X--visits-p`; any `(defmacro X--hermetic`; and, only in spec files that load the fixtures
    (the ones that have the helper to hand), a `(symbol-function 'F)` bound to `(lambda (&rest _) (error ...))`. It self-checks
    against one planted copy of every shape, checks the homes are loaded, and writes its regexps with `[(]` / `[-]` so its own file
    (which it also reads) holds no match. The signalling shape is scoped to fixture-loading files because core specs
    (buffer-reincarnation-spec.el:80, :387) may not load a consumer's fixtures.
  affected_seam: register/invariant/spec-registry-isolation-via-fixtures
  observed:
    happened: true
    evidence: |
      six planted copies in scratch copies of six spec files -> validator FAILED listing
      broken-home-load-spec.el "fake remote file-name handler", discovery "--hermetic", jump "--visits-p", missing-repo
      "file-remote-p stand-in", resolver "registered-names stand-in", stamp "signalling collaborator"; the task's grep -c over
      config/workspaces/test/*-spec.el -> 39 files, total 0.
  recommendation: |
    Reconcile validator 6 to the shipped spec: name the spec as the enforcement, list the widened shapes and the fixture-loading
    scope of the signalling shape, and move it from SPECULATED to the entry's reconciled text. Record the core helper
    workspaces-test-helpers.el as the fake-remote handler's home (and as the natural home, later, of the registry-scope macro the
    naming-debt note wants for file-identity-spec.el).

- discovery_id: disc-test-stand-in-consolidation-2
  class: spec-signal
  description: |
    The body expected the Dired-aware `--visits-p` to turn off-workspace directory-bookmark specs from vacuous into real. None
    exist: off-workspace's `--set` was file-only (`find-file-noselect` + `goto-char`), and no `--visits-p` call in that file, or in
    jump-spec, is on a directory. The only directory `visits-p` call in the three files is missing-repo "opens a directory
    record's canonical directory in Dired, marked, without view-mode", which already had the Dired arm. So the switch is a no-op
    for today's specs; it only prevents a FUTURE off-workspace directory spec from passing vacuously. The list of specs that went
    from vacuous to real is empty.
  affected_seam: register/invariant/spec-registry-isolation-via-fixtures
  observed:
    happened: true
    evidence: |
      advice on workspace-bookmark-fixtures-visits-p logging directory arguments, run per file: off-workspace 0 calls, jump 0,
      missing-repo 1 (…/wsbm-wsbm-mr-a-*/proj/sub/ -> nil, file arm nil).
  recommendation: |
    No amendment needed beyond recording that off-workspace has no directory-bookmark coverage; if the off-workspace flow is meant
    to support directory bookmarks, that is a missing spec, not a vacuous one.

- discovery_id: disc-test-stand-in-consolidation-3
  class: shape-fragmentation
  description: |
    The two fake-remote dialects differed in more than the permitted set: the "name ops pass" dialect recorded only refused ops,
    signalled plain `error`, answered `file-remote-p` with the PREFIX, and prepended itself; the "only file-remote-p" dialect
    recorded EVERY op (asserted as `(file-remote-p)`), signalled `file-error`, answered `file-remote-p` with the "/ssh:host:" match,
    and replaced the handler alist. Both recorded state must also survive the scope: cost-spec reads the ops after the wrapper
    returns, and workspace-delete-purge asserted touches after its handler `let` closed. One shape now: record every op
    (`-asked`) and refused ops (`-touches`), reset on entry and kept on exit; signal `file-error`; answer the prefix (production
    reads only truthiness: persistence.el:136/464, tabs.el:139, workspaces.el:140/208, git-worktrees.el:31-47/216,
    portable-bookmarks.el:79-81/274-280/1046/1243/1368/1378/1547/1564/1623); allowed ops run with no handler at all.
  affected_seam: register/invariant/spec-registry-isolation-via-fixtures
  observed:
    happened: true
    evidence: |
      strict first run of workspace-new-anchor-spec.el: 3 failed, "threw file-error ... file-name-as-directory" /
      "expand-file-name"; then "directory-file-name", then "file-name-nondirectory"; green with those four. broken-home-load and
      workspace-delete-purge green with none.
  recommendation: |
    If a register entry cites the remote-home "never contacted" rule (persistence / tabs / purge remote-home entries), point its test
    corpus at workspaces-test-with-fake-remote's strict default and the allow lists above.

- discovery_id: disc-test-stand-in-consolidation-4
  class: deviation
  description: |
    discovery-spec's fourth `--hermetic` had a different body (only `jf/emacs-dir` nil and `workspace-bookmark--current-workspace`
    nil; no buffer cleanup, no memo clear). The body said "leave it and say why" if the shape differed, but the validator 6 grep
    counts every `defmacro *--hermetic`, so leaving it would fail the suite. Folded as the fixtures macro's
    `:current-workspace nil` option instead; its 29 call sites now also get buffer cleanup, the identity-memo clear on exit and
    the persistence redirect, and all 90 discovery specs pass.
  affected_seam: register/invariant/spec-registry-isolation-via-fixtures
  observed:
    happened: true
    evidence: portable-bookmarks-discovery-spec.el run alone -> Ran 90 specs, 0 failed; isolated suite 814/814.
  recommendation: |
    None for the register; noted so a reviewer reading the body's "otherwise leave it" sees why it was folded.
