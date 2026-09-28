---
name: docs-readme
description: Document portable bookmarks, the outcome set, and the off-workspace restore flow in the workspaces package README
change: workspace-portable-bookmarks
status: done
task_class: doc
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/vocabulary/bookmark-resolution-outcome
  - register/vocabulary/checkout-classification
relations:
  - blocked-by:missing-repo-choice
  - blocked-by:off-workspace-restore
---

## Files to modify
- `config/workspaces/docs/README.org` (modify)
- `state/bookmarks/README.md` (modify — correct a false claim)

## Implementation steps

> Cycle 1 (cycle-1789935943): the outcome table corrected to six outcomes (DD-14); see
> `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-bookmark-resolution-outcome.md`.

1. Add a `* Portable bookmarks` section to `config/workspaces/docs/README.org`,
   placed after `* Buffer reincarnation` (both concern bookmark records, and the
   reader benefits from meeting the two bookmark systems together). Follow the
   file's existing voice: prose that explains the invariant and the reasoning, not
   API listings.

   Cover:
   - **The problem** — a bookmark stores an absolute path, so in a
     worktree-per-workspace workflow it points into whichever checkout was current
     when it was made, silently opening another workspace's copy.
   - **What is stored** — records for files inside a git repository gain a repo
     identity (normalized remote URL) and a repo-relative path; the absolute
     `filename` is kept, so the file still loads in a stock Emacs and unstamped
     bookmarks keep working.
   - **The outcome table** — `resolved`, `not-on-branch`, `ambiguous`, `missing-repo`,
     `no-clone`, `non-portable`, each with what the user sees. State plainly that
     the main checkout is never a silent fallback, and that when two checkouts of
     one repo are in view nothing is chosen for you (`ambiguous`).
   - **Off-workspace jumps** — the prompt lists saved workspaces from the registry
     (not just live tabs), the chosen one is restored, the file opens in the
     selected window, and the perturbation is deliberately *not* saved as the
     workspace's `:working-state`.
   - **Scope** — one location per record; records holding nested records or window
     trees (e.g. `tab-bookmark`) are left alone. Cross-machine sharing, layout
     entry points and templates are separate future work.
   - **Two bookmark systems** — cross-reference the existing
     *Buffer reincarnation* section so readers do not confuse the user-facing
     bookmark list with the layout records stored inside `workspaces.eld`.

2. Note in that section that the module is droppable: removing the
   `jf/load-module` line for `portable-bookmarks.el` removes the advice, and
   bookmarks revert to their stored absolute paths with no data loss.

3. Update the *Deferred features* list in the same README if the portable-bookmark
   work changes any entry's status (the layout-revisions and template entries
   remain deferred; do not claim otherwise).

4. Fix `state/bookmarks/README.md`, which currently claims bookmarks are
   git-controlled and "shared across worktrees automatically". Neither is true:
   `.gitignore` ignores `state/**` except `README.md` files, and each Emacs
   install keeps its own list (dev writes `state/bookmarks/bookmarks` under
   `jf/emacs-dir`; production on `v0.2.0-rc2` predates that setting and writes
   `runtime/bookmarks`). Replace the false claims with what is actually true, and
   point to the new README section for cross-worktree resolution.

5. No tangling needed — both files are documentation, not literate source.

## Design rationale

The workspaces package keeps its user-facing documentation in
`config/workspaces/docs/README.org`, written as explanation rather than reference,
and this feature changes user-visible behaviour in a way that is not
self-evident: a jump can now prompt, can refuse, and can restore a workspace.
Undocumented, those would read as bugs.

The `state/bookmarks/README.md` correction is in scope because that file is the
reason the vault-versus-local storage question was confusing in the first place:
it asserts a sharing property the configuration does not implement. Leaving it
would mislead the next reader — including the future slice that has to decide
where shared bookmarks actually live.

Documenting droppability matters for the same reason the module is built as a
consumer: the package must remain fully functional without it, and a reader
should be able to confirm that without reading the code.

## Verification
- `grep -n "Portable bookmarks" config/workspaces/docs/README.org` shows the new
  section.
- `grep -n "not-on-branch\|missing-repo\|no-clone"
  config/workspaces/docs/README.org` shows the outcome table documented.
- `grep -n "shared across worktrees automatically" state/bookmarks/README.md`
  returns nothing (the false claim is gone).
- Read the new section end to end: every behaviour it promises is implemented by
  the tasks that precede it, and none of the deferred slices are described as
  present.

## Context
- proposal.md § 'Impact'
- design.md § D7, § D8, § D9
- design.md § Context (the dev/production bookmark-file split and
  `bookmark-save-flag`)

## Cycle 1 updates (cycle-1789935943)

### Cited register entries
- `register/vocabulary/bookmark-resolution-outcome`: speculated → reconciled (six members). Document also: a bookmark reached through a tracked symlink resolves to that spelling inside the checkout (DD-16); a hand-edited stamp with `..` or an absolute path is treated as unstamped.
- `register/vocabulary/checkout-classification`: speculated → reconciled. Document the three words a user will meet — workspace-owned, canonical, other — and that a workspace whose home IS a checkout, or sits inside one, owns it (DD-12/13). See `.orchestrator/cycles/cycle-1789935943/reconciliations/vocabulary-checkout-classification.md`.

### User-resolved decisions
- Ask 5: stale stamps after `bookmark-relocate` are a documented limitation. **Implication**: one sentence in the README, pointing at the limitation, not at a fix.
- DD-15: `magit-repository-directories` is `~/src` (depth 1) when it exists, so the worktree picker offers repos not yet in any workspace. **Implication**: mention where picker candidates come from (git-derived in-view checkouts, then magit).

### Related
- `disposition-spec-design-amendments` (audit eoc-01) corrects the delta specs/design.md to the same six-outcome, three-property design; write the README against the decided design, not the current spec text.
- Blocked by `missing-repo-choice` and `off-workspace-restore` (unchanged): the README describes their behaviour.

## Observations

Written against the code at base 6d061857, not the task text, where they differ.

**Departures from the task body (user decisions of 2026-09-27, per the brief):**
- Off-workspace jumps: the README says the result IS remembered — the next
  autosave records it in `:working-state`, `:saved-state` untouched. The task
  body's "deliberately not saved" bullet was not followed.
- Droppability: the loader line lives in `config/workspaces/workspaces.org`
  (line 148), not in `init.org`'s `jf/enabled-modules`; the README names the
  right file. The claim "no side effects" is slightly too strong: the
  git-worktree picker loses its checkouts-in-view source and passes magit's
  list through unclassified (`git-worktrees.org` § Cross-module interface,
  `jf/workspace--worktree-canonical-only` returns DIRS unchanged when the
  function is unbound). The README says so in one sentence.
- Deferred features list left unchanged: no entry's status changes (the
  portable-bookmark work does not use the `:etc` alist; revisions, selected-tab
  and rename remain deferred). The change's own future slices (1b/1c/2/3) are
  listed under *Portable bookmarks › Scope and limitations* instead of being
  added to the package's Deferred list.
- `* Buffer reincarnation` › *The decline gate* rewritten to name all four
  decline cases incl. `no-clone`, to say a restore never asks the missing-repo
  question, that reporting is once per restore, and that unstamped buffers
  (files outside git) are unaffected.

**Each behavioural claim and what it was checked against:**
- Stamp content (identity, relpath, format version; `filename` kept; all three
  or none): `portable-bookmarks.org` § Stamped property names, § Stamping.
- Identity: origin preferred, sole remote otherwise, no remote = no identity,
  userinfo stripped, SSH/HTTPS normalize equal: § Which repository this is,
  § Normalizing a remote URL.
- Unstamped cases (outside repo, untracked, no remote, TRAMP, nested
  records/window trees, `tab-bookmark`): § Which records are Tier 1, § The
  repository-relative path.
- Directories stamped (tracked tree in HEAD): § The repository-relative path.
- Layout records stamped too: § Seam 1 (layout save calls
  `bookmark-make-record`, caller not excluded).
- Jump coverage (`C-x r b`, other-window/frame, consult-bookmark): § Seam 2
  intro.
- Six outcomes, conditions, first-match order, ambiguity (2+ in workspace, or
  none + 2+ separate main clones; two worktrees of one repo not ambiguous):
  § The closed set of outcomes, § The decision, § The safety rule, and
  ambiguity; seam table § What the seam does with each outcome;
  `workspace-bookmark--jump-plan`.
- Report wording ("…; nothing opened", "none chosen"):
  `workspace-bookmark--decline-message`.
- Declines leave no fringe mark in the originating buffer: § Declining is a
  user-error.
- Kind mismatch → not-on-branch: § Relpath forms.
- Tracked-symlink spelling (DD-16), `..`/absolute stamp → unstamped: § Reading
  the stamp.
- Classification words and ownership rules (inside home, is home, contains
  home; same-repo worktree-in-clone exception; symlinks ignored; workspace's
  checkouts = home + immediate children): § Classification, § A workspace's
  checkouts.
- Checkouts in view order (current, other workspaces, `jf/emacs-dir`), no disk
  scan: § Checkouts in view, design D5.
- Missing-repo choice: options, withheld reasons, prompt contents:
  `workspace-bookmark--missing-repo-withheld`, § The prompt,
  `workspace-bookmark--offer-missing-repo`.
- Add-worktree uses the resolver's checkout, no source prompt, branch prompt
  remains, off main, re-resolve once and decline otherwise:
  `git-worktrees.org` `jf/workspace--add-worktree` (`:source-repo`,
  `read-string "Worktree branch: "`, `magit-worktree-branch dir branch main`);
  `portable-bookmarks.org` § Add worktree here.
- View read-only: view-mode, `[canonical …]` mode-line marker surviving
  `C-x C-q` and `revert-buffer`: § View read-only,
  `workspace-bookmark--mark-canonical-view`.
- Cancel opens nothing (quit): § The choice; `…--handle-bookmark-advice`.
- No prompt during layout restore / non-interactive / inhibit-interaction:
  § The choice ("No prompt where nobody can answer"),
  `workspace-bookmark--can-prompt-p`; `layouts.org` § Buffer reincarnation
  ("Asking is not declining").
- Layout decline gate, error buffer text, one message per restore:
  `layouts.org` `workspace--error-buffer`,
  `workspace--report-reincarnation-declines`, `workspace--restore-frameset`.
- Picker candidates (in-view canonical, then magit `~/src` depth 1, canonical
  only): `git-worktrees.org` § Repo-source seam; `major-modes/magit.org`
  `magit-repository-directories`.
- Off-workspace: registry not tabs, holders first + annotated, first holder
  default, broken not offered, `workspace-restore`, same window no split, C-g
  ends, empty answer / nothing offerable → no-workspace plan (chooser without
  add-worktree, or decline), non-interactive declines, unstamped unaffected:
  § Off-workspace (all subsections), `workspace-bookmark--off-workspace-plan`,
  `…--no-workspace-plan`, `…--choose-workspace`, `…--workspace-table`.
- Remembered like any visit: § Remembered like any visit.
- Stale stamps after `bookmark-relocate`: § Known limitation: stale stamps.
- Droppable: `workspaces.org` loader line 148; § Droppable; § Advice
  installation (`workspace-bookmark-remove-advice`); core names no
  `workspace-bookmark-*` symbol (grep of `config/workspaces/*.org`: only
  `git-worktrees.org`, a consumer, with `fboundp` guards).
- `state/bookmarks/README.md`: `.gitignore` lines 20-22 (`state/**`,
  `!state/**/`, `!state/**/README.md`); `config/core/defaults.org` (default
  file + `bookmark-save-flag 1`); `config/local/apploi-mac.el` (the only
  override; the other three local files do not set it); `init.org`
  `jf/emacs-dir` = directory of the loaded init file; `git grep` at tag
  `v0.2.0-rc2` finds no `bookmark-default-file`, and its `early-init.el`
  points `user-emacs-directory` at `runtime/`. I did not inspect `~/.emacs.d`,
  so the README names the tag, not "production".

- Coordinator request mid-task: `* Creating workspaces` › `C-u` anchor, the
  double-registration guard line no longer says `file-equal-p`; it now says
  directories are compared by device and inode (two spellings of one directory
  recognised; remote/TRAMP homes not supported). Checked against
  `workspace--same-file-p` in `data-model.org` at commit 37d04c45 (task
  `workspaces-file-equal-p-timestamp-race`, merging in parallel): truename,
  then device+inode; nil for remote or missing paths. At my base 6d061857 the
  guard still used `file-equal-p`, so this line describes the post-merge code.

**Latent issue noticed:** see disc-docs-readme-1 (the literate claim that the
add-worktree option is absent without magit is false).

Tests not run: documentation-only change (two prose files and this task file);
no `.org` source or `.el` touched.

## Discoveries

- discovery_id: disc-docs-readme-1
  class: invariant-gap
  description: |
    portable-bookmarks.org § "What is offered, and why not" says the
    add-worktree option is fboundp-guarded "so with magit absent the option
    is absent rather than an error". But git-worktrees.el defines
    jf/workspace--add-worktree unconditionally (only the integration
    REGISTRATION sits under with-eval-after-load 'magit), so the fboundp
    guard is always true once git-worktrees loads. With magit absent the
    option is offered and, when chosen, signals void-function
    magit-main-branch, which the chooser's barrier turns into a decline
    ("the missing-repo choice failed (...)"). Safe (nothing opens), but the
    literate claim is wrong. No effect in this config, which always loads
    magit. The README avoids the claim ("needs ... the git-worktree module
    loaded").
  affected_register_entry: register/boundary/portable-bookmarks-consumer
  recommendation: |
    Either gate the offer on (featurep 'magit) as well, or correct the prose
    in portable-bookmarks.org to say the option is offered and declines.

- discovery_id: disc-docs-readme-2
  class: spec-signal
  description: |
    Planning text contradicts the code at base on three points the README had
    to settle: (1) design.md D9's title "…and is not remembered" is superseded
    by the 2026-09-27 user decision (remembered like any visit); (2)
    proposal.md § Impact says layout records in workspaces.eld are
    "untouched by this slice", but seam 1 stamps them and seam 2 resolves
    them on restore (with the layouts.el decline gate); (3) proposal.md
    § Impact says the module is registered in jf/enabled-modules in init.org,
    but it is loaded from config/workspaces/workspaces.org.
  recommendation: |
    Fold into disposition-spec-design-amendments (or its successor) so
    design.md/proposal.md match the code and the README.
