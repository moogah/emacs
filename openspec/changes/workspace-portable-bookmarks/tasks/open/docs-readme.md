---
name: docs-readme
description: Document portable bookmarks, the outcome set, and the off-workspace restore flow in the workspaces package README
change: workspace-portable-bookmarks
status: blocked
task_class: doc
on_critical_path: false
cycle: deferred
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
   - **The outcome table** — `resolved`, `not-on-branch`, `missing-repo`,
     `no-clone`, `non-portable`, each with what the user sees. State plainly that
     the main checkout is never a silent fallback.
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
