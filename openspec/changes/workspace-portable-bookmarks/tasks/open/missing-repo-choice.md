---
name: missing-repo-choice
description: Offer a bounded choice when the current workspace holds no checkout — add worktree, read-only view, or cancel
change: workspace-portable-bookmarks
status: blocked
task_class: feature
on_critical_path: false
cycle: deferred
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
     `:context`) for the current workspace;
   - on success, re-run the resolver and continue the jump; on a `failed`
     outcome, report the reason and open nothing.

3. "View read-only": open the `:path` the resolver computed under the canonical
   checkout, then make it unmistakable that this is not a workspace copy:
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
