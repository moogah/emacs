---
name: off-workspace-restore
description: When not in a workspace, prompt from the registry and restore the chosen workspace before resolving, without capturing the perturbation
change: workspace-portable-bookmarks
status: blocked
task_class: feature
on_critical_path: false
cycle: deferred
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

1. Add `workspace-bookmark--establish-workspace (identity)` called by the jump
   seam when `(workspace--current-name)` returns nil for a *stamped* record:
   - build candidates from `(workspace--registered-names)` — the registry, NOT
     live tabs, so a saved workspace with no tab in this session is offerable;
   - exclude broken workspaces: skip any whose registry plist satisfies
     `workspace--broken-p` (the package refuses to switch to or restore them and
     signals a `user-error` naming `workspace-re-anchor` / `workspace-purge`);
   - order candidates so workspaces that already hold a checkout of IDENTITY come
     first (use `workspace-bookmark--workspace-checkout`), since those resolve
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
   `missing-repo` — read-only canonical checkout, or cancel. Do NOT open the
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
