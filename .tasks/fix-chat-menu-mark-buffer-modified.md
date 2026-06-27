---
name: fix-chat-menu-mark-buffer-modified
description: gptel-chat-menu mutates tracked buffer-local config but never marks the buffer modified, so save-buffer is a no-op and the drawer write hook doesn't fire until the user makes an unrelated edit.
status: ready
source: openspec/changes/archive/2026-05-23-gptel-drawer-as-source-of-truth/tasks/closed/default-chat-menu-scope-to-buffer-local.md
relations:
  - discovered-from:default-chat-menu-scope-to-buffer-local
---

> Surfaced after the `gptel-drawer-as-source-of-truth` change archived. The closed task `default-chat-menu-scope-to-buffer-local` listed four required scenarios for `menu-buffer-local-spec.el`; the integration scenario ("buffer-local tool change persists to drawer on save") was specified but never written. Its absence let this code path ship with the missing-modified-flag bug.

## The bug in one sentence

After a tool toggle (or any tracked-config change) via `gptel-chat-menu`, the chat buffer's modified flag stays nil, so plain `save-buffer` does nothing — the `before-save-hook` registered by `gptel-chat-mode` never fires, and the drawer doesn't pick up the change until the user types an unrelated edit.

## Evidence

`config/gptel/chat/menu.org:1266-1268` — the chat-menu prefix body sets `gptel--set-buffer-locally` to `t` and registers a post-exit restorer, but takes no other action on exit:

```elisp
(setq gptel-chat--scope-prior gptel--set-buffer-locally
      gptel--set-buffer-locally t)
(add-hook 'transient-post-exit-hook #'gptel-chat--restore-scope-on-exit)
(transient-setup 'gptel-chat-menu))
```

`grep -n "set-buffer-modified" config/gptel/chat/menu.org config/gptel/chat/menu.el` → no matches. Nothing in the menu code path marks the buffer modified.

`config/gptel/chat/menu.org:956-958` — the save writer is wired via `before-save-hook`, which Emacs only fires when `save-buffer` actually writes the buffer (i.e. on a modified buffer).

`openspec/specs/gptel/chat-mode.md:742` carries the contract that this work satisfies:

```
#### Scenario: Buffer-local tool change persists to drawer on save
- WHEN the user toggles a tool via `gptel-chat-menu` and then invokes `save-buffer`
- THEN the saved drawer contains a `:GPTEL_TOOLS:` line listing the new buffer-local tool list
```

## Files to modify

- `config/gptel/chat/menu.org` (modify literate source; `bin/tangle-org.sh` regenerates `menu.el`):
  1. Diff-on-exit subsection — entry snapshot + post-exit diff hook (defense in depth).
  2. Live-drawer-write subsection — `:after` advice on `gptel--set-with-scope` so the drawer text updates immediately on commit.
- `config/gptel/chat/test/menu/menu-buffer-local-spec.el` (modify — add specs for both mechanisms).

## Implementation steps

1. **Add the snapshot defvar.** In `config/gptel/chat/menu.org`, alongside the existing `gptel-chat--scope-prior` defvar (~line 1073), add:
   - `gptel-chat--config-snapshot-prior` — global defvar carrying a plist `(:buffer BUF :snapshot ALIST)` where ALIST is `((VARNAME . VALUE) ...)` for the eight tracked vars below. Same single-global-defvar pattern as `gptel-chat--scope-prior`; same "safe because `transient-post-exit-hook` only fires on outermost prefix exit" rationale, documented in the docstring.

2. **Add the capture helper.** A pure function `gptel-chat--capture-config-snapshot` that returns the alist for the current buffer. Tracked vars (matches what `gptel-chat--write-config-drawer` reads, confirmed against `jf/gptel-scope-profile--snapshot-spec` and the writer body at `menu.org` lines ~835-882):
   - `gptel--preset`
   - `gptel-model`
   - `gptel-backend`
   - `gptel-tools`
   - `gptel-temperature`
   - `gptel-max-tokens`
   - `gptel--num-messages-to-send`
   - `jf/gptel--parent-session-id`

3. **Capture on entry.** In the `gptel-chat-menu` prefix body (lines 1266-1268), add — next to the existing scope-prior capture — a setq of `gptel-chat--config-snapshot-prior` to `(list :buffer (current-buffer) :snapshot (gptel-chat--capture-config-snapshot))`.

4. **Diff on exit.** Add a one-shot `gptel-chat--maybe-mark-modified-on-exit` registered on `transient-post-exit-hook` alongside the existing scope restorer. It:
   - Reads `gptel-chat--config-snapshot-prior` (the plist with `:buffer` and `:snapshot`).
   - For each `(var . old-value)` in the snapshot, compares the current buffer-local value (via `buffer-local-value var buf`) against `old-value` with `equal`.
   - If any pair differs and the buffer is still live, calls `(with-current-buffer buf (set-buffer-modified-p t))`.
   - Always clears `gptel-chat--config-snapshot-prior` to nil and `remove-hook`s itself.

5. **Tangle.** `./bin/tangle-org.sh config/gptel/chat/menu.org` — the script auto-validates with `check-parens`.

6. **Add three specs** (see Tests below).

## Tests

In `config/gptel/chat/test/menu/menu-buffer-local-spec.el`, add a new `describe` block before the closing `(provide 'menu-buffer-local-spec)`:

1. **`(describe "snapshot diff on menu exit")` — `it "does not mark the buffer modified when no tracked var changed"`.** With-temp-buffer; stub `transient-setup` to `#'ignore`; bind a fresh `gptel-chat--config-snapshot-prior nil`, `transient-post-exit-hook nil`; `call-interactively 'gptel-chat-menu`; drive `gptel-chat--spec-drive-post-exit`; assert `(buffer-modified-p)` is nil.

2. **`it "marks the buffer modified when a tracked buffer-local changed"`.** Same setup; before driving the post-exit, mutate one tracked var (`(setq-local gptel-tools '(:new-tool))`); drive post-exit; assert `(buffer-modified-p)` is t.

3. **`(describe "integration: menu toggle then save-buffer writes drawer")` — `it "save-buffer reflects the new :GPTEL_TOOLS: line on disk"`.** Reuse the working pattern at `config/gptel/chat/test/menu/system-prompt-file-spec.el:232-276`:
   - `make-temp-file` for a session.org dir; write a minimal session content (drawer at point-min with `:GPTEL_PRESET: coding`, plus `#+begin_user\n\n#+end_user\n` body).
   - `find-file-noselect` the file; in that buffer, activate `gptel-chat-mode` (registers the save hook).
   - `spy-on 'save-buffer :and-call-through`.
   - In the buffer: `call-interactively 'gptel-chat-menu`; mutate `gptel-tools` buffer-locally to a new list; drive post-exit.
   - Assert `(buffer-modified-p)` is t.
   - Call `(save-buffer)`. Assert the spy was hit.
   - Re-read the file with `insert-file-contents` and assert the on-disk `:GPTEL_TOOLS:` line contains the new tool list.
   - `unwind-protect` → `kill-buffer` and `delete-directory`.

## Design rationale

Two complementary mechanisms, both adding only chat-mode-private code:

1. **Snapshot/diff on `gptel-chat-menu` entry/exit (defense in depth).** Mirrors the existing `gptel-chat--scope-prior` + `gptel-chat--restore-scope-on-exit` pattern next door — same defvar discipline, same one-shot post-exit hook, same buffer-keyed semantics. Catches any mutation path that doesn't go through `gptel--set-with-scope`, including direct `setq-local` from elisp.

2. **`:after` advice on `gptel--set-with-scope` (live drawer write).** The natural commit-point hook: upstream's tool-selector sub-transient (`gptel-tools` at `gptel-transient.el:1081`) batches per-tool toggles in its transient-scope and only calls `gptel--set-with-scope` from the `RET` Confirm suffix — the `q` Cancel path doesn't invoke the setter at all. So the advice fires once per confirmed selection, never on cancelled selections, never on individual letter-key toggles. For one-shot infixes (model, temperature, etc.) the suffix calls the setter directly on commit. The advice re-runs `gptel-chat--write-config-drawer` so the drawer text updates immediately; `org-entry-put` modifying the drawer text marks the buffer modified as a side effect.

   Guards: `(eq scope t)` (buffer-local writes only — global / oneshot scopes are upstream-private behaviour we don't claim), `(derived-mode-p 'gptel-chat-mode)` (chat-mode buffers only — `M-x gptel-menu` in non-chat buffers is unaffected), and `(memq sym gptel-chat--tracked-config-vars)` (only drawer-emitted vars trigger a drawer write).

   Why the advice doesn't fire spuriously during preset application: chat-mode's preset application uses `(set (make-local-variable sym) val)` directly, not `gptel--set-with-scope`. So fresh buffers don't get marked modified at mode activation.

Alternative considered:

- `add-variable-watcher` on each tracked var: more general (catches any mutation source) but fires during preset application at mode activation, which would mark fresh buffers modified. Filtering that out requires "after-init" state, which is fragile.

The advice approach is local in effect — only chat-mode buffers see any behaviour change — even though `advice-add` is global in installation.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` succeeds (paren check passes).
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes including the three new specs; pre-existing specs in `menu-buffer-local-spec.el` (entry/exit scope-prior wiring) remain green.
- `./bin/run-tests.sh --report` total ran rises by 3, no new failures elsewhere.
- Manual: `./bin/emacs-isolated.sh`, open a `session.org` under a branch dir, `M-x gptel-chat-menu` → "Select tools" → toggle a tool → exit, `C-x C-s` (no manual edit), inspect drawer: `:GPTEL_TOOLS:` reflects the toggle on the first save.

## Context

- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md`.
- Closed task that specified the missing scenario: `openspec/changes/archive/2026-05-23-gptel-drawer-as-source-of-truth/tasks/closed/default-chat-menu-scope-to-buffer-local.md` (Implementation steps §6, scenario 4 — "buffer-local tool change persists to drawer on save (integration)").
- Spec scenario this fix honors: `openspec/specs/gptel/chat-mode.md:742` ("Buffer-local tool change persists to drawer on save").
- Existing pattern this work mirrors: `config/gptel/chat/menu.org:1062-1092` (`gptel-chat--scope-prior` + `gptel-chat--restore-scope-on-exit`).
- Working test pattern to adapt for the integration spec: `config/gptel/chat/test/menu/system-prompt-file-spec.el:232-276`.
