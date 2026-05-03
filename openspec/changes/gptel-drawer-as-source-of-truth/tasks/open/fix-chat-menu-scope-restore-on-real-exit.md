---
name: fix-chat-menu-scope-restore-on-real-exit
description: Fix `gptel-chat--restore-scope-on-exit` so the buffer-local scope toggle is actually restored after `gptel-chat-menu` closes. The current `(unless transient-current-prefix ...)` discriminator never fires on normal commit-style exit, leaving `gptel--set-buffer-locally` stuck `t` globally after every menu session. Replace the hook + guard, replace the discriminator-only tests with end-to-end coverage, and reconsider the global `gptel-chat--scope-prior` defvar.
change: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:default-chat-menu-scope-to-buffer-local
---

## Files to modify

- `config/gptel/chat/menu.org` — `gptel-chat--restore-scope-on-exit` and the `add-hook` call in `gptel-chat-menu`'s prefix body
- `config/gptel/chat/test/menu/menu-buffer-local-spec.el` — replace the discriminator-only tests with real-call-path coverage
- (verify) `config/gptel/chat/menu.el` — re-tangled output

## Why

Production bug verified against `transient.el` (build/transient/transient.el:3072–3081):

```elisp
(run-hooks 'transient-exit-hook)               ; line 3072 — fires here
(when command
  (setq transient-current-prefix nil)          ; line 3074 — cleared AFTER
  ...)
(cond ...
      ((not replace)
       (transient--quit-kludge 'disable)
       (run-hooks 'transient-post-exit-hook))) ; line 3081 — only this fires when fully done
```

`transient-exit-hook` runs *before* `transient-current-prefix` is cleared, and `transient--export` (called by every committing suffix's pre-command setup) sets `transient-current-prefix` to the prefix object. The `(unless transient-current-prefix ...)` guard therefore fails on every normal commit-style exit, and the restore never runs. After every chat-menu interaction, `gptel--set-buffer-locally` is stuck `t` globally. The C-g path "works" only in the degenerate case where no sub-transient was used since the menu opened.

The register's `exit_invariant` ("regardless of how the transient ends") is broken.

The existing tests pass because they call `gptel-chat--restore-scope-on-exit` directly with a manually-constructed `transient-current-prefix` binding, bypassing the real `transient--export → run-hooks` sequence.

## Implementation steps

1. In `config/gptel/chat/menu.org`, change the hook from `transient-exit-hook` to `transient-post-exit-hook` in both the `add-hook` (in `gptel-chat-menu`'s prefix body) and the `remove-hook` inside `gptel-chat--restore-scope-on-exit`. `transient-post-exit-hook` fires only after `transient-current-prefix` is cleared and only when no transient resumes — exactly the "outermost menu fully done" semantics needed. Drop the `(unless transient-current-prefix ...)` guard since `transient-post-exit-hook` is already correctly scoped.
2. Update the function's docstring to reflect the new mechanism (no more sub-transient discrimination needed; the hook is unconditional).
3. Decide whether to keep `gptel-chat--scope-prior` as a global `defvar` or move it to a buffer-local / closure-captured value. The advisory in the source review (Finding 2) flags that the global accumulates corruption from any prior failed restore. With the hook-mechanism fix in place, single-frame use is safe; multi-frame re-entry is the remaining edge case. Recommend a buffer-local conversion only if the change is small and obviously correct; otherwise leave it as a defvar with a brief code comment about the constraint.
4. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`.
5. In `config/gptel/chat/test/menu/menu-buffer-local-spec.el`, replace the direct-call tests of `gptel-chat--restore-scope-on-exit` with scenarios that exercise the real call path. Acceptable shapes:
   - Stub `transient-setup` to immediately invoke `transient--post-exit` with `command` set to a known suffix (commit-style); assert `gptel--set-buffer-locally` is restored.
   - Use `transient--with-emergency-exit` or `unwind-protect` to simulate an abort path; assert restoration there too.
   - Drop the now-irrelevant "doesn't restore when sub-transient exits" scenario (the new hook never fires for sub-transient exits, so the test is moot).
6. Re-run `./bin/run-tests.sh -d config/gptel/chat`. Expect all menu-buffer-local-spec scenarios pass and that the new end-to-end scenarios fail before the production fix and pass after.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/chat
grep -n "transient-post-exit-hook\|transient-exit-hook" config/gptel/chat/menu.el
grep -n "transient-current-prefix" config/gptel/chat/menu.el
```

Expect: only `transient-post-exit-hook` references in the production code (no `transient-exit-hook`, no `transient-current-prefix` discriminator). Tests green.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/default-chat-menu-scope-to-buffer-local.md` (Findings 1, 2, 3).

Cited register entry: `interfaces.org#register-boundary-chat-menu-scope-default` (the `exit_invariant` clause is the violated contract).
