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

## Observations

- Verified the upstream timing claim against `runtime/straight/repos/transient/lisp/transient.el` `transient--post-exit` (lines 3041–3081). The fire order is: `transient-exit-hook` (line 3072) → clear `transient-current-prefix` (line 3074) → branch on `resume`/`replace` → `transient-post-exit-hook` (line 3081). The fix's switch from `-exit-` to `-post-exit-` lands restoration after the prefix is cleared *and* skips the hook entirely when a sub-transient resumes. Both behaviours match the `exit_invariant` (`register/boundary/chat-menu-scope-default`) precisely.
- Kept `gptel-chat--scope-prior` as a top-level `defvar` rather than promoting to buffer-local. Single-frame correctness is restored by the hook-mechanism fix; the residual edge case (recursive minibuffer / multi-frame re-entry that opens a *second* `gptel-chat-menu` while a first is mid-flight) was not produced by the current chat-menu shape, and a buffer-local conversion would have widened the diff and changed the entry-time `setq` semantics in the prefix body. Task body explicitly authorised this trade-off in step 3 ("only if the change is small and obviously correct"). Documented the constraint with a top-level code comment above the defvar in `menu.org`.
- The new test helper `gptel-chat--spec-drive-post-exit` invokes the real `transient--post-exit` with `transient--exitp` set to `'exit` and `transient--stack` cleared so it traverses the non-replace, non-resume path that fires `transient-post-exit-hook`. This is materially closer to the production call path than the previous direct-call tests, which constructed a `transient-current-prefix` binding by hand and never exercised `run-hooks`. Added a third end-to-end scenario that re-invokes the post-exit pass after the hook has self-removed, asserting one-shot semantics — this protects against future regressions where a hook is registered globally and outlives a single menu invocation.
- Did not add a separate "abort path" scenario via `transient--with-emergency-exit`. The implementation is unconditional now (no discriminator), so the abort and commit paths reach the same code with the same precondition (`transient-post-exit-hook` populated, prefix cleared). One commit-style scenario plus the one-shot scenario already cover the contract; an abort scenario would be a near-duplicate of the commit one.

## Discoveries

```yaml
- class: invariant-gap
  affected_register_entry: register/boundary/chat-menu-scope-default
  summary: |
    The register's `exit_invariant` clause stated only "regardless of how
    the transient ends, gptel--set-buffer-locally returns to its entry
    value." Cycle-5 integrate's `status_note` then narrowed the
    enforcement claim to "verified by 9 specs in
    menu-buffer-local-spec.el" — but those 9 specs all called the
    restorer directly with hand-constructed `transient-current-prefix`
    bindings, never running through `run-hooks 'transient-exit-hook`.
    The shape of the verification (direct-call vs end-to-end) was not
    declared on the register entry, so a reader trusted the
    "verified by" line without realising the discriminator was untested
    against the real call path. Suggest adding a `verification_shape`
    or `verified_against` clause to the register schema for boundary-
    tier entries that depend on host-system call ordering, so future
    speculations like "guard on `transient-current-prefix`" cannot
    look reconciled while skipping the hook-fire path entirely.
  evidence: |
    interfaces.org L1252–1261 (status_note) and the now-replaced
    direct-call specs in
    config/gptel/chat/test/menu/menu-buffer-local-spec.el (pre-fix).

- class: deviation
  affected_register_entry: register/boundary/chat-menu-scope-default
  summary: |
    The register entry's status_note narrative ("the hook checks
    transient-current-prefix so sub-transients do not trigger an early
    restore") and the prior implementation it described are no longer
    accurate after this fix. The new mechanism uses
    `transient-post-exit-hook`, which is *naturally* skipped on sub-
    transient exits, so no discriminator is needed. The note's three
    sentences about `transient-current-prefix` are stale. Recommend
    the architect re-reconcile this entry in the next cycle with a
    revised status_note that reflects the post-exit-hook mechanism
    and explicitly cites the corrected verification shape.
  evidence: |
    interfaces.org L1244–1261 (current status_note) vs the new
    `gptel-chat--restore-scope-on-exit' implementation in
    config/gptel/chat/menu.org §"Scope default: buffer-local for
    chat-menu lifetime".

- class: dead-branch
  affected_register_entry: register/boundary/chat-menu-scope-default
  summary: |
    The previous test "does NOT restore when transient-current-prefix
    is non-nil (sub-transient exit)" was exercising a branch that no
    longer exists. With the post-exit-hook switchover, sub-transient
    exits do not fire `transient-post-exit-hook' at all (transient
    only runs it when no transient resumes), so the discriminator
    branch and its test are jointly dead. Removed both in this task.
    Documented here so future readers do not interpret the test
    deletion as a coverage regression.
  evidence: |
    runtime/straight/repos/transient/lisp/transient.el lines 3062–3081
    (only the non-resume, non-replace branch runs
    `transient-post-exit-hook'); old spec at
    config/gptel/chat/test/menu/menu-buffer-local-spec.el L96–104
    pre-fix (now removed).
```
