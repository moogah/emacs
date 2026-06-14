---
name: retire-find-file-hook
description: Remove the find-file-hook registration, jf/gptel--auto-init-session-buffer, the three layout regexes, and the stale find-file-hook comments across the codebase.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:magic-mode-alist-activation"
  - "blocked-by:mode-hook-binder"
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — remove `(add-hook 'find-file-hook #'jf/gptel--auto-init-session-buffer)` (`:932`), the entire `jf/gptel--auto-init-session-buffer` defun and its three layout regexes / `../..` walks (`:225-372`), and the surrounding stale comments (`:37`, `:247`, `:758`, `:928`).
- `config/gptel/sessions/branching.org` (modify) — update the comment at `:403` ("auto-initializes via find-file-hook") to describe content-addressed activation.
- `config/gptel/tools/persistent-agent.org` (modify) — update the prose at `:527` ("find-file-hook auto-init pipeline") to describe content-addressed activation.
- `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el` (modify/remove) — replace assertions about the find-file-hook pipeline with the content-addressed equivalent (or delete if fully superseded by mode-activation/binder specs).
- `config/gptel/sessions/test/commands/auto-init-resilience-spec.el` (modify/remove) — same treatment.

## Implementation steps

1. Delete the `find-file-hook` `add-hook` registration and the `jf/gptel--auto-init-session-buffer` function (with its `nested-agent-re` / `flat-agent-re` / branch regex `cond` and `../..` / `../../..` walks).
2. Confirm `jf/gptel--ensure-mode-once` is no longer needed (its only caller was auto-init; activation now flows through `magic-mode-alist` + the mode hook). Remove it if unused, or keep only if another caller exists — grep to decide.
3. Rewrite the three stale comments to reference content-addressed activation (magic-mode-alist signature + mode-hook binder).
4. Rework the two `auto-init-*` specs: any test that opens a file and asserts the find-file-hook fired should now assert that `magic-mode-alist` activation + the binder produced the same buffer-local/registry state. Delete cases that only existed to exercise the path regexes.
5. Tangle `commands.org`, `branching.org`, `persistent-agent.org`; run the affected test dirs.

## Design rationale

This is the core retirement: activation by content recognition replaces dispatch-by-path on every file open. Removing the function and its layout regexes is the spec's explicit REMOVED contract — write-only or path-archaeology remnants must not survive. (design.md §Decisions D1, D4, D6; specs `sessions-persistence` REMOVED "Auto-initialization enables gptel-chat-mode", `sessions-branching` REMOVED "Auto-initialization of new branches".)

## Verification

- `./bin/tangle-org.sh` on all three edited `.org` files succeeds.
- `grep -rn "find-file-hook\|auto-init-session-buffer" config/gptel --include="*.org"` returns NOTHING (no registration, no function, no stale comments).
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` — green with the reworked specs.
- Done = the find-file-hook mechanism is entirely gone and sessions still activate+bind via content addressing.

## Context

design.md § Decisions "D1", "D4", "D6"; specs `sessions-persistence` REMOVED "Auto-initialization enables gptel-chat-mode", `sessions-branching` REMOVED "Auto-initialization of new branches".

## Cycle 2 updates (cycle-1781451784)

### Already-shipped / now-available
- `magic-mode-alist-activation` merged (7e524af): content-addressed activation is now **live** as the replacement for the find-file-hook path. The mechanism this task retires is now redundant with the new entry point — proceed with removal once `mode-hook-binder` lands (its other blocker).

### Cited register entries
- `register/invariant/activation-and-identity-are-content-not-path`: still **speculated**. The activation half is confirmed (magic merged); landing THIS task (+ `retire-current-symlink`, consumer migration) is what flips the full invariant to `confirmed`. This task is one of the entry's carriers. See `.orchestrator/cycles/cycle-1781451784/reconciliations/invariant-activation-and-identity-are-content-not-path.md`.

## Cycle 3 updates (cycle-1781453946)

- `discovery-reads-drawers` merged (7dd50b5): `init-registry` and `find-all-branches-with-agents` no longer derive identity from directory names. After cycle-3, `jf/gptel--session-id-from-directory` is reachable for IDENTITY only via the resolver fallback branch. The end-of-cycle architect noted **3 direct `session-id-from-directory` callers still bypass the resolver — in `commands.el`, `branching.el`, and `persistent-agent`** — these are the un-rewired legacy paths this retirement family (this task + `retire-current-symlink` + consumer migration) addresses on the way to flipping `activation-and-identity-are-content-not-path` to confirmed. Blocker remaining: `mode-hook-binder` (now ready).

## Cycle 4 updates (cycle-1781458723)

- **Last blocker cleared — now READY.** `mode-hook-binder` merged (919577a). With `magic-mode-alist-activation` (cycle-2) and `mode-hook-binder` (cycle-4) both done, content-addressed ACTIVATION and BINDING are live; this task is the next critical-path pick.
- **Cited register entry** `register/invariant/activation-and-identity-are-content-not-path`: the BINDING half is now confirmed content-only (binder uses no path test; three independent passes agreed). The invariant remains **speculated** by its own terms — landing THIS task + `retire-current-symlink` + the 3 `session-id-from-directory` caller migrations (commands/branching/persistent-agent) is the REMOVAL work that flips it to `confirmed` (regression-sweep's grep-audit is the closing evidence). See `.orchestrator/cycles/cycle-1781458723/reconciliations/invariant-activation-and-identity-are-content-not-path.md`.
- **Coexistence to retire (disc-mode-hook-binder-3).** The legacy `jf/gptel--auto-init-session-buffer` on `find-file-hook` now COEXISTS with the new binder. It self-suppresses for signature-bearing buffers — the binder runs first on `gptel-chat-mode-hook`, sets `jf/gptel--session-id`, and auto-init early-returns on its `(not (bound-and-true-p jf/gptel--session-id))` guard. So for the content-addressed path this is already dead code; retiring it is the cleanup that resolves the two-hook duplication. Before removal, confirm no SIGNATURELESS legacy path (a session file with no `:GPTEL_` drawer at all) still relies on auto-init — those are the only buffers the binder's content guard skips.
- **Fixture re-audit (disc-mode-hook-binder-2).** When you retire the path-layout auto-init, re-audit any `session.org` fixtures/data that pair `GPTEL_PARENT_SESSION_ID` with a `branches/<b>/` path: under the content model the parent-id key is the sole type discriminator, so such a session is `agent`-typed and resolves to its own branch-dir, NOT a branch session. `auto-init-chat-mode-spec.el` already had one such contradictory fixture corrected this cycle (made self-describing); check `branching.el` / `persistent-agent` test data for the same shape during migration.

## Observations

### What was removed (production)
- `config/gptel/sessions/commands.org`:
  - The `find-file-hook` registration `(add-hook 'find-file-hook #'jf/gptel--auto-init-session-buffer)` (and its "Hook Installation" prose).
  - The entire `jf/gptel--auto-init-session-buffer` defun, including its three layout regexes (`nested-agent-re`, `flat-agent-re`, the inline branch regex) and the `../..` / `../../..` `expand-file-name` walks, plus the `current`-symlink update call (`jf/gptel--update-current-symlink`) and the `session-type` cond.
  - `jf/gptel--ensure-mode-once` (see below).
  - All surrounding stale comments/docstrings (commentary header, the persistent-session docstring "auto-initializes ... via find-file-hook", and two binder comments that said "mirrors auto-init isolation").
- `config/gptel/sessions/branching.org`: rewrote the `find-file new-branch-file` comment to describe content-addressed activation (magic-mode-alist + binder).
- `config/gptel/tools/persistent-agent.org`: rewrote the spawn docstring ("configured by the find-file-hook auto-init pipeline" → content-addressed activation).

### jf/gptel--ensure-mode-once: REMOVED (caller evidence)
Grepped the whole `config/gptel/` for `(jf/gptel--ensure-mode-once)` / `'jf/gptel--ensure-mode-once`. Its ONLY production caller was inside `jf/gptel--auto-init-session-buffer`. All other references were test mocks (`auto-init-resilience-spec.el`, `branching-integration-spec.el`). With auto-init gone it is unused — removed. Content-addressed activation flips the mode via `magic-mode-alist` (`config/gptel/chat/mode.org`), so no `ensure-mode-once` shim is needed. Post-removal grep for live callers returns nothing.

### Spec cases: reworked vs deleted
- **`auto-init-chat-mode-spec.el` — DELETED (git rm).** Every behavior it asserted (four vars set, registry entry, `gptel-mode` never enabled, no `gptel--save-state`/`restore-state`, parent-id from drawer, no-op for non-session `.org`) is already covered by `mode-hook-binder-spec.el` (content-addressed binder) and `config/gptel/chat/test/mode-activation-spec.el`. Its branch / nested-agent / flat-agent `it` cases existed solely to exercise the deleted path regexes, so there was nothing left to rework — it was fully superseded.
- **`auto-init-resilience-spec.el` — REWORKED.** Original pinned "a mode-flip/preset error must not swallow the four buffer-local vars" by mocking `jf/gptel--ensure-mode-once` to error. The content-addressed equivalent: the binder sets the four vars BEFORE the narrow `condition-case` that wraps registry+autosave, so a `jf/gptel--register-session` failure cannot abort var-setting. Rewrote the suite to drive `jf/gptel--bind-session-buffer` on a real on-disk signature-bearing fixture with `jf/gptel--register-session` mocked to error; asserts vars still set, binder does not throw, and a negative control. (Kept the filename so the change's task->file mapping stays legible; header/commentary updated.)
- **`preset-application-spec.el` — PARTIALLY REWORKED (not in task body, but had two live callers).** Two `it` cases ("never reads metadata.yml", "never enables gptel-mode") called the deleted `jf/gptel--auto-init-session-buffer` directly. Reworked both to exercise `jf/gptel--bind-session-buffer` on a real on-disk drawer fixture. The describe-block real-mode integration cases were already content-addressed (real `find-file-noselect` → magic-mode-alist) and only needed comment fixes.
- **`branching-integration-spec.el` — PARTIALLY REWORKED (not in task body, but had the only other live caller).** The "populates the registry lazily on first open" `it` called the deleted function; reworked to load the branch `session.org` content into the buffer (so the signature guard matches) and call `jf/gptel--bind-session-buffer`. Dropped the now-unneeded `gptel-get-preset`/`gptel--apply-preset`/`gptel-chat-mode`/`ensure-mode-once` mocks. Comments referencing the deleted function name updated.

### disc-3 (signatureless legacy path) — NO concern found
The old auto-init's distinguishing capability vs the binder was handling buffers with NO `:GPTEL_` drawer (the binder's `jf/gptel--session-signature-p` guard skips those). I checked whether any signatureless session path matters: (a) all session creation paths write a `:PROPERTIES:` drawer with `:GPTEL_*:` keys (`jf/gptel--create-session-core`, branch creation, agent spawn), so on-disk sessions always carry a signature; (b) the cited disc — pre-drawer "legacy" sessions — are handled by the binder's drawer-first resolver FALLBACK (path basename), which `mode-hook-binder-spec.el` explicitly tests ("resolves a legacy session (no GPTEL_BRANCH/GPTEL_SESSION_ID) from the path"). A drawer with even one `:GPTEL_PRESET:` key (which every created session has) trips the signature. There is no live path that creates a session.org with zero `:GPTEL_` keys, so no coverage is dropped. NOT escalating.

### disc-2 (parent-id-in-branches/ fixtures) — none found in scope
Audited the fixtures in the files I touched and the persistent-agent test data for `GPTEL_PARENT_SESSION_ID` paired with a `branches/<b>/` path WITHOUT a self-describing `GPTEL_SESSION_ID`/`GPTEL_BRANCH`. The only such fixture (in the now-deleted `auto-init-chat-mode-spec.el`) was already made self-describing last cycle. The reworked resilience/preset/branching fixtures all either omit parent-id or are self-describing. No contradictory fixtures remain in the files I changed.

### 3 session-id-from-directory callers (NOTED, not migrated — separate tasks)
Per scope I did NOT migrate these. After removing auto-init, the `jf/gptel--session-id-from-directory` call that lived INSIDE `jf/gptel--auto-init-session-buffer` (commands.el) is gone with the function. The remaining direct callers flagged by cycle-3's architect are in `branching.org`/`branching.el` and `persistent-agent.org`/`persistent-agent.el` (the `(jf/gptel--session-id-from-directory session-dir)` uses for branch/agent identity). Those belong to the consumer-migration tasks, not this one. Likewise the `current` symlink is owned by `retire-current-symlink`; I removed only the auto-init call site that updated it, not the symlink machinery or `jf/gptel--update-current-symlink` itself.

### Latent issue in adjacent code
None blocking. Several test files outside the named scope still carry stale "auto-init" / "find-file-hook" wording in comments and one filename (`config/gptel/tools/test/persistent-agent/auto-init-reload-spec.el`, `helpers-spec.el`, `workspace-integration-spec.el`, `pre-send-refresh-spec.el`). These are comment/string-only (no live calls to the deleted functions — verified by grep), so they don't affect the org grep-gate or test execution, and all run green. Renaming `auto-init-reload-spec.el` and scrubbing every comment is cosmetic churn better handled by regression-sweep; noted here so it isn't lost.

## Discoveries
- discovery_id: disc-retire-find-file-hook-1
  class: invariant-gap
  description: |
    This task lands the REMOVAL half of
    register/invariant/activation-and-identity-are-content-not-path.
    Removed (production): the `find-file-hook` registration, the
    `jf/gptel--auto-init-session-buffer` defun with its three layout
    regexes (nested-agent / flat-agent / branch) and `../..` /
    `../../..` walks, and `jf/gptel--ensure-mode-once`. The org
    grep-gate (`find-file-hook|auto-init-session-buffer` over
    config/gptel/*.org) is now EMPTY. Production `.el` (excluding
    test/) carries none of these symbols.

    REMNANTS that still block flipping the invariant to confirmed
    (NOT in this task's scope):
      1. The `current` symlink and `jf/gptel--update-current-symlink`
         machinery — owned by `retire-current-symlink`.
      2. Three direct `jf/gptel--session-id-from-directory` IDENTITY
         callers that bypass the drawer-first resolver, in
         `branching.org`/`.el` and `persistent-agent.org`/`.el` —
         owned by the consumer-migration task(s).
    Once those land, regression-sweep's grep-audit is the closing
    evidence for the invariant.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    Advance the entry's status to reflect that the removal half's
    largest carrier (find-file-hook auto-init + layout regexes +
    ensure-mode-once) is gone. Keep it speculated until
    retire-current-symlink and the 3 session-id-from-directory caller
    migrations land; then run regression-sweep's grep-audit to flip to
    confirmed.

- discovery_id: disc-retire-find-file-hook-2
  class: dead-branch
  description: |
    disc-3 (coexistence / signatureless-legacy) resolved as a NON-issue.
    The legacy auto-init's only behavior the content-addressed binder
    does not replicate was binding buffers with NO `:GPTEL_` drawer
    (the binder's `jf/gptel--session-signature-p` guard skips those).
    Every session-creation path writes a `:PROPERTIES:` drawer with at
    least `:GPTEL_PRESET:`, so all on-disk sessions are
    signature-bearing; pre-drawer "legacy" sessions are covered by the
    binder's drawer-first resolver path-basename FALLBACK (tested in
    mode-hook-binder-spec.el). No live path produces a signatureless
    session.org, so removing auto-init drops no real coverage.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    No action. Record that the coexistence/signatureless concern was
    checked and is empty, so the two-hook duplication is now fully
    collapsed to the single content-addressed binder.

- discovery_id: disc-retire-find-file-hook-3
  class: duplication
  description: |
    Stale "auto-init" / "find-file-hook" wording survives in
    COMMENTS and one filename across test files outside this task's
    named scope: config/gptel/tools/test/persistent-agent/{auto-init-reload-spec.el,helpers-spec.el},
    config/gptel/sessions/test/workspace-integration-spec.el,
    config/gptel/chat/test/menu/pre-send-refresh-spec.el, and residual
    generic "auto-init" describe wording in preset-application-spec.el.
    These are comment/string-only — no live references to the deleted
    functions (verified by grep for `(jf/gptel--auto-init-session-buffer)`
    and `(jf/gptel--ensure-mode-once)`), all suites run green, and the
    org grep-gate is unaffected.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    Fold a comment/filename scrub (rename auto-init-reload-spec.el,
    update stale prose) into regression-sweep so path-archaeology
    wording doesn't read as a surviving mechanism in future audits.
    Cosmetic; do not block this task on it.
