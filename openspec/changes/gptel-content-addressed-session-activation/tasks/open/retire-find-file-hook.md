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
