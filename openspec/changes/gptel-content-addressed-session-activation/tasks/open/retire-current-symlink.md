---
name: retire-current-symlink
description: Remove the current-symlink machinery (writer and its callers, reader, constant, helper, and tests) now that it has no production reader and identity is drawer-resident.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:retire-find-file-hook"
---

## Files to modify

- `config/gptel/sessions/filesystem.org` (modify) — remove `jf/gptel--update-current-symlink` (`:183`), `jf/gptel--get-current-branch-name` (`:149`), and `jf/gptel--current-symlink-path` (`:145`).
- `config/gptel/sessions/constants.org` (modify) — remove `jf/gptel-session--current-link` (`:85`).
- `config/gptel/sessions/commands.org` (modify) — remove the `jf/gptel--update-current-symlink` call at `:654` (the create path). (The `:363` call lived inside the now-deleted auto-init function — confirm it is already gone.)
- `config/gptel/sessions/branching.org` (modify) — remove the `jf/gptel--update-current-symlink` call at `:321`.
- `config/gptel/sessions/filesystem-test.el` (modify) — remove the `get-current-branch-name` assertion (`:111`).
- `config/gptel/sessions/test/filesystem/directory-templates-spec.el` (modify) — drop the symlink update step / assertions (`:146`, header note `:23`).

## Implementation steps

1. Grep first to confirm the full caller set is exactly the known sites:
   `grep -rn "update-current-symlink\|get-current-branch-name\|current-symlink-path\|current-link" config/gptel --include="*.org" --include="*-spec.el" --include="*-test.el"`.
2. Remove the three functions and the constant.
3. Remove the two production callers (create path `commands.org:654`, branch path `branching.org:321`). Branch/session creation no longer maintains a `current` pointer.
4. Remove the symlink tests / assertions.
5. Tangle every edited `.org`; run the affected test dirs.

## Design rationale

The `current` symlink had no production reader (only a test read it). Once identity is drawer-resident and session-dir is marker-derived, it encodes nothing anyone consumes; leaving write-only dead code would contradict the complete-removal intent. (design.md §Decision D6; risk "Removing the symlink breaks an external consumer" — grep-confirmed none.)

## Verification

- `./bin/tangle-org.sh` on all edited `.org` files succeeds.
- `grep -rn "current-symlink\|get-current-branch-name\|current-link" config/gptel` returns NOTHING outside historical docs.
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` and the ERT `filesystem-test.el` — green.
- Done = no symlink writer, reader, constant, or test remains.

## Context

design.md § Decision "D6. Retire the current symlink".

## Cycle 5 updates (cycle-1781463961)

- **Blocker cleared — now READY.** `retire-find-file-hook` merged (e398898). It already removed the **only production caller that wrote the symlink** — the `jf/gptel--update-current-symlink` call inside the deleted `jf/gptel--auto-init-session-buffer`. So this task's remaining work is the symlink machinery itself: the writer `jf/gptel--update-current-symlink`, its other callers (check `jf/gptel--create-session-core` / branch creation per the on-touch architect note), the reader `jf/gptel--get-current-branch-name`, the `jf/gptel--current-symlink-path` helper / `jf/gptel-session--current-link` constant, and the symlink tests.
- **Cited register entry** `register/invariant/activation-and-identity-are-content-not-path`: still **speculated**. With the find-file-hook/auto-init/regex remnant gone (cycle-5), the symlink is ONE of the two last path-derived remnants (the other is the 2 `session-id-from-directory` identity callers — consumer-migration). Landing this task + that migration + `regression-sweep`'s grep-audit flips the invariant to `confirmed`. See `.orchestrator/cycles/cycle-1781463961/reconciliations/invariant-activation-and-identity-are-content-not-path.md`.
- File contention: this task edits `filesystem.org` + `constants.org` + `commands.org`. No other task is ready, so it is the natural cycle-6 solo pick.
