---
name: regression-sweep
description: Full config/gptel test run plus manual smoke of activation, creation, branching, and agents; assert no find-file-hook or symlink remnants remain.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:discovery-reads-drawers"
  - "blocked-by:retire-find-file-hook"
  - "blocked-by:retire-current-symlink"
---

## Files to modify

- None (verification-only task). May add a short note to the change's verification log if regressions are found.

## Implementation steps

1. Run the full gptel suite: `./bin/run-tests.sh -d config/gptel` (both frameworks). All green.
2. Static remnant checks (must all return nothing in `.org` sources):
   - `grep -rn "find-file-hook" config/gptel --include="*.org"`
   - `grep -rn "auto-init-session-buffer" config/gptel --include="*.org"`
   - `grep -rn "current-symlink\|get-current-branch-name\|current-link" config/gptel --include="*.org"`
3. Manual smoke in an isolated Emacs (`./bin/emacs-isolated.sh`):
   - Create a session (`M-x jf/gptel-persistent-session`) → opens in `gptel-chat-mode`; drawer carries `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:`.
   - Close the buffer, reopen `session.org` via `find-file` and via `dired` → activates in chat-mode (auto-adoption preserved).
   - Open an ordinary `.org` file → stays `org-mode` (no false activation).
   - Branch the session (`M-x jf/gptel-branch-session`) → new branch activates, registry keyed from the new branch drawer, shared session-id, distinct branch.
   - Spawn a PersistentAgent → agent buffer activates via `find-file-noselect`, drawer carries its own id + `:GPTEL_PARENT_SESSION_ID:`, request runs.
   - Move/rename a session directory, reopen its `session.org` → identity unchanged (drawer-resident).
   - Restart Emacs → `init-registry` rebuilds registry keyed from drawers (basename only for any legacy session).
4. Record any defects as follow-up tasks; otherwise mark the change ready for `/opsx-verify` and archive.

## Design rationale

The change spans activation, identity, discovery, and two retirements; a final end-to-end pass confirms the pieces compose and that the complete-removal contract (no find-file-hook, no symlink) actually holds in tangled code, not just in the deltas. The move/rename and restart smokes exercise the move-safety and discovery properties that unit tests approximate. (design.md §Migration Plan, §Testing Approach.)

## Verification

- `./bin/run-tests.sh -d config/gptel` — fully green.
- All three remnant greps return nothing.
- Manual smoke checklist passes (activation by content, auto-adoption, no false-match, branch, agent, move-safety, registry rebuild).
- Done = the change is behaviorally complete and free of retired-mechanism remnants.

## Context

design.md § "Migration Plan" and § "Testing Approach"; proposal Impact (touchpoint map).

## Cycle 1 updates (cycle-1781448273)

- **Baseline caveat:** `./bin/run-tests.sh -d config/gptel` currently reports **21 pre-existing
  buttercup failures** (async-callback/queue + a few scope assertions) that PREDATE this change and
  are externalised to `.tasks/gptel-preexisting-async-scope-test-failures.md`. The ERT side is clean.
  "Fully green" for THIS change means: **no NEW failures vs that baseline** (normalized failset diff)
  and all three **remnant greps return nothing** — not a literally-zero buttercup failure count.
- Already landed this cycle: the signature predicate + head-read, and identity-key emission at all
  three writers (with the case-sensitive anchoring + bounded-scan specs).
