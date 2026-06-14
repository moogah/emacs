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

## Cycle 2 updates (cycle-1781451784)

### Cycle context
- Cycle-2 added `config/gptel/sessions/test/filesystem/identity-resolution-spec.el` and `config/gptel/chat/test/mode-activation-spec.el` (all green). The config/gptel failure floor is **unchanged at 21 pre-existing** (async/scope; externalised in `.tasks/`); spec count grew 1288 → 1304.
- This task's grep-audit (no `find-file-hook` / `auto-init-session-buffer` / `current-symlink` remnants in `config/gptel/**/*.org`) is the enforcement mechanism for `register/invariant/activation-and-identity-are-content-not-path` (still speculated). Run it AFTER `retire-find-file-hook` + `retire-current-symlink` land — until then the greps will (correctly) still match the legacy code.

## Cycle 3 updates (cycle-1781453946)

- **1 of 3 blockers cleared:** `discovery-reads-drawers` is done (7dd50b5). Remaining: `retire-find-file-hook`, `retire-current-symlink`.
- Cycle-3 added 2 spec files (session-dir walk + drawer discovery), all green; full-suite floor unchanged at 23 (async/scope cluster), now 2311 specs (+11 green this cycle).
- **Extend the grep-audit (T2):** sibling-branch session-id consistency is now delegated to the WRITER invariant `register/invariant/branch-drawer-shares-id-not-branch` (discovery no longer enforces a single session-id per session directory — by design, Decision D7). The sweep should verify the writers still emit the same `:GPTEL_SESSION_ID:` to all branch drawers of a session, since discovery now trusts that rather than re-checking it.

## Cycle 4 updates (cycle-1781458723)

- **`mode-hook-binder` done (919577a):** the BINDING half of `register/invariant/activation-and-identity-are-content-not-path` is now confirmed content-only (activation cycle-2 + identity cycle-2 + binding cycle-4 all proven). Only the REMOVAL half remains. **Still blocked** by `retire-find-file-hook` + `retire-current-symlink`.
- **Grep-audit extension (T3):** a NEW path-layout hook (`jf/gptel--auto-init-session-buffer` on `find-file-hook`) coexists with the binder during the retire window. After `retire-find-file-hook` lands, the audit must confirm `jf/gptel--auto-init-session-buffer` AND its `find-file-hook` registration are fully gone — the disc-mode-hook-binder-3 two-hook duplication is resolved only when the legacy hook is removed (not merely shadowed). Existing greps already cover `find-file-hook` / `auto-init-session-buffer`.
- **Data-hygiene check (T4, from disc-mode-hook-binder-2):** part of the sweep — confirm no remaining test fixtures or session data pair `GPTEL_PARENT_SESSION_ID` with a `branches/<b>/` path (contradictory under the content model; agent-typed by content). One such fixture was corrected in `auto-init-chat-mode-spec.el` this cycle.
- Full-suite floor after cycle-4: 2317 buttercup specs, 23 failed (async/scope/bash-parser cluster, signatures identical to baseline) + ERT 620/9; ZERO failures on the sessions/chat surface. This is the floor the final sweep compares against.

## Cycle 5 updates (cycle-1781463961)

- **2nd of 3 blockers cleared:** `retire-find-file-hook` is done (e398898). Remaining blocker: `retire-current-symlink` (now ready). The `find-file-hook`/`auto-init-session-buffer` org grep-gate is ALREADY empty as of this cycle — the sweep's existing greps for those two terms should pass now; what remains to verify at close is the symlink terms and the `session-id-from-directory` identity callers.
- **T5 — cosmetic path-archaeology scrub (NEW, from 3 converging cycle-5 advisories: implementor disc-3, on-touch architect ontouch-2, reviewer).** Stale "auto-init" / "find-file-hook" wording survives in COMMENTS and one filename with NO live calls: `config/gptel/tools/test/persistent-agent/auto-init-reload-spec.el` (rename), `helpers-spec.el`, and residual describe/comment wording in `preset-application-spec.el` / `workspace-integration-spec.el` / `pre-send-refresh-spec.el`. Fold a comment/string scrub + the file rename into the sweep so path-archaeology wording doesn't read as a surviving mechanism in the final audit. Cosmetic — verify-by-grep, not behavior.
- Full-suite floor after cycle-5: 2309 buttercup specs, 23 failed (signatures identical to baseline) + ERT 620/9; ZERO failures on sessions/chat/tools. The −8 spec delta vs cycle-4 is the legitimate removal of path-regex test cases. This is the floor the final sweep compares against.
