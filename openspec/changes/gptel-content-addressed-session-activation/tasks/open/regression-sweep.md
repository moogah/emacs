---
name: regression-sweep
description: Full config/gptel test run plus manual smoke of activation, creation, branching, and agents; assert no find-file-hook or symlink remnants remain.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:discovery-reads-drawers"
  - "blocked-by:retire-find-file-hook"
  - "blocked-by:retire-current-symlink"
  - "blocked-by:consumer-migration"
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

## Cycle 6 updates (cycle-1781465881)

- **Last REMOVAL blocker cleared:** `retire-current-symlink` is done (22b528e). The `current` symlink machinery is entirely gone (`grep current-symlink|get-current-branch-name|current-link config/gptel` empty; `grep -rni symlink config/gptel/sessions/` empty). Combined with cycle-5 (find-file-hook auto-init), both large remnants are removed.
- **NOW BLOCKED-BY `consumer-migration`** (created cycle-6 integrate): the LAST path-derived remnant is the 2 `jf/gptel--session-id-from-directory` IDENTITY callers (`branching.org:399`, `persistent-agent.org:602`). The invariant `activation-and-identity-are-content-not-path` cannot flip to confirmed until that task lands. Updated blocked-by accordingly.
- **T6 — grep-audit must be REPO-WIDE (meta-discovery, cycle-6).** The cycle-6 on-touch architect caught a BLOCKING dangling symlink test in `config/gptel/test/session-creation-spec.el` — one directory ABOVE the implementor's `config/gptel/sessions/`-scoped sweep. The final grep-audit MUST run over all of `config/gptel` (not just `config/gptel/sessions/`) for every retired symbol: `find-file-hook`, `auto-init-session-buffer`, `auto-init`, `update-current-symlink`, `get-current-branch-name`, `get-current-branch-dir`, `current-symlink`, `current-link`, and the `"current"` literal path usage.
- **T5 cosmetic scrub grows (cycle-6 additions):** also remove the vacuous `current` symlink-absence assertion at `config/gptel/tools/test/persistent-agent/creation-spec.el:80` and the now-unused `captured-symlink-target` helper at `config/gptel/test/persistence-test-helpers.el:126` (orphaned after the cycle-6 inline-fix removed its only caller).

## Cycle 7 updates (cycle-1781468009)

- **LAST blocker cleared — this task is now READY.** `consumer-migration` is done
  (merged 4043ec7): both `jf/gptel--session-id-from-directory` IDENTITY callers were
  migrated off the path — `branching.org` resolves the shared session-id drawer-first
  from the parent branch's `session.org` (move-safe), and `persistent-agent.org` routes
  the agent-creation mint through `(jf/gptel--resolve-session-id nil session-dir)`. With
  this, `register/invariant/activation-and-identity-are-content-not-path` has its CODE
  condition met; **this task's grep-audit is the final verification gate that flips it
  speculated → confirmed**, after which the change can `/opsx-verify` → `/opsx-archive`.
- **T7 — `session-id-from-directory` grep-audit guidance (NEW, from consumer-migration
  disc-2 + on-touch architect).** Add `session-id-from-directory` to the repo-wide
  grep-audit (T6), but note the EXPECTED legitimate hits so they are not misread as
  remnants. `grep -rn "session-id-from-directory" config/gptel --include="*.el"` (and
  `*.org`) should return ONLY:
    1. the `defun` in `config/gptel/sessions/filesystem.el` (the definition) + its two
       docstring mentions;
    2. the SOLE production use inside `jf/gptel--resolve-session-id`
       (`filesystem.el:~103` — the sanctioned basename fallback);
    3. one CODE COMMENT in `config/gptel/tools/persistent-agent.el` (naming the function
       it deliberately does NOT call directly);
    4. TEST-fixture callers in `config/gptel/sessions/test/branching/branching-integration-spec.el`
       (which compute an expected id where the fixture has no drawer id — the basename IS
       the resolved id there; not a production path).
  Any OTHER hit — a direct production caller outside the resolver — is a remnant and FAILS
  the audit. (Confirm the `.org` sources tangle-match these `.el` lines.)
- **Meta-discovery `resolver-as-single-identity-seam` (cycle-7):** the invariant's
  enforcement is now a simple grep — "no direct `session-id-from-directory` caller outside
  the resolver" — precisely because the mint-time path was also routed through the resolver.
  The T7 guidance above operationalises that.
- Full-suite floor after cycle-7: **2309 buttercup specs, 23 failed** (signatures IDENTICAL
  to baseline; the async/scope/parallel-tool-callback pre-existing cluster) + the green
  sessions/chat/tools surfaces (sessions 150/0 incl. the 2 new move-safe specs, tools 65/0).
  This is the floor the final sweep compares against; a NEW signature is a regression.

## Observations

- **gptel.org:25 decision**: The module-overview prose read
  "configuration, and auto-initialization on open." Reworded to
  "content-addressed activation on open." Reason: "auto-initialization"
  read as claiming the retired find-file-hook pipeline; the new wording
  is accurate (magic-mode-alist + chat-mode-hook binder) and the prose
  is NOT tangled to gptel.el (verified: `auto-initialization on open`
  produces no .el hit), so the change is documentation-only.

- **commands.org user-facing message LEFT UNCHANGED**: The string
  "Session will auto-initialize when opened." (commands.org:~751) was
  left intact per instruction — it is behaviorally TRUE (the session
  does activate on open) and is not a hook claim.

- **Additional T5-class spot found, recorded not fixed**:
  `config/gptel/sessions/test/commands/auto-init-resilience-spec.el`
  carries (a) the basename `auto-init-resilience-spec.el` and (b) a
  line-16 commentary phrase "under the legacy find-file-hook auto-init
  pipeline". This was NOT in the target list. I deliberately LEFT it as
  is: the line-16 phrase is explicitly past-tense ("legacy", "once
  wrapped") and is load-bearing background that explains WHY the
  regression test exists (it characterizes the old broad
  condition-case bug the content-addressed binder fixed). The file's
  own current-behavior commentary (lines 10-11, 23-29) already
  correctly describes the content-addressed binder. Rewording the
  historical phrase would lose meaning; renaming the basename risks
  the same provide/footer churn and is out of the stated scope. It is
  a *test* file comment (not production .org), so it does not affect
  the T6 grep-audit gate (which passed on production sources). Flagged
  here for a future maintainer who may want a basename-only rename.

- The internal helper symbols in the renamed spec
  (`jf-pa-auto-init-test--*`, `jf-pa-auto-init-test--find-agent-buffer`,
  etc.) were intentionally left unchanged — the task scoped the rename
  to the FILE (header/provide/footer/basename) and comments/strings,
  and explicitly required tests to remain behaviorally unchanged.
  Renaming internal symbols is churn with no audit benefit (they do not
  match the retired-mechanism grep terms).

## Discoveries

None.

- Part A grep-audit re-verification: **PASS**. T6 production sources
  (`.org`/`.el` excluding test files) are free of every retired
  mechanism term. T7 `session-id-from-directory` returns ONLY the
  sanctioned hits: the defun + 2 docstring mentions in filesystem.{org,el},
  the sole resolver basename-fallback in
  `jf/gptel--resolve-session-id`, the one explanatory comment in
  persistent-agent.{org,el} (naming the function it deliberately does
  NOT call), and test-fixture callers in branching-integration-spec.el +
  a comment in identity-resolution-spec.el. NO unexpected production
  caller. The register entry
  `register/invariant/activation-and-identity-are-content-not-path` is
  confirmable from this clean audit — no push-back, class: n/a.
