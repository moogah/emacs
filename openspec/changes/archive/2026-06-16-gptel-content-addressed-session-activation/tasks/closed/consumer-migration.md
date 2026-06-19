---
name: consumer-migration
description: Migrate the last two jf/gptel--session-id-from-directory IDENTITY callers (branching, persistent-agent) to drawer-first resolution — or document why the agent-creation mint is the permitted basename source — so no production path derives identity from directory layout.
change: gptel-content-addressed-session-activation
status: done
relations:
  - "blocked-by:discovery-reads-drawers"
  - "blocked-by:retire-find-file-hook"
---

> Created cycle-6 integrate (cycle-1781465881) to resolve the handshake open
> question carried since cycle-5: the 2 session-id-from-directory identity callers
> had no task file, and regression-sweep cannot flip the invariant to confirmed
> until they are dispositioned. See
> `.orchestrator/cycles/cycle-1781465881/reconciliations/invariant-activation-and-identity-are-content-not-path.md`.

## Files to modify

- `config/gptel/sessions/branching.org` (modify) — the `(jf/gptel--session-id-from-directory session-dir)` call inside `jf/gptel--rewrite-branch-identity-keys` (tangled `branching.el:244`, source ~`:399`).
- `config/gptel/tools/persistent-agent.org` (modify) — the `(jf/gptel--session-id-from-directory session-dir)` call that seeds `agent-session-id` at agent creation (tangled `persistent-agent.el:372`, source ~`:602`).
- Co-located `*-spec.el` for each, as needed, to assert drawer-first behavior (and the move-safe case where the dir basename differs from the drawer `:GPTEL_SESSION_ID:`).

## Implementation steps

1. **branching.org:399 — genuine migration.** This reads the PARENT session's id
   to stamp the new branch's drawer (`register/invariant/branch-drawer-shares-id-not-branch`).
   It currently derives the id from the parent dir basename, bypassing the drawer.
   Switch to `jf/gptel--resolve-session-id` (drawer-first, basename fallback) so a
   moved/renamed parent session resolves its real id from `:GPTEL_SESSION_ID:`.
   Pass the parent session-dir; the resolver reads the parent's drawer. Add/extend
   a spec proving the move-safe case (parent drawer id ≠ dir basename → branch gets
   the drawer id).
2. **persistent-agent.org:602 — DECIDE, don't assume.** This MINTS a new agent's
   own `:GPTEL_SESSION_ID:` from its dir basename AT CREATION, before any drawer
   exists. Two valid resolutions — pick with evidence and record the choice in
   `## Discoveries`:
   - (a) It is the **permitted basename source**: the invariant explicitly allows
     the basename fallback "when a drawer key is absent" — and at creation there is
     no drawer. If so, leave the behavior but route it through
     `jf/gptel--resolve-session-id` (which returns the basename when no drawer key)
     so the code reads as content-first-with-fallback, not raw path archaeology;
     OR
   - (b) The creation code already KNOWS the id it used to name the agent dir —
     thread that value through instead of re-deriving it from the path at all
     (cleaner; removes the `session-id-from-directory` call entirely).
   Prefer (b) if the minting id is available in scope; else (a).
3. After both, `jf/gptel--session-id-from-directory` should remain ONLY as the
   documented fallback inside `jf/gptel--resolve-session-id` (`filesystem.el:78`) —
   confirm by grep that no other production caller invokes it directly.
4. Tangle `branching.org` + `persistent-agent.org`; run the affected test dirs +
   `config/gptel/sessions` + `config/gptel/tools`.

## Design rationale

These are the last two production paths that derive session IDENTITY from
directory layout outside the sanctioned resolver fallback. Migrating them (or
proving the agent-creation case is the permitted basename source) is the final
removal step that lets `register/invariant/activation-and-identity-are-content-not-path`
flip speculated → confirmed. (design.md §D2 drawer-first resolution; D3 agent
identity; invariant `branch-drawer-shares-id-not-branch`.)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/branching.org config/gptel/tools/persistent-agent.org` succeed.
- `grep -rn "session-id-from-directory" config/gptel --include="*.el" | grep -v "/test/"` shows ONLY the `filesystem.el` definition + its use inside `jf/gptel--resolve-session-id` (no other direct production caller).
- `./bin/run-tests.sh -d config/gptel/sessions` and `-d config/gptel/tools` green, incl. the new move-safe spec.
- Done = no production path derives identity from directory layout except the sanctioned resolver fallback; regression-sweep's grep-audit can now flip the invariant to confirmed.

## Context

Cited register entries: `register/invariant/activation-and-identity-are-content-not-path` (this is its LAST remnant), `register/boundary/drawer-first-identity-resolution` (the resolver to migrate onto), `register/invariant/branch-drawer-shares-id-not-branch` (the branching caller's purpose). design.md §D2/D3.

## Observations

- **branching.org migration — parent drawer source.** The parent session's
  authoritative drawer lives in the parent BRANCH's `session.org`, already bound
  as `parent-context` (`(jf/gptel--context-file-path parent-branch-dir)`) inside
  `jf/gptel--create-branch-session`. The migration reads it with
  `jf/gptel--read-session-drawer-head` (the reader the resolvers document) and
  passes the resulting alist to `jf/gptel--resolve-session-id` with `session-dir`
  as the fallback path. `session-dir` is the SESSION container (parent of
  `branches/`), so the basename fallback is identical to the pre-migration
  behavior — only the drawer-first branch is new. No new binding was introduced;
  the value was already in scope, so this is a pure source-swap with the basename
  semantics preserved for legacy parents.

- **Move-safe spec strengthened, not merely added.** The existing
  `branching-integration-spec.el` had no test where the parent drawer id differs
  from the dir basename (all bootstraps used a drawer carrying only
  `GPTEL_PRESET`). The new `describe "drawer-first session-id sourcing"` block
  asserts BOTH branches: (1) drawer id ≠ basename ⇒ branch gets the drawer id (and
  explicitly `:not :to-equal` the basename, so the assertion fails loudly if the
  code reverts to path-archaeology); (2) absent drawer id ⇒ branch falls back to
  the basename (the no-migration grace path). The second test guards the
  `branch-drawer-shares-id-not-branch` invariant for normal (drawer-absent legacy)
  sessions — confirming the migration did NOT change WHICH id a normal session
  stamps.

- **Existing branching tests still use `session-id-from-directory` directly.**
  `branching-integration-spec.el` lines 424/544/569 call
  `jf/gptel--session-id-from-directory` to COMPUTE the expected id in test
  fixtures whose bootstraps deliberately omit a drawer `GPTEL_SESSION_ID`. That is
  correct: those fixtures have no drawer id, so the basename IS the resolved id —
  the test helper and the production fallback agree. These are TEST callers, not
  production identity derivation, and are out of this task's removal scope. The
  repo-wide audit confirms no PRODUCTION caller invokes the function outside the
  resolver fallback.

- **persistent-agent: the minting id is genuinely NOT in scope.**
  `jf/gptel--create-agent-directory` (filesystem.org) builds the dir basename
  (`<preset>-<timestamp>-<slug>`) as a LOCAL and returns only the full `agent-dir`
  path. The caller never sees the basename as a separate value. Threading it
  (option b) would require changing that shared helper's return contract — a file
  (`filesystem.org`) outside this task's files-to-modify, and a signature change
  affecting other callers. See Discoveries for the (a)-vs-(b) decision.

## Discoveries

- discovery_id: disc-consumer-migration-1
  class: deviation
  description: |
    persistent-agent.org — chose option (a) (resolver-routed) over (b)
    (thread-known-id), DEVIATING from the task's stated preference for (b).
    Evidence: the agent's minting id is the basename of `session-dir`, which is
    constructed as a LOCAL `dirname` inside `jf/gptel--create-agent-directory`
    (config/gptel/sessions/filesystem.org:440-457) and never returned — the
    function returns only the full `agent-dir` path. In the persistent-agent
    caller (`jf/gptel-persistent-agent--task`) the only identity-bearing value in
    scope is `session-dir` itself; the id value is NOT available to thread.
    Option (b) ("the creation code already KNOWS the id") is therefore false for
    this call site as written — the helper encapsulates and discards it. Achieving
    (b) would mean changing `jf/gptel--create-agent-directory`'s return contract
    (e.g. returning `(cons agent-dir dirname)` or the dirname separately), which is
    in `filesystem.org` — outside this task's files-to-modify — and would touch a
    shared helper used by other agent-listing code paths. The task's own CAVEAT on
    (a) (at creation no drawer exists, so the resolver degenerates to the basename
    fallback) is accepted as the lesser cost: `(jf/gptel--resolve-session-id nil
    session-dir)` reads as content-first-with-fallback and removes the only direct
    production call to `jf/gptel--session-id-from-directory`, which is the
    invariant-relevant property. The degenerate `nil` drawer-alist is explicit and
    commented, not hidden.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    Accept (a) for this change; the invariant's grep-audit (no direct production
    caller of `session-id-from-directory` outside the resolver) is satisfied. If a
    future change wants the cleaner (b), it should be a SEPARATE task scoped to
    `filesystem.org` that has `jf/gptel--create-agent-directory` return its minted
    basename, then thread it through the persistent-agent caller — at which point
    the `(jf/gptel--resolve-session-id nil session-dir)` degenerate call can be
    replaced by the threaded literal id. Not worth coupling into this consumer
    migration.

- discovery_id: disc-consumer-migration-2
  class: spec-signal
  description: |
    With both callers migrated, the repo-wide audit
    (`grep -rn "session-id-from-directory" config/gptel --include="*.el"`) shows
    exactly: the `defun` in filesystem.el:78, two docstring mentions, the single
    live use inside `jf/gptel--resolve-session-id` (filesystem.el:103), one CODE
    COMMENT in persistent-agent.el:369 (naming the function it deliberately does
    NOT call), and three TEST-fixture callers in branching-integration-spec.el.
    No production code path now derives session IDENTITY from directory layout
    except the sanctioned resolver fallback. This is the last remnant the
    invariant `register/invariant/activation-and-identity-are-content-not-path`
    was waiting on.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    The regression-sweep grep-audit can now flip this invariant SPECULATED →
    CONFIRMED. Note for the auditor: the persistent-agent.el:369 hit is a comment,
    and the branching-integration-spec.el hits are test fixtures computing an
    expected id where the fixture deliberately has no drawer id (the basename IS
    the resolved id there). Neither is a production identity-derivation path.

- discovery_id: disc-consumer-migration-3
  class: interface-drift
  description: |
    `jf/gptel--resolve-session-id` is now called with a freshly-read drawer-alist
    in branching.org (drawer-first as designed) AND with a literal `nil`
    drawer-alist in persistent-agent.org (deliberate degenerate fallback-only
    call). The resolver's contract already documents `nil` drawer-alist as
    meaning "no point-min drawer" and handles it via `(cdr (assoc ... nil))` =>
    nil => basename fallback, so the `nil` call is contract-correct, not a misuse.
    But the boundary entry register/boundary/drawer-first-identity-resolution
    frames the resolver purely as a READ-of-existing-session operation ("a
    session's drawer (+ path for fallback)"); the persistent-agent call uses it at
    MINT time, before the session.org exists. This is a slightly broader usage
    shape than the boundary's framing anticipates (read vs. mint).
  affected_register_entry: register/boundary/drawer-first-identity-resolution
  recommendation: |
    Consider a one-line note on the boundary entry that `resolve-session-id` is
    also the sanctioned mint-time identity source (called with a `nil` drawer
    when the session.org does not yet exist), so the basename fallback is the
    canonical id at creation. This makes the persistent-agent (a) call read as an
    intended use of the boundary rather than an edge exploitation. No code change
    needed.
