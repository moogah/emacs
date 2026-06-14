---
name: consumer-migration
description: Migrate the last two jf/gptel--session-id-from-directory IDENTITY callers (branching, persistent-agent) to drawer-first resolution — or document why the agent-creation mint is the permitted basename source — so no production path derives identity from directory layout.
change: gptel-content-addressed-session-activation
status: ready
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
