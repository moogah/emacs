---
name: discovery-reads-drawers
description: Make init-registry and filesystem discovery learn identity from each session.org drawer head-read instead of from directory names.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:drawer-signature-and-head-read"
  - "blocked-by:drawer-identity-resolver"
---

## Files to modify

- `config/gptel/sessions/registry.org` (modify) — `jf/gptel--init-registry` keys entries on drawer-resolved identity.
- `config/gptel/sessions/filesystem.org` (modify) — discovery callers that derive identity from `session-id-from-directory` (`:209`, `:291`) switch to the head-read + resolver.
- `config/gptel/sessions/test/registry/discovery-drawer-spec.el` (new) — Buttercup specs for drawer-keyed discovery and basename fallback.

## Implementation steps

1. Write the spec first. Cover:
   - `init-registry` over a fixture tree whose `session.org` drawers carry `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` → registry keys come from the drawers (use a fixture whose dir basename differs from the drawer id);
   - a legacy session whose drawer omits the keys → registry falls back to basename/segment;
   - registry count matches the number of valid branches.
2. In `init-registry` (`registry.org:58-81`): keep `list-session-directories` / `list-branches` as the file *locator* (directory traversal), but for each located branch `session.org`, call `jf/gptel--read-session-drawer-head` and resolve id/branch via the resolvers; build the `"session-id/branch-name"` key and the plist (`:session-id :session-dir :branch-name :branch-dir :buffer`) from those values. `session-dir` for discovery is the located session directory; `branch-dir` the located branch directory.
3. Update the `filesystem.org` discovery callers (`:209`, `:291`) the same way — resolve identity from the drawer head-read, not the basename.
4. If a located `session.org` carries no `:GPTEL_` drawer at all (corrupt/partial), skip it from the registry and log at debug (consistent with today's `valid-*-directory-p` gating — see design Open Questions).
5. Tangle both `.org` files; run the new spec.

## Design rationale

The user chose full path-independence over a basename==drawer-id invariant (which would have re-introduced the coupling this change removes). Directory traversal still *locates* files cheaply; a bounded drawer head-read per session supplies authoritative identity. Cost is one small read per session at init, modest next to the directory stat-walk already performed. (design.md §Decision D7; risk "Discovery I/O regression".)

## Verification

- `./bin/tangle-org.sh` on `registry.org` and `filesystem.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/registry` and `-d config/gptel/sessions/test/filesystem` — green, including the "basename differs from drawer id" discovery case.
- Done = registry keys are drawer-sourced; directory names carry no identity meaning.

## Context

design.md § Decision "D7. Discovery"; specs `sessions-persistence` Requirement "Session discovery and registry".

## Cycle 1 updates (cycle-1781448273)

- Use `jf/gptel--read-session-drawer-head` (merged) for the per-session head-read: it returns an
  **alist keyed by the bare key string** (string values), nil on no-drawer — feed it to the
  resolvers from `drawer-identity-resolver`. Do not re-implement the scan.
- `register/boundary/session-content-signature` → **reconciled**; `identity-drawer-keys` →
  **confirmed**; `drawer-first-identity-resolution` still **speculated** (resolvers land in
  `drawer-identity-resolver`, this task's blocker).
- The task references `config/gptel/sessions/test/registry/` — that directory does **not** exist yet
  (only `branching/`, `commands/`, `filesystem/` do); create it for the new spec.
- **Meta M3 (load order):** `branching.el` loads before `commands.el`; if discovery needs drawer-write
  helpers keep that constraint in mind (registry init is read-only, so likely n/a).

## Cycle 2 updates (cycle-1781451784)

### Cited register entries
- `register/boundary/drawer-first-identity-resolution`: speculated → **confirmed**. The resolvers are merged (commit 1ec479f). See `.orchestrator/cycles/cycle-1781451784/reconciliations/boundary-drawer-first-identity-resolution.md`.

### Already-shipped / now-available
- The resolvers this task depends on now exist in `config/gptel/sessions/filesystem.el`: `jf/gptel--resolve-session-id` and `jf/gptel--resolve-branch-name` take `(drawer-alist path)`; `jf/gptel--session-type` takes `(drawer-alist)`. Feed them the alist from `jf/gptel--read-session-drawer-head` (bare-key strings, string values, nil on no-drawer). **Do not re-implement the scan or the fallback logic** — call the resolvers.
- **Unblocked**: both blockers (`drawer-signature-and-head-read`, `drawer-identity-resolver`) are now done. This task is now **ready** — a natural cycle-3 candidate alongside `session-dir-ancestor-walk`.

### Meta-discoveries
- `cross-subsystem-reference`: registry init is read-only, so the chat→sessions load-order constraint does not bite here; but if any drawer-write helper is ever needed, recall that `branching.el` loads before `commands.el`.

### Reminder (carried)
- `config/gptel/sessions/test/registry/` still does not exist — create it for the new spec.
