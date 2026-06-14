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

## Observations

- **Per-branch head-read, not per-session.** The task spoke of "for each located
  branch `session.org`, call `read-session-drawer-head`", but the OLD code resolved
  `session-id` ONCE per session-dir (outer loop) and reused it for every branch. Identity
  is now branch-resident (each branch carries its own `:GPTEL_SESSION_ID:` /
  `:GPTEL_BRANCH:` drawer), so the head-read necessarily moved INSIDE the branch loop in
  both `init-registry` and `find-all-branches-with-agents`. The `session-id` for a branch
  now comes from THAT branch's drawer (resolved against the located `session-dir` as the
  basename fallback). This matches design D7's intent but is worth flagging: two sibling
  branches of the same session could in principle now resolve to DIFFERENT `:session-id`
  values if their drawers disagree — discovery no longer enforces a single session-id per
  session directory. That is a direct consequence of "directory names carry no identity
  meaning" and is the correct content-addressed behavior, but it removes an implicit
  per-directory consistency check the old code had for free.

- **`resolve-branch-name` needs the file/segment-bearing path, not the bare branch-dir.**
  The resolver's basename fallback regex is `"/branches/\\([^/]+\\)/"` (trailing slash
  required). `jf/gptel--branch-dir-path` returns `.../branches/<branch>` with NO trailing
  slash, so passing the bare branch-dir would FAIL the segment fallback for legacy (no
  `GPTEL_BRANCH`) sessions. I pass the `session.org` file path
  (`.../branches/<branch>/session.org`) instead, which both head-reads cleanly and gives
  the regex its trailing `/`. The resolver contract is fine; callers must feed it a path
  that includes the trailing `branches/<branch>/` segment.

- **Pre-existing fixture broke on the new skip rule (expected).** `directory-templates-spec.el`'s
  "excludes branches without session.org" test wrote a valid `session.org` whose body had
  NO `:GPTEL_` drawer (`#+begin_user...`). Under the new skip-on-no-drawer gate that branch
  is now (correctly) skipped, so `find-all-branches-with-agents` returned nil instead of
  `("main")`. I updated that fixture to carry a real identity drawer; its INTENT (the
  `session.md`-only legacy branch must not leak) is preserved. Any other test or production
  code that creates a branch `session.org` WITHOUT an identity drawer will now find that
  branch absent from discovery/registry. This is the intended Decision-19/D7 behavior, but
  is a latent trap for fixtures authored before this change.

- **Skip gate keys on `read-session-drawer-head` returning nil**, i.e. "no point-min
  `:GPTEL_*:` drawer at all." A branch whose drawer is PRESENT but omits the identity keys
  (e.g. carries only `:GPTEL_PRESET:`) is NOT skipped — it falls through to the basename /
  segment fallback. This matches the task's "carries NO `:GPTEL_` drawer at all" wording
  and the resolvers' grace path, but the distinction (no-drawer ⇒ skip vs. drawer-without-id
  ⇒ fallback) is subtle and worth an Architect eye.

## Discoveries

- discovery_id: disc-discovery-reads-drawers-1
  class: deviation
  description: |
    The head-read had to move from per-session (outer loop, once) to per-branch (inner
    loop, once per branch) in both `init-registry` and `find-all-branches-with-agents`,
    because identity is now branch-resident. A consequence: discovery no longer enforces a
    single `:session-id` per session DIRECTORY — two branches under the same directory whose
    drawers carry different `GPTEL_SESSION_ID` values will register under different
    session-ids. This is the correct content-addressed behavior (paths carry no identity),
    but it silently drops a per-directory consistency invariant the old basename-keyed code
    had implicitly.
  affected_register_entry: register/boundary/drawer-first-identity-resolution
  recommendation: |
    Confirm with the Architect that per-branch (not per-directory) identity is intended at
    discovery time (design D7 implies yes). If a "all branches of a directory share one
    session-id" invariant is desired, it must be asserted explicitly elsewhere — discovery
    no longer provides it for free.

- discovery_id: disc-discovery-reads-drawers-2
  class: interface-drift
  description: |
    `jf/gptel--resolve-branch-name`'s basename fallback regex `"/branches/\\([^/]+\\)/"`
    requires a TRAILING slash, so it only fires on a path that still contains the
    `branches/<branch>/` segment (e.g. the `session.org` file path), NOT on the bare
    branch-dir returned by `jf/gptel--branch-dir-path` (which has no trailing slash).
    Discovery callers must pass the file path, not the directory, or legacy (no-GPTEL_BRANCH)
    sessions silently get a nil branch-name.
  affected_register_entry: register/boundary/drawer-first-identity-resolution
  recommendation: |
    Document the "pass a path that retains the trailing branches/<branch>/ segment" caller
    contract on `resolve-branch-name`, or make the regex tolerate a path that ENDS at the
    branch directory (no trailing slash). I chose the caller-side fix (pass session.org path).

### Cited load_bearing entries — held as written?
- `register/boundary/session-content-signature` (reconciled): **confirmed-held.**
  `jf/gptel--read-session-drawer-head` returns the bare-key-string alist (nil on no-drawer)
  exactly as documented; consumed directly with no adaptation.
- `register/boundary/drawer-first-identity-resolution` (confirmed): **held, with a caller
  caveat** — see disc-2. The resolver logic is correct; the trailing-slash requirement of
  the branch-name fallback is an undocumented caller obligation, not a defect.
- `register/vocabulary/identity-drawer-keys` (confirmed): **confirmed-held.** Canonical keys
  `GPTEL_SESSION_ID` / `GPTEL_BRANCH` / `GPTEL_PARENT_SESSION_ID` consumed verbatim.
