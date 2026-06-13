---
name: purge-teardown-dispatch
description: Wire :on-purge dispatch into workspace-purge before delete-directory
change: add-workspace-git-worktrees
status: blocked
relations:
  - blocked-by:registry-on-purge-surface
---

## Files to modify
- config/workspaces/workspaces.org (modify) — workspace-purge, and tangled workspaces.el
- config/workspaces/test/workspace-delete-purge-spec.el (modify)

## Implementation steps
1. Locate `workspace-purge` in config/workspaces/workspaces.org (see the
   "Delete and purge commands" section).
2. After the `yes-or-no-p` confirmation succeeds and BEFORE the recursive
   `delete-directory` of `:home`, call
   `(workspace--dispatch-purge-integrations NAME HOME 'purge)` where HOME is the
   workspace's `:home` (still present on disk at this point) and NAME its name.
   - Place it so the existing-on-disk guard (purge skips the filesystem step
     when `:home` is missing) does not skip the dispatch when you still want
     cleanup; if `:home` is already gone there is nothing to tear down, so it is
     fine to dispatch only when the directory exists. Match the spec: dispatch
     occurs after confirmation, before deletion.
   - Do NOT let dispatch affect whether the purge proceeds — it is additive and
     error-guarded inside the dispatcher.
3. Ensure dispatch runs ONLY in `workspace-purge`, never in `workspace-delete`
   (delete keeps `:home`, so there is nothing to clean).
4. Tangle + validate: `./bin/tangle-org.sh config/workspaces/workspaces.org`.
5. Extend `workspace-delete-purge-spec.el`: spy `workspace--dispatch-purge-integrations`
   and assert it is called once before `delete-directory` on purge with the
   correct name/home; assert it is NOT called on `workspace-delete`; assert a
   handler reported `failed` does not abort the purge (home still deleted).

## Design rationale
Integrations must clean up resources anchored under `:home` (git worktrees)
while the directory still exists, so dispatch must precede the filesystem
deletion. Teardown fires on purge only — `workspace-delete` is the
non-destructive command that leaves `:home` and its worktrees intact, so
cleaning them there would be wrong.

## Design pattern
Follow how `workspace-new` already calls
`workspace--dispatch-create-integrations` at the additive tail of creation —
same "outcome never changes whether the operation completes" discipline,
applied here before deletion instead of after creation.

## Verification
- `./bin/tangle-org.sh config/workspaces/workspaces.org` validates
- `./bin/run-tests.sh -d config/workspaces` — purge specs pass
- Acceptance (spec scenarios): on-purge runs before deletion; skipped on
  delete; a throwing/ failed handler does not abort the purge.

## Context
design.md § Decisions 'D3'
specs/workspaces/spec.md (MODIFIED workspace-purge)
specs/workspace-integrations/spec.md (ADDED Purge-time teardown dispatch)
