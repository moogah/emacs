---
name: int-migration-command
description: Implement the idempotent workspace-migrate-to-vault command that creates project notes, relocates sessions, re-stamps branch IDs, renames agents dirs, cleans homes, and repoints layout blobs.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-workspaces-org-graph
  - blocked-by:int-workspaces-gptel
  - blocked-by:int-gptel-org-graph
  - blocked-by:gptel-hidden-agents
  - enables:configure-migrate-and-verify
---

## Files to modify
- config/integrations/workspaces-org-graph.org (modify; tangle — new "Migration" section)
- config/integrations/test/migration-spec.el (new)

## Implementation steps
Implement `workspace-migrate-to-vault` (interactive, takes an optional
OLD-SESSIONS-ROOT defaulting to `~/.gptel/sessions/`). Every step logs a
line to a `*workspace-migration*` buffer; every step is a no-op when its
precondition is already satisfied.

1. Back up the persistence file to `<file>.pre-vault` (copy) before any
   write.
2. For each registered workspace with nil `:node-id`: if
   `<home>/home.org` exists, read its `#+TITLE:` (fallback: name) and body
   (everything after the keyword/property preamble); call
   `org-graph/create-project-note title body`; `workspace-set-node-id`.
3. For each `<home>/sessions/<sid>/` directory: `rename-file` to
   `<jf/gptel-sessions-directory>/<sid>/` (error if target exists; report
   and skip). Then for each `branches/*/session.org` in it:
   `org-graph/write-edge file 'part-of node-id`; for each non-`main` branch,
   re-stamp a fresh file-level `:ID:` (reuse the identity-rewrite helper
   from `gptel-branch-identity` if exposed, else the same `org-id-new` +
   `org-id-add-location` recipe) and, if `branch-metadata.yml` names a
   parent branch, write `branched-from` to that parent's file ID.
4. For each `<OLD-ROOT>/<sid>/` when OLD-ROOT ≠ the configured root:
   `rename-file` into the configured root; re-stamp non-`main` branch IDs and
   write `branched-from` as above (no `part-of` — unknown workspace).
5. Under the configured root, rename every `branches/*/agents/` to
   `.agents/` (skip if `.agents/` exists).
6. Delete `<home>/home.org` and `<home>/sessions/` when the latter is empty.
7. Rewrite layout blobs: walk the registry's layouts; any `workspace-buffer`
   record whose filename equals a removed `home.org` path is re-pointed at
   the project note's path (both the filename slot and the bookmark record's
   filename). Flush persistence.
8. `org-graph/configure-sync` (full scan) then
   `org-graph/seed-org-id-locations`.
9. Specs (temp home tree, temp sessions roots, temp vault, stub registry):
   after one run — notes exist with carried-over title, `:node-id` set,
   sessions moved with `part-of` rows written (spy on write-edge), branch
   IDs unique, `.agents/` renamed, `home.org`/`sessions/` gone, backup file
   present; a second run performs no file operations (spy on
   `rename-file`/`delete-file`).

## Design rationale
Three real workspaces and four global sessions exist on disk (seven branch
directories, four agent directories, one `home.org` with an ID, two
without, one hand-authored `id:` link in a `home.org` body). Migrating by
hand is error-prone and the layout blobs embed `home.org` paths twice per
leaf. One idempotent command with a persistence backup makes the cutover
safe and repeatable per machine.

## Design pattern
`rename-file` with explicit existence checks (never `delete-directory`
recursively); logging buffer like `*org-graph*`; the persistence file's own
atomic-write path for the flush.

## Verification
- `./bin/tangle-org.sh config/integrations/workspaces-org-graph.org` validates.
- `./bin/run-tests.sh -d config/integrations` — green.
- `grep -n 'defun workspace-migrate-to-vault' config/integrations/workspaces-org-graph.el` → defined.

## Context
design.md § D9 'Migration command'; § Risks (migration touches the persistence file)
specs/workspace-graph-integration/spec.md § 'Migration of co-located workspaces'
