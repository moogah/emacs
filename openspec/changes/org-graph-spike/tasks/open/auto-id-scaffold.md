---
name: auto-id-scaffold
description: Extend workspace scaffold and gptel session creation to auto-assign org IDs so home.org and session files participate in the vulpea index.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:install-packages
---

## Files to modify
- `config/workspaces/scaffold.org` (modify) — add `org-id-get-create` to the
  `home.org` write stage
- `config/gptel/sessions/commands.org` (modify) — add `org-id-get-create` in
  `jf/gptel--create-session-core` after the session file is created

## Implementation steps
1. In the workspace scaffold pipeline (the stage that writes the `home.org`
   skeleton, `config/workspaces/scaffold.org` ~line 152-183), after writing
   the skeleton, open the file and call `org-id-get-create` so `home.org`
   gets a stable `:ID:`. Ensure the ID is written before the initial `git
   add . && commit` so it is captured in the first commit.
2. In `jf/gptel--create-session-core` (session org writer,
   `config/gptel/sessions/commands.el:326`), after the session file exists
   and its property drawer is written, call `org-id-get-create` on the
   session buffer so `sessions/*.org` files are indexable nodes.
3. Make both additive and idempotent: `org-id-get-create` is a no-op if an
   `:ID:` already exists, so re-running scaffold or re-saving a session does
   not churn IDs.
4. Do NOT change any other scaffold/session behavior, naming, or the
   `GPTEL_WORK_ROOT` drawer. This task only adds IDs.
5. Tangle both org files and validate: `./bin/tangle-org.sh
   config/workspaces/scaffold.org` and `./bin/tangle-org.sh
   config/gptel/sessions/commands.org`.

## Design rationale
RE-2a: vulpea only indexes notes carrying an `:ID:`
(`vulpea-db-extract.el:475`). Workspace `home.org` and `sessions/*.org` are
scaffolded without IDs, so without this they are invisible to the index and
cannot be `id:` link targets. Auto-assigning IDs at birth is the minimal,
purely-additive way to make workspace homes and sessions first-class nodes in
the graph. This is the one place org-graph reaches into the workspaces /
gptel-sessions code.

## Design pattern
Match the existing literate style of `scaffold.org` and `commands.org`. The
change is a single `org-id-get-create` call at the right point — keep the
babel block small and well-commented as to WHY (RE-2a).

## Verification
- `./bin/run-tests.sh -d config/workspaces` and `-d config/gptel/sessions`
  stay green (existing scaffold/session specs unaffected; assertions about
  the skeleton content still hold since IDs are additive).
- Manual: create a workspace, confirm `home.org` and a new session file each
  contain an `:ID:` property and appear in `(vulpea-db-query)` after a sync.
- Re-running scaffold against an existing home does not change the existing
  IDs.

## Context
design.md § Re-evaluation (RE-2a); config/workspaces/scaffold.org;
config/gptel/sessions/commands.el (jf/gptel--create-session-core).
</content>
