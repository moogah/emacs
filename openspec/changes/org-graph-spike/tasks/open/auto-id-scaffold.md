---
name: auto-id-scaffold
description: Extend workspace scaffold and gptel session creation to auto-assign org IDs so home.org and session files participate in the vulpea index.
change: org-graph-spike
status: ready
relations:
  - blocked-by:install-packages
cites_register_entries:
  - register/invariant/indexable-requires-id
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

## Orchestrator brief addenda (cycle-1782551613)

From the foundation Architect audit; cited register entry in `interfaces.org`.

- **`register/invariant/indexable-requires-id` is your acceptance contract.**
  ID assignment MUST be additive and idempotent: a file that already has an
  `:ID:` is left unchanged. Acceptance gate: existing `config/workspaces` and
  `config/gptel/sessions` specs stay green, AND a new assertion that a re-run
  (re-scaffold / re-save) leaves an existing `:ID:` untouched (no churn).

- **This is the ONLY batch task that edits code outside `config/org-graph/`**
  (it touches `config/workspaces/scaffold.org` and
  `config/gptel/sessions/commands.org`). Confirm zero behavioural change beyond
  the ID stamping — no change to scaffold naming, skeleton content, or the
  `GPTEL_WORK_ROOT` drawer. The reviewer will check exactly this.

- **Literate discipline.** Edit the `.org` sources and tangle both
  (`./bin/tangle-org.sh config/workspaces/scaffold.org` and
  `… config/gptel/sessions/commands.org`); commit `.org` + `.el` together.
