---
name: int-workspaces-org-graph
description: Create the project note at workspace birth and record its ID, mark it done on purge, install display-name and home-builder from the note, register the session schema, add the adopt-project-note command.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-scaffold-and-lint
  - blocked-by:ws-inversion-points
  - blocked-by:ws-scaffold-and-anchor
  - blocked-by:og-edge-writer-and-project-note
  - blocked-by:og-connected-dblock
  - enables:int-workspace-commands
  - enables:int-migration-command
  - blocked-by:ws-node-id-setter-live-record
---

## Files to modify
- config/integrations/workspaces-org-graph.org (modify; tangle)
- config/integrations/test/workspaces-org-graph-spec.el (new)

## Implementation steps
> Cycle 1: step 1's setter call corrected — `workspace-set-node-id` needs the LIVE registry record and handlers have no public name→record lookup (eoc-2); `ws-node-id-setter-live-record` (now blocked-by) makes the setter accept the payload's `:name`. See .orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-plist-v3.md.

1. Register integration `org-graph-project` with `workspace-register-integration`
   **before** any other integration in this layer (registration order is
   dispatch order; the gptel birth session must observe `:node-id`).
   - `:on-create (payload)`: if `(plist-get payload :node-id)` resolves via
     `vulpea-db-get-by-id`, return `skipped`; else
     `(org-graph/create-project-note (plist-get payload :name))` and
     `(workspace-set-node-id (plist-get payload :name) id)` — pass the NAME,
     never a record you captured (`workspace--registry` is private and a
     stale copy silently no-ops on flush); return `ok`.
     Non-interactive; errors propagate to the dispatcher's guard.
   - `:on-purge (payload)`: if `:node-id` resolves, set the note's `status`
     field to `done` through the same mechanism `create-project-note` used
     (vulpea meta or property) under the coordinator lock; never delete the
     note.
2. Install `workspace-display-name-function` → function that returns
   `(vulpea-note-title (vulpea-db-get-by-id node-id))` when `:node-id` is
   non-nil and resolves, else nil (workspaces falls back to the name).
3. Install `workspace-home-builder` → function that resolves the note's path
   (`vulpea-note-path`), `find-file`s it in a single window, and if the
   buffer is unmodified runs `org-dblock-update-all` (or updates the first
   `org-graph-connected` block) and saves; if the note cannot be resolved,
   call `workspace-default-home-builder`.
4. Register the `session` schema:
   `(vulpea-schema-define 'org-graph-session :predicate (org-graph-schemas--tag-predicate "gptel_session") :fields nil)`
   and add `session` to `org-graph-note-types` (or whatever the taxonomy
   list is) so finders and the dblock grouping know it. Do this inside the
   guard, after org-graph's own schema registration.
5. `workspace-adopt-project-note`: interactive; `vulpea-find`-style
   selection restricted to `project` notes; then prompts for a directory
   (`read-directory-name`) and calls `workspace-new` with that directory and
   the note's ID (prefix-arg anchor semantics). Register it as a `:menu`
   entry too (`a`).
6. Specs (stub vulpea DB, temp registry): on-create creates a note and sets
   node-id; on-create with a resolvable node-id is `skipped` and creates
   nothing; on-purge sets status done and leaves the file; display-name
   returns the title or nil; home builder opens the note and regenerates
   only when unmodified; `session` schema present after load; adopt calls
   `workspace-new` with the chosen ID.

## Design rationale
The project note is the workspace's canonical identity; it must exist from
birth and survive purge (notes outlive worktrees — that was the whole
point). Display name and home view are read from the note through
workspaces' inversion points so workspaces never reads a file. The
`session` schema lives here because its predicate names gptel's filetag;
org-graph must not know gptel. Adopt-project-note is the cross-machine
path: the vault syncs, the registry does not.

## Design pattern
Integration registration shape from the archived
`config/org-graph/workspace-integration.org` (handlers return
`ok`/`skipped`); vulpea access via `vulpea-db-get-by-id` /
`vulpea-note-title` / `vulpea-note-path` as in `discovery.org`'s seed
function.

## Verification
- `./bin/tangle-org.sh config/integrations/workspaces-org-graph.org` validates.
- `./bin/run-tests.sh -d config/integrations` — green.
- `grep -n 'workspace-register-integration\|workspace-display-name-function\|workspace-home-builder\|org-graph-session' config/integrations/workspaces-org-graph.el` → all present.

## Context
design.md § D8 (workspaces-org-graph.org), § D3, § Risks (dynamic-block regeneration)
specs/workspace-graph-integration/spec.md § 'Project note as workspace identity', 'Display name and home layout come from the project note', 'Virtual home via the connected-notes block'

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/shape/workspace-plist-v3`: speculated → reconciled. `:node-id` (string | nil); reader `workspace-node-id` (data-model.el:178); setter `workspace-set-node-id` in persistence.el:276-295 mutates IN PLACE and flushes, so it must be handed the live record — after `ws-node-id-setter-live-record` it resolves the live record from a NAME (step 1 now calls it that way). See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-plist-v3.md`.
- `register/shape/workspace-integration-anchor-payload`: speculated → reconciled. Both dispatchers take `(ws context)`; keys are exactly `:name :home :node-id :context`; the payload is rebuilt per handler from the live record (`workspace--integration-live-record`, integrations.el:59 — to be retired by the setter task), so a later handler (`gptel-session`) sees the `:node-id` you set. `workspace-purge` unregisters (remhash, workspaces.el:184) BEFORE its `:on-purge` dispatch (:201), so `:on-purge` must work from the payload alone — no registry read. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-integration-anchor-payload.md`.
- `register/vocabulary/workspace-creation-context`: reconciled; `purge` is a real member (`:context purge` reaches your `:on-purge` handler). `fresh` / `anchored` arrive only after `ws-scaffold-and-anchor` (blocked-by, already listed). See `.orchestrator/cycles/cycle-1789334430/reconciliations/vocabulary-workspace-creation-context.md`.
- `register/boundary/project-note-creation`: speculated → reconciled. `(org-graph/create-project-note TITLE &optional BODY)` (authoring.el:192) returns the ID; `status: active` is a vulpea META item (`:meta '(("status" . "active"))`), and BODY travels via `:context (:body BODY)` because vulpea expands `%(elisp)` / `${var}` in `:body`. **Implication for step 1 `:on-purge`**: "the same mechanism" is vulpea META — set `status` to `done` through vulpea's meta API (verify the exact function in runtime/straight/repos/vulpea; e.g. `vulpea-meta-set`), under the coordinator lock, then reindex the file. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-project-note-creation.md`.
- `register/invariant/edge-drawer-immediately-after-property-drawer`: speculated → DIVERGENT (ask-cycle-1789334430-8). Project notes RECEIVE edges and carry META (`status`) but no EDGES drawer, so nothing this task writes is affected; the dblock / home builder read the index only. Not blocking here; note it in Observations. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md`.

### User-resolved decisions
- `ask-cycle-1789334430-5` (open; default carried): `workspace-home-builder` receives the workspace RECORD (D3, `ws-inversion-points`). **Implication for step 3**: the installed builder takes `ws`, reads `(workspace-node-id ws)`, and falls back to `(workspace-default-home-builder ws)` — same signature.

### Meta-discoveries
- `vulpea-api-verify-against-source`: every vulpea claim in this task (`vulpea-db-get-by-id`, `vulpea-note-title/path`, `vulpea-schema-define`, meta set/get, `vulpea-find :filter-fn`) must be read from `runtime/straight/repos/vulpea` before use — three speculated vulpea entries were wrong this cycle.
- `stub-bounded-regression`: step 6 stubs the DB and registry; keep ONE un-stubbed end-to-end scenario — real temp vault, real `vulpea-db-sync`, real `workspace--dispatch-create-integrations` — asserting the project note exists, `:node-id` persisted through `workspace--flush-state`, and the `gptel-session` handler (or a probe handler registered after yours) observed the id (the setter task's spec (c) is the template).
