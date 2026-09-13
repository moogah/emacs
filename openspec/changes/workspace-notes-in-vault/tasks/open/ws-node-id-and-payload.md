---
name: ws-node-id-and-payload
description: Add the opaque :node-id slot to the workspace record, payload, and persistence; delete the sessions-dir helpers and payload key.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:ws-scaffold-and-anchor
  - enables:int-workspaces-org-graph
---

## Files to modify
- config/workspaces/data-model.org (modify; tangle to data-model.el)
- config/workspaces/integrations.org (modify; tangle)
- config/workspaces/persistence.org (modify; tangle)
- config/workspaces/workspaces.org (modify; tangle — delete `workspace-sessions-dir`)
- config/workspaces/test/data-model-spec.el (modify)
- config/workspaces/test/integration-dispatch-spec.el (modify)
- config/workspaces/test/persistence*-spec.el (modify: round-trip + absent-key specs)
- openspec/specs/workspaces/spec.md, openspec/specs/workspace-integrations/spec.md (read only — the delta specs under this change's `specs/` are the contract)

## Implementation steps
1. In `data-model.org`, extend `workspace--make (name home)` to
   `(name home &optional node-id)`; `cl-check-type node-id (or null string)`;
   emit `:node-id node-id` as a sixth key. Add `workspace-node-id (ws)` and
   `workspace-set-node-id (ws node-id)` (public names, no `--`); the setter
   validates the type and calls the persistence flush (`workspace--persist`
   or whatever the synchronous-flush entry point is named in persistence.org)
   after mutating — it is an identity change, same class as `workspace-new`.
2. Delete `workspace--sessions-dir` (data-model.org ~:429) and
   `workspace-sessions-dir` (workspaces.org ~:425-441) and their prose
   sections. Grep the package for callers: `grep -n 'sessions-dir' config/workspaces/*.org`
   must be empty after this task except the integrations payload line you
   remove in step 3.
3. In `integrations.org`, `workspace--integration-payload` returns
   `(list :name name :home home :node-id (workspace-node-id ws) :context context)`.
   The constructor currently receives `(name home context)`; change it to take
   the workspace record (or look it up by name) so `:node-id` is live — later
   handlers in the same dispatch must see a node-id set by an earlier handler.
4. In `persistence.org`, add `:node-id` to the serialize whitelist
   (~:110-122) and let the deserializer (~:132-190) accept a missing or nil
   `:node-id` silently (no notice). Schema version stays 3. Add
   `workspace-set-node-id` to the list of synchronous flush triggers in the
   prose.
5. Tangle all four files with `./bin/tangle-org.sh`; run the workspaces suite.
6. Specs: `workspace--make` with and without node-id; payload has `:node-id`
   and no `:sessions-dir`; persistence round-trips `"ABC"`; a v3 plist without
   `:node-id` loads with nil and no `*Messages*` notice; the setter flushes
   (spy on the flush function).

## Design rationale
The workspace has no identity beyond `name == basename(home)`; the org ID
stamped into `home.org` was never read back. This change makes a vault
project note the canonical, machine-independent identity and stores its
ID on the record as an opaque token the package never interprets, so the
per-machine registry can map a synced note ID to a local home. The
registry stays keyed by name (rekeying would churn every completing-read,
tab name, and persistence lookup) and a nil node-id must remain valid so a
workspace works without any integration. `:sessions-dir` leaves the payload
because the package no longer has a session concept — sessions are
gptel's, associated through the graph by the integration layer.

## Design pattern
Follow the existing plist-record accessors in `data-model.org`
(`workspace--home` / `workspace--set-home`) and the whitelist serializer in
`persistence.org`. Public setters that mutate identity flush synchronously —
mirror how `workspace-new`'s home stamp triggers the flush.

## Verification
- `./bin/tangle-org.sh config/workspaces/data-model.org` (and the other three) validate.
- `./bin/run-tests.sh -d config/workspaces` — green.
- `grep -n 'sessions-dir' config/workspaces/data-model.el config/workspaces/workspaces.el config/workspaces/integrations.el` → no matches.
- `grep -n 'node-id' config/workspaces/integrations.el config/workspaces/persistence.el` → payload key and whitelist entry present.

## Context
design.md § D2 'node-id is identity; the registry name stays the key'
specs/workspaces/spec.md § 'Workspace node identity', 'Per-machine persistence and restoration'
specs/workspace-integrations/spec.md § 'Anchor payload contract (push, not consult)'
