---
name: og-edge-writer-and-project-note
description: Add org-graph/write-edge (append an EDGES item under the coordinator lock) and org-graph/create-project-note.
change: workspace-notes-in-vault
status: ready
task_class: feature
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:int-workspaces-org-graph
  - enables:int-gptel-org-graph
cites_register_entries:
  - register/boundary/edge-writer-api
  - register/boundary/project-note-creation
  - register/invariant/edge-drawer-immediately-after-property-drawer
  - register/invariant/write-edge-idempotent
  - register/invariant/coordinator-lock-contract
  - register/vocabulary/relation-types
  - register/invariant/edge-drawer-discriminator
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
---

## Files to modify
- config/org-graph/authoring.org (modify; tangle)
- config/org-graph/extractor.org (read only — reuse the drawer-name resolver `org-graph-extractor--drawer-name` or equivalent ~:69-78)
- config/org-graph/coordinator.org (read only — `org-graph-coordinator/with-file-lock`)
- config/org-graph/test/authoring-spec.el (modify)

## Implementation steps
1. `org-graph/write-edge (file type target &optional node-id)`:
   - Under `(org-graph-coordinator/with-file-lock file ...)`: `insert-file-contents`
     into a temp buffer, `(delay-mode-hooks (org-mode))`.
   - Locate the node: nil node-id → `point-min`; otherwise search for the
     heading whose `:ID:` property equals node-id (`org-find-property "ID" node-id`
     / `org-id-find` restricted to this file) and signal `user-error` if absent.
   - Find the node's property drawer end (`org-get-property-block` at file
     level or heading level). If an `EDGES` drawer (name from the extractor's
     resolver, case-insensitive) directly follows it, append the item inside
     it; else insert `:EDGES:\n- <type> :: [[id:<target>]]\n:END:\n`
     immediately after the property drawer's `:END:` line. Skip when an item
     with the same normalised type and target already exists (idempotent).
   - `write-region` back; then `(vulpea-db-update-file file)` so the edge is
     indexed without waiting for autosync.
   - Type normalisation: lowercase, spaces/underscores → hyphens (same rule
     the extractor applies when reading).
2. `org-graph/create-project-note (title &optional body)`: call
   `vulpea-create` with a template that yields `#+title: TITLE`,
   `#+filetags: :project:`, a file-level property `status: active` (match
   how the `project` schema expects the field — check
   `org-graph-schemas-register` / `vulpea-schema` field lookup; it reads
   note meta, so emit it as vulpea meta or a property consistently with the
   schema predicate), an empty `#+BEGIN: org-graph-connected\n#+END:` block,
   then BODY. Return the note ID; index immediately (`vulpea-create` already
   calls `vulpea-db-update-file`).
3. Specs (temp vault): write-edge on a file with only a property drawer
   creates the drawer right after it and leaves the rest byte-identical;
   second identical call is a no-op; heading node-id targets the heading's
   drawer; the lock function is invoked (spy); `vulpea-db-update-file`
   called with the file. create-project-note produces the expected
   keywords/fields and an ID the `project` finder's predicate accepts.

## Design rationale
Association in this design is a typed `part-of` edge in the note's
`EDGES` drawer, written by the integration layer from gptel hooks and
workspace dispatch. org-graph owns the drawer format and the file lock, so
it must publish the writer; callers never touch drawer syntax. Placing the
drawer directly after the node's property drawer matters for sessions: a
branch's verbatim head copy carries the edge, and gptel's identity rewrite
never touches it. The project-note constructor exists so the integration
layer creates the workspace identity note through org-graph's API rather
than by writing org text itself.

## Design pattern
`org-graph-tools/write-node` (tools.org ~:217-243) shows lock usage and
file writing; `org-graph/find-or-create` (authoring.org) shows the
`vulpea-create` wrapper style. Drawer parsing rules are in extractor.org
§ Edge-drawer items — the writer must produce exactly what the reader
accepts.

## Verification
- `./bin/tangle-org.sh config/org-graph/authoring.org` validates.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -n 'defun org-graph/write-edge\|defun org-graph/create-project-note' config/org-graph/authoring.el` → both defined.
- Round-trip scenario (un-stubbed): `org-graph/write-edge` on a temp file, then run the real extractor over it and expect exactly one edge row with the normalised type.
- Read `runtime/straight/repos/vulpea/` for `vulpea-create` / `vulpea-db-update-file` signatures before writing the project-note template; quote them in your report.

## Context
design.md § D6 (Edge-writer API, Project-note API bullets), § D7
specs/org-graph/spec.md § 'Edge-writer API', 'Project note creation API'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/boundary/edge-writer-api` — NEW, speculated. Eight stages; pressure-test stage 3 (heading lookup restricted to this file) and stage 4 (drawer directly after the property drawer, case-insensitive name from `org-graph-extractor--edge-drawer-name`).
- `register/boundary/project-note-creation` — NEW, speculated. The `status: active` form must satisfy the `project` schema predicate in `schemas.org` — check how it reads the field and record which form you emitted.
- `register/invariant/edge-drawer-immediately-after-property-drawer` — NEW, speculated. For the file-level node the drawer lands BEFORE `#+title:`; the byte-identity spec is the enforcement.
- `register/invariant/write-edge-idempotent` — NEW, speculated. Same (node, normalised type, target) twice → one item, file byte-identical.
- `register/invariant/coordinator-lock-contract` — Confirmed; the writer must run inside `org-graph-coordinator/with-file-lock` — spy on it.
- `register/vocabulary/relation-types` — Reconciled; the normalisation rule is the contract. Route TYPE through `org-graph-extractor--normalize-rel` — a second inlined normaliser is a duplication finding.
- `register/invariant/edge-drawer-discriminator` — Confirmed; the writer must emit exactly what the extractor's drawer parser accepts. Add a round-trip spec: write an edge, run the extractor on the file, expect one edge row.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (og-connected-dblock, int-workspaces-org-graph, int-gptel-org-graph).

## Plan-phase notes (architect forward-mode, cycle-1789334430)

- `status: active` is a vulpea META item (`- status :: active` in the note body) because the `project` schema predicate in `schemas.org` reads `vulpea-note-meta-get`; it is not a property-drawer entry as D6's wording suggests. Emit it as META and say so in your report.
- `org-graph/write-edge`'s heading lookup is pinned by the register entry as an in-file `:ID:` scan (not `org-id-find`), so an unindexed file still works. If you disagree, push back in Discoveries.

## Observations

(empty — implementor appends during execution)

## Discoveries

(empty — implementor appends structured discoveries; see task-body template)
