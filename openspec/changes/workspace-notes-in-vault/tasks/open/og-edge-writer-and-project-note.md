---
name: og-edge-writer-and-project-note
description: Add org-graph/write-edge (append an EDGES item under the coordinator lock) and org-graph/create-project-note.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:int-workspaces-org-graph
  - enables:int-gptel-org-graph
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

## Context
design.md § D6 (Edge-writer API, Project-note API bullets), § D7
specs/org-graph/spec.md § 'Edge-writer API', 'Project note creation API'
