---
name: int-gptel-org-graph
description: Subscribe to the gptel session and branch hooks to write part-of edges to the active workspace's project note and branched-from edges to the parent branch.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-scaffold-and-lint
  - blocked-by:gptel-session-keywords-and-hooks
  - blocked-by:gptel-branch-identity
  - blocked-by:og-edge-writer-and-project-note
  - enables:int-migration-command
---

## Files to modify
- config/integrations/gptel-org-graph.org (modify; tangle)
- config/integrations/test/gptel-org-graph-spec.el (new)

## Implementation steps
1. Session subscriber `jf-integrations--session-created (payload)` on
   `jf/gptel-session-created-hook`: resolve the target node-id as
   (a) `jf-integrations--creating-for-node-id` if bound non-nil (set by the
   birth-session handler), else (b) the workspace owning the current tab:
   `(workspace--current-name)` → registry → `workspace-node-id`. If nil,
   return without writing. Else
   `(org-graph/write-edge (plist-get payload :session-file) 'part-of node-id)`.
2. Branch subscriber on `jf/gptel-branch-created-hook`: read the parent
   file's file-level `:ID:` (head-read of `:parent-file`'s drawer; reuse
   `jf/gptel--read-session-drawer-head` if it exposes `ID`, else a small
   regex read) and `(org-graph/write-edge (plist-get payload :branch-file) 'branched-from parent-id)`.
   The `part-of` edge needs no action: it rode along in the verbatim copy.
3. Both subscribers wrap their body in `condition-case` that logs and
   returns nil (belt and braces; gptel's runner already guards).
4. Add/remove the hook functions inside the `with-eval-after-load` guards so
   they are registered only when both packages are present; make
   installation idempotent (`add-hook` is).
5. Specs: with a stub current workspace carrying a node-id, the session
   subscriber calls `write-edge` with `(file part-of node-id)`; with no
   workspace it does not call it; with the dynamic variable bound it uses
   that id over the tab's; the branch subscriber writes
   `(branch-file branched-from <parent-id>)` where parent-id is read from a
   temp parent file; an error inside `write-edge` is contained.

## Design rationale
Sessions live in one root; what links a session to a workspace is a typed
edge, written by the only layer allowed to know both gptel and org-graph.
Using gptel's hooks keeps gptel ignorant of the graph; using org-graph's
edge writer keeps the drawer format in one place. Lineage becomes a
`branched-from` edge so the project note's home view can render session
trees from the graph instead of YAML sidecars. Edge writes happen before
the chat buffer is displayed and under the coordinator lock, so they do
not race the buffer.

## Design pattern
Hook subscribers as small named functions (not lambdas) so they can be
removed and spied on; drawer head reading as in
`config/gptel/sessions/filesystem.org` `jf/gptel--read-session-drawer-head`.

## Verification
- `./bin/tangle-org.sh config/integrations/gptel-org-graph.org` validates.
- `./bin/run-tests.sh -d config/integrations` — green.
- `grep -n 'session-created-hook\|branch-created-hook\|write-edge' config/integrations/gptel-org-graph.el` → both subscriptions and both edge writes present.

## Context
design.md § D8 (gptel-org-graph.org), § D7, § Risks (hook subscribers race)
specs/workspace-graph-integration/spec.md § 'Session association via typed edges'
