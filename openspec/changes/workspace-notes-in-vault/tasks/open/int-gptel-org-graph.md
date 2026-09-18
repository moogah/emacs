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
> Cycle 1: step 2's parent-ID read corrected — `jf/gptel--read-session-drawer-head` returns only `:GPTEL_*:` keys (filesystem.el:392-413), so the "if it exposes ID" branch is dead; see ask-cycle-1789334430-4 in `disposition-cycle2-design-confirmations` and .orchestrator/cycles/cycle-1789334430/reconciliations/boundary-session-lifecycle-hooks.md.

1. Session subscriber `jf-integrations--session-created (payload)` on
   `jf/gptel-session-created-hook`: resolve the target node-id as
   (a) `jf-integrations--creating-for-node-id` if bound non-nil (set by the
   birth-session handler), else (b) the workspace owning the current tab:
   `(workspace--current-name)` → registry → `workspace-node-id`. If nil,
   return without writing. Else
   `(org-graph/write-edge (plist-get payload :session-file) 'part-of node-id)`.
2. Branch subscriber on `jf/gptel-branch-created-hook`: obtain the parent
   file's file-level `:ID:` — prefer the payload's `:parent-id` if
   `gptel-branch-identity` adds it (recommended there); otherwise a small
   regex read of the point-min property drawer of `:parent-file`
   (`jf/gptel--read-session-drawer-head` returns only `:GPTEL_*:` keys, not
   `ID`) — and `(org-graph/write-edge (plist-get payload :branch-file) 'branched-from parent-id)`.
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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/boundary/session-lifecycle-hooks`: speculated → reconciled. The hook runs at the end of `jf/gptel--create-session-core` (commands.el:661) via `jf/gptel--run-lifecycle-hook` (commands.el:416-437) AFTER session.org, its file-level `:ID:` and the sibling are on disk, with NO visiting buffer and NO `jf/gptel--session-registry` entry (registration is caller-side at find-file). **Implication**: step 1 must resolve the workspace from the payload + the current tab only — never from gptel's registry; design D5's "after the registry entry exists" is wrong (ask-3). See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-session-lifecycle-hooks.md`.
- `register/shape/session-created-hook-payload`: speculated → reconciled. Keys `:session-file :session-id :branch :session-dir :branch-dir :project-root` (commands.el:661-667); `:branch-dir` is ALWAYS present; `:session-file` and `:session-dir` are absolute. Step 1's `(plist-get payload :session-file)` is the right key. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-session-created-hook-payload.md`.
- `register/shape/branch-created-hook-payload`: speculated (annotated). The hook is DECLARED and INERT (commands.el:395); `gptel-branch-identity` (blocked-by) is its first producer with keys `:branch-file :parent-file :session-id :branch` and, if it takes the ask-4 recommendation, `:parent-id`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-branch-created-hook-payload.md`.
- `register/boundary/edge-writer-api`: speculated → reconciled. `(org-graph/write-edge FILE TYPE TARGET &optional NODE-ID)` (authoring.el:133): validates arguments BEFORE taking the coordinator lock; REFUSES (`user-error`, file byte-identical) a node whose property drawer has no `:ID:`; returns the appended item text, or nil on the idempotent no-op; TYPE is normalised (`'part-of` fine). **Implication**: core-created sessions are always stamped (commands.el:632-637) so the file-level write is valid; a nil return is the "already linked" signal, not an error; step 3's `condition-case` must also catch `user-error`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-edge-writer-api.md`.
- `register/invariant/edge-drawer-immediately-after-property-drawer` / `register/invariant/write-edge-idempotent`: speculated → DIVERGENT (ask-8 / ask-13; `disposition-edge-drawer-meta-and-placement`). This task writes only onto session notes, which carry no META and no hand-authored drawer → PROCEED, but record the divergence in Observations and do not add any META to session notes. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md`, `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-write-edge-idempotent.md`.
- `register/shape/session-file-head`: reconciled. PersistentAgent transcripts are a second producer variant that never fires the hook (ask-12 default (a)), so agent transcripts under `.agents/` never receive `part-of` edges — expected, not a bug. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-session-file-head.md`.

### User-resolved decisions
- `ask-cycle-1789334430-3` (open; default carried — `disposition-design-doc-corrections` option (a)): subscribers never read `jf/gptel--session-registry` at hook time. Step 1 complies as written (payload + `workspace--current-name` → workspace registry).
- `ask-cycle-1789334430-4` (open; default carried — `disposition-cycle2-design-confirmations`): parent `:ID:` is read by the subscriber itself, or supplied as `:parent-id` by `gptel-branch-identity` — coordinate; step 2 now describes both.

### Meta-discoveries
- `stub-bounded-regression`: step 5 spies on `write-edge`; add ONE un-stubbed end-to-end scenario — real `jf/gptel--create-session-core` into a temp `jf/gptel-sessions-directory`, real `org-graph/write-edge` into a temp vault — asserting the session.org gains the drawer item and the index gains the `part-of` row.
- `vulpea-api-verify-against-source`: if you touch `vulpea-db-update-file` for the reindex after the write, read it in `runtime/straight/repos/vulpea` first (write-edge already calls it once per actual write; do not double-index).
