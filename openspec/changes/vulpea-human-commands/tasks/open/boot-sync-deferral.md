---
name: boot-sync-deferral
description: Defer org-graph/configure-sync to emacs-startup-hook as the third resilient deferred op
change: vulpea-human-commands
status: blocked
relations:
  - blocked-by:authoring-module
  - enables:menu-module
---

## Files to modify
- config/org-graph/org-graph.org (modify — third deferred op; tangle)
- config/org-graph/test/module-load-spec.el (modify — deferred-op wiring)

## Implementation steps
1. In `org-graph.org`, alongside the two existing deferred ops
   (`org-graph--register-extractor`,
   `org-graph--seed-org-id-locations-deferred`), add a third using the same
   shared resilient-guard body (`org-graph--run-deferred` or equivalent —
   see the existing "Shared body for the org-graph `emacs-startup-hook'
   deferrals" helper around line 112 of `org-graph.el`):
   ```elisp
   (defun org-graph--configure-sync-deferred ()
     "Run sync configuration once, resiliently, for `emacs-startup-hook'.
   Failures are reported via `display-warning' rather than aborting
   startup."
     ...)
   ```
   Warning tag pattern: mirror the existing ops (they warn
   "org-id-locations seed skipped" / "typed-edge extractor registration
   skipped"); use e.g. "sync configuration skipped".
2. Hook registration ORDER matters and must be: extractor registration →
   configure-sync → org-id seed. `add-hook` without APPEND pushes to the
   front, so place the `add-hook` calls so the final hook order is as above
   (check the existing two `add-hook` lines at `org-graph.el:133` and
   `:144` and slot the new one to land between them at runtime).
3. Cold load must stay DB-free: only the *hook registration* happens at
   load; `org-graph/configure-sync` (which sets
   `vulpea-db-sync-directories`, enables `vulpea-db-autosync-mode`, and
   triggers an async full scan) runs at startup-hook time only.
4. Tangle `./bin/tangle-org.sh config/org-graph/org-graph.org`.
5. Update `test/module-load-spec.el` deferred-op wiring specs (they
   currently assert the two ops are on `emacs-startup-hook` and drive them
   with the DB stubbed): add the third op — present on the hook, ordered
   between the other two, drives `org-graph/configure-sync` when fired
   (stub `vulpea-db-autosync-mode` / `vulpea-db-sync-full-scan` via
   `cl-letf`), and reports via `display-warning` instead of signaling when
   the stub throws.

## Design rationale
The eval demonstrated that on a normal boot NOTHING enables vulpea
autosync: `org-graph/configure-sync` is only reachable from the
workspaces Integrations menu, so the DB stays frozen at the last manual
re-index and notes authored in prior sessions are invisible to the graph
(observed 2026-08-13 with a fresh roam note). Meanwhile the spike spec's
Discovery requirement already promises save-time pickup and
external-change detection. This op lands implementation where the spec
already is, and makes the widened `~/org/` root actually watched from
boot. Resilient-guard + `emacs-startup-hook` (NOT `after-init-hook` —
under `emacs -q --load init.el` the post-init seam is emacs-startup-hook)
follows the established deferral pattern, keeping cold `(require
'org-graph)` DB-free per the loader invariant
(`register/invariant/org-graph-loader-ordered-sequence`). Known limits,
unchanged here: a mid-session `jf/reload-module` does NOT re-fire
startup-hook ops, and roots added mid-session (new workspaces) still get
only the `:on-create` one-shot.

## Design pattern
The two existing deferred ops in `org-graph.org` — shared guard body,
`display-warning` on failure, `add-hook` at load. Their wiring specs in
`test/module-load-spec.el` show the stub-and-fire test approach.

## Verification
- `./bin/tangle-org.sh config/org-graph/org-graph.org` — validates.
- `./bin/run-tests.sh -d config/org-graph` — all pass.
- `grep -n "configure-sync-deferred\|add-hook 'emacs-startup-hook" config/org-graph/org-graph.el` — three hook adds, correct order.
- Manual (fresh boot, runbook-style): `(bound-and-true-p vulpea-db-autosync-mode)`
  → t with no manual step; create+save an ID-bearing note under `~/org/`,
  it appears in `org-graph/find-any` within a few seconds; `*Warnings*`
  clean of "sync configuration skipped".

## Context
design.md § Decisions 'D5 — Third deferred startup op: configure-sync'
specs/org-graph/spec.md § 'Workspace-Substrate Discovery' scenarios 'Note added during a session…' and 'Externally modified file…'
config/org-graph/docs/spike-eval.org § 'Boot model and the reload caveat'
