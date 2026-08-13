---
name: menu-module
description: New menu module — edge-at-point commands, org-graph-menu transient, SPC v evil binding
change: vulpea-human-commands
status: blocked
relations:
  - blocked-by:boot-sync-deferral
  - enables:runbook-and-verify
---

## Files to modify
- config/org-graph/menu.org (new; tangles to menu.el)
- config/org-graph/org-graph.org (modify — loader entry, last position; tangle)
- config/org-graph/test/menu-spec.el (new)
- config/org-graph/test/module-load-spec.el (modify — canonical order grows to eleven)

## Implementation steps
1. Create `config/org-graph/menu.org` (literate headers, `:comments no`
   first block with the lexical-binding mode line, `(require 'transient)`).
2. Note-at-point resolution helper:
   ```elisp
   (defun org-graph-menu--note-id-at-point ()
     "Return the :ID: governing point, or signal `user-error'.
   Enclosing ID-bearing heading first, then file level (inherited
   lookup, same idiom as `vulpea-find-backlink')."
     (or (org-entry-get nil "ID" t)
         (user-error "No note with an :ID: at point")))
   ```
3. Edge-query display commands: `org-graph/edges-outgoing-at-point`,
   `org-graph/edges-incoming-at-point`, `org-graph/edges-connected-at-point`
   (interactive). Each resolves the id, calls the matching
   `org-graph-query/outgoing|incoming|connected`, and renders into a
   read-only org-mode buffer `*org-graph-edges*`: one section per
   direction, one list item per edge —
   `- <rel-type> :: [[id:<uuid>][<title>]]` — titles via
   `vulpea-db-get-by-id` with the raw id as fallback. Use
   `with-current-buffer` + `special-mode`-style read-only setup but keep
   `org-mode` active so `id:` links are followable; display with
   `pop-to-buffer`.
4. Transient prefix (pattern: `workspace-menu` in
   `config/workspaces/workspaces-transient.el`):
   ```elisp
   (transient-define-prefix org-graph-menu ()
     "Human interaction surface for the org-graph note graph."
     [["Find"
       ("t" "topic" org-graph/find-topic)
       ("d" "debug" org-graph/find-debug)
       ("l" "log" org-graph/find-log)
       ("r" "reference" org-graph/find-reference)
       ("p" "project" org-graph/find-project)
       ("a" "any" org-graph/find-any)
       ("D" "agent drafts" org-graph/find-agent-drafts)]
      ["Author"
       ("f" "find or create" org-graph/find-or-create)
       ("i" "insert link" org-graph/insert-link)]
      ["Edges (at point)"
       ("o" "outgoing" org-graph/edges-outgoing-at-point)
       ("n" "incoming" org-graph/edges-incoming-at-point)
       ("c" "connected" org-graph/edges-connected-at-point)]
      ["Maintain"
       ("s" "re-index (sync)" org-graph/configure-sync)
       ("v" "validate note type" org-graph/validate-note-at-point-or-prompt)
       ("h" "doctor" vulpea-doctor)]])
   ```
   For the validate entry, dispatch to the existing validation surface
   (`org-graph/validate-note-type` / `org-graph/validate-all-of-type` —
   check `schemas.el` for the exact interactive entry point; add a thin
   interactive wrapper here only if none is interactive).
3. Binding, self-installed and evil-guarded (evil.org untouched):
   ```elisp
   (with-eval-after-load 'evil
     (evil-define-key 'normal 'global (kbd "<SPC> v") #'org-graph-menu))
   ```
4. Loader entry LAST in `org-graph.org` (menu references finders,
   authoring, query, discovery). Canonical eleven-module order: schemas,
   extractor, coordinator, query, finders, authoring, edge-type, tools,
   discovery, workspace-integration, — with menu appended after
   workspace-integration.
5. Tangle both org files.
6. Update `test/module-load-spec.el` to the eleven-module order.
7. New `test/menu-spec.el` (Buttercup):
   - `org-graph-menu` is a command; transient layout contains bindings for
     every listed command (use `transient-get-suffix` or parse the prefix's
     layout, pattern: `config/workspaces/test` transient specs if present,
     else assert `(get 'org-graph-menu 'transient--layout)` mentions each
     target symbol).
   - `org-graph-menu--note-id-at-point`: heading `:ID:` found; file-level
     `:ID:` found; no id → `user-error` (temp org buffers).
   - Edge rendering: stub `org-graph-query/outgoing` + `vulpea-db-get-by-id`
     via `cl-letf`, invoke the command in a temp buffer with an `:ID:`,
     assert `*org-graph-edges*` contains the `- rel :: [[id:...][title]]`
     line.
   - Load without evil: binding install is wrapped in
     `with-eval-after-load 'evil`, so loading menu.el with evil absent
     signals nothing (assert `featurep` guard behavior).

## Design rationale
The menu is the discoverable front door: every human graph interaction
reachable from `SPC v` without memorizing `M-x` names, and each entry
dispatches the SAME command available via `M-x` (spec: menu adds
discoverability, not divergent behavior). Binding lives in the menu
module, not `config/core/evil.org`, to keep org-graph self-contained
while it is still a spike (one module to delete on rollback) and to
degrade gracefully to `M-x org-graph-menu` when evil is absent. `SPC v`
verified unbound. Edge results go to a dedicated org buffer rather than
the echo area because multi-edge results need navigation, and org `id:`
links give free follow-through (resolvable thanks to the startup
org-id seed). Note-at-point uses the inherited `org-entry-get` idiom
vulpea itself uses in `vulpea-find-backlink`, with a clear `user-error`
when nothing at point has an `:ID:` (spec requirement). `transient` is
loaded early in `jf/enabled-modules`, well before org-graph, so a hard
require is safe.

## Design pattern
Transient prefix: `config/workspaces/workspaces-transient.el`
(`workspace-menu`). SPC bindings: `config/core/evil.el:97-129` shows the
`evil-define-key 'normal 'global (kbd "<SPC> ...")` idiom. Module and
test shapes: as in the authoring-module task.

## Verification
- Both tangles validate; `./bin/run-tests.sh -d config/org-graph` passes.
- `grep -n "menu" config/org-graph/org-graph.el` — loader entry last.
- Manual: fresh boot → `SPC v` in a normal-state buffer opens the menu
  with four groups; each Find/Author entry behaves identically to its
  `M-x`; edge queries on a roam concept note render `*org-graph-edges*`
  with followable links; edge query in a plain buffer → clear user-error;
  `emacs -Q`-style no-evil load leaves `M-x org-graph-menu` working.

## Context
design.md § Decisions 'D4', 'D6 — SPC v installed by the menu module', 'D7 — Note-at-point resolution', 'D8 — Edge results render in a dedicated org buffer'
specs/org-graph-menu/spec.md (all three requirements)

## Observations

- The connected view (`org-graph/edges-connected-at-point`) calls the two
  directional queries (`org-graph-query/outgoing` + `org-graph-query/incoming`)
  rather than `org-graph-query/connected`, deviating from step 3's literal
  "calls the matching ... connected". Rationale: connected is BY DEFINITION
  their append (query.el:50-56), so behavior is identical, and per-direction
  querying is what lets the renderer attribute the correct far end per
  section — a flat connected list cannot re-attribute a self-edge (it appears
  in both halves with FROM = TO). `org-graph-query/connected` keeps its live
  caller (tools.el:89, the agent query tool); no dead branch.
- Confirmed the plan-phase note: NO validator in schemas.el is interactive
  (`org-graph/validate-note-type` takes a NOTE, `org-graph/validate-all-of-type`
  takes a TYPE; neither is `commandp`). Added the thin interactive wrapper
  `org-graph/validate-note-at-point-or-prompt` in menu.el as the task
  anticipated: note-at-point path validates that note via
  `vulpea-db-get-by-id` + `org-graph/validate-note-type`; no-note path prompts
  over `org-graph-note-types` and runs `org-graph/validate-all-of-type`. No
  validation logic added. Violations are echoed as a `%S` summary — vulpea 2.4
  has no violation formatter (only the `vulpea-violation` struct); a prettier
  renderer would be new presentation logic and is left out of this thin wrapper.
- The "load without evil" scenario cannot be executed in the test process
  (the `make` runner loads init.el, so evil is already loaded and cannot be
  unloaded safely). menu-spec covers the invariant two ways instead: a
  source-level assertion that menu.el wraps the install in
  `with-eval-after-load 'evil` (holds regardless of process state), plus a
  live `evil-normal-state-map` lookup asserting SPC v → `org-graph-menu` when
  evil is present (with an `M-x`-reachability fallback branch for evil-less
  processes). Real evil-less boot behavior remains a runbook item
  (runbook-and-verify task).
- The installed transient's layout suffix shape is `(CLASS . PLIST)` — the
  legacy `(LEVEL CLASS PLIST)` list the task's fallback pattern implies was
  upgraded away (transient.el `transient--layout-upgrade`). menu-spec's
  `org-graph-menu-spec--suffix-command` helper tolerates both shapes, and the
  green-on-empty guard (fboundp/commandp of every dispatched symbol, including
  `vulpea-doctor`, verified interactive in vulpea 2.4) is asserted separately
  from layout membership as the brief required.
- The edges buffer render adds one header line ("Typed edges for id:<subject>")
  above the direction sections — not in the register shape's required keys, but
  a plain org line that names the subject (and is itself a followable plain
  `id:` link). Empty directions render "No edges." under their section heading
  rather than an empty section.

## Discoveries

- discovery_id: disc-menu-module-1
  class: interface-drift
  description: |
    register/boundary/note-at-point-resolution declares the resolver
    signature "() -> id string | signals user-error", but the same entry
    lists the validate front door as a consumer that "may prompt instead
    when point has no note" — and its duplication clause forbids a second
    inlined org-entry-get. Those three constraints are only jointly
    satisfiable if the ONE shared resolver can be asked not to signal.
    Implemented org-graph-menu--note-id-at-point with an &optional NOERROR
    parameter: default behavior is exactly the entry's contract (inherited
    org-entry-get lookup, nil -> user-error "No note with an :ID: at
    point"); the validate wrapper passes 'noerror and prompts on nil. The
    inherited lookup still lives in exactly one place; edge-query commands
    call it argument-less and keep the signal-on-nil guarantee (menu-spec
    asserts the query is never reached with a nil id).
  affected_register_entry: register/boundary/note-at-point-resolution
  recommendation: |
    Reconcile the entry's functions block to signature
    "(&optional noerror) -> id string | nil (only when noerror) | signals
    user-error"; keep the "commands MUST NOT query with a nil/empty id"
    teeth and note the validate front door as the only noerror caller.
