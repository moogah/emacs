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
