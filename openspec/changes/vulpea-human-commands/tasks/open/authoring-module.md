---
name: authoring-module
description: New authoring module with find-or-create and insert-link commands (thin vulpea wrappers)
change: vulpea-human-commands
status: done
merge_commit: 4b3bf068
relations:
  - blocked-by:vault-root-discovery
  - enables:boot-sync-deferral
---

## Files to modify
- config/org-graph/authoring.org (new; tangles to authoring.el)
- config/org-graph/org-graph.org (modify — loader entry; tangle)
- config/org-graph/test/authoring-spec.el (new)
- config/org-graph/test/module-load-spec.el (modify — canonical order grows to ten)

## Implementation steps
1. Create `config/org-graph/authoring.org` with the required literate headers
   (`#+title:`, `#+property: header-args:emacs-lisp :tangle authoring.el`,
   `#+auto_tangle: y`), a `:comments no` first block for the
   `-*- lexical-binding: t; -*-` mode line, `(require 'vulpea)` (hard
   require, same as `finders.el`), and two commands:
   ```elisp
   (defun org-graph/find-or-create ()
     "Find a note by title, creating it when no indexed note matches.
   Thin wrapper over `vulpea-find' with completion over every indexed
   note; creation is vulpea's synchronous birth-index path (file +
   :ID: + org-id registration + DB insert complete before return)."
     (interactive)
     (vulpea-find :require-match nil))

   (defun org-graph/insert-link ()
     "Insert an id: link to a note at point, creating the note on miss.
   Thin wrapper over `vulpea-insert': active region becomes the link
   description and is replaced by the link."
     (interactive)
     (vulpea-insert))
   ```
   End with `(provide 'org-graph-authoring)`.
2. Add the loader entry in `org-graph.org` immediately AFTER `finders.el`
   and before `edge-type.el`:
   `(jf/load-module (expand-file-name "config/org-graph/authoring.el" jf/emacs-dir))`
   Canonical order becomes: schemas, extractor, coordinator, query, finders,
   authoring, edge-type, tools, discovery, workspace-integration.
3. Tangle both org files with `./bin/tangle-org.sh`.
4. Update `test/module-load-spec.el`: canonical ordered sequence now has ten
   submodules with `authoring` after `finders`; cold load remains DB-free
   (authoring touches no DB at load — assert per the existing pattern).
5. New `test/authoring-spec.el` (Buttercup): spy on `vulpea-find` /
   `vulpea-insert` via `cl-letf` (pattern: existing `finders-spec.el` spies
   on `vulpea-find`) and assert:
   - `org-graph/find-or-create` calls `vulpea-find` with `:require-match nil`
     (and no filter — completes over everything).
   - `org-graph/insert-link` calls `vulpea-insert`.
   - Both are `commandp`.

## Design rationale
vulpea 2.4's built-ins already satisfy every spec scenario, verified
against the installed source: `vulpea-find` with `require-match nil`
routes misses through `vulpea-find-default-create-fn` → `vulpea-create`;
`vulpea-insert` (with `vulpea-insert-default-create-fn` nil, the default)
handles region-as-description with replacement inside an
`atomic-change-group`, creates missing notes, and inserts the id: link.
`vulpea-create` is fully synchronous — writes the file, calls
`org-id-add-location`, calls `vulpea-db-update-file`, errors if the note
is not queryable afterwards — which is exactly the "immediately findable
/ immediately id-resolvable" contract. So the commands are thin wrappers:
they exist to give the surface stable org-graph names (menu targets,
future filtering, tests), not to add behavior. Bespoke
completing-read/creation plumbing was rejected as duplication. A separate
module (not `finders.org`) keeps finders scoped to schema-aware type
finders and keeps the cold-load invariant auditable per file. Placement
defaults (directory + filename template) come from vault-root-discovery,
which is why this task is blocked on it.

## Design pattern
Module shape: `config/org-graph/finders.el` (hard `(require 'vulpea)`,
interactive commands, `provide`). Literate headers + `:comments no` first
block: `config/org-graph/discovery.org`. Test spying via `cl-letf`:
`config/org-graph/test/finders-spec.el` and `test/helpers-spec.el`.

## Verification
- `./bin/tangle-org.sh config/org-graph/authoring.org && ./bin/tangle-org.sh config/org-graph/org-graph.org` — validate.
- `./bin/run-tests.sh -d config/org-graph` — all pass, including the updated
  module-load-spec order.
- `grep -n "authoring" config/org-graph/org-graph.el` — loader entry present,
  positioned after finders.
- Manual (spec scenarios): `M-x org-graph/find-or-create`, type a novel
  title → file created under `~/org/` named `<timestamp>-<slug>.org`, buffer
  visited, immediately listed on re-invoke, `(org-id-find <id>)` resolves.
  `M-x org-graph/insert-link` with an active region → link replaces region
  with region text as description.

## Context
design.md § Decisions 'D3 — Authoring commands are thin wrappers' and 'D4 — Two new modules'
specs/org-graph-note-commands/spec.md (both requirements, all scenarios)

## Observations

- The canonical-order listing in `module-load-spec.el`'s commentary was
  already stale before this task: it read "schemas -> extractor ->
  coordinator -> query -> finders -> tools -> discovery", omitting
  `edge-type` even though edge-type landed in cycle-1786458912 (the
  Step 5b assertion block for it was present and correct — only the
  prose order list had drifted). Fixed in passing while updating the
  listing to the ten-module order, since the task owns that comment's
  update anyway.
- The task body's implementation steps were accurate end to end; the
  two commands were implemented exactly as prescribed (verbatim
  signatures/docstrings from the task body), and the loader entry slots
  cleanly between finders and edge-type. No deviation from the
  prescribed approach.
- `authoring-spec.el` asserts `:require-match nil` via `plist-member` +
  `plist-get` so an explicitly-passed nil is distinguished from the key
  being absent — `(vulpea-find)` with no args would also default
  require-match, but the register entry's contract is the explicit
  `:require-match nil` call shape, so the spec pins that.
- The count-drift warning in
  `register/invariant/org-graph-loader-ordered-sequence` (trust the
  name lists, not stale counts) was accurate but not needed here: this
  task's body consistently said ten modules and listed ten names.

## Discoveries

none
