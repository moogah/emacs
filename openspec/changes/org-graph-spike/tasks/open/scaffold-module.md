---
name: scaffold-module
description: Create the config/org-graph/ module skeleton with literate loader, defcustoms, and empty submodule sections so subsequent tasks have a place to land code.
change: org-graph-spike
status: ready
relations: []
---

## Files to modify
- `config/org-graph/org-graph.org` (new) — top-level literate loader, tangles to `org-graph.el`
- `config/org-graph/org-graph.el` (new, generated) — do not edit by hand

## Implementation steps
1. Create `config/org-graph/org-graph.org` with the required literate headers:
   ```org
   #+title: Org Graph
   #+property: header-args:emacs-lisp :tangle org-graph.el
   #+auto_tangle: y
   ```
2. First babel block MUST use `:comments no` and put the lexical-binding mode
   line on line 1 (see memory: lexical-binding must be line 1, or closures
   break):
   ```elisp
   ;;; org-graph.el --- Layered knowledge graph over vulpea -*- lexical-binding: t; -*-
   ```
3. Define the module's defcustoms in a `defgroup org-graph`:
   - `org-graph-roam-root` — default `"~/org/roam/"` (the durable concept
     vault and the typed-edge extraction scope).
   - `org-graph-relation-types` — default `'(implements contradicts supersedes relates-to)`.
   - `org-graph-watch-workspace-homes` — boolean, default `t` (whether to add
     workspace `:home` dirs to the vulpea sync set).
   - `org-graph-note-types` — list of note-type symbols, default
     `'(log debug topic reference project)`.
4. Add empty, clearly-labelled org subtree sections (with placeholder
   `;; implemented in <task>` comments) for each submodule so later tasks
   land code in a predictable place: Discovery, Note-type schemas, Finders,
   Typed-edge parser, Extractor, Query, Coordinator, gptel tools, Workspace
   integration.
5. Do NOT add `"org-graph"` to `jf/enabled-modules` yet — that is the
   `wire-into-init` task, gated on the smoke test.
6. Tangle and validate: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale
The spike is one new literate module organized into focused sub-modules
(design.md "Components"). A skeleton-first task gives every downstream task a
known landing site and lets the loader's defcustoms be referenced before the
behavior exists. Defaults encode the resolved decisions: roam-only typed-edge
scope (RE-4/D2), the closed initial relation set (D3), and registry-driven
home watching (RE-2).

## Design pattern
Follow the existing multi-file literate module layout under
`config/workspaces/` (an `*.org` loader plus focused sub-modules) and
`config/gptel/`. Keep babel blocks small — one function per block — per
CLAUDE.md.

## Verification
- `./bin/tangle-org.sh config/org-graph/org-graph.org` tangles and passes
  `check-parens`.
- Loading the tangled file in an isolated Emacs defines the defcustoms with
  the documented defaults and raises no error:
  `./bin/emacs-isolated.sh -nw --batch --eval "(progn (load \"config/org-graph/org-graph.el\") (message \"%S\" org-graph-relation-types))"`.

## Context
design.md § Components; design.md § Re-evaluation (RE-1, RE-2, RE-3, RE-5);
CLAUDE.md § Literate Programming; memory: lexical-binding mode line position.
</content>
