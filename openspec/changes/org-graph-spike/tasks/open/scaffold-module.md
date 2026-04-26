---
name: scaffold-module
description: Create the config/org-graph/ module skeleton with literate loader, defcustoms, and empty submodule sections so subsequent tasks have a place to land code.
change: org-graph-spike
status: ready
relations: []
---

## Files to modify

- `config/org-graph/org-graph.org` (new) — literate loader with lexical-binding header, tangle headers, defcustoms, empty submodule sections.
- `config/org-graph/test/` (new directory) — create with subdirs: `extractor/`, `finders/`, `query/`, `coordinator/`, `integration/`.

## Implementation steps

1. Create `config/org-graph/org-graph.org` with the standard literate header pattern. CRITICAL: use `:comments no` for the lexical-binding block to keep it on line 1 of the tangled `.el` (per repo memory: line-1 lexical-binding requirement).

   ```org
   #+title: Org Graph
   #+property: header-args:emacs-lisp :tangle org-graph.el
   #+auto_tangle: y

   * Lexical Binding
   #+begin_src elisp :comments no
   ;;; org-graph.el --- Layered knowledge graph (org-node + vulpea) -*- lexical-binding: t; -*-
   #+end_src
   ```

2. Add a `Customization` subtree with these defcustoms (no behavior yet):
   - `org-graph-watched-roots` — `'("~/org/roam/" "~/work/")`, type `(repeat directory)`.
   - `org-graph-typed-graph-root` — `"~/org/roam/"`, type `directory`.
   - `org-graph-relation-types` — `'(implements contradicts supersedes relates-to)`, type `(repeat symbol)`.
   - `org-graph-taxonomy-tags` — `'(log debug topic reference project agent-draft)`, type `(repeat symbol)`.
   - `org-graph-coordinator-timeout` — `5.0`, type `number`, docstring "Seconds before with-file-lock raises timeout error."

3. Add empty top-level subtrees `Discovery`, `Extractor`, `Finders`, `Query`, `Coordinator`, `Tools` — placeholder `;; TODO` blocks. Subsequent tasks fill them.

4. Create the test directory tree: `mkdir -p config/org-graph/test/{extractor,finders,query,coordinator,integration}`.

5. Tangle and validate: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale

A skeleton-first approach keeps the literate file's table of contents stable so parallel tasks can land in dedicated subtrees without merge conflicts (architecture.md §Components). Defcustoms live at the top of the file because every submodule reads them, and putting them in a separate file would force ordering hazards during tangle (design.md §D7). The `:comments no` rule on the header block is non-negotiable per repo memory — the wrapper-callback bug class is hard to debug.

## Verification

- `./bin/tangle-org.sh config/org-graph/org-graph.org` — exits 0.
- `head -1 config/org-graph/org-graph.el` — the line is exactly `;;; org-graph.el --- Layered knowledge graph (org-node + vulpea) -*- lexical-binding: t; -*-`.
- `ls config/org-graph/test/` — shows the five subdirs.
- `grep -n "defcustom org-graph-" config/org-graph/org-graph.el` — five matches.

## Context

- architecture.md §Components
- design.md §D7 Module load order
