---
name: ws-recipe-layouts
description: "Recipe layouts: tagged :saved-state recipe expanded to window-state at the restore choke point"
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "enables:ws-persistence-v4"
  - "enables:ws-agent-tool-add-layout"
---

## Files to modify
- config/workspaces/layouts.org (modify) — recipe predicate, recipe->window-state, restore branch, explicit save replaces recipe
- config/workspaces/test/recipe-layouts-spec.el (new)

## Implementation steps
1. Specs first:
   - `workspace--recipe-p` recognizes `(:recipe :split columns|rows :panes ((:file F :line L?) ...))`
     and rejects captured window-states;
   - `workspace--recipe->window-state RECIPE` returns a `window-state-put`-able form:
     a `hc` (columns) or `vc` (rows) combination of N leaves with equal `normal-width`/
     `normal-height` fractions, each leaf's `parameters` carrying a `workspace-buffer`
     with `:filename` set (and a bookmark-style position for `:line` — compute the
     position by reading the file with `insert-file-contents` into a temp buffer, NOT
     `find-file`);
   - applying a recipe layout via `workspace--restore-frameset` in a test frame shows
     the panes in order with point on the requested line;
   - a pane whose file is missing yields the existing restore fallback's placeholder
     buffer and the other panes still build;
   - `workspace-revert` on a recipe layout re-applies the recipe and keeps it as
     `:saved-state`;
   - autosave never writes a recipe into `:working-state` (it captures the frame);
   - an explicit save over a recipe layout stores a captured window-state.
2. Implement. The only new branch in the apply path is at the top of
   `workspace--restore-frameset`: if the effective state is a recipe, expand it, then
   continue through the existing deserialize → deferred `window-state-put` path with
   its generation guard untouched.
3. Tangle; run tests.

## Design rationale
Building a layout from real windows needs a live frame (the serializer calls
`get-buffer` per leaf), which for an agent means an invisible frame — rejected for
its side effects (frame hooks, macOS focus/flash issues, bufferlo per-tab state,
visiting files in the live session). A recipe is declarative: the agent's tool
arguments are stored as-is, nothing is built until the user switches to it, and
revert rebuilds against the current files. Expanding at apply time keeps a single
restore path. MVP is file panes and one split direction; non-file panes and nested
splits are deferred.

## Verification
- ./bin/tangle-org.sh config/workspaces/layouts.org
- ./bin/run-tests.sh -d config/workspaces

## Context
- design.md § Decisions 'D16. ...' (W6 Recipe layouts)
- specs/workspaces/spec.md § 'Recipe layouts'
