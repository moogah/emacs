---
name: ws-agent-tool-add-layout
description: "gptel tool workspace_add_layout: validate args, store recipe with agent origin"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:ws-programmatic-add-layout"
  - "blocked-by:ws-recipe-layouts"
---

## Files to modify
- config/workspaces/agent-tools.org (new) — tool definition, gated on gptel
- config/workspaces/workspaces.org (modify) — load agent-tools after gptel when available
- config/workspaces/test/agent-tools-spec.el (new)

## Implementation steps
1. Specs first (call the tool's `:function` directly; no gptel request needed):
   - valid call adds a recipe layout with origin `agent`, description, source, and
     absolute paths (relative paths resolved against the workspace `:home`);
   - nonexistent workspace / missing file / non-positive line → error string naming
     the problem, no layout added;
   - R2/R3 refusals surface as error strings naming the conflict;
   - the call creates no buffer visiting any pane file (compare `buffer-list` /
     `find-buffer-visiting` before and after) and no frame.
2. Define the tool with `gptel-make-tool` (category `"workspaces"`, sync), args:
   `workspace` (string), `layout_name` (string), `split` (string, enum
   columns/rows), `panes` (array of objects `{file: string, line?: integer}`),
   `description` (string, optional), `source` (string, optional). Gate registration
   on `(fboundp 'gptel-make-tool)` exactly as `config/org-graph/tools.org` does
   (lines ~259–364), including a `with-eval-after-load 'gptel` path.
3. Tangle; run tests.

## Design rationale
The demonstration MVP: an agent prepares layouts in the relevant workspace;
switching to them stays the user's action. The tool's arguments are the recipe, so
no translation layer exists and nothing is materialized at call time. Defining it as
a gptel tool means both Claude Code (via the write preset) and in-Emacs gptel agents
get it.

## Verification
- ./bin/tangle-org.sh config/workspaces/agent-tools.org
- ./bin/tangle-org.sh config/workspaces/workspaces.org
- ./bin/run-tests.sh -d config/workspaces

## Context
- design.md § Decisions 'D17. `workspace_add_layout` is the demonstration MVP'
- specs/workspaces/spec.md § 'Agent tool for adding layouts'
