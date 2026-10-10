---
name: ws-agent-tool-add-layout
description: "gptel tools workspace_add_layout / workspace_remove_layout: resolve workspace by name or directory, store recipe with agent origin, remove agent layouts"
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
     absolute paths (relative paths resolved against the workspace `:home`),
     including nested `{split, panes}` panes;
   - the workspace may be given as `directory` instead of `workspace`; it resolves via
     `workspace-for-directory` (error when no workspace owns it; error when both or
     neither are given);
   - `workspace_remove_layout` removes an agent layout and surfaces refusals as error
     strings;
   - nonexistent workspace / missing file / non-positive line → error string naming
     the problem, no layout added;
   - R2/R3 refusals surface as error strings naming the conflict;
   - the call creates no buffer visiting any pane file (compare `buffer-list` /
     `find-buffer-visiting` before and after) and no frame.
2. Define both tools with `gptel-make-tool` (category `"workspaces"`, sync).
   `workspace_add_layout` args: `workspace` (string, optional), `directory` (string,
   optional), `layout_name` (string), `split` (string, enum columns/rows), `panes`
   (array of objects: `{file: string, line?: integer}` or `{split: string, panes:
   array}` — describe the recursion in the description since gptel's JSON schema for
   `:items` is not recursive; validate the tree in Elisp), `description` (string,
   optional), `source` (string, optional). `workspace_remove_layout` args:
   `workspace` / `directory` (one required), `layout_name`. Document the `ask/<id>`
   naming convention for orchestrator asks in the add tool's description. Gate registration
   on `(fboundp 'gptel-make-tool)` exactly as `config/org-graph/tools.org` does
   (lines ~259–364), including a `with-eval-after-load 'gptel` path.
3. Tangle; run tests.

## Design rationale
The demonstration MVP: an agent prepares layouts in the relevant workspace;
switching to them stays the user's action. The motivating repeated case is one
layout per open orchestrator ask (ask file beside its referenced files), created
when the ask is written and removed when it leaves the inbox — hence directory
addressing (the orchestrator knows its repo path, not the workspace name) and the
remove tool. The tool's arguments are the recipe, so
no translation layer exists and nothing is materialized at call time. Defining it as
a gptel tool means both Claude Code (via the write preset) and in-Emacs gptel agents
get it.

## Verification
- ./bin/tangle-org.sh config/workspaces/agent-tools.org
- ./bin/tangle-org.sh config/workspaces/workspaces.org
- ./bin/run-tests.sh -d config/workspaces

## Context
- design.md § Decisions 'D17. `workspace_add_layout` is the demonstration MVP'
- design.md § Decisions 'D19. Follow-up outside this repository: orchestrator ask layouts'
- specs/workspaces/spec.md § 'Agent tool for adding layouts'
