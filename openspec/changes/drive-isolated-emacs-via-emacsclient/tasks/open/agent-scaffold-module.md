---
name: agent-scaffold-module
description: "Scaffold the literate emacs-agent module, loader, enabled-modules entry and test layout"
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "enables:agent-introspection-tools"
  - "enables:target-support-frame-state"
---

## Files to modify
- config/emacs-agent/emacs-agent.org (new) — loader
- init.org (modify) — add to `jf/enabled-modules` after gptel, org-graph and workspaces
- config/emacs-agent/test/helpers-spec.el (new)
- config/emacs-agent/test/unit/.gitkeep, config/emacs-agent/test/behavioral/.gitkeep (new)

## Implementation steps
1. Create `config/emacs-agent/emacs-agent.org` with the required literate headers
   (`#+title`, `#+property: header-args:emacs-lisp :tangle emacs-agent.el`,
   `#+auto_tangle: y`) that loads the submodules created by later tasks
   (dispatcher, presets, manifest, introspection-tools, action-log, target-support)
   via `jf/load-module`-style relative loads, tolerating not-yet-present files only
   during scaffolding (remove tolerance once all exist).
2. Add `("emacs-agent/emacs-agent" "...")` to `jf/enabled-modules` in `init.org`,
   ordered after `gptel`, `workspaces/workspaces`, `org-graph/org-graph`.
3. Add `helpers-spec.el` with shared fixtures: a stub `gptel-tool` builder
   (`cl-defstruct`-compatible via `gptel--make-tool-internal` when available, else a
   local stub) and a temporary `gptel--known-tools` binding macro.
4. Tangle; confirm `./bin/run-tests.sh -d config/emacs-agent` runs (zero specs OK).
(module path `config/emacs-agent/` is the working name — design.md Open Questions; rename consistently if the name changes)

## Design rationale
All agent-surface code lives in one literate module so the dispatcher, presets,
manifest, introspection tools, action log and target-support helpers share a test
layout (unit/ + behavioral/ + helpers-spec.el, mirroring the scope test layout).

## Verification
- ./bin/tangle-org.sh config/emacs-agent/emacs-agent.org
- ./bin/tangle-org.sh init.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D8. Module layout (literate)'
- design.md § Decisions 'D7. Testing approach'
