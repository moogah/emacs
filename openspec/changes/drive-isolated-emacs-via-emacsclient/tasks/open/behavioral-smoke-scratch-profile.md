---
name: behavioral-smoke-scratch-profile
description: "Guarded smoke: full-config target with scratch vault; call/call-write org-graph stays in scratch"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:emacs-agent-wrapper-targets"
  - "blocked-by:agent-presets"
---

## Files to modify
- config/emacs-agent/test/behavioral/scratch-profile-smoke-spec.el (new)

## Implementation steps
1. Skip when unavailable (full-config start is slow; mark accordingly).
2. Create a temp vault; start `--worktree . --data scratch:<tmp>`; assert
   `org-roam-directory` and org-graph roots are under `<tmp>`; `call-write
   org_graph_write_node` creates a file under `<tmp>`; `call org_graph_query` finds
   it; `call org_graph_write_node` (wrong tier) is refused; assert no file under the
   real `~/org/` changed (compare mtimes of the roam dir before/after); stop.

## Design rationale
Proves the data-profile isolation and the tier enforcement end to end, against
the very feature (org-graph) the scratch profile exists to develop safely.

## Verification
- ./bin/run-tests.sh -d config/emacs-agent/test/behavioral

## Context
- design.md § Decisions 'D12', 'D15'
- specs/isolated-emacs-targets/spec.md § 'Data Profiles'
- specs/agent-target-policy/spec.md § 'Permission Tiers'
