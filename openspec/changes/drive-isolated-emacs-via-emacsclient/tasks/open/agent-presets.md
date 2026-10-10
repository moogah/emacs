---
name: agent-presets
description: "Register emacs-agent-read / emacs-agent-write presets as the tool allowlists"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-introspection-tools"
  - "enables:agent-dispatcher"
  - "enables:behavioral-smoke-scratch-profile"
---

## Files to modify
- config/emacs-agent/presets.org (new)
- config/emacs-agent/test/unit/presets-spec.el (new)

## Implementation steps
1. Specs first:
   - both presets exist after load (`gptel-get-preset`);
   - `jf/agent-preset-tool-names 'emacs-agent-read` resolves to the expected names via
     `jf/gptel-scope-profile--resolve-tool-names` (config/gptel/scope-profiles.org);
   - read preset: org_graph_query, org_graph_typed_edges, search_roam_nodes,
     list_roam_nodes, read_roam_node, get_roam_node_metadata, query_roam_backlinks, and
     the introspection tool names; write preset: org_graph_write_node,
     workspace_add_layout, workspace_remove_layout;
   - neither preset contains scope tools (category "scope"), `ask_user_questions`,
     `elisp_eval`, or the target-support control helpers.
2. Register via `jf/gptel-preset-register` (config/gptel/presets/registration.org) with
   only `:tools` (and a short `:description`); decide visibility in gptel's preset
   menu per design Open Questions (default: visible).
3. Provide `jf/agent-preset-tool-names` and `jf/agent-tool-tier NAME` → `read` /
   `write` / nil.

## Design rationale
Presets are the existing mechanism for a curated tool list (e.g. system-explorer is
a read-only allowlist). Two presets give the read/write split with no new metadata
on tools — gptel has no read-only flag and `:confirm` is not reliable — and map one
to one onto the `call` / `call-write` tiers. The same presets can drive an in-Emacs
gptel session.

## Verification
- ./bin/tangle-org.sh config/emacs-agent/presets.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D11. gptel's tool registry is the registry; presets are the allowlists'
- specs/control-surface-discoverability/spec.md § 'Presets as Allowlists'
