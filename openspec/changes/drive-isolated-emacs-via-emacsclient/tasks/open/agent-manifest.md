---
name: agent-manifest
description: "Manifest: JSON list of preset-allowlisted tools with tier and typed args"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-dispatcher"
  - "enables:emacs-agent-wrapper-live"
---

## Files to modify
- config/emacs-agent/manifest.org (new)
- config/emacs-agent/test/unit/manifest-spec.el (new)

## Implementation steps
1. Specs first:
   - `(jf/agent-api)` returns JSON `[{name, category, tier, description, args:[{name,
     type, optional, description}]}]` for exactly the preset tools present in the
     registry;
   - adding a stub tool to the read preset makes it appear with tier `read`;
   - with gptel absent (simulate by unbinding), returns `[]` plus no error.
2. Implement over `jf/agent-preset-tool-names`, `jf/agent-tool-tier` and
   `jf/agent--registry-lookup`; group by registry alist key, not
   `gptel-tool-category` (nil for uncategorized tools).

## Design rationale
The manifest replaces MCP's tool list but is generated from the live presets and
registry, so it cannot drift. Per-tool drill-down uses the introspection tools.

## Verification
- ./bin/tangle-org.sh config/emacs-agent/manifest.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D9. Discoverability — layered, generated from the live registry, minimal static seed'
- specs/control-surface-discoverability/spec.md § 'Self-Describing Manifest'
