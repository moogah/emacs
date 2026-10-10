---
name: agent-dispatcher
description: "Dispatcher: resolve tool, enforce tier, map JSON args as gptel does, return JSON envelope"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-presets"
  - "enables:agent-manifest"
  - "enables:agent-action-log"
---

## Files to modify
- config/emacs-agent/dispatcher.org (new)
- config/emacs-agent/test/unit/dispatcher-spec.el (new)

## Implementation steps
1. Specs first, with stub tools from helpers-spec.el:
   - `(jf/agent-call "call" NAME JSON)` on a read tool runs it and returns
     `{"ok":true,"result":"..."}`;
   - `call` on a write tool → `{"ok":false,"error":"... requires call-write"}`, tool not run;
   - any tier on a tool in neither preset → refused, not run; preset tool missing from
     registry → error naming it;
   - argument mapping: declared `(a b c)` + `{"c":3,"a":1}` → called with `1 nil 3`;
     arrays → vectors; JSON false → `:json-false`; null → nil;
   - tool error → `{"ok":false,"error":msg}` and the next call succeeds;
   - async tool (`gptel-tool-async` non-nil) → error "asynchronous tools are not
     supported yet", nothing blocks.
2. Implement:
   - `jf/agent--registry-lookup NAME` — the ONLY function touching
     `gptel--known-tools` / `gptel-get-tool`;
   - JSON decode with `json-parse-string ... :object-type 'plist :null-object nil
     :false-object :json-false`; positional mapping copied from
     `gptel--handle-tool-use` (runtime/straight/repos/gptel/gptel-request.el ~1726–1731);
   - result stringification like `gptel--to-string`; envelope via `json-serialize`.
3. Leave a clearly named hook point for the action log (next task).

## Design rationale
Invoking tools exactly as gptel does means a tool behaves identically whether called
by an in-Emacs gptel session or by Claude Code. Tier enforcement inside Emacs is
defense in depth beneath the wrapper's subcommands. Confining registry access to one
function contains the deliberate coupling to gptel internals.

## Verification
- ./bin/tangle-org.sh config/emacs-agent/dispatcher.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D14. The dispatcher invokes tools the way gptel does; async uses request + poll'
- specs/control-surface-discoverability/spec.md § 'Dispatcher Invokes Tools as gptel Does'
- specs/agent-target-policy/spec.md § 'Permission Tiers', § 'Asynchronous Tool Results'
