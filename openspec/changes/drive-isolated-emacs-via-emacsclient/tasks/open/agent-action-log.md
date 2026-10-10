---
name: agent-action-log
description: "Live-session action log and no-disruption guarantee for dispatched calls"
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:agent-dispatcher"
  - "enables:emacs-agent-wrapper-live"
---

## Files to modify
- config/emacs-agent/action-log.org (new)
- config/emacs-agent/dispatcher.org (modify) — call the log; wrap calls in frame/window/tab preservation checks
- config/emacs-agent/test/unit/action-log-spec.el (new)

## Implementation steps
1. Specs first:
   - when `server-name` is `"server"` (the live session), each dispatched call appends
     an entry (time, tier, tool, truncated args summary, ok/error) to
     `*emacs-agent-log*`; when it is anything else, nothing is logged;
   - a dispatched preset tool leaves `selected-frame`, `selected-window`,
     `current-window-configuration` (compare with `compare-window-configurations`) and
     the current tab unchanged — assert across the stub tools and, in a behavioral spec,
     across `workspace_add_layout`;
   - the log buffer is read-only and bounded (oldest entries trimmed).
2. Implement `jf/agent-log-call`; call it from the dispatcher's hook point; wrap the
   dispatch in `save-selected-window`-style protection only as a safety net — tools are
   expected not to disturb state on their own.
3. Also log `eval` calls when the wrapper routes them through `jf/agent-eval` (see
   wrapper task).

## Design rationale
The live session is the user's stable environment; the user must be able to see
everything the agent did there, and calls must not move frames, windows or tabs.

## Verification
- ./bin/tangle-org.sh config/emacs-agent/action-log.org
- ./bin/tangle-org.sh config/emacs-agent/dispatcher.org
- ./bin/run-tests.sh -d config/emacs-agent

## Context
- design.md § Decisions 'D12. Target policy' (Live-session safeguards)
- specs/agent-target-policy/spec.md § 'Live-Session Safeguards'
