---
name: add-broker-launcher-and-client-docs
description: Add the dedicated broker-daemon launcher script and document external client MCP configuration
change: connect-external-llm-clients-mcp-broker
status: ready
relations:
  - "blocked-by:implement-control-and-observation-tools"
---

## Files to modify
- bin/mcp-broker.sh (new)
- config/mcp-broker/mcp-broker.org (modify) — add a "Connecting a client" doc section

## Implementation steps
1. Write `bin/mcp-broker.sh` to launch the DEDICATED broker daemon: a headless
   `emacs -Q --daemon=mcp-broker` (its own isolated invocation — not the user's live
   Emacs) that loads the broker module (`config/mcp-broker/mcp-broker.el` and its
   deps) and calls `jf/mcp-broker-start`. Reuse the Emacs detection logic pattern from
   `bin/emacs-isolated.sh`.
2. Bind the endpoint to loopback only. Resolve the port question (design open
   question): pick a default port with an override flag/env, and on startup PRINT the
   resolved MCP URL (and optionally write it to a known runtime file) so the client can
   be configured.
3. Add a stop path (e.g. `bin/mcp-broker.sh --stop` → `emacsclient -s mcp-broker
   --eval '(jf/mcp-broker-stop)'` then kill the daemon), ensuring targets are disposed
   on shutdown.
4. In `mcp-broker.org`, add a "Connecting a client" section documenting: how to start
   the broker, the loopback URL, an example MCP client config (e.g. Claude Code) using
   the HTTP/SSE transport, and the trust caveat — loopback-only, trusted clients only,
   NOT a filesystem sandbox.
5. Tangle/validate the org change.

## Design rationale
The broker is a dedicated headless daemon, never the user's live Emacs (D1) — so it
gets its own launcher rather than being loaded by `init`. Loopback-only binding plus
explicit trusted-clients-only documentation is the operational half of the permissive
trust posture: permissive eval is safe against the live session but is not a
filesystem sandbox, so exposure must be constrained at launch.

## Verification
- `./bin/mcp-broker.sh` starts the daemon and prints a loopback MCP URL; an MCP client
  pointed at that URL can list tools and complete a start→eval→frame-state→dispose
  cycle.
- `./bin/mcp-broker.sh --stop` disposes targets and stops the daemon cleanly (no
  orphan target daemons remain).
- The "Connecting a client" doc section is present and states the loopback/trust
  caveats.

## Context
design.md § Decisions 'D1. Two tiers of Emacs: broker daemon + target daemons' and
§ Migration Plan; specs/mcp-broker/spec.md (loopback endpoint).
