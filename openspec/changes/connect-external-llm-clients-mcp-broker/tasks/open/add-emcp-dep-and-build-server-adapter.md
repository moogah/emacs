---
name: add-emcp-dep-and-build-server-adapter
description: Vendor emcp via straight.el (pinned) and add the single adapter that builds an MCP server from our own tool list
change: connect-external-llm-clients-mcp-broker
status: ready
relations: []
---

## Files to modify
- config/mcp-broker/mcp-broker.org (new) — module loader + straight(emcp) declaration
- config/mcp-broker/broker-core.org (new) — `jf/mcp-broker--build-server` adapter (scaffold only here)

## Implementation steps
1. Create `config/mcp-broker/mcp-broker.org` as the literate loader with the required
   headers (`#+title`, `#+property: header-args:emacs-lisp :tangle mcp-broker.el`,
   `#+auto_tangle: y`). First babel block uses `:comments no` for the lexical-binding
   header line (see project memory on mode-line position).
2. Declare emcp as a straight.el dependency, PINNED to a specific commit:
   `(use-package emcp :straight (emcp :type git :host codeberg :repo "martenlienen/emcp" :commit "<PIN>"))`.
   Record the chosen commit hash in the org prose. Note transitive deps pulled:
   `http-server`, `elisp-refs`, `magit-section`, and the Emacs 30.1 floor.
3. Create `config/mcp-broker/broker-core.org` with a single adapter function
   `jf/mcp-broker--build-server` that wraps emcp's internal `emcp--server-build`
   (`:name`, `:tools`, optionally `:prompts`/`:resources`). This is the ONLY place
   allowed to reference `emcp--server-build` — all other modules call the adapter.
   Leave the tool list a parameter; do not define tools here.
4. Add a short defgroup/defcustom block for broker settings (loopback host, port) so
   later tasks can use them. Do not start a server here.
5. Tangle and validate: `./bin/tangle-org.sh config/mcp-broker/mcp-broker.org` and
   `./bin/tangle-org.sh config/mcp-broker/broker-core.org`.
6. Add a unit spec asserting the adapter calls `emcp--server-build` with the tools it
   is given (mock `emcp--server-build` via `cl-letf`), in
   `config/mcp-broker/test/unit/broker-core-spec.el`.

## Design rationale
Extending emcp (rather than building our own MCP transport) is decided: emcp is the
only Elisp MCP server tracking the current spec (2025-11-25), and protocol compliance
is the expensive, non-differentiating work. The lone coupling to emcp's double-dash
internal `emcp--server-build` is deliberately isolated behind one adapter so an
upstream API change hits exactly one function. Pinning the commit contains the
young-single-author dependency risk. This module is NOT added to `jf/enabled-modules`
— only the dedicated broker daemon loads it (a later task).

## Verification
- `./bin/tangle-org.sh config/mcp-broker/mcp-broker.org` and `broker-core.org` both
  succeed (check-parens passes).
- `grep -n "emcp--server-build" config/mcp-broker/*.el` shows the symbol ONLY inside
  `jf/mcp-broker--build-server`.
- `./bin/run-tests.sh -d config/mcp-broker` runs the adapter spec green.

## Context
design.md § Decisions 'D2. Extend emcp as a vendored transport; isolate the one
coupling' and 'D8. Module layout (literate)'.
