---
name: implement-tool-spec-registry
description: Frontend-agnostic jf/agent tool-spec registry with typed-arg metadata
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:scaffold-target-support-module"
---

## Files to modify
- config/emacs-test-targets/target-support.org (modify) — add the registry mechanism
- config/emacs-test-targets/test/unit/registry-spec.el (new) — Buttercup specs

## Implementation steps
1. Decide the registration mechanism (design.md Open Questions): either a
   `jf/agent-deftool` macro or a plain `jf/agent-register-tool` function backed by an
   alist/hash. Either is fine; it MUST capture, per operation:
   - `name` (string — the stable operation name, e.g. `"apropos"`),
   - `function` (the implementing symbol/lambda),
   - `doc` (human-facing docstring),
   - `args` — a *typed* arg schema: a list of `(:name <str> :type <symbol> :description <str>)`
     entries (mirror gptel's `:args` shape so a future adapter maps 1:1),
   - `category` (string),
   - `read-only` (boolean — t for query ops, nil for state-modifying).
2. Store entries in a single registry (e.g. `jf/agent--tools`, a hash keyed by name).
   Re-registering the same name SHALL replace, not duplicate (idempotent reload).
3. Provide frontend-agnostic accessors the other surfaces read:
   - `jf/agent-tools` → the list of registered specs,
   - `jf/agent-tool` (name) → one spec,
   so neither the manifest nor a gptel adapter needs to touch internals.
4. The registry and accessors MUST reference no emacsclient-, MCP-, or gptel-specific
   code (frontend-agnostic invariant).
5. Write unit specs (`registry-spec.el`):
   - registering an op makes it retrievable with all six metadata fields intact;
   - re-registering the same name replaces rather than duplicates;
   - a spec asserting the registry/accessors carry the typed-arg schema, category, and
     read-only flag needed by an external frontend (the gptel seam) — i.e. the metadata
     is sufficient to build a `gptel-make-tool` call *without* importing gptel.
6. Tangle + validate; `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup`.

## Design rationale
A single tool-spec registry is the linchpin of the discoverability design: it is the
one source of truth that drives three surfaces — raw `--eval` (L0), the `jf/agent-api`
JSON manifest (L2), and a future gptel `gptel-make-tool` adapter (D11). The typed-arg
metadata is the reason a registry is needed rather than relying on docstrings/arglists:
a plain Elisp arglist cannot express the `:type` information gptel requires, so the
registry carries it and every consumer reads it. Keeping the registry frontend-agnostic
is what lets the gptel adapter (a planned sibling change) be built with no core changes.

## Design pattern
Mirror gptel's declarative tool shape (`:name`/`:function`/`:description`/`:args`/
`:category`) so the future adapter is a near-mechanical map. See
`config/gptel/tools/filesystem-tools.org` for the `gptel-make-tool` arg-spec shape the
`args` schema must be convertible to.

## Verification
- `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup` passes registry specs.
- Each `it` cites `specs/control-surface-discoverability/spec.md § Curated Agent Control
  Registry` or `§ Frontend-Agnostic Tool Metadata`.
- Registry code greps clean of `gptel`/`emacsclient`/`mcp` references:
  `grep -n 'gptel\|emacsclient\|mcp' config/emacs-test-targets/target-support.el` shows
  none in the registry section.

## Context
- design.md § Decisions 'D9. Discoverability — layered, self-describing, minimal static seed' (L1)
- design.md § Decisions 'D11. One frontend-agnostic core; gptel adapter is a sibling change'
- specs/control-surface-discoverability/spec.md § 'Curated Agent Control Registry'
- specs/control-surface-discoverability/spec.md § 'Frontend-Agnostic Tool Metadata'
