---
name: implement-emcp-baseline-introspection-ops
description: Thin built-in wrappers for apropos/describe/find-definition/get/set-variable, registered
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:implement-tool-spec-registry"
---

## Files to modify
- config/emacs-test-targets/target-support.org (modify) — add introspection/variable ops
- config/emacs-test-targets/test/unit/introspection-ops-spec.el (new) — Buttercup specs

## Implementation steps
1. Implement the emcp-derived baseline ops as THIN wrappers over built-in Emacs (no
   external dependency — this preserves the zero-dep posture; design.md D10):
   - `apropos` — symbol search by regex with optional `kind` filter
     (function/command/macro/variable/...). Use `apropos-internal` / `apropos`
     primitives; return matching symbol names. Read-only.
   - `describe` — documentation lookup for a symbol. Use `documentation` /
     `documentation-property` (functions and variables). Read-only.
   - `find-definition` — source location for a symbol via `find-function-noselect` /
     `find-variable-noselect` (or `xref-find-definitions` backend); return file + line.
     Read-only.
   - `get-variable` — read a global variable's value (`symbol-value` with `boundp`
     guard). Read-only.
   - `set-variable` — set a global variable's value (`set`/`set-default`). Mark
     `read-only` nil (state-modifying).
2. Register each via the registry mechanism from implement-tool-spec-registry, with a
   complete docstring, typed `args` schema, `category` (e.g. "introspection" /
   "variables"), and correct `read-only` flag.
3. `find-references` (needs `elisp-refs`) and `info-search` are OPTIONAL — design.md
   Open Questions flags them as "earns its keep?" decisions. Default: SKIP both to keep
   zero-dep; if implemented, `find-references` must degrade gracefully when `elisp-refs`
   is absent (do not add it as a dependency). Document whichever choice you make.
4. Write unit specs (`introspection-ops-spec.el`):
   - apropos returns known symbols for a pattern (e.g. matches `jf/agent-tools`);
   - describe returns non-empty documentation for a documented symbol;
   - get-variable reads, set-variable writes, and the set op is registered `read-only`
     nil while the query ops are `read-only` t.
5. Tangle + validate; `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup`.

## Design rationale
emcp (`martenlienen/emcp`) is adopted as PRIOR ART ONLY (design.md D10). Its operation
set is well-chosen and introspection-heavy — exactly the runtime-discovery primitives
this design wants — but its value is the *shape* (which ops, their schemas, the
read-only/confirm split), not its implementation, which is coupled to an MCP-over-HTTP
transport stack (`mcp-server-lib`, `http-server`, `elisp-refs`, `magit-section`) we
deliberately do not take on. The ops themselves are one-liners over built-in Emacs, so
reimplementing thin is cheaper than the dependency.

## Design pattern
Each op is a small `defun` returning a value serialisable over `emacsclient --eval`
(string / list / number), registered exactly like the control helpers. See
implement-tool-spec-registry for the registration shape.

## Verification
- `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup` passes the ops specs.
- Each `it` cites `specs/emacs-control-tools/spec.md § Emacs Introspection and Variable
  Operations`.
- No new dependency: `grep -n 'elisp-refs\|mcp-server-lib\|emcp\|magit-section'
  config/emacs-test-targets/target-support.el` shows none.

## Context
- design.md § Decisions 'D10. emcp's tool set is prior art, reimplemented thin — not a dependency'
- specs/emacs-control-tools/spec.md § 'Emacs Introspection and Variable Operations'
