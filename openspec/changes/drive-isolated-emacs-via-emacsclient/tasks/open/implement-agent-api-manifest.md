---
name: implement-agent-api-manifest
description: Self-describing (jf/agent-api) JSON manifest + per-operation drill-down
change: drive-isolated-emacs-via-emacsclient
status: blocked
relations:
  - "blocked-by:implement-tool-spec-registry"
---

## Files to modify
- config/emacs-test-targets/target-support.org (modify) — add jf/agent-api + drill-down
- config/emacs-test-targets/test/unit/agent-api-spec.el (new) — Buttercup specs

## Implementation steps
1. Implement `jf/agent-api` — returns, as a JSON string (`json-serialize`), a list of
   the registry entries, each with at least `name`, the typed `args` schema, `doc`,
   `category`, and `read-only`. Generate it AT CALL TIME from the live registry
   (`jf/agent-tools`) so it can never drift from the implemented operations.
2. Register `jf/agent-api` itself as a registry operation (category "meta", read-only t)
   so it appears in its own listing (self-describing invariant).
3. Implement per-operation drill-down so a client can pull detail for ONE op without
   materialising the whole manifest. Standard `documentation` / `describe-function`
   suffices; optionally add a thin `jf/agent-describe` (name) wrapper that returns one
   entry's doc + args.
4. Ensure JSON is well-formed for emacsclient consumption (the result prints cleanly
   through `emacsclient --eval`; mind `json-serialize` keyword/string handling and any
   non-ASCII in docstrings).
5. Write unit specs (`agent-api-spec.el`):
   - `jf/agent-api` returns parseable JSON whose entries match `jf/agent-tools`
     (name/args/doc/category/read-only present);
   - the manifest reflects the LIVE registry: register a throwaway op in a `let`/spy and
     assert it appears; this is the "cannot drift" guarantee;
   - `jf/agent-api` appears in its own output (self-listing);
   - drill-down returns one op's doc without requiring the full manifest.
6. Tangle + validate; `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup`.

## Design rationale
This is the L2 surface — the `tools/list` analogue that replaces MCP's static tool
manifest. The decisive advantage over MCP is that the manifest is *generated from live
code* (the registry), so it is always in sync; MCP ships a separately-maintained,
drift-prone list. Per-op drill-down lets the client pull only what it needs into context
instead of front-loading every operation (design.md D9 / L2).

## Design pattern
`(json-serialize (vconcat (mapcar #'jf/agent--tool->plist (jf/agent-tools))))` style —
map each registry spec to a plist/hash with string keys, then serialise. Keep the
mapping in one helper so the gptel adapter (sibling change) can reuse the same
spec→external-shape transform conceptually.

## Verification
- `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup` passes agent-api specs.
- Manual: `emacsclient -s <name> --eval '(jf/agent-api)'` against a target prints a JSON
  list of operations (covered end-to-end by the real-daemon smoke; see
  behavioral-smoke-real-daemon).
- Each `it` cites `specs/control-surface-discoverability/spec.md § Self-Describing API
  Manifest` or `§ Per-Operation Detail Drill-Down`.

## Context
- design.md § Decisions 'D9' (L2)
- specs/control-surface-discoverability/spec.md § 'Self-Describing API Manifest'
- specs/control-surface-discoverability/spec.md § 'Per-Operation Detail Drill-Down'
