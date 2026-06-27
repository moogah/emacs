---
name: test-helpers
description: Build the shared Buttercup test helpers (AST builder, vulpea stub macro, schema/extractor stubs, note fixture) that the parser, finder, query, schema, and coordinator specs all consume.
change: org-graph-spike
status: done
relations:
  - blocked-by:scaffold-module
---

## Files to modify
- `config/org-graph/test/helpers-spec.el` (new) — shared fixtures and macros

## Implementation steps
1. `org-graph-test/build-tree` — build an `org-element` AST (or the parsed
   structure the typed-edge parser consumes) from a plist spec describing a
   note: its `:id`, a `:properties` alist (including typed-edge props like
   `IMPLEMENTS`), and `:filetags`. No file I/O, no live org-mode.
2. `org-graph-test/with-stubbed-vulpea` — a macro that wraps common `cl-letf`
   stubs (function-scoped, NOT global) for the vulpea API surface the specs
   touch: `vulpea-db-query`, `vulpea-db-query-links`, `vulpea-db-query-links-from`,
   `vulpea-db-query-links-to`, and the extractor/schema entry points
   (`vulpea-db-register-extractor`, `vulpea-schema-define`,
   `vulpea-schema-validate`). Accept fixture return values as arguments.
3. `org-graph-test/note-fixture` — construct a `vulpea-note`-shaped value (id,
   title, tags, properties, path) for query and finder tests.
4. `org-graph-test/link-plist` — build a vulpea link plist
   `(:source :dest :type :pos :description)` for query tests.
5. Keep helpers framework-pure: they construct data and install scoped mocks,
   they do not assert.

## Design rationale
The codebase's behavioral-test convention is function-scoped mocks via
`cl-letf` (CLAUDE.md § Testing — Behavioral), never global state. Centralising
the vulpea stubs here means the parser/query/finder/schema specs never spin up
a real SQLite DB and stay deterministic. The pure-parser design (D4) depends
on being able to feed synthetic ASTs without org-mode state.

## Design pattern
Mirror existing `helpers-spec.el` files under `config/gptel/scope/test/` and
`config/gptel/tools/test/`. Buttercup `*-spec.el` suffix (preferred for new
tests, CLAUDE.md). Stub example:
```elisp
(cl-letf (((symbol-function 'vulpea-db-query)
           (lambda (&rest _) <fixture rows>)))
  ...)
```

## Verification
- `./bin/run-tests.sh -d config/org-graph` loads `helpers-spec.el` without
  error (even before other specs exist).
- A throwaway `(describe ...)` using each helper passes, proving the helpers
  produce the shapes downstream specs expect.

## Context
design.md § Testing Approach (Test Patterns, Shared helpers);
architecture.md § Testing Approach; CLAUDE.md § Testing Infrastructure.
</content>
