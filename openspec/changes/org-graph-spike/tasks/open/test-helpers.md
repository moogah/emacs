---
name: test-helpers
description: Build the shared Buttercup test helpers (AST builder, vulpea stub macro, note fixture) that the parser, finder, query, and coordinator specs all consume.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:scaffold-module"
---

## Files to modify

- `config/org-graph/test/helpers-spec.el` (new) — Buttercup file with helper macros and defuns. Despite the `-spec` suffix, this file does not contain `describe` blocks — it provides infrastructure that other specs require.

## Implementation steps

1. Create `config/org-graph/test/helpers-spec.el` with a lexical-binding header. (This is a hand-written `.el` file, not tangled — Buttercup helpers don't need org-mode wrappers.)

2. Implement `org-graph-test/build-tree` — accepts a plist describing a note (`:id`, `:title`, `:filetags`, `:properties` alist) and returns an org-element AST suitable for passing into the pure parser. Use `org-element-create` and friends; do not write to a file.

3. Implement the macro `org-graph-test/with-stubbed-vulpea`:
   ```elisp
   (defmacro org-graph-test/with-stubbed-vulpea (rows &rest body)
     "Run BODY with vulpea-db-query stubbed to return ROWS, and
      vulpea-db-insert / vulpea-db-get-by-id stubbed to no-ops."
     (declare (indent 1) (debug t))
     `(cl-letf (((symbol-function 'vulpea-db-query)
                 (lambda (&rest _) ,rows))
                ((symbol-function 'vulpea-db-insert)
                 (lambda (&rest _) nil))
                ((symbol-function 'vulpea-db-get-by-id)
                 (lambda (id) (cl-find id ,rows :key (lambda (n) (plist-get n :id)) :test #'equal))))
        ,@body))
   ```

4. Implement `org-graph-test/note-fixture` — returns a plist shaped like a `vulpea-note` with reasonable defaults, accepts overrides as keyword args. Used as fixture data for query tests.

5. Add a top-of-file `(require 'cl-lib)` and any other primitives helpers depend on. Do NOT require `vulpea` or `org-node` here — that would force the runtime deps on every test invocation.

## Design rationale

Function-scoped `cl-letf` mocks are the codebase's behavioral-test convention (CLAUDE.md §Test levels: "Mocks are scoped to the function-under-test via cl-letf, not global"). Routing all vulpea API stubs through one macro keeps every spec consistent and makes it easy to extend the stub surface when the query API grows. The `build-tree` helper is the linchpin of the pure-parser test strategy (design.md §D4) — without it we'd be writing `.org` strings and re-parsing them, which is both slow and fragile.

## Verification

- `./bin/run-tests.sh -d config/org-graph/test -f buttercup` — exits 0 even though no `describe` blocks exist (helpers-only file is valid).
- `grep -n "defmacro org-graph-test/with-stubbed-vulpea" config/org-graph/test/helpers-spec.el` — matches.
- `grep -n "defun org-graph-test/build-tree" config/org-graph/test/helpers-spec.el` — matches.
- `grep -n "defun org-graph-test/note-fixture" config/org-graph/test/helpers-spec.el` — matches.

## Context

- architecture.md §Testing Approach §Test Patterns
- architecture.md §Testing Approach §Shared helpers
- design.md §D4
