---
name: og-parser-hardening
description: Advise vulpea's parser to never evaluate file-local variables and contain per-file sync errors so indexing cannot enter the debugger.
change: workspace-notes-in-vault
status: ready
relations: []
---

## Files to modify
- config/org-graph/discovery.org (modify; tangle — new "Parser hardening" section)
- config/org-graph/test/discovery-spec.el (modify)

## Implementation steps
1. Add `org-graph--vulpea-parse-safely (orig &rest args)` and install it
   `:around` `vulpea-db--parse-with-temp-buffer` (declare-function it;
   install with `advice-add` at load, idempotent). The advice binds
   `enable-local-variables nil` and `enable-dir-local-variables nil` around
   `(apply orig args)`. vulpea re-runs `org-mode` with hooks in that function
   when `vulpea-db-parse-method` is `temp-buffer`, which is what triggered
   `hack-local-variables`.
2. Add `org-graph--vulpea-update-safely (orig path &rest args)` `:around`
   `vulpea-db-sync--update-file-if-changed`: `condition-case` the call; on
   error log `(path . err)` via `message` and to a `*org-graph*` buffer
   (create a tiny `org-graph--log` helper if none exists), return nil. The
   queue processor then continues with the next file.
3. Prose: explain the two hazards (untrusted `eval:` locals in third-party
   org files; timer callbacks without a guard) and why advice rather than a
   vulpea fork (two stable functions in the pinned 2.4.0).
4. Specs: with a stub `vulpea-db--parse-with-temp-buffer` that records the
   value of `enable-local-variables`, the advised call sees nil; with a stub
   update function that signals, the advised wrapper returns nil and the
   error text appears in the log buffer; both advices are present
   (`advice-member-p`) after module load.

## Design rationale
The failure that started this change: vulpea parsed
`.../runtime/straight/repos/elisp-tree-sitter/doc/ox-hugo/doc/github-files.org`,
ran its `eval: (org-hugo-auto-export-mode -1)` local variable, and dropped
into the debugger from a timer. Vault-only discovery removes that
particular file, but vault files can carry `eval:` locals too; an indexer
must never evaluate them. Defense in depth, independent of roots.

## Design pattern
`advice-add` with named, idempotent installers in the same module that
configures sync (discovery.org already owns the vulpea configuration
surface). Follow the `declare-function` soft-dependency block at the top of
`discovery.org`.

## Verification
- `./bin/tangle-org.sh config/org-graph/discovery.org` validates.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -n 'enable-local-variables\|advice-add' config/org-graph/discovery.el` → both advices installed.

## Context
design.md § D6 (Parser hardening bullet); § Risks (advice on vulpea internals)
specs/org-graph/spec.md § 'Index parser hardening'
