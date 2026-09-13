---
name: og-parser-hardening
description: Advise vulpea's parser to never evaluate file-local variables and contain per-file sync errors so indexing cannot enter the debugger.
change: workspace-notes-in-vault
status: ready
task_class: bug
on_critical_path: false
cycle: cycle-1789334430
relations: []
cites_register_entries:
  - register/invariant/index-parser-never-evaluates-file-locals
  - register/invariant/index-sync-per-file-errors-contained
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
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
- Read `runtime/straight/repos/vulpea/vulpea-db.el` (and the sync file) and quote the two advised function signatures in your report; if names differ from the task, record a Discovery against the cited invariant entries.
- One un-stubbed scenario: a temp vault file with a `Local Variables` block containing `eval: (org-graph-test--must-not-run)`; run the real advised parse path; expect the function never called (stub-bounded regression pattern).

## Context
design.md § D6 (Parser hardening bullet); § Risks (advice on vulpea internals)
specs/org-graph/spec.md § 'Index parser hardening'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/invariant/index-parser-never-evaluates-file-locals` — NEW, speculated. The entry names `vulpea-db--parse-with-temp-buffer` from the design; READ the checked-out vulpea source (`runtime/straight/repos/vulpea/`, cloned on first boot) and confirm the function name, that it re-runs `org-mode`, and that `enable-local-variables` is the binding that matters. If the seam differs, re-state via Discoveries.
- `register/invariant/index-sync-per-file-errors-contained` — NEW, speculated. Same instruction for `vulpea-db-sync--update-file-if-changed`. Confirm it is the per-file unit of the sync queue so containment does not swallow a whole batch.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (og-vault-only-discovery (runs in parallel; do not edit its sections of discovery.org)).

## Observations

(empty — implementor appends during execution)

## Discoveries

(empty — implementor appends structured discoveries; see task-body template)
