---
name: og-parser-hardening
description: Advise vulpea's parser to never evaluate file-local variables and contain per-file sync errors so indexing cannot enter the debugger.
change: workspace-notes-in-vault
status: needs_review
merge_commit: cbea6143
merged_at: 2026-09-13T23:20:00Z
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

- Seam deviation (parse guard): advised `vulpea-db--parse-file (path)` — the
  single dispatcher `vulpea-db-update-file` calls (vulpea-db-extract.el:975) —
  rather than `vulpea-db--parse-with-temp-buffer (path rerun-org-mode)`. The
  named function exists in v2.4.0 (vulpea-db-extract.el:298) and does re-run
  `(org-mode)` with `delay-mode-hooks nil` after setting `buffer-file-name`
  (line 327), which is what makes `run-mode-hooks` call `hack-local-variables`.
  But it is only the `temp-buffer` / `single-temp-buffer` branch; the
  dispatcher's `find-file` branch (`find-file-noselect`, forced for
  `vulpea-db-extra-extensions` such as `.org.gpg`) runs `hack-local-variables`
  too and also honours `.dir-locals.el`. Binding around the dispatcher makes the
  invariant independent of `vulpea-db-parse-method`; the dispatcher is DB-free
  (parse completes before `(vulpea-db)` is opened), so the un-stubbed spec can
  drive it directly.
- The task's Verification names `runtime/straight/repos/vulpea/vulpea-db.el`;
  the parser lives in `vulpea-db-extract.el`. The sync unit
  `vulpea-db-sync--update-file-if-changed` is a `cl-defun (path &optional
  hash-cache)` in `vulpea-db-sync.el:722`, called once per path from the
  queue processor (`vulpea-db-sync--process-queue`, line 665, timer-driven) and
  from `vulpea-db-sync-full-scan` (line 952) — confirmed as the per-file unit.
- vulpea 2.4.0 already wraps both of those per-path calls in a plain
  `condition-case` that `message`s "Vulpea: Error updating %s: %s" and
  continues. The containment advice is therefore partly redundant with the
  pinned version; its marginal value is the `*org-graph*` log line and
  independence from vulpea's callers (a future version dropping the handler,
  or a direct caller). Documented in the section prose.
- Debugger precision (why the incident reached the debugger despite vulpea's
  handler): `run-mode-hooks` wraps `hack-local-variables` in
  `with-demoted-errors` (= `condition-case-unless-debug`), whose `debug`
  condition enters the debugger at signal time when `debug-on-error` is t,
  before any outer handler unwinds. No outer `condition-case` — ours or
  vulpea's — can intercept that frame; only removing the work (the parse-side
  binding) closes it. The containment guard still keeps the debugger out for
  errors that reach it unhandled. Recorded as a Discovery against the
  containment entry because its statement claims more than a `condition-case`
  can deliver.
- Batch Emacs with `enable-local-variables` t silently declines an unsafe
  `eval:` (the confirm path returns nil non-interactively), so a behavioural
  spec under `t` would pass with or without the advice. The un-stubbed spec
  binds the global to `:all` (the one value under which batch does evaluate)
  and a control spec calls the un-advised inner parser on the same fixture to
  prove it fires (calls = 1) — the stub-bounded regression pattern the task
  asked for. `org-mode-hook` is let-bound to nil in both for hermeticity;
  `hack-local-variables` runs from `run-mode-hooks` regardless of hooks.
- Test-pattern note for future specs: `cl-letf` on `(symbol-function
  'advised-fn)` replaces the advised wrapper wholesale, so the advice does not
  run. To go "through the advice" stub what the original calls instead (here:
  the inner method function for the parse seam; `vulpea-db-update-file` for the
  sync seam).
- Merge hygiene with `og-vault-only-discovery`: the two `declare-function`
  forms live inside the new "Parser hardening" subtree rather than in the
  shared soft-dependency block at the top of discovery.org, and the file
  header prose was left untouched. The sibling that rewrites that header /
  declarations block may fold them in afterwards.
- `org-graph--log` / `org-graph--log-buffer-name` are new and minimal, defined
  in discovery.org because this is the module's first logging need. If another
  module grows a logging need they belong in the loader (`org-graph.org`),
  which currently uses `display-warning` for startup-time failures.
- Behavioural consequence worth knowing: on a contained error the wrapper
  returns nil, the caller counts the file as "unchanged", and the file's
  stored hash is NOT updated — so the file is retried on the next scan rather
  than permanently skipped. Each retry logs again; a vault with many broken
  files would repeat the echo (vulpea's own per-file `message` already does).
- `runtime/straight/build/vulpea/` is byte-identical to `repos/vulpea/` for
  both advised files (checked with `diff -q`); the pinned tag is v2.4.0
  (`git describe` in the repo checkout).

## Discoveries

- discovery_id: disc-og-parser-hardening-1
  class: deviation
  description: |
    The entry (and design D6 / task body) name
    `vulpea-db--parse-with-temp-buffer` as the advised seam and
    `org-graph--parse-without-local-variables` as the advice symbol. In
    vulpea v2.4.0 the function exists, with signature
    `(vulpea-db--parse-with-temp-buffer PATH RERUN-ORG-MODE)` in
    vulpea-db-extract.el:298, and re-runs `(org-mode)` with
    `delay-mode-hooks nil` after setting `buffer-file-name` (line 327),
    which is what triggers `hack-local-variables`. It is however only
    the temp-buffer branch of `vulpea-db--parse-file (PATH)` (line 371),
    the single DB-free dispatcher `vulpea-db-update-file` calls for every
    indexed file; the dispatcher's `find-file` branch (forced for
    `vulpea-db-extra-extensions`) also runs `hack-local-variables` and
    `.dir-locals.el`. The advice was installed on the dispatcher so the
    invariant holds for all three parse methods, under the task-body
    name `org-graph--vulpea-parse-safely`. `enable-local-variables` nil
    is confirmed as the binding that matters (probe: t -> unsafe eval
    declined in batch, :all -> evaluated, nil -> never read);
    `enable-dir-local-variables` nil is belt-and-braces for the
    find-file branch.
  affected_register_entry: register/invariant/index-parser-never-evaluates-file-locals
  recommendation: |
    Reconcile: statement/enforcement location = `:around` advice
    `org-graph--vulpea-parse-safely` on `vulpea-db--parse-file`
    (vulpea-db-extract.el), covering all `vulpea-db-parse-method` values;
    spec asserts `(advice-member-p #'org-graph--vulpea-parse-safely
    'vulpea-db--parse-file)`. Drop the
    `org-graph--parse-without-local-variables` name. Keep
    `vulpea-db--parse-with-temp-buffer` in notes only as the
    incident-path explanation (the control spec calls it directly to
    prove the fixture fires).

- discovery_id: disc-og-parser-hardening-2
  class: invariant-gap
  description: |
    Two findings against the containment entry. (a) vulpea v2.4.0
    already contains per-file errors at exactly this boundary: both
    callers of `vulpea-db-sync--update-file-if-changed` — the
    timer-driven `vulpea-db-sync--process-queue` (vulpea-db-sync.el:663)
    and `vulpea-db-sync-full-scan` (line 945) — wrap each per-path call
    in a plain `condition-case` that `message`s and continues. The
    advice's marginal value is the `(FILE . ERROR)` line in `*org-graph*`
    and independence from vulpea's callers. (b) The statement "the
    debugger is never entered from the sync timer even when
    debug-on-error is t" is stronger than a `condition-case` can
    deliver: Emacs enters the debugger at signal time when the
    innermost handler carries the `debug` condition
    (`condition-case-unless-debug` / `with-demoted-errors`), before any
    outer handler runs. `run-mode-hooks` wraps `hack-local-variables` in
    exactly such a frame — which is why the incident reached the
    debugger despite vulpea's existing handler, and why the parse-side
    binding (sibling invariant) is the load-bearing fix for it. The
    guard as specified (a `condition-case`, deliberately not a
    `debug-on-error` toggle) was implemented as
    `org-graph--vulpea-update-safely`; it keeps the debugger out for
    errors that reach it unhandled and returns nil so the batch
    continues (three-file spec passes through the real advised
    function with `vulpea-db-update-file` stubbed to signal).
  affected_register_entry: register/invariant/index-sync-per-file-errors-contained
  recommendation: |
    Soften the statement to: errors that propagate out of the per-file
    update are caught at that file's boundary, logged, and skipped, and
    the debugger is not entered for them; note explicitly that
    `debug`-conditioned frames deeper in the call (demoted errors) are
    outside its reach and are closed by
    index-parser-never-evaluates-file-locals for the known case. Record
    the redundancy with vulpea 2.4.0's own per-path handler in
    status_note so a future reader does not mistake the advice for the
    sole guard. If a strict "never the debugger from the timer"
    guarantee is wanted, that is a separate design decision (binding
    `debug-on-error` nil in the wrapper) the entry currently rejects.
