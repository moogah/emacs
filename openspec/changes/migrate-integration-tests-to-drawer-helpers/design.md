## Context

The YAML loader was removed in cycle-3 of `gptel-scope-in-org-properties`. The migration of `validation/*` and `expansion/*` test fixtures landed; the migration of `integration/*` was deferred and tracked for follow-up but never filed. The deprecated stubs in `helpers-spec.el` (lines 419–441) currently `(error "…YAML helpers removed; use helpers-spec-make-scope-config")` to give a clear migration message rather than `void-function`. ~100 test failures sit on these stubs.

The replacement helpers already exist in `config/gptel/scope/test/helpers-spec.el`:

- `helpers-spec-make-scope-config` (line 170) — `cl-defun` accepting `:read :write :execute :modify :deny :read-metadata :auth-detection :allowed-providers`. Returns the canonical `(:paths (…) :cloud (…))` plist that `jf/gptel-scope--load-from-buffer` would have produced. No file I/O.
- `jf/gptel-test--with-scope-drawer` macro (line 588) — wraps `body` in a temp buffer whose `:PROPERTIES:` drawer is built from an alist of `(:GPTEL_SCOPE_<KEY> . VALUE)` pairs. Used by drawer-loader tests under `config/gptel/scope/test/drawer/`.

## Goals / Non-Goals

**Goals:**

- `./bin/run-tests.sh -d config/gptel/scope/test/integration` exits 0.
- The full-suite total failure count drops by ~100.
- No production code changes; this change is purely fixture migration + small test deletions + one spec wording tightening.
- Each affected file is its own task — closure is per-file, conflict surface stays small.
- The deprecated stubs in `helpers-spec.el` survive this change; they are removed in a separate cleanup once a full grep confirms no remaining callers.

**Non-Goals:**

- Reworking what the integration tests assert. Each migrated test continues to exercise the same production code path; only the fixture-construction step changes.
- Adding new test coverage. Where deletions remove coverage (e.g., `expansion-roundtrip-spec.el`), confirm the dedicated drawer test directory (`test/drawer/*`) already exercises the surviving contract — that's a check, not a new test write.
- Touching the `scope-rearch-followups` change. That change has its own bugs to fix; if those bugs are masked by YAML errors today, this change unmasks them; the residual assertion fix is in scope of the other change.

## Decisions

**D1. MIGRATE vs DELETE per file.** The agent investigation produced a definitive call per file (see proposal.md "What Changes"). The dividing line: does the file test current behavior or removed behavior?

- Tests of current behavior (filesystem tool routing, parallel callback queue, bash semantic pipeline → scope validation, multi-violation expansion writer, add-to-scope bug 4, bash-parser integration layers 3–6) → MIGRATE.
- Tests of removed behavior (YAML parsing pipeline schema-merge / boolean normalization / `:security` round-trip; scope.yml on-disk reload via the deleted loader) → DELETE.
- One file (`bash-parser-contract-layers-spec.org`) is mixed: Layer 1–2 tests YAML semantics; Layers 3–6 test the bash-parser ↔ scope-validation wiring. SURGICAL: delete L1–L2 describes, migrate L3–L6 fixtures.

**D2. Migration shape: prefer `helpers-spec-make-scope-config` over `jf/gptel-test--with-scope-drawer`.** All affected integration tests construct a scope config to feed into a validator or to mock `jf/gptel-scope--load-config`. The plist builder is the right tool — it returns the canonical loader-output plist directly, no temp buffer, no drawer parsing. Reserve `jf/gptel-test--with-scope-drawer` for tests that genuinely exercise the loader's drawer-reading path (none of the integration files do; the drawer reader is covered by `test/drawer/`).

**D3. Per-test-file local helpers stay (refactored).** Each integration file has its own `<prefix>--make-scope-config` or `<prefix>--load-config-from-yaml` helper that all tests in the file call. Keep the helper; rewrite its body to call `helpers-spec-make-scope-config`. Avoids touching the dozens of inline call sites and keeps each file's test bodies untouched.

**D4. Writer round-trip tests in `bash-multi-violation-expansion-spec.el` and `bash-add-to-scope-bug-spec.el` switch their reload step from "parse scope.yml on disk" to "read drawer in current chat buffer via `org-entry-get-multivalued-property`."** The writers (`jf/gptel-scope--add-path-to-scope`, `--add-bash-to-scope`) already write to the drawer via `jf/gptel-scope--write-pattern-to-drawer` (`scope-expansion.el:136`). The test's invariant ("writer output round-trips to a valid scope config") is still valid; only the read step needs the new vocabulary. The test uses a temp chat buffer with a `:PROPERTIES:` drawer (build via `jf/gptel-test--with-scope-drawer` or `(with-temp-buffer (org-mode) (insert …))`), runs the writer, reads back the drawer.

**D5. `bash-parser-contract-layers-spec.org` describe-block surgery.** Layers 1–2 (lines 189–332 of the org source) test the deleted YAML schema/security/bash_tools sections. These cannot be salvaged. The remaining Layers 3–6 (permissive/restrictive/corpus/error-shape) test the bash-parser ↔ scope-validation contract layers — still real. Surgery removes ~140 lines from one file and rewrites the fixture builders (lines 205–224) to use `helpers-spec-make-scope-config`.

**D6. Per-task verification command.** Every task's `Verification` runs `./bin/run-tests.sh -d config/gptel/scope/test/integration` and asserts that the file's own buttercup result line shows zero failures. Whole-suite verification is the WS-A close-out task (number 11 in the plan).

**D7. Spec delta scope.** `openspec/specs/gptel/scope.md` already states that `scope.yml` is "no longer used per-session." Tightening: add a sentence to that paragraph stating that the loader does not read from `scope.yml` under any circumstance, and the writers do not produce it. Captures the test-side contract that the integration tests now enforce. One short delta scenario.

## Risks / Trade-offs

- **Risk: a "MIGRATE" file turns out to need new coverage to replace deleted assertions.** Mitigation: per-file tasks verify the file's own pass before closing. If a surprise gap surfaces (e.g., a behavior was only covered in `scope-config-integration-spec.org` and nowhere else), file a follow-up to add coverage in the right home (`test/drawer/`, `test/validation/`, etc.).
- **Trade-off: deprecated helper stubs survive this change.** They could be deleted as a 10th task, but doing so would prevent a future test (in a parallel branch) from getting a useful "helpers removed" error message while it migrates. Cleaner to delete once we're certain the migration is global.
- **Coordination: do not run in parallel with `scope-rearch-followups`.** Both change `bash-add-to-scope-bug-spec.el`. WS-A first; WS-B inherits a clean file.

## Migration Plan

1. Land all 9 file tasks (per-file, can be done in any order or in parallel as long as `bash-add-to-scope-bug-spec.el` and the others don't share a worktree — they don't).
2. Update the spec delta wording (final task, after the file migrations).
3. Whole-suite verification (`./bin/run-tests.sh --report`); confirm ~100 failure drop with no regressions elsewhere.
4. (Out of scope for this change) Once a future grep confirms no remaining callers of the deprecated stubs, file a small cleanup to delete them from `helpers-spec.el`.

## Open Questions

_None — the investigation answered the architectural questions (writers are drawer-based, scope-config plist shape is canonical, per-file local helpers are the right migration unit)._
