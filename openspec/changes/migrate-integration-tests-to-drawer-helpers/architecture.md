## Components

This change is purely a test-fixture migration. No production modules change.

- **`config/gptel/scope/test/integration/*.el` / `*.org`** — the 9 affected files. Each file owns one or more local fixture-helper defuns (e.g. `fs-integ--load-config-from-yaml`, `parallel--make-empty-scope-config`, `multi--parse-scope-yml`). Migration touches the helper body; test bodies are unchanged (or, for SURGICAL/DELETE files, whole describe blocks or whole files are removed).
- **`config/gptel/scope/test/helpers-spec.el`** — read-only here. Already provides the migration targets:
  - `helpers-spec-make-scope-config` (line 170) — `cl-defun` returning the canonical `(:paths (…) :cloud (…))` plist
  - `jf/gptel-test--with-scope-drawer` macro (line 588) — drawer-backed temp buffer for loader tests
- **`openspec/specs/gptel/scope.md`** — one paragraph tightened to reflect that the test corpus enforces "no scope.yml read or write."

## Interfaces

**Reused (no changes):**
- `helpers-spec-make-scope-config (&key read write execute modify deny read-metadata auth-detection allowed-providers)` → scope-config plist
- `jf/gptel-test--with-scope-drawer ALIST &rest BODY` → macro
- `jf/gptel-scope--write-pattern-to-drawer BUFFER OPERATION PATTERN` (`scope-expansion.el:136`) — production writer; the writer-round-trip tests in WS-A still call this and verify its output.
- `org-entry-get-multivalued-property PT KEY` — the new "read back the drawer" primitive that replaces the old "parse scope.yml from disk" step in writer-round-trip tests.

**Deprecated (left in place for this change):**
- `helpers-spec-load-scope-config` (`helpers-spec.el:419`)
- `helpers-spec--scope-with-paths` (line 427)
- `helpers-spec--scope-with-cloud` (line 431)
- `helpers-spec-make-scope-with-cloud-deny` (line 435)
- `helpers-spec-make-scope-with-allowed-providers` (line 439)
- `helpers-spec-make-scope-yml` (line 402)

These already signal a clear "YAML helpers removed; use helpers-spec-make-scope-config" error. They survive this change to preserve the migration message for any future code; cleanup is a separate task once a full grep confirms zero callers.

**Modified call sites:**
- Each integration test's local fixture helper. See per-task details.

## Testing Approach

**Test Framework**: Buttercup. The integration directory is entirely Buttercup (no ERT). Single-process runs per `./bin/run-tests.sh -d` invocation.

**Test Organization**: `config/gptel/scope/test/integration/*-spec.el` — co-located with the scope subsystem. No new test files added; the change is purely modifications to existing files plus two whole-file deletions.

**Naming Conventions**: existing `*-spec.el` Buttercup convention. Each file's `describe` / `it` block names are preserved (or whole describes deleted for SURGICAL files).

**Running Tests**:
- All integration: `./bin/run-tests.sh -d config/gptel/scope/test/integration`
- Single file's behavior change: the directory run reports per-file pass/fail counts.
- Whole-suite regression sweep: `./bin/run-tests.sh --report`

**Test Patterns**:
- Fixture construction: `(helpers-spec-make-scope-config :read '(…) :write '(…) …)` returns the canonical plist. Replaces both the inline `(jf/gptel-scope-yaml--merge-schema-defaults (jf/gptel-scope-yaml--parse-string YAML-STR))` chain and the on-disk `(helpers-spec-make-scope-yml YAML-STR)` + `(helpers-spec-load-scope-config FILE)` pair.
- Mock-based: `(spy-on 'jf/gptel-scope--load-config :and-return-value CONFIG)` continues to work — the plist shape is unchanged.
- Writer round-trip: replace the YAML reload step with a drawer read:
  ```elisp
  (jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_READ . "/baseline/**"))
    (call-the-writer …)
    (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
            :to-equal '("/baseline/**" "/new/**")))
  ```

**Scenario Mapping**:
- Each MIGRATE task closes when the file's existing `describe` / `it` tree passes (no semantic-meaning change).
- Each DELETE task closes when the file is gone and the directory test run reports `0 failed` for the affected scenarios.
- The SURGICAL task closes when L1–L2 describes are gone and L3–L6 pass with their new fixture builders.

## Cross-cutting Concerns

- **Tangle order**: `.org` files (bash-parser-integration-spec.org, bash-parser-contract-layers-spec.org, scope-config-integration-spec.org) must be tangled via `./bin/tangle-org.sh` before tests can be run against the regenerated `.el`. DELETE actions for `.org` files must also delete the tangled `.el` (no auto-rebuild on deletion).
- **No worktree isolation needed**: per-file tasks touch independent files. Can be tackled in any order; serial or parallel as the implementer prefers.
- **WS-A → WS-B serialization**: `bash-add-to-scope-bug-spec.el` is also a target in the drafted `scope-rearch-followups`. WS-A must land first so WS-B inherits a clean fixture.
