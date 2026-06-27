## Why

The archived change `gptel-scope-in-org-properties` (2026-04-30) migrated per-session scope configuration from `scope.yml` sidecar files to org-mode `:PROPERTIES:` drawers in `session.org`. The accompanying task `delete-yaml-and-security-residue` (closed) deleted the YAML loader module (`scope-yaml.el`), the schema-merge helper, and the test helpers that constructed scope configs via YAML round-trips. The validation/* and expansion/* test directories were migrated to drawer-based fixtures and a direct plist-builder (`helpers-spec-make-scope-config`). The integration/* directory was not.

The closed task `migrate-validation-tests` explicitly named the follow-up: "Migration of those tests is follow-up work tracked by `migrate-integration-tests` and `migrate-expansion-tests`." The expansion-tests task was created and closed; the integration-tests task was never filed. The closed task `finalize-no-scope-config-removal` confirmed the gap at archive time: "Cleaning them up is a separate task — candidate for a follow-up `migrate-integration-tests-to-drawer-helpers`."

The result has lived in the test suite since: **~100 buttercup failures in `config/gptel/scope/test/integration/*-spec.el`**, all rooted in calls to the deleted YAML helpers (`jf/gptel-scope-yaml--merge-schema-defaults`, `jf/gptel-scope-yaml--parse-{file,string}`, `helpers-spec-load-scope-config`, `helpers-spec--scope-with-paths`). The dispatcher tests run; the integration tests can't even reach their assertions because the helpers signal `void-function` at fixture-construction time.

This change closes the test-side of the YAML-to-drawer migration. No production code changes — the writers are already drawer-based (`jf/gptel-scope--write-pattern-to-drawer` at `config/gptel/scope/scope-expansion.el:136`). The spec contract `openspec/specs/gptel/scope.md` already describes the post-YAML world; this change brings the integration test corpus into compliance with it.

## What Changes

**Tests migrated** (writer/validator behavior unchanged, fixture path moves from "build YAML string → parse → merge" to "call `helpers-spec-make-scope-config` directly" — or, where the test exercises the loader's drawer-reading path, "build a drawer fixture via `jf/gptel-test--with-scope-drawer`"):

- `config/gptel/scope/test/integration/filesystem-scope-integration-spec.el` — rewrite `fs-integ--load-config-from-yaml`
- `config/gptel/scope/test/integration/parallel-tool-callback-spec.el` — rewrite `parallel--make-empty-scope-config` + one inline construction
- `config/gptel/scope/test/integration/bash-scope-expansion-integration-spec.el` — rewrite `bash-integ--make-scope-config` (this also unblocks two named "gptel callback contract" failures masked behind the YAML error)
- `config/gptel/scope/test/integration/bash-multi-violation-expansion-spec.el` — rewrite `multi--parse-scope-yml` + one inline; replace the writer's scope.yml-reload step with a drawer read via `org-entry-get-multivalued-property`
- `config/gptel/scope/test/integration/bash-add-to-scope-bug-spec.el` — same drawer-read substitution; unblocks the named "Bug 4" assertion that's masked by the YAML error today
- `config/gptel/scope/test/integration/bash-parser-integration-spec.org` — rewrite two fixture helpers in the literate source, then tangle
- `config/gptel/scope/test/integration/bash-parser-contract-layers-spec.org` — surgical: delete the "Scope config layers" describe (`Layer 1–2`, ~140 lines testing the deleted schema/security/bash_tools sections); migrate fixture builders for the remaining real bash-parser wiring tests (Layers 3–6)

**Tests deleted** (assertions cover removed behavior; nothing to migrate to):

- `config/gptel/scope/test/integration/scope-config-integration-spec.org` — every scenario tests the YAML parsing pipeline (schema-merge defaults, YAML-boolean normalization, `:security :enforce-parse-complete` round-trip). All this behavior was deleted in cycle-3 (`register/invariant/scope-no-security-key-in-plist`). No corresponding post-YAML behavior to assert.
- `config/gptel/scope/test/integration/expansion-roundtrip-spec.el` — tests "expand → write `scope.yml` → reload via YAML loader → contract valid." Both the on-disk YAML format and the loader are gone. The drawer round-trip the writer now produces is already covered by the dedicated `config/gptel/scope/test/drawer/*-spec.el` suite.

**Deprecated helper stubs** (`config/gptel/scope/test/helpers-spec.el:419-441`) are left in place for now. Once all callers are gone (after this change), a follow-up cleanup can delete them. Out of scope here to keep this change reviewable as a single concern.

## Capabilities

### New Capabilities

_None — this change reshapes test fixtures, it does not introduce new behavior._

### Modified Capabilities

- `gptel/scope`: the spec at `openspec/specs/gptel/scope.md` already states that per-session scope lives in `session.org`'s `:PROPERTIES:` drawer rather than `scope.yml` on disk. This change brings the integration test corpus into compliance with that contract. One small wording tightening: explicitly note that `scope.yml` is **never** read or written by the loader / writer (the spec currently says it's "no longer used per-session" but stops short of the absolute statement that the surrounding test contract now needs).

## Impact

**Code**: none. Production writers (`jf/gptel-scope--write-pattern-to-drawer`, `--add-path-to-scope`, `--add-bash-to-scope`) are already drawer-based and remain unchanged. This change is purely test fixtures + (one) spec-text tightening.

**Tests**: 9 files in `config/gptel/scope/test/integration/`. Closure target: `./bin/run-tests.sh -d config/gptel/scope/test/integration` exits 0; full-suite buttercup failures drop from ~120 to ~20.

**Specs**: one wording tightening in `openspec/specs/gptel/scope.md`.

**Migration**: none for users. Existing `session.org` files continue to work — they already use the drawer-based contract.

**Not affected**:
- Production code paths (writer, loader, validator).
- The `validation/*` and `expansion/*` test directories (already migrated).
- `config/gptel/scope/test/drawer/*-spec.el` (drawer round-trip coverage, which is what the deleted `expansion-roundtrip-spec.el` tested in a now-stale shape).
- The dispatcher tests (`tool-wrapper/`, etc.) and contract tests.

**Dependencies**:
- Builds on archived `gptel-scope-in-org-properties` (drawer is the scope home) and `delete-yaml-and-security-residue` (YAML loader is gone).
- No upstream gptel coordination required.

**Coordination note**: the drafted `scope-rearch-followups` change also touches `bash-add-to-scope-bug-spec.el` for an orthogonal bug fix. This change should land first so that change inherits a clean test fixture and isn't blocked by YAML errors.
