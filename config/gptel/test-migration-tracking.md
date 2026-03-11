# Test Migration Tracking: scope-test-organization

Baseline: 470 test cases (172 ERT + 262 Buttercup in tools/test, 36 Buttercup in scope/test)
Status legend: ○ pending | → moved | ⊕ merged | ✕ removed (redundant) | ✎ modified | ✚ new

---

## Infrastructure: tools/test/helpers-spec.el

| File | Status | Destination | Notes |
|------|--------|-------------|-------|
| helpers-spec.el | → moved | scope/test/helpers-spec.el | Shared test infrastructure (matchers, mocks, fixtures). Primary consumer location is now scope/test/. Tests in tools/test/ that `(require 'helpers-spec)` will need load-path updates in later beads. |

---

## ERT: tools/test/integration/test-schema.el (62 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | test-scope-schema-valid-document-loads | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads valid document successfully" |
| 2 | test-scope-schema-full-document | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads all sections from complete document" |
| 3 | test-scope-schema-paths-read-section | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads paths.read section" |
| 4 | test-scope-schema-paths-write-section | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads paths.write section" |
| 5 | test-scope-schema-paths-execute-section | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads paths.execute section" |
| 6 | test-scope-schema-paths-modify-section | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads paths.modify section" |
| 7 | test-scope-schema-paths-deny-unchanged | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads paths.deny section" |
| 8 | test-scope-schema-cloud-auth-detection-field | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads auth_detection field" |
| 9 | test-scope-schema-cloud-allowed-providers-field | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads allowed_providers field" |
| 10 | test-scope-schema-cloud-missing-defaults | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "adds defaults when cloud section missing" |
| 11 | test-scope-schema-security-enforce-parse-complete | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads enforce_parse_complete field" |
| 12 | test-scope-schema-security-max-coverage-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads max_coverage_threshold field" |
| 13 | test-scope-schema-security-missing-defaults | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "adds defaults when security section missing" |
| 14 | test-scope-schema-bash-tools-deny | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads deny list" |
| 15 | test-scope-schema-bash-tools-categories-rejected | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects categories section" |
| 16 | test-scope-schema-kebab-case-normalization | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "normalizes all keys to kebab-case" |
| 17 | test-scope-schema-snake-case-keys | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "handles snake_case keys" |
| 18 | test-scope-schema-nested-normalization | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "normalizes nested structures" |
| 19 | test-scope-schema-boolean-false | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects YAML boolean false" |
| 20 | test-scope-schema-empty-arrays | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads empty arrays correctly" |
| 21 | test-scope-schema-mixed-quote-styles | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "handles mixed YAML quote styles" |
| 22 | test-scope-schema-numeric-threshold-variations | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "handles numeric threshold variations" |
| 23 | test-scope-schema-legacy-document-no-execute-modify | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "legacy document does not auto-add execute/modify" |
| 24 | test-scope-schema-no-automatic-migration | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "no automatic migration from legacy schemas" |
| 25 | test-scope-schema-write-path-allows-read | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "write path section loads separately from read" |
| 26 | test-scope-schema-read-path-does-not-allow-write | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "read path does not imply write" |
| 27 | test-scope-schema-defaults-empty-schema | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "empty schema gets all defaults" |
| 28 | test-scope-schema-defaults-partial-paths | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "partial paths section merges with defaults" |
| 29 | test-scope-schema-defaults-partial-cloud | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "partial cloud section merges with defaults" |
| 30 | test-scope-schema-defaults-partial-security | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "partial security section merges with defaults" |
| 31 | test-scope-schema-defaults-mixed-present-absent | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "mixed present/absent sections merge correctly" |
| 32 | test-scope-schema-defaults-all-sections-override | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "all sections can be overridden" |
| 33 | test-scope-schema-defaults-one-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "1.0 threshold is preserved" |
| 34 | test-scope-schema-defaults-zero-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "zero threshold is preserved" |
| 35 | test-scope-schema-defaults-override-cloud-auth | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "user-specified cloud auth overrides default" |
| 36 | test-scope-schema-defaults-override-security-enforce | ✕ removed | | Redundant with #30 (partial security merges) and #32 (all overrides) |
| 37 | test-scope-schema-defaults-override-security-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "user-specified security threshold overrides default" |
| 38 | test-scope-schema-defaults-empty-bash-tools | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "bash_tools not modified by default merging" |
| 39 | test-scope-schema-defaults-minimal-plus-bash-tools | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "minimal plus bash_tools gets other defaults" |
| 40 | test-scope-schema-invalid-paths-structure | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads invalid path structure as-is" |
| 41 | test-scope-schema-invalid-auth-detection | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects invalid auth_detection value" |
| 42 | test-scope-schema-invalid-coverage-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects invalid coverage threshold > 1.0" |
| 43 | test-scope-schema-reject-invalid-yaml-syntax | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "handles malformed YAML syntax" |
| 44 | test-scope-schema-reject-paths-number | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads numeric path value" |
| 45 | test-scope-schema-reject-malformed-paths-object | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads malformed path object structure" |
| 46 | test-scope-schema-reject-mixed-types-in-path-array | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads mixed types in path array" |
| 47 | test-scope-schema-reject-duplicate-path-patterns | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads duplicate path patterns as-is" |
| 48 | test-scope-schema-reject-empty-auth-detection | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects empty auth_detection" |
| 49 | test-scope-schema-reject-numeric-auth-detection | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects numeric auth_detection" |
| 50 | test-scope-schema-reject-invalid-auth-detection-typo | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects misspelled auth_detection" |
| 51 | test-scope-schema-reject-null-cloud-section | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "handles null cloud section with defaults" |
| 52 | test-scope-schema-reject-empty-cloud-providers-is-valid | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "accepts empty allowed_providers array" |
| 53 | test-scope-schema-reject-non-boolean-enforce-parse-complete | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects non-boolean enforce_parse_complete" |
| 54 | test-scope-schema-reject-numeric-enforce-parse-complete | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects numeric enforce_parse_complete" |
| 55 | test-scope-schema-reject-non-numeric-coverage-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects non-numeric coverage threshold" |
| 56 | test-scope-schema-reject-negative-coverage-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects negative coverage threshold" |
| 57 | test-scope-schema-reject-coverage-threshold-above-one | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects coverage threshold above 1.0" |
| 58 | test-scope-schema-reject-coverage-threshold-at-boundary | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "accepts boundary threshold values" |
| 59 | test-scope-schema-reject-extremely-large-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "rejects extremely large threshold" |
| 60 | test-scope-schema-reject-scientific-notation-threshold | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "handles scientific notation threshold" |
| 61 | test-scope-schema-reject-invalid-bash-tools-structure | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "loads invalid bash_tools structure" |
| 62 | test-scope-schema-reject-unknown-top-level-keys | ⊕ merged | scope/test/core/config-loading-spec.el | ERT->Buttercup: "ignores unknown top-level keys" |

## ERT: tools/test/integration/test-file-paths.el (44 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | test-scope-file-paths-read-matches-paths-read | ○ | | |
| 2 | test-scope-file-paths-read-matches-paths-write | ○ | | |
| 3 | test-scope-file-paths-read-operation-denied | ○ | | |
| 4 | test-scope-file-paths-write-requires-paths-write | ○ | | |
| 5 | test-scope-file-paths-execute-requires-paths-execute | ○ | | |
| 6 | test-scope-file-paths-modify-requires-paths-modify | ○ | | |
| 7 | test-scope-file-paths-modify-allowed-by-paths-write | ○ | | |
| 8 | test-scope-file-paths-modify-allowed-by-paths-modify | ○ | | |
| 9 | test-scope-file-paths-deny-overrides-read-scope | ○ | | |
| 10 | test-scope-file-paths-deny-overrides-write-scope | ○ | | |
| 11 | test-scope-file-paths-deny-overrides-execute-scope | ○ | | |
| 12 | test-scope-file-paths-empty-patterns-deny-all | ○ | | |
| 13 | test-scope-file-paths-nil-patterns-deny-all | ○ | | |
| 14 | test-scope-file-paths-overlapping-patterns | ○ | | |
| 15 | test-scope-file-paths-case-sensitive-patterns | ○ | | |
| 16 | test-scope-file-paths-dot-files-matched | ○ | | |
| 17 | test-scope-file-paths-special-chars-in-filename | ○ | | |
| 18 | test-scope-file-paths-trailing-slash-handling | ○ | | |
| 19 | test-scope-file-paths-absolute-path-unchanged | ○ | | |
| 20 | test-scope-file-paths-resolve-relative-path | ○ | | |
| 21 | test-scope-file-paths-resolve-parent-directory | ○ | | |
| 22 | test-scope-file-paths-resolve-symlinks | ○ | | |
| 23 | test-scope-file-paths-extract-read-operation | ○ | | |
| 24 | test-scope-file-paths-extract-write-operation | ○ | | |
| 25 | test-scope-file-paths-extract-execute-operation | ○ | | |
| 26 | test-scope-file-paths-extract-modify-operation | ○ | | |
| 27 | test-scope-file-paths-extract-multiple-operations | ○ | | |
| 28 | test-scope-file-paths-multiple-reads-all-validated | ○ | | |
| 29 | test-scope-file-paths-mixed-operations-all-validated | ○ | | |
| 30 | test-scope-file-paths-first-operation-fails-immediate-return | ○ | | |
| 31 | test-scope-file-paths-copy-both-in-scope | ○ | | |
| 32 | test-scope-file-paths-copy-destination-out-of-scope | ○ | | |
| 33 | test-scope-file-paths-delete-requires-paths-write | ○ | | |
| 34 | test-scope-file-paths-delete-denied-without-write | ○ | | |
| 35 | test-scope-file-paths-glob-recursive-wildcard | ○ | | |
| 36 | test-scope-file-paths-glob-single-level-wildcard | ○ | | |
| 37 | test-scope-file-paths-glob-extension-pattern | ○ | | |
| 38 | test-scope-file-paths-glob-directory-exact-match | ○ | | |
| 39 | test-scope-file-paths-glob-multiple-patterns | ○ | | |
| 40 | test-scope-file-paths-glob-recursive-middle | ○ | | |
| 41 | test-scope-file-paths-glob-middle-wildcard | ○ | | |
| 42 | test-scope-file-paths-error-path-out-of-scope-structure | ○ | | |
| 43 | test-scope-file-paths-error-path-denied-structure | ○ | | |
| 44 | test-scope-file-paths-error-message-actionable | ○ | | |

## ERT: tools/test/integration/test-pipelines.el (36 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | test-pipeline-extract-commands-from-pipe | ○ | | |
| 2 | test-pipeline-extract-commands-from-and-chain | ○ | | |
| 3 | test-pipeline-extract-commands-from-or-chain | ○ | | |
| 4 | test-pipeline-extract-commands-from-semicolon-chain | ○ | | |
| 5 | test-pipeline-extract-commands-from-multi-stage-pipeline | ○ | | |
| 6 | test-pipeline-extract-command-from-segment-with-arguments | ○ | | |
| 7 | test-pipeline-extract-command-from-segment-with-flags | ○ | | |
| 8 | test-pipeline-extract-git-subcommand | ○ | | |
| 9 | test-pipeline-single-command-no-pipeline | ○ | | |
| 10 | test-pipeline-empty-string | ○ | | |
| 11 | test-pipeline-deny-list-blocks-command | ○ | | |
| 12 | test-pipeline-deny-list-first-command | ○ | | |
| 13 | test-pipeline-deny-list-first-failure | ○ | | |
| 14 | test-pipeline-deny-list-only-validation | ○ | | |
| 15 | test-pipeline-deny-list-no-categories | ○ | | |
| 16 | test-pipeline-command-not-in-deny-list | ○ | | |
| 17 | test-pipeline-empty-deny-list | ○ | | |
| 18 | test-pipeline-all-commands-allowed | ○ | | |
| 19 | test-pipeline-all-allowed-commands | ○ | | |
| 20 | test-pipeline-validation-empty-list | ○ | | |
| 21 | test-pipeline-second-command-denied | ○ | | |
| 22 | test-pipeline-middle-command-denied | ○ | | |
| 23 | test-pipeline-error-includes-message | ○ | | |
| 24 | test-pipeline-error-identifies-second-command | ○ | | |
| 25 | test-pipeline-error-position-for-first-command | ○ | | |
| 26 | test-pipeline-error-position-for-third-command | ○ | | |
| 27 | test-pipeline-mixed-allowed-and-denied | ○ | | |
| 28 | test-pipeline-complex-four-stage-pipeline | ○ | | |
| 29 | test-pipeline-mixed-chains-and-pipes | ○ | | |
| 30 | test-pipeline-legitimate-data-pipeline | ○ | | |
| 31 | test-pipeline-security-curl-pipe-sh | ○ | | |
| 32 | test-pipeline-security-find-xargs-rm-pattern | ○ | | |
| 33 | test-pipeline-security-cat-eval | ○ | | |
| 34 | test-pipeline-prevent-chmod-bypass | ○ | | |
| 35 | test-pipeline-prevent-sh-execution-bypass | ○ | | |
| 36 | test-pipeline-prevent-xargs-rm-bypass | ○ | | |

## ERT: tools/test/integration/test-cloud-auth.el (30 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | test-scope-cloud-auth-detect-aws-cli-authentication | ○ | | |
| 2 | test-scope-cloud-auth-detect-aws-authentication | ○ | | |
| 3 | test-scope-cloud-auth-detect-gcp-authentication | ○ | | |
| 4 | test-scope-cloud-auth-detect-azure-authentication | ○ | | |
| 5 | test-scope-cloud-auth-no-detection-for-non-auth-commands | ○ | | |
| 6 | test-scope-cloud-auth-config-default-to-warn | ○ | | |
| 7 | test-scope-cloud-auth-config-allow-mode | ○ | | |
| 8 | test-scope-cloud-auth-config-warn-mode | ○ | | |
| 9 | test-scope-cloud-auth-config-deny-mode | ○ | | |
| 10 | test-scope-cloud-auth-invalid-mode-fails-closed | ○ | | |
| 11 | test-scope-cloud-auth-warn-aws-command | ○ | | |
| 12 | test-scope-cloud-auth-allow-aws-command | ○ | | |
| 13 | test-scope-cloud-auth-deny-aws-command | ○ | | |
| 14 | test-scope-cloud-auth-allow-gcp-command | ○ | | |
| 15 | test-scope-cloud-auth-warn-identifies-provider | ○ | | |
| 16 | test-scope-cloud-auth-warn-suggests-reviewing-scope | ○ | | |
| 17 | test-scope-cloud-auth-error-denied-structure | ○ | | |
| 18 | test-scope-cloud-auth-error-suggests-scope-expansion | ○ | | |
| 19 | test-scope-cloud-auth-error-provider-restriction-explains | ○ | | |
| 20 | test-scope-cloud-auth-filter-specific-provider-allowed | ○ | | |
| 21 | test-scope-cloud-auth-filter-unlisted-provider-denied | ○ | | |
| 22 | test-scope-cloud-auth-filter-empty-list-denies-all | ○ | | |
| 23 | test-scope-cloud-auth-provider-filtering-precedence | ○ | | |
| 24 | test-scope-cloud-auth-warn-mode-with-filtering | ○ | | |
| 25 | test-scope-cloud-auth-deny-allowed-provider-permitted | ○ | | |
| 26 | test-scope-cloud-auth-deny-disallowed-provider-rejected | ○ | | |
| 27 | test-scope-cloud-auth-nil-cloud-auth-ops-passes | ○ | | |
| 28 | test-scope-cloud-auth-combined-all-pass | ○ | | |
| 29 | test-scope-cloud-auth-combined-allowed-but-other-validations-may-fail | ○ | | |
| 30 | test-scope-cloud-auth-combined-fails-auth-policy | ○ | | |

## Buttercup: behavioral/run-bash-command/cloud-authentication-spec.el (10 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | allows cloud auth with warning in warn mode (default behavior) | ○ | | |
| 2 | allows cloud auth with warning for GCP commands in warn mode | ○ | | |
| 3 | allows cloud auth with warning for Azure commands in warn mode | ○ | | |
| 4 | allows cloud auth silently when auth_detection is allow | ○ | | |
| 5 | denies cloud auth when auth_detection is deny | ○ | | |
| 6 | allows cloud auth when provider is in allowed list | ○ | | |
| 7 | denies cloud auth when provider is not in allowed list | ○ | | |
| 8 | denies unlisted providers even in warn mode | ○ | | |
| 9 | skips cloud auth validation when no cloud auth detected | ○ | | |
| 10 | validates both cloud auth and file operations together | ○ | | |

## Buttercup: behavioral/run-bash-command/deny-list-validation-spec.el (9 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | blocks simple denied command | ○ | | |
| 2 | blocks denied command in pipeline position 2 | ○ | | |
| 3 | blocks denied command in complex pipeline | ○ | | |
| 4 | blocks sudo in pipeline | ○ | | |
| 5 | validates chain operators (&&) | ○ | | |
| 6 | validates chain operators (||) | ○ | | |
| 7 | validates sequential commands (;) | ○ | | |
| 8 | allows commands not in deny list to proceed to next stage | ○ | | |
| 9 | allows multiple non-denied commands in pipeline to proceed | ○ | | |

## Buttercup: behavioral/run-bash-command/error-messages-spec.el (9 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | command denied error includes command and position | ○ | | |
| 2 | parse incomplete error includes parse errors | ○ | | |
| 3 | path out of scope error includes operation details | ○ | | |
| 4 | path denied error distinguishes from out-of-scope | ○ | | |
| 5 | cloud auth denied includes provider details | ○ | | |
| 6 | pipeline command denied includes full context | ○ | | |
| 7 | multiple operation failure reports first violation | ○ | | |
| 8 | success response includes coverage metrics | ○ | | |
| 9 | warning structure for cloud auth in warn mode | ○ | | |

## Buttercup: behavioral/run-bash-command/integration-scenarios-spec.el (10 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | validates pipeline with mixed operations succeeds | ○ | | |
| 2 | validates pipeline with denied command fails at stage 3 | ○ | | |
| 3 | validates command substitution with execute operation | ○ | | |
| 4 | validates cloud auth with file operations (both validated) | ○ | | |
| 5 | validates no-op in deny list fails before no-op check | ○ | | |
| 6 | validates multiple file operations with partial scope failure | ○ | | |
| 7 | validates execute operation denied despite read access | ○ | | |
| 8 | validates modify operation allowed via write scope (hierarchical) | ○ | | |
| 9 | validates deny pattern overrides broad permissions | ○ | | |
| 10 | validates complete successful execution with output | ○ | | |

## Buttercup: behavioral/run-bash-command/no-op-allowance-spec.el (6 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | allows version checks without path configuration | ○ | | |
| 2 | allows help flags as no-ops | ○ | | |
| 3 | allows informational commands | ○ | | |
| 4 | denies no-op commands in deny list before no-op check | ○ | | |
| 5 | bypasses file path validation for no-op commands | ○ | | |
| 6 | enforces deny list even for zero-file-operation commands | ○ | | |

## Buttercup: behavioral/run-bash-command/operation-specific-paths-spec.el (13 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | allows read operation within read scope | ○ | | |
| 2 | allows read operation within write scope (hierarchical permissions) | ○ | | |
| 3 | denies read operation outside scope | ○ | | |
| 4 | denies write operation when only read scope exists | ○ | | |
| 5 | allows write operation within write scope | ○ | | |
| 6 | denies execute operation without execute scope | ○ | | |
| 7 | allows execute operation within execute scope | ○ | | |
| 8 | denies modify operation without modify scope | ○ | | |
| 9 | allows modify operation within modify scope | ○ | | |
| 10 | allows modify operation within write scope (hierarchical permissions) | ○ | | |
| 11 | denies operations on deny list paths regardless of allow patterns | ○ | | |
| 12 | denies when multiple file operations include paths outside scope | ○ | | |
| 13 | allows multiple operations when all are within scope | ○ | | |

## Buttercup: behavioral/run-bash-command/parse-completeness-spec.el (9 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | strict mode rejects incomplete parse (syntax error) | ○ | | |
| 2 | strict mode rejects incomplete loop | ○ | | |
| 3 | strict mode rejects incomplete conditional | ○ | | |
| 4 | strict mode allows valid complex syntax | ○ | | |
| 5 | strict mode allows valid conditionals | ○ | | |
| 6 | permissive mode allows incomplete parse with warning | ○ | | |
| 7 | permissive mode allows partial syntax | ○ | | |
| 8 | default mode is strict | ○ | | |
| 9 | parse completeness checked before deny list | ○ | | |

## Buttercup: behavioral/run-bash-command/path-resolution-spec.el (9 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | relative paths resolved from directory argument | ○ | | |
| 2 | dot-dot paths resolved from directory | ○ | | |
| 3 | path traversal detected and denied | ○ | | |
| 4 | current directory (.) resolved | ○ | | |
| 5 | directory itself in deny list blocks operation | ○ | | |
| 6 | symlink directory resolved (mock file-truename) | ○ | | |
| 7 | absolute path in command bypasses directory context | ○ | | |
| 8 | multiple relative paths resolved independently | ○ | | |
| 9 | working directory provided to command execution | ○ | | |

## Buttercup: behavioral/run-bash-command/resource-limits-spec.el (8 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | allows command that completes within timeout | ○ | | |
| 2 | enforces timeout for command exceeding 30 seconds | ○ | | |
| 3 | warns when long-running command is terminated | ○ | | |
| 4 | returns full output when within limit | ○ | | |
| 5 | truncates output exceeding 10,000 chars limit | ○ | | |
| 6 | suggests filters in truncation notice | ○ | | |
| 7 | captures non-zero exit code | ○ | | |
| 8 | handles execution errors gracefully | ○ | | |

## Buttercup: behavioral/run-bash-command/scope-expansion-spec.el (7 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | validates read operation when path is in read scope | → moved | scope/test/expansion/expansion-ui-spec.el | Bash command expansion UI test |
| 2 | denies read operation when path is out of scope | → moved | scope/test/expansion/expansion-ui-spec.el | Bash command expansion UI test |
| 3 | triggers expansion UI when validation fails, user approves with add-to-scope, scope updated | → moved | scope/test/expansion/expansion-ui-spec.el | Bash command expansion UI test |
| 4 | triggers expansion UI when validation fails, user approves with allow-once, retry succeeds | → moved | scope/test/expansion/expansion-ui-spec.el | Bash command expansion UI test |
| 5 | triggers expansion UI when validation fails, user denies, error returned | → moved | scope/test/expansion/expansion-ui-spec.el | Bash command expansion UI test |
| 6 | deny action invokes callback with user_denied result | → moved | scope/test/expansion/expansion-ui-spec.el | Transient action handler (bash) |
| 7 | validation succeeds after allow-once permission granted | → moved | scope/test/expansion/expansion-ui-spec.el | Allow-once lifecycle test |

## Buttercup: behavioral/filesystem-tools-scope-expansion-spec.el (37 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | read_file > allows read operation when file is in read scope | ○ | | |
| 2 | read_file > allows read operation when file is in write scope (hierarchical) | ○ | | |
| 3 | read_file > denies read operation when file is outside scope | ○ | | |
| 4 | read_file > triggers expansion UI when out of scope, user adds to scope, approval granted | ○ | | |
| 5 | read_file > triggers expansion UI when out of scope, user allows once, permission granted | ○ | | |
| 6 | read_file > triggers expansion UI when out of scope, user denies, error returned | ○ | | |
| 7 | read_file > returns error when file does not exist | ○ | | |
| 8 | read_file > validation succeeds after allow-once permission granted | ○ | | |
| 9 | read_file > allow-once permission can be consumed and removed | ○ | | |
| 10 | read_file > allow-once permission is isolated per tool | ○ | | |
| 11 | read_file > includes git-tracked status in expansion UI | ○ | | |
| 12 | read_file > includes non-existent status in expansion UI | ○ | | |
| 13 | write_file_in_scope > allows write operation when file is in write scope | ○ | | |
| 14 | write_file_in_scope > denies write operation when file is in read-only scope | ○ | | |
| 15 | write_file_in_scope > triggers expansion UI when out of scope, user adds to scope, approval granted | ○ | | |
| 16 | write_file_in_scope > triggers expansion UI when out of scope, user allows once, permission granted | ○ | | |
| 17 | write_file_in_scope > validates parent directory scope when creating directories | ○ | | |
| 18 | write_file_in_scope > denies directory creation when parent directory is out of scope | ○ | | |
| 19 | write_file_in_scope > allow-once permission consumed after use | ○ | | |
| 20 | edit_file_in_scope > allows edit operation when file is in write scope | ○ | | |
| 21 | edit_file_in_scope > denies edit operation when file is in read-only scope | ○ | | |
| 22 | edit_file_in_scope > allows edit operation when file exists | ○ | | |
| 23 | edit_file_in_scope > validation handles non-existent files with metadata | ○ | | |
| 24 | edit_file_in_scope > triggers expansion UI when out of scope, user adds to scope, approval granted | ○ | | |
| 25 | edit_file_in_scope > triggers expansion UI when out of scope, user allows once, permission granted | ○ | | |
| 26 | edit_file_in_scope > triggers expansion UI when out of scope, user denies, error returned | ○ | | |
| 27 | edit_file_in_scope > allow-once permission consumed after use | ○ | | |
| 28 | Transient > Add-to-scope > updates read scope for read operations | → moved | scope/test/expansion/expansion-ui-spec.el | Filesystem transient action handler |
| 29 | Transient > Add-to-scope > updates write scope for write operations | → moved | scope/test/expansion/expansion-ui-spec.el | Filesystem transient action handler |
| 30 | Transient > Add-to-scope > updates write scope for edit operations | → moved | scope/test/expansion/expansion-ui-spec.el | Filesystem transient action handler |
| 31 | Transient > Deny > returns user_denied when user denies request | → moved | scope/test/expansion/expansion-ui-spec.el | Filesystem transient action handler |
| 32 | Transient > Allow-once > grants temporary permission when user allows once | → moved | scope/test/expansion/expansion-ui-spec.el | Filesystem transient action handler |

## Buttercup: behavioral/workflows-spec.el (30 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | Schema loading > loads schema with operation-specific paths | ○ | | |
| 2 | Schema loading > merges cloud config with defaults | ○ | | |
| 3 | Schema loading > merges security config with defaults | ○ | | |
| 4 | File operation validation > allows read operation within read scope | ○ | | |
| 5 | File operation validation > denies read operation outside scope | ○ | | |
| 6 | File operation validation > denies access to paths in deny list despite broad permissions | ○ | | |
| 7 | Operation-specific path permissions > validates write operation requires write scope | ○ | | |
| 8 | Operation-specific path permissions > validates execute operation requires execute scope | ○ | | |
| 9 | Operation-specific path permissions > validates modify operation requires modify or write scope | ○ | | |
| 10 | File operation validation with glob patterns > matches paths against glob patterns | ○ | | |
| 11 | File operation validation with glob patterns > validates multiple file operations independently | ○ | | |
| 12 | Cloud authentication > loads cloud config with auth-detection setting | ○ | | |
| 13 | Cloud authentication > loads cloud config with allowed-providers | ○ | | |
| 14 | Cloud authentication > validates cloud auth against config | ○ | | |
| 15 | File path validation with deny precedence > allows file access within read scope | ○ | | |
| 16 | File path validation with deny precedence > denies file access outside scope | ○ | | |
| 17 | File path validation with deny precedence > denies access to paths in deny list despite broad permissions | ○ | | |
| 18 | Operation-specific path validation > validates read operation requires read or write scope | ○ | | |
| 19 | Operation-specific path validation > validates write operation requires write scope | ○ | | |
| 20 | Operation-specific path validation > validates execute operation requires execute scope | ○ | | |
| 21 | Operation-specific path validation > validates modify operation requires modify or write scope | ○ | | |
| 22 | Parse completeness > validates complete parse with strict mode | ○ | | |
| 23 | Parse completeness > validates permissive mode allows incomplete parse | ○ | | |
| 24 | Error message structure > validates operation returns structured error for path out of scope | ○ | | |
| 25 | Error message structure > validates operation returns nil for success | ○ | | |
| 26 | Multiple file operations > validates each operation independently | ○ | | |
| 27 | Multiple file operations > fails if any operation is out of scope | ○ | | |

## Buttercup: integration/combined-validation-spec.el (5 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | pipeline + file operations > validates both pipeline commands and file paths | ○ | | |
| 2 | file operations + cloud auth > validates both file paths and cloud auth policy | ○ | | |
| 3 | pipeline + file operations + cloud auth > validates all three layers together | ○ | | |
| 4 | parse incomplete with file operations > fails on parse before checking file operations | ○ | | |
| 5 | coverage warning > warns about low coverage but allows command | ○ | PENDING | |

## Buttercup: integration/command-execution-spec.el (7 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | successful execution > returns output and zero exit code | ○ | | |
| 2 | timeout handling > returns timeout error when command exceeds timeout | ○ | PENDING | |
| 3 | output truncation > truncates output over 10000 chars and sets truncated flag | ○ | | |
| 4 | absolute path warnings > warns when command contains absolute paths | ○ | PENDING | |
| 5 | absolute path warnings > does not warn for relative paths | ○ | PENDING | |
| 6 | non-zero exit codes > captures non-zero exit code but still returns output | ○ | | |
| 7 | execution errors > catches and returns execution errors | ○ | | |

## Buttercup: integration/pipeline-orchestration-spec.el (10 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | full pipeline success > passes all 7 stages for valid command | ○ | | |
| 2 | stage 1: parse completeness > fails when parse incomplete and enforce=true | ○ | | |
| 3 | stage 1: parse completeness > warns but continues when parse incomplete and enforce=false | ○ | | |
| 4 | stage 2-3: pipeline command extraction > fails on denied command in pipeline | ○ | | |
| 5 | stage 4: file operations validation > fails on file operation out of scope | ○ | | |
| 6 | stage 4: file operations validation > passes when all file operations in scope | ○ | | |
| 7 | stage 5: cloud auth validation > fails when cloud auth denied | ○ | PENDING | |
| 8 | stage 5: cloud auth validation > warns when cloud auth detected and mode=warn | ○ | PENDING | |
| 9 | stage 6: coverage threshold > warns but does not fail when coverage below threshold | ○ | PENDING | |
| 10 | early exit behavior > exits at first failing stage | ○ | | |

## Buttercup: unit/config-spec.el (12 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | jf/gptel-scope--load-schema > merges provided paths with defaults | → moved | scope/test/core/config-loading-spec.el | Same test |
| 2 | jf/gptel-scope--load-schema > uses defaults when sections missing | → moved | scope/test/core/config-loading-spec.el | Same test |
| 3 | jf/gptel-scope--load-schema > preserves bash-tools deny list with normalized keys | → moved | scope/test/core/config-loading-spec.el | Same test |
| 4 | jf/gptel-scope--load-cloud-config > loads auth-detection setting | → moved | scope/test/core/config-loading-spec.el | Same test |
| 5 | jf/gptel-scope--load-cloud-config > returns nil for nil config | → moved | scope/test/core/config-loading-spec.el | Same test |
| 6 | jf/gptel-scope--load-security-config > loads both security settings | → moved | scope/test/core/config-loading-spec.el | Same test |
| 7 | jf/gptel-scope--load-security-config > returns nil for nil config | → moved | scope/test/core/config-loading-spec.el | Same test |
| 8 | jf/gptel-scope--infer-validation-type > infers path validation for read_file tool | → moved | scope/test/core/tool-routing-spec.el | Expanded with more tools |
| 9 | jf/gptel-scope--infer-validation-type > infers bash validation for run_bash_command tool | → moved | scope/test/core/tool-routing-spec.el | Expanded with more tools |
| 10 | jf/gptel-scope--extract-pipeline-commands > extracts all commands from pipeline | → moved | scope/test/core/config-loading-spec.el | Same test |
| 11 | jf/gptel-scope--extract-pipeline-commands > returns empty list for no commands | → moved | scope/test/core/config-loading-spec.el | Same test |
| 12 | jf/gptel-scope--extract-pipeline-commands > handles missing command-name gracefully | → moved | scope/test/core/config-loading-spec.el | Same test |

## Buttercup: unit/validators-spec.el (17 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | validate-operation > read > allows read if in read patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 2 | validate-operation > read > allows read if in write patterns (write includes read) | → moved | scope/test/core/path-validation-spec.el | Same test |
| 3 | validate-operation > read > denies read if not in read or write patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 4 | validate-operation > write > allows write if in write patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 5 | validate-operation > write > denies write if not in write patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 6 | validate-operation > modify > allows modify if in modify patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 7 | validate-operation > modify > allows modify if in write patterns (write includes modify) | → moved | scope/test/core/path-validation-spec.el | Same test |
| 8 | validate-operation > execute > requires explicit execute permission | → moved | scope/test/core/path-validation-spec.el | Same test |
| 9 | validate-operation > execute > denies execute even if in write patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 10 | validate-operation > deny > denies access even if in allow patterns | → moved | scope/test/core/path-validation-spec.el | Same test |
| 11 | validate-operation > deny > deny overrides write permission | → moved | scope/test/core/path-validation-spec.el | Same test |
| 12 | validate-cloud-config > accepts valid 'allow' mode | ○ | | Cloud config tests stay for emacs-7kqz bead |
| 13 | validate-cloud-config > accepts valid 'warn' mode | ○ | | Cloud config tests stay for emacs-7kqz bead |
| 14 | validate-cloud-config > accepts valid 'deny' mode | ○ | | Cloud config tests stay for emacs-7kqz bead |
| 15 | validate-cloud-config > rejects invalid mode | ○ | | Cloud config tests stay for emacs-7kqz bead |
| 16 | validate-cloud-config > handles nil config | ○ | | Cloud config tests stay for emacs-7kqz bead |
| 17 | validate-security-config > accepts valid boolean enforce-parse-complete | → moved | scope/test/core/path-validation-spec.el | Same test |

## Buttercup: unit/helpers-spec.el (28 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | glob-to-regex > single star > converts * to [^/]* | → moved | scope/test/core/path-validation-spec.el | Same test |
| 2 | glob-to-regex > single star > handles multiple single stars | → moved | scope/test/core/path-validation-spec.el | Same test |
| 3 | glob-to-regex > recursive /**/ > converts /**/ to /(?:.*/)? | → moved | scope/test/core/path-validation-spec.el | Same test |
| 4 | glob-to-regex > recursive /**/ > handles middle recursive wildcard | → moved | scope/test/core/path-validation-spec.el | Same test |
| 5 | glob-to-regex > recursive at end > converts /** at end to /.* | → moved | scope/test/core/path-validation-spec.el | Same test |
| 6 | glob-to-regex > recursive at end > matches everything after slash | → moved | scope/test/core/path-validation-spec.el | Same test |
| 7 | glob-to-regex > literal characters > escapes special regex characters | → moved | scope/test/core/path-validation-spec.el | Same test |
| 8 | glob-to-regex > literal characters > handles paths with dashes and underscores | → moved | scope/test/core/path-validation-spec.el | Same test |
| 9 | glob-to-regex > edge cases > handles empty pattern | → moved | scope/test/core/path-validation-spec.el | Same test |
| 10 | glob-to-regex > edge cases > handles pattern with only ** | → moved | scope/test/core/path-validation-spec.el | Same test |
| 11 | glob-to-regex > edge cases > anchors pattern with ^ and $ | → moved | scope/test/core/path-validation-spec.el | Same test |
| 12 | glob-match-p > exact matches > matches exact path | → moved | scope/test/core/path-validation-spec.el | Same test |
| 13 | glob-match-p > exact matches > rejects different path | → moved | scope/test/core/path-validation-spec.el | Same test |
| 14 | glob-match-p > single star > matches single directory component | → moved | scope/test/core/path-validation-spec.el | Same test |
| 15 | glob-match-p > single star > does not match across slashes | → moved | scope/test/core/path-validation-spec.el | Same test |
| 16 | glob-match-p > recursive > matches zero directories with /**/ | → moved | scope/test/core/path-validation-spec.el | Same test |
| 17 | glob-match-p > recursive > matches one directory with /**/ | → moved | scope/test/core/path-validation-spec.el | Same test |
| 18 | glob-match-p > recursive > matches multiple directories with /**/ | → moved | scope/test/core/path-validation-spec.el | Same test |
| 19 | glob-match-p > recursive > matches everything with /** at end | → moved | scope/test/core/path-validation-spec.el | Same test |
| 20 | glob-match-p > case sensitivity > is case-sensitive by default | → moved | scope/test/core/path-validation-spec.el | Same test |
| 21 | path-matches-any-pattern-p > returns t if path matches first pattern | → moved | scope/test/core/path-validation-spec.el | Same test |
| 22 | path-matches-any-pattern-p > returns t if path matches second pattern | → moved | scope/test/core/path-validation-spec.el | Same test |
| 23 | path-matches-any-pattern-p > returns nil if no patterns match | → moved | scope/test/core/path-validation-spec.el | Same test |
| 24 | path-matches-any-pattern-p > handles empty pattern list | → moved | scope/test/core/path-validation-spec.el | Same test |
| 25 | path-matches-any-pattern-p > normalizes path before matching | → moved | scope/test/core/path-validation-spec.el | Same test |
| 26 | normalize-keys > converts snake_case to kebab-case | → moved | scope/test/core/path-validation-spec.el | Same test |
| 27 | normalize-keys > recursively normalizes nested plists | → moved | scope/test/core/path-validation-spec.el | Same test |
| 28 | normalize-keys > preserves non-plist values | → moved | scope/test/core/path-validation-spec.el | Same test |

## Buttercup: unit/no-op-allowance-spec.el (15 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | Commands with no file operations are allowed by default > allows version check commands | ○ | | |
| 2 | Commands with no file operations are allowed by default > allows help flag commands | ○ | | |
| 3 | Commands with no file operations are allowed by default > allows unknown commands with no operations | ○ | | |
| 4 | Commands with no file operations are allowed by default > blocks no-op commands in deny list | ○ | | |
| 5 | No-op validation occurs before file operation validation > short-circuits validation when no file operations | ○ | | |
| 6 | No-op validation occurs before file operation validation > proceeds to path validation when operations exist | ○ | | |
| 7 | Empty operations list defines no-op commands > classifies empty filesystem operations as no-op | ○ | | |
| 8 | Empty operations list defines no-op commands > classifies non-empty filesystem operations as not no-op | ○ | | |
| 9 | Empty operations list defines no-op commands > classifies no filesystem domain as no-op | ○ | | |
| 10 | No-op allowance is independent of command name > allows unknown command names with no operations | ○ | | |
| 11 | No-op allowance is independent of command name > allows commands without checking categories when no operations | ○ | | |
| 12 | No-op check applies to all pipeline commands > allows simple pipeline with no operations | ○ | | |
| 13 | No-op check applies to all pipeline commands > validates pipeline with file operations in later stage | ○ | | |
| 14 | Error messages distinguish no-op allowance > returns success without mentioning categories for no-op commands | ○ | | |
| 15 | Error messages distinguish no-op allowance > references operation and path for validation failures | ○ | | |

## Buttercup: unit/validators-spec.el (continued - parse/error tests) (7 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | validate-security-config > accepts valid threshold in range | → moved | scope/test/core/path-validation-spec.el | Same test |
| 2 | validate-security-config > rejects threshold below 0.0 | → moved | scope/test/core/path-validation-spec.el | Same test |
| 3 | validate-security-config > rejects threshold above 1.0 | → moved | scope/test/core/path-validation-spec.el | Same test |
| 4 | validate-security-config > accepts edge values 0.0 and 1.0 | → moved | scope/test/core/path-validation-spec.el | Same test |
| 5 | validate-parse-completeness > returns nil for complete parse with strict mode | ○ | | Parse completeness stays for semantic/ (emacs-7kqz bead) |
| 6 | validate-parse-completeness > returns error for incomplete parse with strict mode | ○ | | Parse completeness stays for semantic/ (emacs-7kqz bead) |
| 7 | validate-parse-completeness > includes error details in message | ○ | | Parse completeness stays for semantic/ (emacs-7kqz bead) |

## Buttercup: unit/validators-spec.el (error structure tests) (3 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | validation error structure > includes all required fields for path_out_of_scope | → moved | scope/test/core/path-validation-spec.el | Same test |
| 2 | validation error structure > includes all required fields for path_denied | → moved | scope/test/core/path-validation-spec.el | Same test |
| 3 | validation error structure > formats human-readable messages | → moved | scope/test/core/path-validation-spec.el | Same test |

## Buttercup: test-arg-extraction-spec.el (5 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | extracts single argument from plist correctly | ○ | | |
| 2 | extracts multiple arguments in correct order | ○ | | |
| 3 | handles missing arguments | ○ | | |
| 4 | demonstrates correct apply usage with callback | ○ | | |
| 5 | reproduces wrong-number-of-arguments when missing callback | ○ | | |

## Buttercup: scope/test/integration-validator-expansion-spec.el (7 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | Path validator integration > transforms path validator denial into valid violation-info | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |
| 2 | Path validator integration > handles denied-pattern reason from path validator | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |
| 3 | Pattern validator integration > transforms pattern validator denial into valid violation-info | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |
| 4 | Bash validator integration > handles legacy validation-error format with :message field | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |
| 5 | Bash validator integration > handles cloud auth denial with :message field | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |
| 6 | Format priority > prioritizes :reason when both fields present | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |
| 7 | Transformation robustness > returns nil for :reason when neither :reason nor :message present | ⊕ merged | scope/test/expansion/expansion-integration-spec.el | Absorbed into expansion integration |

## Buttercup: scope/test/metadata-spec.el (24 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | plist structure > returns a plist | ○ | | |
| 2 | plist structure > contains all required keys | ○ | | |
| 3 | plist structure > has exactly 6 key-value pairs | ○ | | |
| 4 | path handling > expands relative paths to absolute | ○ | | |
| 5 | path handling > preserves absolute paths | ○ | | |
| 6 | path handling > expands ~ to home directory | ○ | | |
| 7 | path handling > resolves symlinks for :real-path | ○ | | |
| 8 | file existence > detects existing files | ○ | | |
| 9 | file existence > detects existing directories | ○ | | |
| 10 | file existence > detects non-existent files | ○ | | |
| 11 | git repository detection > detects git repository for files in repo | ○ | | |
| 12 | git repository detection > returns nil for files outside git repo | ○ | | |
| 13 | git repository detection > git-repo path ends with repository root directory | ○ | FAILED | Pre-existing: expects "emacs/" but gets worktree name |
| 14 | git tracking status > detects git-tracked files | ○ | | |
| 15 | git tracking status > returns nil for untracked files in git repo | ○ | | |
| 16 | git tracking status > returns nil for files outside git repo | ○ | | |
| 17 | git tracking status > git-tracked is nil when git-repo is nil | ○ | | |
| 18 | file type detection > detects regular files | ○ | | |
| 19 | file type detection > detects directories | ○ | | |
| 20 | file type detection > returns 'other for non-existent files | ○ | | |
| 21 | file type detection > detects config directory as directory | ○ | | |
| 22 | graceful degradation > handles non-existent files without errors | ○ | | |
| 23 | graceful degradation > returns complete plist even for non-existent files | ○ | | |
| 24 | graceful degradation > handles paths with special characters | ○ | | |

## Buttercup: scope/test/test-async-flag-spec.el (5 tests)

| # | Test Name | Status | Destination | Notes |
|---|-----------|--------|-------------|-------|
| 1 | async scoped tools > marks async tools as async in the registry | ⊕ merged | scope/test/core/allow-once-spec.el | Absorbed into allow-once; source file deleted |
| 2 | async scoped tools > creates callback-first function signature for async tools | ⊕ merged | scope/test/core/allow-once-spec.el | Absorbed into allow-once; source file deleted |
| 3 | sync scoped tools > does not mark sync tools as async | ⊕ merged | scope/test/core/allow-once-spec.el | Absorbed into allow-once; source file deleted |
| 4 | regression test > read_file should be marked as async | ⊕ merged | scope/test/core/allow-once-spec.el | Absorbed into allow-once; source file deleted |
| 5 | regression test > read_file function should have callback-first signature | ⊕ merged | scope/test/core/allow-once-spec.el | Absorbed into allow-once; source file deleted |

---

## Summary

| Source | Tests | Passed | Pending | Failed |
|--------|-------|--------|---------|--------|
| ERT tools/test/integration/ | 172 | 172 | 0 | 0 |
| Buttercup tools/test/ | 262 | 246 | 8 | 0 |
| Buttercup scope/test/ | 36 | 35 | 0 | 1 |
| **TOTAL** | **470** | **453** | **8** | **1** |

Post-migration target: ~400-450 tests (dedup reduces count), 0 regressions in coverage.

## Bead emacs-bdvf: Move scope-shell-tools to scope/ directory

**Change:** Moved `scope-shell-tools.{org,el}` from `config/gptel/tools/` to `config/gptel/scope/`.
**Impact on tests:** No test logic or assertions changed. Updated require paths in 23 test files to point to new location. All 172 ERT tests pass. 304/306 Buttercup specs pass (2 pre-existing failures unrelated to move).

## Bead emacs-6v50: Consolidate expansion tests into scope/test/expansion/

**Change:** Consolidated scope expansion UI tests into `scope/test/expansion/`.

**New files created:**
- `scope/test/expansion/expansion-ui-spec.el` (14 tests): 7 moved from `scope-expansion-spec.el` + 5 moved from `filesystem-tools-scope-expansion-spec.el` (Transient section) + 2 existing inline tests
- `scope/test/expansion/expansion-integration-spec.el` (11 tests): 7 absorbed from `integration-validator-expansion-spec.el` + 4 new end-to-end scenarios

**Deleted:**
- `scope/test/integration-validator-expansion-spec.el` (absorbed into expansion-integration-spec.el)

**New tests (✚):**
| # | Test Name | File | Notes |
|---|-----------|------|-------|
| 1 | path validation failure triggers expansion, add-to-scope approves | expansion-integration-spec.el | End-to-end path validator flow |
| 2 | bash validation failure triggers expansion, allow-once grants access | expansion-integration-spec.el | End-to-end bash validator flow |
| 3 | denial flow preserves original validation error context | expansion-integration-spec.el | End-to-end denial flow |
| 4 | all validator types produce compatible violation-info for expansion UI | expansion-integration-spec.el | Cross-validator format consistency |

**Impact:** All 25 expansion tests pass. Full suite passes with no regressions. Source files in tools/test/ NOT deleted (handled by later bead emacs-s88q).

## Bead emacs-p4wm: Consolidate scope core tests into scope/test/core/

**Change:** Consolidated scope-core validation tests from tools/test/ into scope/test/core/. Converted ERT tests to Buttercup. Created 4 new consolidated spec files.

**New files created:**
- `scope/test/core/config-loading-spec.el` - Schema loading, defaults, YAML normalization (~50 tests from test-schema.el + config-spec.el)
- `scope/test/core/path-validation-spec.el` - Operation validation, glob matching, key normalization, error structure (~46 tests from validators-spec.el + helpers-spec.el)
- `scope/test/core/tool-routing-spec.el` - Validation type inference per tool category (~11 tests, expanded from config-spec.el)
- `scope/test/core/allow-once-spec.el` - Allow-once lifecycle + async flag propagation (~13 tests, absorbed test-async-flag-spec.el)

**Deleted:** `scope/test/test-async-flag-spec.el` (absorbed into allow-once-spec.el)

**Impact:** 141 Buttercup specs in scope/test/core/, all passing. Full test suite passes with no regressions. Source files in tools/test/ NOT deleted (later bead handles cleanup).
