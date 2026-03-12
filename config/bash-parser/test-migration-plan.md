# Test Migration Plan - Tracking Document

Generated: 2026-03-07
Baseline snapshot: config/experiments/bash-parser/test-results.txt

## Critical: Individual Test Accountability

**EVERY test case MUST be tracked.** When moving, removing, or modifying ANY test:
1. Update the test's status in the "Individual Test Tracking" section below
2. Document the reason for any removal or modification
3. Update destination paths for moved tests
4. No test may be removed without documented justification

**Agents working on migration phases:** You MUST update this document for every test you touch. This is non-negotiable.

## Test Count Explanation

**Executed Tests (Baseline):** 683 tests
- This is the actual number of tests run in the test suite
- Source: test-results.txt baseline snapshot

**Defined Tests (Inventory):** 548 explicit test definitions
- Count of `(ert-deftest ...)` forms in test files
- Source: test-inventory-baseline.txt

**Difference:** 135 corpus-generated tests
- Corpus data files (corpus-parse-*.el) contain test data as plists
- Corpus runner files (test-corpus-*.el) dynamically generate tests from that data
- These don't appear as ert-deftest forms but execute as real tests

**For Migration Purposes:**
- Track ALL 683 executed tests (see Individual Test Tracking below)
- File moves track the 44 source files
- Both views are necessary for complete accountability

## Migration Status

### Phase 0: Baseline ✓
- [x] Snapshot generated: test-results.txt (683 tests captured)
- [x] Test inventory created: test-inventory-baseline.txt (548 deftest forms)
- [x] Tracking document created with individual test accountability
- [x] Test count discrepancy explained (683 executed vs 548 defined)

### Phase 1: Structure & Corpus ✓ COMPLETED
- [x] Directory structure created
- [x] Corpus data files moved (7 files)
- [x] Corpus runners moved (3 files)
- [x] Corpus index moved (1 file)
- [x] Individual test tracking updated for moved tests

### Phase 2: Behavioral & Unit ✓ COMPLETED
- [x] Behavioral tests moved (4 files, 129 tests)
- [x] Unit/core tests moved (1 file, 35 tests)
- [x] Unit/semantic tests moved (5 files, 146 tests)
- [x] Unit/analysis tests moved (3 files, 59 tests)
- [x] Individual test tracking updated for each moved test

### Phase 3: Integration & Construct ✓ COMPLETED
- [x] Integration tests moved (4 files, 48 tests)
- [x] Construct tests moved (5 files, 94 tests)
- [x] Individual test tracking updated for each moved test

### Phase 4: Redundancy & Deprecated ✓ COMPLETED
- [x] Deprecated tests removed: 0 tests (corpus tests already removed in prior phases)
- [x] Redundancy analysis completed (see test/docs/REDUNDANCY-ANALYSIS.md)
- [x] Bead verification tests integrated (3 tests moved to integration/)
- [x] Individual test tracking updated with movement reasons
- [x] Final test count: 683 tests (548 explicit + 135 corpus-generated)

### Phase 5: Final Documentation ✓ COMPLETED
- [x] README files created
- [x] Migration coverage report created
- [x] CLAUDE.md updated
- [x] Individual test tracking verified complete

## File Inventory & Destinations

### Phase 1: Corpus Files (12 files)

**Corpus Data Files (7 files → corpus/data/)**
- test/corpus-parse-command-substitution.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed
- test/corpus-parse-conditional.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed
- test/corpus-parse-for-loop.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed
- test/corpus-parse-heredoc.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed
- test/corpus-parse-process-substitution.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed
- test/corpus-parse-combined-patterns.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed
- test/corpus-parse-llm-scenarios.el → corpus/data/ | 0 deftest | MOVE | ✓ Completed

**Corpus Runners (3 files → corpus/runners/)**
- test/test-corpus-parse.el → corpus/runners/ | 4 deftest | MOVE | ✓ Completed
- test/test-corpus-file-operations.el → corpus/runners/ | 0 deftest (generates ~98 tests) | MOVE | ✓ Completed
- test/test-corpus-script-execution.el → corpus/runners/ | 30 deftest | MOVE | ✓ Completed

**Corpus Index (1 file → corpus/)**
- test/corpus-index.el → corpus/ | 0 deftest | MOVE | ✓ Completed

**Infrastructure Files (stay at root)**
- test/test-helper.el | 0 deftest | KEEP | N/A
- test/test-assertions.el | 0 deftest | KEEP | N/A

### Phase 2: Behavioral & Unit Tests (13 files, 369 explicit tests)

**Behavioral Tests (4 files → behavioral/)**
- test/test-file-operations.el → behavioral/ | 49 tests | MOVE | ✓ Completed (Phase 2)
- test/test-security-validator.el → behavioral/ | 60 tests | MOVE | ✓ Completed (Phase 2)
- test/test-command-injection.el → behavioral/ | 7 tests | MOVE | ✓ Completed (Phase 2)
- test/test-backward-compatibility.el → behavioral/ | 13 tests | MOVE | ✓ Completed (Phase 2)

**Unit - Core Layer (1 file → unit/core/)**
- test/test-glob-matching.el → unit/core/ | 35 tests | MOVE | ✓ Completed (Phase 2)

**Unit - Semantic Layer (5 files → unit/semantic/)**
- test/test-command-semantics.el → unit/semantic/ | 68 tests | MOVE | ✓ Completed (Phase 2)
- test/test-bash-parser-semantics.el → unit/semantic/ | 27 tests | MOVE | ✓ Completed (Phase 2)
- test/test-semantics-validation.el → unit/semantic/ | 11 tests | MOVE | ✓ Completed (Phase 2)
- test/test-variable-resolution-unit.el → unit/semantic/ | 36 tests | MOVE | ✓ Completed (Phase 2)
- test/test-variable-chain-ampersand.el → unit/semantic/ | 4 tests | MOVE | ✓ Completed (Phase 2)

**Unit - Analysis Layer (3 files → unit/analysis/)**
- test/test-bash-parser-recursive.el → unit/analysis/ | 25 tests | MOVE | ✓ Completed (Phase 2)
- test/test-parser-extension.el → unit/analysis/ | 26 tests | MOVE | ✓ Completed (Phase 2)
- test/test-input-validation.el → unit/analysis/ | 8 tests | MOVE | ✓ Completed (Phase 2)

### Phase 3: Integration & Construct Tests (9 files, 142 tests)

**Integration Tests (4 files → integration/)**
- test/test-command-substitution.el → integration/ | 28 tests | MOVE | ✓ Completed (Phase 3)
- test/test-pattern-flow.el → integration/ | 10 tests | MOVE | ✓ Completed (Phase 3)
- test/test-expect-file-ops-validation.el → integration/ | 3 tests | MOVE | ✓ Completed (Phase 3)
- test/test-treesitter-workarounds.el → integration/ | 7 tests | MOVE | ✓ Completed (Phase 3)

**Construct Tests (5 files → construct/)**
- test/test-loop-context.el → construct/ | 9 tests | MOVE | ✓ Completed (Phase 3)
- test/test-conditional-context.el → construct/ | 10 tests | MOVE | ✓ Completed (Phase 3)
- test/test-heredoc-context.el → construct/ | 10 tests | MOVE | ✓ Completed (Phase 3)
- test/test-directory-changing-commands.el → construct/ | 36 tests | MOVE | ✓ Completed (Phase 3)
- test/test-pwd-directory-context.el → construct/ | 29 tests | MOVE | ✓ Completed (Phase 3)

### Phase 4: Bead Verification Tests (1 file, 3 tests)

**Bead Verification Test → Integration (Phase 4)**
- test/test-bead-3kgg-verification.el → integration/test-find-exec-patterns.el | 3 tests | MOVE | ✓ Completed (Phase 4)
  - Note: Integrated verification tests into regular test suite
  - Tests renamed to remove bead-specific prefixes
  - Decision: Keep as integration tests - provide unique coverage for find -exec patterns

## Individual Test Tracking (683 Tests)

**Instructions for Agents:**
- Update Status column when you move/remove/modify a test
- Status values: ⏳ Pending | ✓ Moved | ✗ Removed | ⚠ Modified | ➕ Added
- For Moved: Update Destination column
- For Removed: Document reason in Notes column
- For Modified: Document what changed in Notes column
- Never remove a test without documenting justification

**Format:**
`Test Name | Source File | Status | Destination | Notes`

---

### test-backward-compatibility.el (13 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-backward-compat-simple-read | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-simple-write | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-simple-delete | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-copy-command | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-pipeline | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-variable-resolution | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-unresolved-variables | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-corpus-read-operations | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-corpus-write-operations | ✓ Moved | behavioral/ | Phase 2 |
| test-backward-compat-corpus-delete-operations | ✓ Moved | behavioral/ | Phase 2 |
| test-feature-detection-recursive-analysis | ✓ Moved | behavioral/ | Phase 2 |
| test-feature-detection-pattern-flow | ✓ Moved | behavioral/ | Phase 2 |
| test-feature-detection-unknown-feature | ✓ Moved | behavioral/ | Phase 2 |

### test-bash-parser-recursive.el (25 tests → unit/analysis/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-recursive-single-substitution | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-substitution-with-flags | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-substitution-grep | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-nested-substitution | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-nested-find | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-depth-limiting | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-depth-with-deep-nesting | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-depth-abort | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-command-substitution-in-args | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-mixed-quotes-substitution | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-basic | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-nested | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-command-substitution | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-pipeline | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-multiple-ops | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-invalid-syntax | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-empty-command | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-only-whitespace | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-disabled | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-depth-limit | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-extraction-variable-expansion | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-unmatched-quotes | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-empty-quotes | ✓ Moved | unit/analysis/ | Phase 2 |
| test-arithmetic-expansion-still-works | ✓ Moved | unit/analysis/ | Phase 2 |
| test-recursive-analysis-skips-functions | ✓ Moved | unit/analysis/ | Phase 2 |

### test-bash-parser-semantics.el (27 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-file-operation-basic-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-basic-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-basic-delete | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-copy | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-move | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-make-directory | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-remove-directory | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-find-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-find-delete | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-pipeline-read-then-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-pipeline-intermediate-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-command-substitution-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-nested-command-substitution | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-variable-in-path | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-no-variable-resolution-literal | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-glob-expansion-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-glob-expansion-delete | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-redirect-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-redirect-append | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-redirect-heredoc | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-brace-expansion | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-tilde-expansion | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-multiple-ops-same-command | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-complex-pipeline-multiple-ops | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-find-exec-complex | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-ls-is-no-op | ✓ Moved | unit/semantic/ | Phase 2 |
| test-file-operation-echo-is-no-op | ✓ Moved | unit/semantic/ | Phase 2 |

### test-bead-3kgg-verification.el (3 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-bead-3kgg-find-with-exec-rm → test-find-exec-rm-pattern | ✓ Moved | integration/ | Phase 4 - Integrated from bead verification |
| test-bead-3kgg-find-with-multiple-exec-blocks → test-find-multiple-exec-blocks | ✓ Moved | integration/ | Phase 4 - Integrated from bead verification |
| test-bead-3kgg-workspace-validation-scenario → test-find-exec-workspace-validation | ✓ Moved | integration/ | Phase 4 - Integrated from bead verification |

### test-command-injection.el (7 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-injection-basic-semicolon | ✓ Moved | behavioral/ | Phase 2 |
| test-injection-ampersand-background | ✓ Moved | behavioral/ | Phase 2 |
| test-injection-pipe-to-shell | ✓ Moved | behavioral/ | Phase 2 |
| test-injection-command-substitution | ✓ Moved | behavioral/ | Phase 2 |
| test-injection-nested-quotes | ✓ Moved | behavioral/ | Phase 2 |
| test-injection-multiline-commands | ✓ Moved | behavioral/ | Phase 2 |
| test-injection-metadata-validation | ✓ Moved | behavioral/ | Phase 2 |

### test-command-semantics.el (68 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-command-semantic-read-cat | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-head | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-tail | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-grep | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-less | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-more | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-write-echo-redirect | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-write-printf-redirect | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-write-tee | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-write-dd | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-delete-rm | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-delete-rm-rf | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-delete-find-delete | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-copy-cp | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-copy-cp-r | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-move-mv | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-archive-tar-c | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-archive-tar-x | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-archive-zip | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-archive-unzip | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-modify-sed | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-modify-awk | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-modify-perl-pie | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-no-op-echo-no-redirect | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-no-op-ls | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-no-op-pwd | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-no-op-cd | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-vim-readonly | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-write-vim-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-modify-vim-substitute | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-emacs-find-file | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-write-emacs-write-file | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-read-nano | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-directory-mkdir | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-directory-rmdir | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-directory-rm-rf | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-read-log | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-read-show | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-read-diff | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-write-add | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-write-commit | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-write-checkout | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-delete-rm | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-git-delete-clean | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-rsync-copy | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-scp-copy | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-curl-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-wget-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-curl-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-find-read-plain | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-find-read-with-cat | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-find-delete-plain | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-unknown-command | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-complex-git-chain | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-docker-read-logs | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-docker-write-cp | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-docker-exec-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-npm-read-list | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-npm-write-install | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-make-read-targets | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-make-write-build | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-python-read-module | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-python-write-pip-install | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-jq-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-jq-modify | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-xargs-passthrough | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-sort-no-op | ✓ Moved | unit/semantic/ | Phase 2 |
| test-command-semantic-uniq-no-op | ✓ Moved | unit/semantic/ | Phase 2 |

### test-command-substitution.el (28 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-command-substitution-basic | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-argument | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-variable | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-nested | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-deeply-nested | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-pipeline | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-glob | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-multiple-in-command | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-heredoc | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-condition | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-backticks | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-mixed-quotes | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-empty | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-whitespace-handling | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-redirect | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-for-loop | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-special-chars | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-multiline | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-escaped-backticks | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-array | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-heredoc-delimiter | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-preserves-variables | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-complex-nesting | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-arithmetic | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-in-case-statement | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-error-handling | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-preserves-context | ✓ Moved | integration/ | Phase 3 |
| test-command-substitution-with-process-substitution | ✓ Moved | integration/ | Phase 3 |

### test-conditional-context.el (10 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-conditional-basic-if | ✓ Moved | construct/ | Phase 3 |
| test-conditional-if-else | ✓ Moved | construct/ | Phase 3 |
| test-conditional-if-elif-else | ✓ Moved | construct/ | Phase 3 |
| test-conditional-nested-if | ✓ Moved | construct/ | Phase 3 |
| test-conditional-test-command | ✓ Moved | construct/ | Phase 3 |
| test-conditional-bracket-test | ✓ Moved | construct/ | Phase 3 |
| test-conditional-double-bracket | ✓ Moved | construct/ | Phase 3 |
| test-conditional-with-file-operations | ✓ Moved | construct/ | Phase 3 |
| test-conditional-case-statement | ✓ Moved | construct/ | Phase 3 |
| test-conditional-complex-nested | ✓ Moved | construct/ | Phase 3 |

### test-directory-changing-commands.el (36 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-directory-context-basic-cd | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-absolute | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-relative | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-subshell | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-in-command-substitution | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-pushd | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-popd | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-pushd-popd-sequence | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-pipeline | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-logical-and | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-logical-or | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-semicolon | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-background | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-in-function | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-in-if-block | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-in-for-loop | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-in-while-loop | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-variable | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-tilde | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-hyphen | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-multiple-cd | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-file-operations | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-error-handling | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-quotes | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-spaces | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-special-chars | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-relative-dot | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-relative-dotdot | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-complex-path | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-symbolic-link | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-env-var | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-nested-subshells | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-preserves-outside-subshell | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-heredoc | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-with-process-substitution | ✓ Moved | construct/ | Phase 3 |
| test-directory-context-cd-integration | ✓ Moved | construct/ | Phase 3 |

### test-expect-file-ops-validation.el (3 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-expect-file-ops-validation-basic | ✓ Moved | integration/ | Phase 3 |
| test-expect-file-ops-validation-multiple | ✓ Moved | integration/ | Phase 3 |
| test-expect-file-ops-validation-complex | ✓ Moved | integration/ | Phase 3 |

### test-file-operations.el (49 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-extraction-simple-read-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-simple-write-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-simple-delete-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-copy-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-move-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-tar-archive-create | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-tar-archive-extract | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-multiple-paths-same-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-pipeline-read-write | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-pipeline-intermediate-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-command-substitution-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-nested-command-substitution | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-variable-in-path | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-unresolved-variable | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-multiple-variables | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-variable-and-literal-mix | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-glob-pattern-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-glob-pattern-delete | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-redirect-write | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-redirect-append | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-redirect-heredoc | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-redirect-input | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-brace-expansion | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-tilde-expansion | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-find-exec-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-find-exec-delete | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-complex-find-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-sed-modify | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-awk-modify | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-vim-edit | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-git-operations | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-rsync-copy | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-docker-operations | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-npm-operations | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-curl-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-wget-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-no-ops-echo | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-no-ops-ls | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-empty-command | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-only-whitespace | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-complex-multiline | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-confidence-scoring-read | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-confidence-scoring-variable | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-confidence-scoring-glob | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-metadata-structure | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-metadata-completeness | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-result-format | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-error-propagation | ✓ Moved | behavioral/ | Phase 2 |
| test-extraction-disabled-recursive | ✓ Moved | behavioral/ | Phase 2 |

### test-glob-matching.el (35 tests → unit/core/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-glob-basic-wildcard | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-question-mark | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-character-class | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-negated-character-class | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-range-character-class | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-multiple-wildcards | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-leading-wildcard | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-trailing-wildcard | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-middle-wildcard | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-literal-match | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-no-match | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-path-separator | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-relative-path | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-absolute-path | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-tilde-expansion | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-with-extension | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-multiple-extensions | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-hidden-files | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-dotfiles-explicit | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-complex-pattern | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-special-chars-escape | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-empty-pattern | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-whitespace-pattern | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-pattern-with-spaces | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-after-command | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-in-middle-of-arg | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-multiple-patterns-same-arg | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-in-quoted-string | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-in-double-quotes | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-mixed-quotes | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-brace-expansion | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-extended-glob | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-with-redirect | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-with-pipeline | ✓ Moved | unit/core/ | Phase 2 |
| test-glob-case-sensitivity | ✓ Moved | unit/core/ | Phase 2 |

### test-heredoc-context.el (10 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-heredoc-basic | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-with-variables | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-quoted-delimiter | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-indented | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-multiple-commands | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-with-command-substitution | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-multiline | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-empty | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-nested-in-function | ✓ Moved | construct/ | Phase 3 |
| test-heredoc-complex-delimiter | ✓ Moved | construct/ | Phase 3 |

### test-input-validation.el (8 tests → unit/analysis/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-input-validation-empty-string | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-whitespace-only | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-nil-input | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-very-long-command | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-special-characters | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-unicode | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-binary-data | ✓ Moved | unit/analysis/ | Phase 2 |
| test-input-validation-newlines | ✓ Moved | unit/analysis/ | Phase 2 |

### test-loop-context.el (9 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-loop-context-basic-for | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-for-in-list | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-for-in-range | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-while-loop | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-until-loop | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-nested-loops | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-with-file-operations | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-with-break-continue | ✓ Moved | construct/ | Phase 3 |
| test-loop-context-complex-iteration | ✓ Moved | construct/ | Phase 3 |

### test-parser-extension.el (26 tests → unit/analysis/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-extension-register-command | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-register-multiple-commands | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-override-builtin | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-custom-semantic-read | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-custom-semantic-write | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-custom-semantic-delete | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-custom-semantic-no-op | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-custom-pattern-handler | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-command-with-flags | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-command-with-complex-args | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-priority-ordering | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-fallback-to-default | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-nil-handler-fallback | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-error-handling | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-handler-returns-nil | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-handler-returns-empty-list | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-handler-modifies-metadata | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-integration-with-pipeline | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-integration-with-variables | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-integration-with-globs | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-chained-handlers | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-conditional-handling | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-context-preservation | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-clear-extensions | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-list-registered | ✓ Moved | unit/analysis/ | Phase 2 |
| test-extension-query-by-command | ✓ Moved | unit/analysis/ | Phase 2 |

### test-pattern-flow.el (10 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-pattern-flow-sequential-commands | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-pipeline-simple | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-pipeline-complex | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-logical-and | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-logical-or | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-background-job | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-subshell | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-command-group | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-mixed-operators | ✓ Moved | integration/ | Phase 3 |
| test-pattern-flow-nested-subshells | ✓ Moved | integration/ | Phase 3 |

### test-pwd-directory-context.el (29 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-pwd-context-basic | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-after-cd | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-in-subshell | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-in-command-substitution | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-variable | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-multiple-pwd-calls | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-pwd-in-pipeline | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-pwd-logical | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-pwd-physical | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-symbolic-link | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-after-pushd | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-after-popd | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-in-function | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-in-if-block | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-in-for-loop | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-in-while-loop | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-nested-subshells | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-preserves-outside-subshell | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-cd-dash | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-cd-tilde | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-cd-dotdot | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-env-var | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-complex-path | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-spaces-in-path | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-special-chars | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-integration-cd-pwd-chain | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-error-handling | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-heredoc | ✓ Moved | construct/ | Phase 3 |
| test-pwd-context-with-process-substitution | ✓ Moved | construct/ | Phase 3 |

### test-security-validator.el (60 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-security-dangerous-rm-root | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-rm-rf-root | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-rm-star-root | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-rm-recursive-slash | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-dd-device | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-mkfs | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-format-device | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-fork-bomb | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-chmod-777-recursive | ✓ Moved | behavioral/ | Phase 2 |
| test-security-dangerous-chown-recursive-root | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-rm-specific-file | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-rm-rf-subdirectory | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-dd-specific-file | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-chmod-specific-file | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-chown-specific-file | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-semicolon | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-ampersand | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-pipe | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-backticks | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-dollar-paren | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-logical-and | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-logical-or | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-redirect-overwrite | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-redirect-append | ✓ Moved | behavioral/ | Phase 2 |
| test-security-injection-heredoc | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-simple-command | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-with-arguments | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-pipeline-no-injection | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-quoted-arguments | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-escaped-chars | ✓ Moved | behavioral/ | Phase 2 |
| test-security-path-traversal-dotdot | ✓ Moved | behavioral/ | Phase 2 |
| test-security-path-traversal-absolute | ✓ Moved | behavioral/ | Phase 2 |
| test-security-path-traversal-tilde | ✓ Moved | behavioral/ | Phase 2 |
| test-security-path-traversal-symbolic-link | ✓ Moved | behavioral/ | Phase 2 |
| test-security-path-traversal-device | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-path-relative | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-path-subdirectory | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-path-workspace | ✓ Moved | behavioral/ | Phase 2 |
| test-security-network-curl-http | ✓ Moved | behavioral/ | Phase 2 |
| test-security-network-wget-http | ✓ Moved | behavioral/ | Phase 2 |
| test-security-network-nc-listen | ✓ Moved | behavioral/ | Phase 2 |
| test-security-network-ssh-command | ✓ Moved | behavioral/ | Phase 2 |
| test-security-network-scp-upload | ✓ Moved | behavioral/ | Phase 2 |
| test-security-network-rsync-remote | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-network-localhost | ✓ Moved | behavioral/ | Phase 2 |
| test-security-safe-network-read-only | ✓ Moved | behavioral/ | Phase 2 |
| test-security-validation-result-structure | ✓ Moved | behavioral/ | Phase 2 |
| test-security-validation-multiple-violations | ✓ Moved | behavioral/ | Phase 2 |
| test-security-validation-empty-command | ✓ Moved | behavioral/ | Phase 2 |
| test-security-validation-nil-input | ✓ Moved | behavioral/ | Phase 2 |
| test-security-combined-dangerous-and-injection | ✓ Moved | behavioral/ | Phase 2 |
| test-security-combined-path-and-network | ✓ Moved | behavioral/ | Phase 2 |
| test-security-combined-multiple-issues | ✓ Moved | behavioral/ | Phase 2 |
| test-security-edge-case-very-long-command | ✓ Moved | behavioral/ | Phase 2 |
| test-security-edge-case-unicode | ✓ Moved | behavioral/ | Phase 2 |
| test-security-edge-case-special-chars | ✓ Moved | behavioral/ | Phase 2 |
| test-security-edge-case-whitespace-variants | ✓ Moved | behavioral/ | Phase 2 |
| test-security-metadata-severity-critical | ✓ Moved | behavioral/ | Phase 2 |
| test-security-metadata-severity-high | ✓ Moved | behavioral/ | Phase 2 |
| test-security-metadata-severity-medium | ✓ Moved | behavioral/ | Phase 2 |

### test-semantics-validation.el (11 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-semantics-validation-valid-read | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-valid-write | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-valid-delete | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-valid-no-op | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-invalid-semantic-type | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-missing-command | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-missing-args | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-empty-result | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-nil-result | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-malformed-result | ✓ Moved | unit/semantic/ | Phase 2 |
| test-semantics-validation-result-consistency | ✓ Moved | unit/semantic/ | Phase 2 |

### test-treesitter-workarounds.el (7 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-treesitter-workaround-malformed-heredoc | ✓ Moved | integration/ | Phase 3 |
| test-treesitter-workaround-incomplete-pipeline | ✓ Moved | integration/ | Phase 3 |
| test-treesitter-workaround-unclosed-quotes | ✓ Moved | integration/ | Phase 3 |
| test-treesitter-workaround-unclosed-command-substitution | ✓ Moved | integration/ | Phase 3 |
| test-treesitter-workaround-invalid-redirect | ✓ Moved | integration/ | Phase 3 |
| test-treesitter-workaround-broken-for-loop | ✓ Moved | integration/ | Phase 3 |
| test-treesitter-workaround-partial-if-statement | ✓ Moved | integration/ | Phase 3 |

### test-variable-chain-ampersand.el (4 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-variable-chain-ampersand-single | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-chain-ampersand-double | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-chain-ampersand-multiple | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-chain-ampersand-mixed-with-pipe | ✓ Moved | unit/semantic/ | Phase 2 |

### test-variable-resolution-unit.el (36 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-variable-resolution-simple | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-braces | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-multiple | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-path | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-concatenated | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-nested | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-with-default | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-with-assignment | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-substring | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-length | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-replacement | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-removal | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-case-modification | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-indirect | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-array-element | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-array-all | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-quotes | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-escaped | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-single-quotes | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-special-parameters | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-positional-parameters | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-unset-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-empty-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-readonly-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-exported-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-local-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-environment-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-home-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-path-variable | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-command-substitution | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-arithmetic | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-conditional | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-in-loop | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-complex-nested | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-metadata-confidence | ✓ Moved | unit/semantic/ | Phase 2 |
| test-variable-resolution-integration | ✓ Moved | unit/semantic/ | Phase 2 |

### test-corpus-parse.el (4 tests → corpus/runners/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-corpus-parse-all-tests | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-parse-by-category | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-parse-by-gap-type | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-parse-real-only | ✓ Moved | corpus/runners/ | Phase 1 complete |

### test-corpus-script-execution.el (30 tests → corpus/runners/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-script-exec-simple-echo | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-file-read | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-file-write | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-file-delete | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-pipeline | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-command-substitution | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-variable-expansion | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-glob-expansion | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-for-loop | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-while-loop | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-if-statement | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-case-statement | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-function-definition | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-function-call | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-heredoc | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-redirect-output | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-redirect-input | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-redirect-append | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-background-job | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-subshell | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-command-group | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-logical-and | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-logical-or | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-arithmetic-expression | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-array-operations | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-string-operations | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-complex-nesting | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-error-handling | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-multiline-command | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-script-exec-integration-end-to-end | ✓ Moved | corpus/runners/ | Phase 1 complete |

### Corpus-Generated Tests (135 tests from corpus data)

**Note:** These tests are dynamically generated from corpus data files (corpus-parse-*.el) by corpus runner test-corpus-parse.el. They don't have explicit ert-deftest definitions but execute as real tests.

**test-corpus-file-operations.el generates ~98 tests from embedded corpus data:**

| Test Prefix | Count | Status | Destination | Notes |
|-------------|-------|--------|-------------|-------|
| test-corpus-read-* | ~15 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-write-* | ~15 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-delete-* | ~12 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-copy-* | ~8 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-move-* | ~8 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-archive-* | ~6 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-modify-* | ~5 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-directory-* | ~5 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-git-* | ~4 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-find-* | ~4 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-glob-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-pipeline-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-redirect-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-variable-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-chain-* | ~2 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-integration-* | ~1 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-match-pattern-* | ~1 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-no-ops-* | ~1 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| test-corpus-variable-chain-* | ~1 | ✓ Moved | corpus/runners/ | Phase 1 complete |

**test-corpus-parse.el generates tests from corpus-parse-*.el data files:**

| Test Prefix | Count | Status | Destination | Notes |
|-------------|-------|--------|-------------|-------|
| jf/bash-parser-test-simple-* | ~15 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-complex-* | ~5 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-pipeline-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-variable-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-quote-* | ~2 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-redirect-* | ~2 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-substitution-* | ~2 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-heredoc-* | ~1 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-background-* | ~2 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-chain-* | ~4 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-dangerous-* | ~2 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-find-* | ~4 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-git-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-glob-* | ~3 | ✓ Moved | corpus/runners/ | Phase 1 complete |
| jf/bash-parser-test-wrapper-* | ~1 | ✓ Moved | corpus/runners/ | Phase 1 complete |

---

## Test Count Summary

**Total Tests Executed:** 683 tests

**By Definition Type:**
- Explicit ert-deftest: 548 tests (in 44 test files)
- Corpus-generated: 135 tests (from corpus data + runners)

**By Migration Phase (Explicit Tests):**
- Phase 1 (Corpus runners): 34 explicit tests
- Phase 2 (Behavioral & Unit): 369 tests
- Phase 3 (Integration & Construct): 142 tests
- Special/TBD: 3 tests

**By Category (Post-Migration):**
- Behavioral: 129 tests
- Unit/Core: 35 tests
- Unit/Semantic: 146 tests
- Unit/Analysis: 59 tests
- Integration: 51 tests (48 original + 3 from bead verification)
- Construct: 94 tests
- Corpus (explicit + generated): 169 tests (34 explicit + 135 generated)

## Redundancy Decisions (Phase 4) ✓ COMPLETED

**Summary:** Comprehensive redundancy analysis completed and documented in test/docs/REDUNDANCY-ANALYSIS.md

**Areas Investigated:**
1. Basic file operation extraction (behavioral vs corpus tests) - KEEP BOTH
2. Variable resolution (unit vs behavioral) - KEEP BOTH
3. Command semantics lookup (unit vs integration) - KEEP BOTH
4. Bead verification tests - INTEGRATED into test suite

**Results:**
- Total overlaps investigated: 4 areas
- Kept both: 4 cases (different focus/layers/data provide unique value)
- Merged: 0 cases (no true redundancy found)
- Moved: 1 file (3 tests from bead verification → integration tests)
- Removed: 0 tests (all tests provide unique coverage)

**Rationale for Conservative Approach:**
Analysis revealed that tests appearing similar actually test different aspects at different layers. The test suite is well-differentiated after reorganization:
- Unit tests validate individual functions
- Behavioral tests validate end-to-end extraction
- Integration tests validate feature combinations
- Construct tests validate language constructs
- Corpus tests validate data-driven expectations

Each layer provides unique diagnostic value when tests fail.

## Deprecated Tests Removed (Phase 4) ✓ COMPLETED

**Summary:** Deprecated corpus tests were already removed in prior phases during corpus file refinement.

**Corpus Tests Already Removed (pre-Phase 4):**
- Heredoc edge cases: 2 tests removed (empty delimiter, special chars in delimiter)
- Command-sub edge cases: 3 tests removed (empty substitution, escaped substitution, pure output cases)
- Conditional test operators: 6 tests removed (string/numeric tests with no file impact)
- Process substitution: 16 tests deferred (extremely rare patterns)

**Total Pre-Removed:** ~27 corpus test entries removed from corpus data files before Phase 4 began

**Phase 4 Removals:** 0 explicit tests removed
- Reason: Deprecated tests were corpus data entries, not explicit ert-deftest forms
- Corpus data refinement documented in corpus file headers (e.g., corpus-parse-heredoc.el line 6)

## Final Accounting ✓ COMPLETED

**Migration completed:** 2026-03-07

### Test Count Summary
- Original baseline: 548 tests (explicit ert-deftest forms)
- After reorganization: 683 tests (548 explicit + 135 corpus-generated)
- Deprecated removed: 27 corpus entries (removed from data files before Phase 4)
- Redundant merged: 0 (no true redundancy found)
- Final: 683 tests maintained at 100% pass rate

### File Moves
- Phase 1: 12 corpus files
- Phase 2: 13 behavioral & unit files
- Phase 3: 9 integration & construct files
- Phase 4: 1 bead verification file integrated

Total: 35 test files moved, all 548 original tests accounted for

### Coverage Validation
- Pass rate: 683/683 (100%)
- Coverage maintained: YES
- Documentation complete: YES

### Test Distribution After Migration
- Behavioral: 129 tests (23.5%) - User-facing behavior
- Unit (Core + Semantic + Analysis): 240 tests (43.8%) - Architecture layers
  - Core: 35 tests (6.4%)
  - Semantic: 146 tests (26.6%)
  - Analysis: 59 tests (10.8%)
- Integration: 51 tests (9.3%) - Multi-component interactions
- Construct: 94 tests (17.2%) - Language constructs
- Corpus (explicit runners): 34 tests (6.2%)
- **Total Explicit:** 548 tests ✓
- **Corpus-Generated:** 135 tests (from corpus data files)
- **Grand Total:** 683 tests ✓

### Issues Encountered
- None - migration completed successfully
- Corpus tests now properly counted (was underreported before)
- No true redundancy found - suite is well-organized

**Migration Status: ✓ COMPLETE**

### Documentation Created
- test/README.md - Master guide with all categories
- test/behavioral/README.md - Behavioral test philosophy (129 tests)
- test/unit/README.md - Unit test organization by layer (240 tests)
- test/integration/README.md - Integration test philosophy (51 tests)
- test/construct/README.md - Construct test philosophy (94 tests)
- test/corpus/README.md - Corpus test philosophy (34 + 135 tests)
- test/docs/MIGRATION-COVERAGE-REPORT.md - Complete migration report
- CLAUDE.md updated with bash-parser test organization

### Conclusion
Phase 5 completed successfully. All README files created, migration report finalized, CLAUDE.md updated. Test suite reorganization is now complete with comprehensive documentation for all categories. All 683 baseline tests accounted for with 100% pass rate maintained. The reorganization provides clear organization by purpose, easy navigation by architectural layer, and improved diagnostic value when tests fail.
