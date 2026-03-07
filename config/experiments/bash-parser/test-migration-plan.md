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

### Phase 1: Structure & Corpus ⏳
- [ ] Directory structure created
- [ ] Corpus data files moved (7 files)
- [ ] Corpus runners moved (3 files)
- [ ] Corpus index moved (1 file)
- [ ] Individual test tracking updated for moved tests

### Phase 2: Behavioral & Unit ⏳
- [ ] Behavioral tests moved (4 files, 129 tests)
- [ ] Unit/core tests moved (1 file, 35 tests)
- [ ] Unit/semantic tests moved (5 files, 146 tests)
- [ ] Unit/analysis tests moved (3 files, 59 tests)
- [ ] Individual test tracking updated for each moved test

### Phase 3: Integration & Construct ⏳
- [ ] Integration tests moved (4 files, 48 tests)
- [ ] Construct tests moved (5 files, 94 tests)
- [ ] Individual test tracking updated for each moved test

### Phase 4: Redundancy & Deprecated ⏳
- [ ] Deprecated tests removed (with justification in tracking)
- [ ] Redundancy analysis completed
- [ ] Individual test tracking updated with removal reasons
- [ ] Final test count determined

### Phase 5: Final Documentation ⏳
- [ ] README files created
- [ ] Migration coverage report created
- [ ] CLAUDE.md updated
- [ ] Individual test tracking verified complete

## File Inventory & Destinations

### Phase 1: Corpus Files (12 files)

**Corpus Data Files (7 files → corpus/data/)**
- test/corpus-parse-command-substitution.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending
- test/corpus-parse-conditional.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending
- test/corpus-parse-for-loop.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending
- test/corpus-parse-heredoc.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending
- test/corpus-parse-process-substitution.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending
- test/corpus-parse-combined-patterns.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending
- test/corpus-parse-llm-scenarios.el → corpus/data/ | 0 deftest | MOVE | ⏳ Pending

**Corpus Runners (3 files → corpus/runners/)**
- test/test-corpus-parse.el → corpus/runners/ | 4 deftest | MOVE | ⏳ Pending
- test/test-corpus-file-operations.el → corpus/runners/ | 0 deftest (generates ~98 tests) | MOVE | ⏳ Pending
- test/test-corpus-script-execution.el → corpus/runners/ | 30 deftest | MOVE | ⏳ Pending

**Corpus Index (1 file → corpus/)**
- test/corpus-index.el → corpus/ | 0 deftest | MOVE | ⏳ Pending

**Infrastructure Files (stay at root)**
- test/test-helper.el | 0 deftest | KEEP | N/A
- test/test-assertions.el | 0 deftest | KEEP | N/A

### Phase 2: Behavioral & Unit Tests (13 files, 369 explicit tests)

**Behavioral Tests (4 files → behavioral/)**
- test/test-file-operations.el → behavioral/ | 49 tests | MOVE | ⏳ Pending
- test/test-security-validator.el → behavioral/ | 60 tests | MOVE | ⏳ Pending
- test/test-command-injection.el → behavioral/ | 7 tests | MOVE | ⏳ Pending
- test/test-backward-compatibility.el → behavioral/ | 13 tests | MOVE | ⏳ Pending

**Unit - Core Layer (1 file → unit/core/)**
- test/test-glob-matching.el → unit/core/ | 35 tests | MOVE | ⏳ Pending

**Unit - Semantic Layer (5 files → unit/semantic/)**
- test/test-command-semantics.el → unit/semantic/ | 68 tests | MOVE | ⏳ Pending
- test/test-bash-parser-semantics.el → unit/semantic/ | 27 tests | MOVE | ⏳ Pending
- test/test-semantics-validation.el → unit/semantic/ | 11 tests | MOVE | ⏳ Pending
- test/test-variable-resolution-unit.el → unit/semantic/ | 36 tests | MOVE | ⏳ Pending
- test/test-variable-chain-ampersand.el → unit/semantic/ | 4 tests | MOVE | ⏳ Pending

**Unit - Analysis Layer (3 files → unit/analysis/)**
- test/test-bash-parser-recursive.el → unit/analysis/ | 25 tests | MOVE | ⏳ Pending
- test/test-parser-extension.el → unit/analysis/ | 26 tests | MOVE | ⏳ Pending
- test/test-input-validation.el → unit/analysis/ | 8 tests | MOVE | ⏳ Pending

### Phase 3: Integration & Construct Tests (9 files, 142 tests)

**Integration Tests (4 files → integration/)**
- test/test-command-substitution.el → integration/ | 28 tests | MOVE | ⏳ Pending
- test/test-pattern-flow.el → integration/ | 10 tests | MOVE | ⏳ Pending
- test/test-expect-file-ops-validation.el → integration/ | 3 tests | MOVE | ⏳ Pending
- test/test-treesitter-workarounds.el → integration/ | 7 tests | MOVE | ⏳ Pending

**Construct Tests (5 files → construct/)**
- test/test-loop-context.el → construct/ | 9 tests | MOVE | ⏳ Pending
- test/test-conditional-context.el → construct/ | 10 tests | MOVE | ⏳ Pending
- test/test-heredoc-context.el → construct/ | 10 tests | MOVE | ⏳ Pending
- test/test-directory-changing-commands.el → construct/ | 36 tests | MOVE | ⏳ Pending
- test/test-pwd-directory-context.el → construct/ | 29 tests | MOVE | ⏳ Pending

### Special Cases

**Bead Verification Test (1 file)**
- test/test-bead-3kgg-verification.el | 3 tests | TBD | ⏳ Pending
  - Note: Temporary verification test for a specific bead
  - Decision needed: Remove after verification complete, or move to unit/core/?

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
| test-backward-compat-simple-read | ⏳ Pending | behavioral/ | |
| test-backward-compat-simple-write | ⏳ Pending | behavioral/ | |
| test-backward-compat-simple-delete | ⏳ Pending | behavioral/ | |
| test-backward-compat-copy-command | ⏳ Pending | behavioral/ | |
| test-backward-compat-pipeline | ⏳ Pending | behavioral/ | |
| test-backward-compat-variable-resolution | ⏳ Pending | behavioral/ | |
| test-backward-compat-unresolved-variables | ⏳ Pending | behavioral/ | |
| test-backward-compat-corpus-read-operations | ⏳ Pending | behavioral/ | |
| test-backward-compat-corpus-write-operations | ⏳ Pending | behavioral/ | |
| test-backward-compat-corpus-delete-operations | ⏳ Pending | behavioral/ | |
| test-feature-detection-recursive-analysis | ⏳ Pending | behavioral/ | |
| test-feature-detection-pattern-flow | ⏳ Pending | behavioral/ | |
| test-feature-detection-unknown-feature | ⏳ Pending | behavioral/ | |

### test-bash-parser-recursive.el (25 tests → unit/analysis/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-recursive-single-substitution | ⏳ Pending | unit/analysis/ | |
| test-recursive-substitution-with-flags | ⏳ Pending | unit/analysis/ | |
| test-recursive-substitution-grep | ⏳ Pending | unit/analysis/ | |
| test-recursive-nested-substitution | ⏳ Pending | unit/analysis/ | |
| test-recursive-nested-find | ⏳ Pending | unit/analysis/ | |
| test-recursive-depth-limiting | ⏳ Pending | unit/analysis/ | |
| test-recursive-depth-with-deep-nesting | ⏳ Pending | unit/analysis/ | |
| test-recursive-depth-abort | ⏳ Pending | unit/analysis/ | |
| test-recursive-command-substitution-in-args | ⏳ Pending | unit/analysis/ | |
| test-recursive-mixed-quotes-substitution | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-basic | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-nested | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-command-substitution | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-pipeline | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-multiple-ops | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-invalid-syntax | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-empty-command | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-only-whitespace | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-disabled | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-depth-limit | ⏳ Pending | unit/analysis/ | |
| test-recursive-extraction-variable-expansion | ⏳ Pending | unit/analysis/ | |
| test-recursive-unmatched-quotes | ⏳ Pending | unit/analysis/ | |
| test-recursive-empty-quotes | ⏳ Pending | unit/analysis/ | |
| test-arithmetic-expansion-still-works | ⏳ Pending | unit/analysis/ | |
| test-recursive-analysis-skips-functions | ⏳ Pending | unit/analysis/ | |

### test-bash-parser-semantics.el (27 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-file-operation-basic-read | ⏳ Pending | unit/semantic/ | |
| test-file-operation-basic-write | ⏳ Pending | unit/semantic/ | |
| test-file-operation-basic-delete | ⏳ Pending | unit/semantic/ | |
| test-file-operation-copy | ⏳ Pending | unit/semantic/ | |
| test-file-operation-move | ⏳ Pending | unit/semantic/ | |
| test-file-operation-make-directory | ⏳ Pending | unit/semantic/ | |
| test-file-operation-remove-directory | ⏳ Pending | unit/semantic/ | |
| test-file-operation-find-read | ⏳ Pending | unit/semantic/ | |
| test-file-operation-find-delete | ⏳ Pending | unit/semantic/ | |
| test-file-operation-pipeline-read-then-write | ⏳ Pending | unit/semantic/ | |
| test-file-operation-pipeline-intermediate-read | ⏳ Pending | unit/semantic/ | |
| test-file-operation-command-substitution-read | ⏳ Pending | unit/semantic/ | |
| test-file-operation-nested-command-substitution | ⏳ Pending | unit/semantic/ | |
| test-file-operation-variable-in-path | ⏳ Pending | unit/semantic/ | |
| test-file-operation-no-variable-resolution-literal | ⏳ Pending | unit/semantic/ | |
| test-file-operation-glob-expansion-read | ⏳ Pending | unit/semantic/ | |
| test-file-operation-glob-expansion-delete | ⏳ Pending | unit/semantic/ | |
| test-file-operation-redirect-write | ⏳ Pending | unit/semantic/ | |
| test-file-operation-redirect-append | ⏳ Pending | unit/semantic/ | |
| test-file-operation-redirect-heredoc | ⏳ Pending | unit/semantic/ | |
| test-file-operation-brace-expansion | ⏳ Pending | unit/semantic/ | |
| test-file-operation-tilde-expansion | ⏳ Pending | unit/semantic/ | |
| test-file-operation-multiple-ops-same-command | ⏳ Pending | unit/semantic/ | |
| test-file-operation-complex-pipeline-multiple-ops | ⏳ Pending | unit/semantic/ | |
| test-file-operation-find-exec-complex | ⏳ Pending | unit/semantic/ | |
| test-file-operation-ls-is-no-op | ⏳ Pending | unit/semantic/ | |
| test-file-operation-echo-is-no-op | ⏳ Pending | unit/semantic/ | |

### test-bead-3kgg-verification.el (3 tests → TBD)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-no-glob-match-after-redirection-operator | ⏳ Pending | TBD | Verification test - decision needed |
| test-no-glob-match-in-variable-assignment | ⏳ Pending | TBD | Verification test - decision needed |
| test-no-glob-match-in-command-substitution | ⏳ Pending | TBD | Verification test - decision needed |

### test-command-injection.el (7 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-injection-basic-semicolon | ⏳ Pending | behavioral/ | |
| test-injection-ampersand-background | ⏳ Pending | behavioral/ | |
| test-injection-pipe-to-shell | ⏳ Pending | behavioral/ | |
| test-injection-command-substitution | ⏳ Pending | behavioral/ | |
| test-injection-nested-quotes | ⏳ Pending | behavioral/ | |
| test-injection-multiline-commands | ⏳ Pending | behavioral/ | |
| test-injection-metadata-validation | ⏳ Pending | behavioral/ | |

### test-command-semantics.el (68 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-command-semantic-read-cat | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-head | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-tail | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-grep | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-less | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-more | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-write-echo-redirect | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-write-printf-redirect | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-write-tee | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-write-dd | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-delete-rm | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-delete-rm-rf | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-delete-find-delete | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-copy-cp | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-copy-cp-r | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-move-mv | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-archive-tar-c | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-archive-tar-x | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-archive-zip | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-archive-unzip | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-modify-sed | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-modify-awk | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-modify-perl-pie | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-no-op-echo-no-redirect | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-no-op-ls | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-no-op-pwd | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-no-op-cd | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-vim-readonly | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-write-vim-write | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-modify-vim-substitute | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-emacs-find-file | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-write-emacs-write-file | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-read-nano | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-directory-mkdir | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-directory-rmdir | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-directory-rm-rf | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-read-log | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-read-show | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-read-diff | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-write-add | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-write-commit | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-write-checkout | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-delete-rm | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-git-delete-clean | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-rsync-copy | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-scp-copy | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-curl-read | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-wget-read | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-curl-write | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-find-read-plain | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-find-read-with-cat | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-find-delete-plain | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-unknown-command | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-complex-git-chain | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-docker-read-logs | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-docker-write-cp | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-docker-exec-write | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-npm-read-list | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-npm-write-install | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-make-read-targets | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-make-write-build | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-python-read-module | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-python-write-pip-install | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-jq-read | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-jq-modify | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-xargs-passthrough | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-sort-no-op | ⏳ Pending | unit/semantic/ | |
| test-command-semantic-uniq-no-op | ⏳ Pending | unit/semantic/ | |

### test-command-substitution.el (28 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-command-substitution-basic | ⏳ Pending | integration/ | |
| test-command-substitution-in-argument | ⏳ Pending | integration/ | |
| test-command-substitution-in-variable | ⏳ Pending | integration/ | |
| test-command-substitution-nested | ⏳ Pending | integration/ | |
| test-command-substitution-deeply-nested | ⏳ Pending | integration/ | |
| test-command-substitution-with-pipeline | ⏳ Pending | integration/ | |
| test-command-substitution-with-glob | ⏳ Pending | integration/ | |
| test-command-substitution-multiple-in-command | ⏳ Pending | integration/ | |
| test-command-substitution-in-heredoc | ⏳ Pending | integration/ | |
| test-command-substitution-in-condition | ⏳ Pending | integration/ | |
| test-command-substitution-backticks | ⏳ Pending | integration/ | |
| test-command-substitution-mixed-quotes | ⏳ Pending | integration/ | |
| test-command-substitution-empty | ⏳ Pending | integration/ | |
| test-command-substitution-whitespace-handling | ⏳ Pending | integration/ | |
| test-command-substitution-with-redirect | ⏳ Pending | integration/ | |
| test-command-substitution-in-for-loop | ⏳ Pending | integration/ | |
| test-command-substitution-with-special-chars | ⏳ Pending | integration/ | |
| test-command-substitution-multiline | ⏳ Pending | integration/ | |
| test-command-substitution-escaped-backticks | ⏳ Pending | integration/ | |
| test-command-substitution-in-array | ⏳ Pending | integration/ | |
| test-command-substitution-with-heredoc-delimiter | ⏳ Pending | integration/ | |
| test-command-substitution-preserves-variables | ⏳ Pending | integration/ | |
| test-command-substitution-complex-nesting | ⏳ Pending | integration/ | |
| test-command-substitution-with-arithmetic | ⏳ Pending | integration/ | |
| test-command-substitution-in-case-statement | ⏳ Pending | integration/ | |
| test-command-substitution-error-handling | ⏳ Pending | integration/ | |
| test-command-substitution-preserves-context | ⏳ Pending | integration/ | |
| test-command-substitution-with-process-substitution | ⏳ Pending | integration/ | |

### test-conditional-context.el (10 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-conditional-basic-if | ⏳ Pending | construct/ | |
| test-conditional-if-else | ⏳ Pending | construct/ | |
| test-conditional-if-elif-else | ⏳ Pending | construct/ | |
| test-conditional-nested-if | ⏳ Pending | construct/ | |
| test-conditional-test-command | ⏳ Pending | construct/ | |
| test-conditional-bracket-test | ⏳ Pending | construct/ | |
| test-conditional-double-bracket | ⏳ Pending | construct/ | |
| test-conditional-with-file-operations | ⏳ Pending | construct/ | |
| test-conditional-case-statement | ⏳ Pending | construct/ | |
| test-conditional-complex-nested | ⏳ Pending | construct/ | |

### test-directory-changing-commands.el (36 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-directory-context-basic-cd | ⏳ Pending | construct/ | |
| test-directory-context-cd-absolute | ⏳ Pending | construct/ | |
| test-directory-context-cd-relative | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-subshell | ⏳ Pending | construct/ | |
| test-directory-context-cd-in-command-substitution | ⏳ Pending | construct/ | |
| test-directory-context-pushd | ⏳ Pending | construct/ | |
| test-directory-context-popd | ⏳ Pending | construct/ | |
| test-directory-context-pushd-popd-sequence | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-pipeline | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-logical-and | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-logical-or | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-semicolon | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-background | ⏳ Pending | construct/ | |
| test-directory-context-cd-in-function | ⏳ Pending | construct/ | |
| test-directory-context-cd-in-if-block | ⏳ Pending | construct/ | |
| test-directory-context-cd-in-for-loop | ⏳ Pending | construct/ | |
| test-directory-context-cd-in-while-loop | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-variable | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-tilde | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-hyphen | ⏳ Pending | construct/ | |
| test-directory-context-multiple-cd | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-file-operations | ⏳ Pending | construct/ | |
| test-directory-context-cd-error-handling | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-quotes | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-spaces | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-special-chars | ⏳ Pending | construct/ | |
| test-directory-context-cd-relative-dot | ⏳ Pending | construct/ | |
| test-directory-context-cd-relative-dotdot | ⏳ Pending | construct/ | |
| test-directory-context-cd-complex-path | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-symbolic-link | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-env-var | ⏳ Pending | construct/ | |
| test-directory-context-cd-nested-subshells | ⏳ Pending | construct/ | |
| test-directory-context-cd-preserves-outside-subshell | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-heredoc | ⏳ Pending | construct/ | |
| test-directory-context-cd-with-process-substitution | ⏳ Pending | construct/ | |
| test-directory-context-cd-integration | ⏳ Pending | construct/ | |

### test-expect-file-ops-validation.el (3 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-expect-file-ops-validation-basic | ⏳ Pending | integration/ | |
| test-expect-file-ops-validation-multiple | ⏳ Pending | integration/ | |
| test-expect-file-ops-validation-complex | ⏳ Pending | integration/ | |

### test-file-operations.el (49 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-extraction-simple-read-command | ⏳ Pending | behavioral/ | |
| test-extraction-simple-write-command | ⏳ Pending | behavioral/ | |
| test-extraction-simple-delete-command | ⏳ Pending | behavioral/ | |
| test-extraction-copy-command | ⏳ Pending | behavioral/ | |
| test-extraction-move-command | ⏳ Pending | behavioral/ | |
| test-extraction-tar-archive-create | ⏳ Pending | behavioral/ | |
| test-extraction-tar-archive-extract | ⏳ Pending | behavioral/ | |
| test-extraction-multiple-paths-same-command | ⏳ Pending | behavioral/ | |
| test-extraction-pipeline-read-write | ⏳ Pending | behavioral/ | |
| test-extraction-pipeline-intermediate-read | ⏳ Pending | behavioral/ | |
| test-extraction-command-substitution-read | ⏳ Pending | behavioral/ | |
| test-extraction-nested-command-substitution | ⏳ Pending | behavioral/ | |
| test-extraction-variable-in-path | ⏳ Pending | behavioral/ | |
| test-extraction-unresolved-variable | ⏳ Pending | behavioral/ | |
| test-extraction-multiple-variables | ⏳ Pending | behavioral/ | |
| test-extraction-variable-and-literal-mix | ⏳ Pending | behavioral/ | |
| test-extraction-glob-pattern-read | ⏳ Pending | behavioral/ | |
| test-extraction-glob-pattern-delete | ⏳ Pending | behavioral/ | |
| test-extraction-redirect-write | ⏳ Pending | behavioral/ | |
| test-extraction-redirect-append | ⏳ Pending | behavioral/ | |
| test-extraction-redirect-heredoc | ⏳ Pending | behavioral/ | |
| test-extraction-redirect-input | ⏳ Pending | behavioral/ | |
| test-extraction-brace-expansion | ⏳ Pending | behavioral/ | |
| test-extraction-tilde-expansion | ⏳ Pending | behavioral/ | |
| test-extraction-find-exec-read | ⏳ Pending | behavioral/ | |
| test-extraction-find-exec-delete | ⏳ Pending | behavioral/ | |
| test-extraction-complex-find-command | ⏳ Pending | behavioral/ | |
| test-extraction-sed-modify | ⏳ Pending | behavioral/ | |
| test-extraction-awk-modify | ⏳ Pending | behavioral/ | |
| test-extraction-vim-edit | ⏳ Pending | behavioral/ | |
| test-extraction-git-operations | ⏳ Pending | behavioral/ | |
| test-extraction-rsync-copy | ⏳ Pending | behavioral/ | |
| test-extraction-docker-operations | ⏳ Pending | behavioral/ | |
| test-extraction-npm-operations | ⏳ Pending | behavioral/ | |
| test-extraction-curl-read | ⏳ Pending | behavioral/ | |
| test-extraction-wget-read | ⏳ Pending | behavioral/ | |
| test-extraction-no-ops-echo | ⏳ Pending | behavioral/ | |
| test-extraction-no-ops-ls | ⏳ Pending | behavioral/ | |
| test-extraction-empty-command | ⏳ Pending | behavioral/ | |
| test-extraction-only-whitespace | ⏳ Pending | behavioral/ | |
| test-extraction-complex-multiline | ⏳ Pending | behavioral/ | |
| test-extraction-confidence-scoring-read | ⏳ Pending | behavioral/ | |
| test-extraction-confidence-scoring-variable | ⏳ Pending | behavioral/ | |
| test-extraction-confidence-scoring-glob | ⏳ Pending | behavioral/ | |
| test-extraction-metadata-structure | ⏳ Pending | behavioral/ | |
| test-extraction-metadata-completeness | ⏳ Pending | behavioral/ | |
| test-extraction-result-format | ⏳ Pending | behavioral/ | |
| test-extraction-error-propagation | ⏳ Pending | behavioral/ | |
| test-extraction-disabled-recursive | ⏳ Pending | behavioral/ | |

### test-glob-matching.el (35 tests → unit/core/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-glob-basic-wildcard | ⏳ Pending | unit/core/ | |
| test-glob-question-mark | ⏳ Pending | unit/core/ | |
| test-glob-character-class | ⏳ Pending | unit/core/ | |
| test-glob-negated-character-class | ⏳ Pending | unit/core/ | |
| test-glob-range-character-class | ⏳ Pending | unit/core/ | |
| test-glob-multiple-wildcards | ⏳ Pending | unit/core/ | |
| test-glob-leading-wildcard | ⏳ Pending | unit/core/ | |
| test-glob-trailing-wildcard | ⏳ Pending | unit/core/ | |
| test-glob-middle-wildcard | ⏳ Pending | unit/core/ | |
| test-glob-literal-match | ⏳ Pending | unit/core/ | |
| test-glob-no-match | ⏳ Pending | unit/core/ | |
| test-glob-path-separator | ⏳ Pending | unit/core/ | |
| test-glob-relative-path | ⏳ Pending | unit/core/ | |
| test-glob-absolute-path | ⏳ Pending | unit/core/ | |
| test-glob-tilde-expansion | ⏳ Pending | unit/core/ | |
| test-glob-with-extension | ⏳ Pending | unit/core/ | |
| test-glob-multiple-extensions | ⏳ Pending | unit/core/ | |
| test-glob-hidden-files | ⏳ Pending | unit/core/ | |
| test-glob-dotfiles-explicit | ⏳ Pending | unit/core/ | |
| test-glob-complex-pattern | ⏳ Pending | unit/core/ | |
| test-glob-special-chars-escape | ⏳ Pending | unit/core/ | |
| test-glob-empty-pattern | ⏳ Pending | unit/core/ | |
| test-glob-whitespace-pattern | ⏳ Pending | unit/core/ | |
| test-glob-pattern-with-spaces | ⏳ Pending | unit/core/ | |
| test-glob-after-command | ⏳ Pending | unit/core/ | |
| test-glob-in-middle-of-arg | ⏳ Pending | unit/core/ | |
| test-glob-multiple-patterns-same-arg | ⏳ Pending | unit/core/ | |
| test-glob-in-quoted-string | ⏳ Pending | unit/core/ | |
| test-glob-in-double-quotes | ⏳ Pending | unit/core/ | |
| test-glob-mixed-quotes | ⏳ Pending | unit/core/ | |
| test-glob-brace-expansion | ⏳ Pending | unit/core/ | |
| test-glob-extended-glob | ⏳ Pending | unit/core/ | |
| test-glob-with-redirect | ⏳ Pending | unit/core/ | |
| test-glob-with-pipeline | ⏳ Pending | unit/core/ | |
| test-glob-case-sensitivity | ⏳ Pending | unit/core/ | |

### test-heredoc-context.el (10 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-heredoc-basic | ⏳ Pending | construct/ | |
| test-heredoc-with-variables | ⏳ Pending | construct/ | |
| test-heredoc-quoted-delimiter | ⏳ Pending | construct/ | |
| test-heredoc-indented | ⏳ Pending | construct/ | |
| test-heredoc-multiple-commands | ⏳ Pending | construct/ | |
| test-heredoc-with-command-substitution | ⏳ Pending | construct/ | |
| test-heredoc-multiline | ⏳ Pending | construct/ | |
| test-heredoc-empty | ⏳ Pending | construct/ | |
| test-heredoc-nested-in-function | ⏳ Pending | construct/ | |
| test-heredoc-complex-delimiter | ⏳ Pending | construct/ | |

### test-input-validation.el (8 tests → unit/analysis/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-input-validation-empty-string | ⏳ Pending | unit/analysis/ | |
| test-input-validation-whitespace-only | ⏳ Pending | unit/analysis/ | |
| test-input-validation-nil-input | ⏳ Pending | unit/analysis/ | |
| test-input-validation-very-long-command | ⏳ Pending | unit/analysis/ | |
| test-input-validation-special-characters | ⏳ Pending | unit/analysis/ | |
| test-input-validation-unicode | ⏳ Pending | unit/analysis/ | |
| test-input-validation-binary-data | ⏳ Pending | unit/analysis/ | |
| test-input-validation-newlines | ⏳ Pending | unit/analysis/ | |

### test-loop-context.el (9 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-loop-context-basic-for | ⏳ Pending | construct/ | |
| test-loop-context-for-in-list | ⏳ Pending | construct/ | |
| test-loop-context-for-in-range | ⏳ Pending | construct/ | |
| test-loop-context-while-loop | ⏳ Pending | construct/ | |
| test-loop-context-until-loop | ⏳ Pending | construct/ | |
| test-loop-context-nested-loops | ⏳ Pending | construct/ | |
| test-loop-context-with-file-operations | ⏳ Pending | construct/ | |
| test-loop-context-with-break-continue | ⏳ Pending | construct/ | |
| test-loop-context-complex-iteration | ⏳ Pending | construct/ | |

### test-parser-extension.el (26 tests → unit/analysis/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-extension-register-command | ⏳ Pending | unit/analysis/ | |
| test-extension-register-multiple-commands | ⏳ Pending | unit/analysis/ | |
| test-extension-override-builtin | ⏳ Pending | unit/analysis/ | |
| test-extension-custom-semantic-read | ⏳ Pending | unit/analysis/ | |
| test-extension-custom-semantic-write | ⏳ Pending | unit/analysis/ | |
| test-extension-custom-semantic-delete | ⏳ Pending | unit/analysis/ | |
| test-extension-custom-semantic-no-op | ⏳ Pending | unit/analysis/ | |
| test-extension-custom-pattern-handler | ⏳ Pending | unit/analysis/ | |
| test-extension-command-with-flags | ⏳ Pending | unit/analysis/ | |
| test-extension-command-with-complex-args | ⏳ Pending | unit/analysis/ | |
| test-extension-priority-ordering | ⏳ Pending | unit/analysis/ | |
| test-extension-fallback-to-default | ⏳ Pending | unit/analysis/ | |
| test-extension-nil-handler-fallback | ⏳ Pending | unit/analysis/ | |
| test-extension-error-handling | ⏳ Pending | unit/analysis/ | |
| test-extension-handler-returns-nil | ⏳ Pending | unit/analysis/ | |
| test-extension-handler-returns-empty-list | ⏳ Pending | unit/analysis/ | |
| test-extension-handler-modifies-metadata | ⏳ Pending | unit/analysis/ | |
| test-extension-integration-with-pipeline | ⏳ Pending | unit/analysis/ | |
| test-extension-integration-with-variables | ⏳ Pending | unit/analysis/ | |
| test-extension-integration-with-globs | ⏳ Pending | unit/analysis/ | |
| test-extension-chained-handlers | ⏳ Pending | unit/analysis/ | |
| test-extension-conditional-handling | ⏳ Pending | unit/analysis/ | |
| test-extension-context-preservation | ⏳ Pending | unit/analysis/ | |
| test-extension-clear-extensions | ⏳ Pending | unit/analysis/ | |
| test-extension-list-registered | ⏳ Pending | unit/analysis/ | |
| test-extension-query-by-command | ⏳ Pending | unit/analysis/ | |

### test-pattern-flow.el (10 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-pattern-flow-sequential-commands | ⏳ Pending | integration/ | |
| test-pattern-flow-pipeline-simple | ⏳ Pending | integration/ | |
| test-pattern-flow-pipeline-complex | ⏳ Pending | integration/ | |
| test-pattern-flow-logical-and | ⏳ Pending | integration/ | |
| test-pattern-flow-logical-or | ⏳ Pending | integration/ | |
| test-pattern-flow-background-job | ⏳ Pending | integration/ | |
| test-pattern-flow-subshell | ⏳ Pending | integration/ | |
| test-pattern-flow-command-group | ⏳ Pending | integration/ | |
| test-pattern-flow-mixed-operators | ⏳ Pending | integration/ | |
| test-pattern-flow-nested-subshells | ⏳ Pending | integration/ | |

### test-pwd-directory-context.el (29 tests → construct/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-pwd-context-basic | ⏳ Pending | construct/ | |
| test-pwd-context-after-cd | ⏳ Pending | construct/ | |
| test-pwd-context-in-subshell | ⏳ Pending | construct/ | |
| test-pwd-context-in-command-substitution | ⏳ Pending | construct/ | |
| test-pwd-context-with-variable | ⏳ Pending | construct/ | |
| test-pwd-context-multiple-pwd-calls | ⏳ Pending | construct/ | |
| test-pwd-context-pwd-in-pipeline | ⏳ Pending | construct/ | |
| test-pwd-context-pwd-logical | ⏳ Pending | construct/ | |
| test-pwd-context-pwd-physical | ⏳ Pending | construct/ | |
| test-pwd-context-with-symbolic-link | ⏳ Pending | construct/ | |
| test-pwd-context-after-pushd | ⏳ Pending | construct/ | |
| test-pwd-context-after-popd | ⏳ Pending | construct/ | |
| test-pwd-context-in-function | ⏳ Pending | construct/ | |
| test-pwd-context-in-if-block | ⏳ Pending | construct/ | |
| test-pwd-context-in-for-loop | ⏳ Pending | construct/ | |
| test-pwd-context-in-while-loop | ⏳ Pending | construct/ | |
| test-pwd-context-nested-subshells | ⏳ Pending | construct/ | |
| test-pwd-context-preserves-outside-subshell | ⏳ Pending | construct/ | |
| test-pwd-context-with-cd-dash | ⏳ Pending | construct/ | |
| test-pwd-context-with-cd-tilde | ⏳ Pending | construct/ | |
| test-pwd-context-with-cd-dotdot | ⏳ Pending | construct/ | |
| test-pwd-context-with-env-var | ⏳ Pending | construct/ | |
| test-pwd-context-complex-path | ⏳ Pending | construct/ | |
| test-pwd-context-with-spaces-in-path | ⏳ Pending | construct/ | |
| test-pwd-context-with-special-chars | ⏳ Pending | construct/ | |
| test-pwd-context-integration-cd-pwd-chain | ⏳ Pending | construct/ | |
| test-pwd-context-error-handling | ⏳ Pending | construct/ | |
| test-pwd-context-with-heredoc | ⏳ Pending | construct/ | |
| test-pwd-context-with-process-substitution | ⏳ Pending | construct/ | |

### test-security-validator.el (60 tests → behavioral/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-security-dangerous-rm-root | ⏳ Pending | behavioral/ | |
| test-security-dangerous-rm-rf-root | ⏳ Pending | behavioral/ | |
| test-security-dangerous-rm-star-root | ⏳ Pending | behavioral/ | |
| test-security-dangerous-rm-recursive-slash | ⏳ Pending | behavioral/ | |
| test-security-dangerous-dd-device | ⏳ Pending | behavioral/ | |
| test-security-dangerous-mkfs | ⏳ Pending | behavioral/ | |
| test-security-dangerous-format-device | ⏳ Pending | behavioral/ | |
| test-security-dangerous-fork-bomb | ⏳ Pending | behavioral/ | |
| test-security-dangerous-chmod-777-recursive | ⏳ Pending | behavioral/ | |
| test-security-dangerous-chown-recursive-root | ⏳ Pending | behavioral/ | |
| test-security-safe-rm-specific-file | ⏳ Pending | behavioral/ | |
| test-security-safe-rm-rf-subdirectory | ⏳ Pending | behavioral/ | |
| test-security-safe-dd-specific-file | ⏳ Pending | behavioral/ | |
| test-security-safe-chmod-specific-file | ⏳ Pending | behavioral/ | |
| test-security-safe-chown-specific-file | ⏳ Pending | behavioral/ | |
| test-security-injection-semicolon | ⏳ Pending | behavioral/ | |
| test-security-injection-ampersand | ⏳ Pending | behavioral/ | |
| test-security-injection-pipe | ⏳ Pending | behavioral/ | |
| test-security-injection-backticks | ⏳ Pending | behavioral/ | |
| test-security-injection-dollar-paren | ⏳ Pending | behavioral/ | |
| test-security-injection-logical-and | ⏳ Pending | behavioral/ | |
| test-security-injection-logical-or | ⏳ Pending | behavioral/ | |
| test-security-injection-redirect-overwrite | ⏳ Pending | behavioral/ | |
| test-security-injection-redirect-append | ⏳ Pending | behavioral/ | |
| test-security-injection-heredoc | ⏳ Pending | behavioral/ | |
| test-security-safe-simple-command | ⏳ Pending | behavioral/ | |
| test-security-safe-with-arguments | ⏳ Pending | behavioral/ | |
| test-security-safe-pipeline-no-injection | ⏳ Pending | behavioral/ | |
| test-security-safe-quoted-arguments | ⏳ Pending | behavioral/ | |
| test-security-safe-escaped-chars | ⏳ Pending | behavioral/ | |
| test-security-path-traversal-dotdot | ⏳ Pending | behavioral/ | |
| test-security-path-traversal-absolute | ⏳ Pending | behavioral/ | |
| test-security-path-traversal-tilde | ⏳ Pending | behavioral/ | |
| test-security-path-traversal-symbolic-link | ⏳ Pending | behavioral/ | |
| test-security-path-traversal-device | ⏳ Pending | behavioral/ | |
| test-security-safe-path-relative | ⏳ Pending | behavioral/ | |
| test-security-safe-path-subdirectory | ⏳ Pending | behavioral/ | |
| test-security-safe-path-workspace | ⏳ Pending | behavioral/ | |
| test-security-network-curl-http | ⏳ Pending | behavioral/ | |
| test-security-network-wget-http | ⏳ Pending | behavioral/ | |
| test-security-network-nc-listen | ⏳ Pending | behavioral/ | |
| test-security-network-ssh-command | ⏳ Pending | behavioral/ | |
| test-security-network-scp-upload | ⏳ Pending | behavioral/ | |
| test-security-network-rsync-remote | ⏳ Pending | behavioral/ | |
| test-security-safe-network-localhost | ⏳ Pending | behavioral/ | |
| test-security-safe-network-read-only | ⏳ Pending | behavioral/ | |
| test-security-validation-result-structure | ⏳ Pending | behavioral/ | |
| test-security-validation-multiple-violations | ⏳ Pending | behavioral/ | |
| test-security-validation-empty-command | ⏳ Pending | behavioral/ | |
| test-security-validation-nil-input | ⏳ Pending | behavioral/ | |
| test-security-combined-dangerous-and-injection | ⏳ Pending | behavioral/ | |
| test-security-combined-path-and-network | ⏳ Pending | behavioral/ | |
| test-security-combined-multiple-issues | ⏳ Pending | behavioral/ | |
| test-security-edge-case-very-long-command | ⏳ Pending | behavioral/ | |
| test-security-edge-case-unicode | ⏳ Pending | behavioral/ | |
| test-security-edge-case-special-chars | ⏳ Pending | behavioral/ | |
| test-security-edge-case-whitespace-variants | ⏳ Pending | behavioral/ | |
| test-security-metadata-severity-critical | ⏳ Pending | behavioral/ | |
| test-security-metadata-severity-high | ⏳ Pending | behavioral/ | |
| test-security-metadata-severity-medium | ⏳ Pending | behavioral/ | |

### test-semantics-validation.el (11 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-semantics-validation-valid-read | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-valid-write | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-valid-delete | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-valid-no-op | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-invalid-semantic-type | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-missing-command | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-missing-args | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-empty-result | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-nil-result | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-malformed-result | ⏳ Pending | unit/semantic/ | |
| test-semantics-validation-result-consistency | ⏳ Pending | unit/semantic/ | |

### test-treesitter-workarounds.el (7 tests → integration/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-treesitter-workaround-malformed-heredoc | ⏳ Pending | integration/ | |
| test-treesitter-workaround-incomplete-pipeline | ⏳ Pending | integration/ | |
| test-treesitter-workaround-unclosed-quotes | ⏳ Pending | integration/ | |
| test-treesitter-workaround-unclosed-command-substitution | ⏳ Pending | integration/ | |
| test-treesitter-workaround-invalid-redirect | ⏳ Pending | integration/ | |
| test-treesitter-workaround-broken-for-loop | ⏳ Pending | integration/ | |
| test-treesitter-workaround-partial-if-statement | ⏳ Pending | integration/ | |

### test-variable-chain-ampersand.el (4 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-variable-chain-ampersand-single | ⏳ Pending | unit/semantic/ | |
| test-variable-chain-ampersand-double | ⏳ Pending | unit/semantic/ | |
| test-variable-chain-ampersand-multiple | ⏳ Pending | unit/semantic/ | |
| test-variable-chain-ampersand-mixed-with-pipe | ⏳ Pending | unit/semantic/ | |

### test-variable-resolution-unit.el (36 tests → unit/semantic/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-variable-resolution-simple | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-braces | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-multiple | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-path | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-concatenated | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-nested | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-with-default | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-with-assignment | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-substring | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-length | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-replacement | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-removal | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-case-modification | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-indirect | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-array-element | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-array-all | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-quotes | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-escaped | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-single-quotes | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-special-parameters | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-positional-parameters | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-unset-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-empty-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-readonly-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-exported-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-local-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-environment-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-home-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-path-variable | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-command-substitution | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-arithmetic | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-conditional | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-in-loop | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-complex-nested | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-metadata-confidence | ⏳ Pending | unit/semantic/ | |
| test-variable-resolution-integration | ⏳ Pending | unit/semantic/ | |

### test-corpus-parse.el (4 tests → corpus/runners/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-corpus-parse-all-tests | ⏳ Pending | corpus/runners/ | |
| test-corpus-parse-by-category | ⏳ Pending | corpus/runners/ | |
| test-corpus-parse-by-gap-type | ⏳ Pending | corpus/runners/ | |
| test-corpus-parse-real-only | ⏳ Pending | corpus/runners/ | |

### test-corpus-script-execution.el (30 tests → corpus/runners/)

| Test Name | Status | Destination | Notes |
|-----------|--------|-------------|-------|
| test-script-exec-simple-echo | ⏳ Pending | corpus/runners/ | |
| test-script-exec-file-read | ⏳ Pending | corpus/runners/ | |
| test-script-exec-file-write | ⏳ Pending | corpus/runners/ | |
| test-script-exec-file-delete | ⏳ Pending | corpus/runners/ | |
| test-script-exec-pipeline | ⏳ Pending | corpus/runners/ | |
| test-script-exec-command-substitution | ⏳ Pending | corpus/runners/ | |
| test-script-exec-variable-expansion | ⏳ Pending | corpus/runners/ | |
| test-script-exec-glob-expansion | ⏳ Pending | corpus/runners/ | |
| test-script-exec-for-loop | ⏳ Pending | corpus/runners/ | |
| test-script-exec-while-loop | ⏳ Pending | corpus/runners/ | |
| test-script-exec-if-statement | ⏳ Pending | corpus/runners/ | |
| test-script-exec-case-statement | ⏳ Pending | corpus/runners/ | |
| test-script-exec-function-definition | ⏳ Pending | corpus/runners/ | |
| test-script-exec-function-call | ⏳ Pending | corpus/runners/ | |
| test-script-exec-heredoc | ⏳ Pending | corpus/runners/ | |
| test-script-exec-redirect-output | ⏳ Pending | corpus/runners/ | |
| test-script-exec-redirect-input | ⏳ Pending | corpus/runners/ | |
| test-script-exec-redirect-append | ⏳ Pending | corpus/runners/ | |
| test-script-exec-background-job | ⏳ Pending | corpus/runners/ | |
| test-script-exec-subshell | ⏳ Pending | corpus/runners/ | |
| test-script-exec-command-group | ⏳ Pending | corpus/runners/ | |
| test-script-exec-logical-and | ⏳ Pending | corpus/runners/ | |
| test-script-exec-logical-or | ⏳ Pending | corpus/runners/ | |
| test-script-exec-arithmetic-expression | ⏳ Pending | corpus/runners/ | |
| test-script-exec-array-operations | ⏳ Pending | corpus/runners/ | |
| test-script-exec-string-operations | ⏳ Pending | corpus/runners/ | |
| test-script-exec-complex-nesting | ⏳ Pending | corpus/runners/ | |
| test-script-exec-error-handling | ⏳ Pending | corpus/runners/ | |
| test-script-exec-multiline-command | ⏳ Pending | corpus/runners/ | |
| test-script-exec-integration-end-to-end | ⏳ Pending | corpus/runners/ | |

### Corpus-Generated Tests (135 tests from corpus data)

**Note:** These tests are dynamically generated from corpus data files (corpus-parse-*.el) by corpus runner test-corpus-parse.el. They don't have explicit ert-deftest definitions but execute as real tests.

**test-corpus-file-operations.el generates ~98 tests from embedded corpus data:**

| Test Prefix | Count | Status | Destination | Notes |
|-------------|-------|--------|-------------|-------|
| test-corpus-read-* | ~15 | ⏳ Pending | corpus/runners/ | Read operation corpus tests |
| test-corpus-write-* | ~15 | ⏳ Pending | corpus/runners/ | Write operation corpus tests |
| test-corpus-delete-* | ~12 | ⏳ Pending | corpus/runners/ | Delete operation corpus tests |
| test-corpus-copy-* | ~8 | ⏳ Pending | corpus/runners/ | Copy operation corpus tests |
| test-corpus-move-* | ~8 | ⏳ Pending | corpus/runners/ | Move operation corpus tests |
| test-corpus-archive-* | ~6 | ⏳ Pending | corpus/runners/ | Archive operation corpus tests |
| test-corpus-modify-* | ~5 | ⏳ Pending | corpus/runners/ | Modify operation corpus tests |
| test-corpus-directory-* | ~5 | ⏳ Pending | corpus/runners/ | Directory operation corpus tests |
| test-corpus-git-* | ~4 | ⏳ Pending | corpus/runners/ | Git operation corpus tests |
| test-corpus-find-* | ~4 | ⏳ Pending | corpus/runners/ | Find command corpus tests |
| test-corpus-glob-* | ~3 | ⏳ Pending | corpus/runners/ | Glob pattern corpus tests |
| test-corpus-pipeline-* | ~3 | ⏳ Pending | corpus/runners/ | Pipeline corpus tests |
| test-corpus-redirect-* | ~3 | ⏳ Pending | corpus/runners/ | Redirect corpus tests |
| test-corpus-variable-* | ~3 | ⏳ Pending | corpus/runners/ | Variable corpus tests |
| test-corpus-chain-* | ~2 | ⏳ Pending | corpus/runners/ | Command chain corpus tests |
| test-corpus-integration-* | ~1 | ⏳ Pending | corpus/runners/ | Integration corpus tests |
| test-corpus-match-pattern-* | ~1 | ⏳ Pending | corpus/runners/ | Pattern match corpus tests |
| test-corpus-no-ops-* | ~1 | ⏳ Pending | corpus/runners/ | No-op corpus tests |
| test-corpus-variable-chain-* | ~1 | ⏳ Pending | corpus/runners/ | Variable chain corpus tests |

**test-corpus-parse.el generates tests from corpus-parse-*.el data files:**

| Test Prefix | Count | Status | Destination | Notes |
|-------------|-------|--------|-------------|-------|
| jf/bash-parser-test-simple-* | ~15 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-complex-* | ~5 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-pipeline-* | ~3 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-variable-* | ~3 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-quote-* | ~2 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-redirect-* | ~2 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-substitution-* | ~2 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-heredoc-* | ~1 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-background-* | ~2 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-chain-* | ~4 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-dangerous-* | ~2 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-find-* | ~4 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-git-* | ~3 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-glob-* | ~3 | ⏳ Pending | corpus/runners/ | From corpus-parse files |
| jf/bash-parser-test-wrapper-* | ~1 | ⏳ Pending | corpus/runners/ | From corpus-parse files |

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
- Integration: 48 tests
- Construct: 94 tests
- Corpus (explicit + generated): 169 tests (34 explicit + 135 generated)
- TBD: 3 tests

## Redundancy Decisions (Phase 4)
[Will be filled during Phase 4 - Every removal must document which tests were redundant and why]

## Deprecated Tests Removed (Phase 4)
[Will be filled during Phase 4 - Every removal must document test name, file, and deprecation reason]

## Final Accounting
[Will be filled during Phase 5 - Must account for all 683 tests from baseline]
