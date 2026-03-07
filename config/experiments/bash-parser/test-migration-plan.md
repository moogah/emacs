# Test Migration Plan - Tracking Document

Generated: 2026-03-07
Baseline snapshot: config/experiments/bash-parser/test-results.txt
Initial test count: 548 tests

## Migration Status

### Phase 0: Baseline ✓
- [x] Snapshot generated: test-results.txt
- [x] Test inventory created: test-inventory-baseline.txt
- [x] Tracking document created

### Phase 1: Structure & Corpus ⏳
- [ ] Directory structure created
- [ ] Corpus data files moved (7 files)
- [ ] Corpus runners moved (3 files)
- [ ] Corpus index moved (1 file)

### Phase 2: Behavioral & Unit ⏳
- [ ] Behavioral tests moved (4 files, 129 tests)
- [ ] Unit/core tests moved (1 file, 35 tests)
- [ ] Unit/semantic tests moved (5 files, 146 tests)
- [ ] Unit/analysis tests moved (3 files, 59 tests)

### Phase 3: Integration & Construct ⏳
- [ ] Integration tests moved (4 files, 48 tests)
- [ ] Construct tests moved (5 files, 94 tests)

### Phase 4: Redundancy & Deprecated ⏳
- [ ] Deprecated tests removed
- [ ] Redundancy analysis completed
- [ ] Final test count determined

### Phase 5: Final Documentation ⏳
- [ ] README files created
- [ ] Migration coverage report created
- [ ] CLAUDE.md updated

## Test Inventory & Decisions

### Phase 1: Corpus Files (12 files)

**Corpus Data Files (7 files → corpus/data/)**
- test/corpus-parse-command-substitution.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending
- test/corpus-parse-conditional.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending
- test/corpus-parse-for-loop.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending
- test/corpus-parse-heredoc.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending
- test/corpus-parse-process-substitution.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending
- test/corpus-parse-combined-patterns.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending
- test/corpus-parse-llm-scenarios.el → corpus/data/ | 0 tests | MOVE | ⏳ Pending

**Corpus Runners (3 files → corpus/runners/)**
- test/test-corpus-parse.el → corpus/runners/ | 4 tests | MOVE | ⏳ Pending
- test/test-corpus-file-operations.el → corpus/runners/ | 0 tests | MOVE | ⏳ Pending
- test/test-corpus-script-execution.el → corpus/runners/ | 30 tests | MOVE | ⏳ Pending

**Corpus Index (1 file → corpus/)**
- test/corpus-index.el → corpus/ | 0 tests | MOVE | ⏳ Pending

**Infrastructure Files (stay at root)**
- test/test-helper.el | 0 tests | KEEP | N/A
- test/test-assertions.el | 0 tests | KEEP | N/A

### Phase 2: Behavioral & Unit Tests (13 files, 369 tests)

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
  - Note: This appears to be a temporary verification test for a specific bead
  - Decision needed: Remove after verification complete, or move to unit/core/?

## Test Count Summary

**By Phase:**
- Phase 1 (Corpus): 34 tests (corpus runners only)
- Phase 2 (Behavioral & Unit): 369 tests
- Phase 3 (Integration & Construct): 142 tests
- Special/TBD: 3 tests
- **Total: 548 tests**

**By Category (post-migration):**
- Behavioral: 129 tests
- Unit/Core: 35 tests
- Unit/Semantic: 146 tests
- Unit/Analysis: 59 tests
- Integration: 48 tests
- Construct: 94 tests
- Corpus runners: 34 tests
- TBD: 3 tests

## Redundancy Decisions (Phase 4)
[Will be filled during Phase 4]

## Deprecated Tests Removed (Phase 4)
[Will be filled during Phase 4]

## Final Accounting
[Will be filled during Phase 5]
