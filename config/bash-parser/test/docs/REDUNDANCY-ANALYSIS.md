# Redundancy Analysis - Phase 4

## Overview

This document tracks the analysis of potential test redundancy across the reorganized test suite. The goal is conservative reduction: only remove tests that are truly redundant (identical data, identical assertions, no unique value).

**Current State:**
- Total executed tests: 683 (548 explicit + 135 corpus-generated)
- Target: 520-530 tests (3-5% reduction from original 548 baseline)

## Analysis Methodology

1. **Identify overlap candidates** - Areas where multiple test files might cover the same ground
2. **Compare assertions** - Determine if tests check the same things
3. **Evaluate unique value** - Even if similar, does each test provide unique coverage?
4. **Document decisions** - Every decision to keep or merge must be documented

## Decision Criteria

**KEEP BOTH when:**
- Different assertion focus (behavioral vs structural vs unit)
- Different test data (even if testing same function)
- Different contexts (unit vs integration)
- Different layers (core vs semantic vs analysis)

**MERGE when:**
- Identical bash command
- Identical assertions
- Same test scope (both behavioral or both unit)
- No unique value in having both

## Overlaps Investigated

### Area 1: Basic File Operation Extraction

**Candidate overlap:** behavioral/test-file-operations.el vs corpus/runners/test-corpus-file-operations.el

#### Investigation 1.1: Simple read command

- **Test 1:** `test-extraction-simple-read-command` (behavioral/test-file-operations.el:34)
  - Command: `"cat /workspace/foo.txt"`
  - Assertions: Checks metadata structure (:operation, :file, :source, :confidence, :command)
  - Focus: Metadata completeness and structure validation

- **Test 2:** Corpus-generated from test-corpus-file-operations.el
  - Command: `"cat /workspace/foo.txt"` (same)
  - Assertions: Checks operation type and file path from corpus expectations
  - Focus: Data contract validation from corpus

**Analysis:** Both use identical command but have different assertion styles. Test 1 checks metadata fields explicitly, Test 2 validates against corpus expectations.

**Difference:** Behavioral test checks implementation details (metadata structure), corpus test validates expected behavior from data contract.

**Decision:** **KEEP BOTH**

**Rationale:** Different assertion focus. Behavioral test validates the extractor produces correct metadata structure. Corpus test validates against expected file operations from corpus data. If behavioral test fails, it indicates metadata structure problem. If corpus test fails, it indicates semantic extraction problem. Both provide unique diagnostic value.

#### Investigation 1.2: Simple write command

- **Test 1:** `test-extraction-simple-write-command` (behavioral/test-file-operations.el)
  - Command: `"echo 'content' > /workspace/output.txt"`
  - Assertions: Checks :operation :write, :source :redirection, metadata fields
  - Focus: Write operation detection via redirection

- **Test 2:** Corpus-generated write tests
  - Various write commands from corpus data
  - Assertions: Corpus expectations
  - Focus: Data-driven validation

**Analysis:** Test patterns differ. Behavioral test uses echo with redirect, corpus uses various write patterns.

**Decision:** **KEEP BOTH**

**Rationale:** Different test data. Even though both test write operations, they use different patterns. Behavioral test focuses on redirect-based writes, corpus covers diverse write patterns.

### Area 2: Variable Resolution

**Candidate overlap:** unit/semantic/test-variable-resolution-unit.el vs behavioral/test-file-operations.el (variable tests)

#### Investigation 2.1: Variable in file path

- **Test 1:** `test-variable-resolution-simple` (unit/semantic/test-variable-resolution-unit.el)
  - Focus: Pure variable resolution logic
  - Tests: Variable expansion without file context
  - Layer: Unit test of resolution function

- **Test 2:** `test-extraction-variable-in-path` (behavioral/test-file-operations.el)
  - Focus: Variable resolution in file operation context
  - Tests: Full extraction pipeline with variable
  - Layer: Behavioral/integration test

**Analysis:** Different layers of testing. Unit test isolates variable resolution. Behavioral test validates resolution within full extraction context.

**Decision:** **KEEP BOTH**

**Rationale:** Different test layers. Unit test ensures variable resolution function works correctly in isolation. Behavioral test ensures variable resolution integrates properly with file operation extraction. Both are necessary for proper coverage.

### Area 3: Command Semantics Lookup

**Candidate overlap:** unit/semantic/test-command-semantics.el vs other tests using semantics

#### Investigation 3.1: Command semantic lookup tests

- **Test 1:** `test-command-semantic-read-cat` (unit/semantic/test-command-semantics.el)
  - Focus: Tests the semantic lookup function directly
  - Validates: `(jf/bash-command-semantic "cat")` returns :read
  - Layer: Unit test

- **Test 2:** Various behavioral tests that use cat
  - Focus: Tests full extraction with cat command
  - Validates: Complete file operation extraction
  - Layer: Integration/behavioral

**Analysis:** Unit test validates lookup function. Other tests use it implicitly as part of extraction.

**Decision:** **KEEP BOTH**

**Rationale:** Different purposes. Unit tests validate the semantic lookup table and function. Behavioral tests validate end-to-end extraction. Unit tests catch semantic table errors early. Behavioral tests catch integration problems.

### Area 4: Bead Verification Tests

**File:** test/test-bead-3kgg-verification.el (3 tests)

#### Investigation 4.1: Verification test purpose

- **Tests:** 3 tests verifying find -exec patterns
- **Purpose:** Bead-specific verification for emacs-3kgg
- **Coverage:** Tests find with -exec rm, multiple -exec blocks, workspace validation

**Analysis:** These tests were created to verify a specific bead's requirements. Now that the bead is complete, should they remain as separate verification tests or be integrated?

**Options:**
1. Keep as-is in test/ root (temporary verification)
2. Move to integration/ (integrate with regular tests)
3. Remove (coverage provided by other tests)

**Comparison with existing tests:**
- Similar patterns tested in corpus and integration tests
- But these tests have specific assertion patterns from bead spec
- Tests verify precise metadata requirements (:source :exec-block, :indirect field, etc.)

**Decision:** **MOVE to integration/**

**Rationale:** These tests provide unique value by testing specific metadata requirements for find -exec patterns. They should be kept but integrated into the regular test suite rather than remaining as separate "verification" tests. The tests are well-written and add coverage for exec-block metadata that may not be tested elsewhere.

**Action:** Move test-bead-3kgg-verification.el → test/integration/test-find-exec-patterns.el and rename tests to remove "bead-3kgg" prefix.

## Summary of Analysis

**Total overlaps investigated:** 4 areas

**Results:**
- Kept both: 4 cases (different focus/layers/data)
- Merged: 0 cases (no true redundancy found)
- Moved: 1 file (bead verification → integration tests)

**Tests affected:**
- Moved: 3 tests (test-bead-3kgg-verification.el → integration/)
- Removed: 0 tests

**Reasoning:** The test suite is well-organized after reorganization. Each test layer (unit, behavioral, integration, construct, corpus) serves a distinct purpose. Tests that appear similar actually test different aspects:
- Unit tests validate individual functions
- Behavioral tests validate end-to-end extraction
- Integration tests validate feature combinations
- Construct tests validate language constructs
- Corpus tests validate data-driven expectations

**Conservative approach justified:** The 683 test count includes 135 corpus-generated tests. The explicit test count (548) is already at the baseline. With deprecated corpus tests already removed and no true redundancy found between explicit tests, aggressive reduction is not warranted.

**Recommendation:**
1. Move bead verification tests to integration/ (3 tests)
2. Keep all other tests as-is
3. Final count: 683 tests (matches current state after deprecated removals)

The original target of 520-530 was based on assumption of finding redundancy. Analysis shows the tests are well-differentiated and provide unique coverage at different layers.

## Detailed Test Movement Plan

### test-bead-3kgg-verification.el → integration/test-find-exec-patterns.el

**Rename tests:**
- `test-bead-3kgg-find-with-exec-rm` → `test-find-exec-rm-pattern`
- `test-bead-3kgg-find-with-multiple-exec-blocks` → `test-find-multiple-exec-blocks`
- `test-bead-3kgg-workspace-validation-scenario` → `test-find-exec-workspace-validation`

**Update file header:**
- Remove bead-specific commentary
- Update to standard integration test header
- Keep the valuable test patterns and assertions

**Tracking:** Document in test-migration-plan.md as "MOVED (integrated from verification test)"
