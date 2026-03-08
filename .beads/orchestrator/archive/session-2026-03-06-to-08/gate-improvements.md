# Orchestrator Gate Improvements

## Required Changes to Prevent Future Regressions

### 1. Mandatory Test Verification After Each Merge

**Current (broken):**
```bash
git merge --no-ff branch
# Move to next merge
```

**Required:**
```bash
git merge --no-ff branch

# MANDATORY: Test and verify
./bin/run-tests.sh --snapshot
CURRENT_TESTS=$(grep "Ran.*tests" test-results.txt | grep -oE "[0-9]+ results as expected")
UNEXPECTED=$(grep "Ran.*tests" test-results.txt | grep -oE "[0-9]+ unexpected")

if [ "$UNEXPECTED" != "0 unexpected" ]; then
  echo "⚠️ REGRESSION DETECTED after merge $BEAD_ID"
  echo "Unexpected: $UNEXPECTED"
  
  # STOP orchestration
  # Report to user
  # Keep all worktrees
  exit 1
fi

echo "✓ Tests pass: $CURRENT_TESTS, $UNEXPECTED"
```

### 2. Automated Test Count Verification

Add to state file after each merge:
```json
{
  "bead_id": "emacs-qstn",
  "status": "merged",
  "tests_before_merge": 588,
  "tests_after_merge": 588,
  "unexpected_failures": 0,
  "regression_detected": false
}
```

### 3. Conflict Resolution Protocol

When conflicts occur:
1. Resolve conflicts
2. **Re-run full test suite** (don't skip!)
3. Verify test count unchanged
4. Only then continue to next merge

**Add explicit gate:**
```bash
if [ conflict resolved ]; then
  echo "Running tests after conflict resolution..."
  ./bin/run-tests.sh --snapshot
  # Check results (as above)
fi
```

### 4. Agent Work Verification

Currently: Trust agent's final message
Required: Verify in main orchestrator

```bash
# After agent completes
cd "$WORKTREE_PATH"
./bin/run-tests.sh

if [ tests fail ]; then
  echo "⚠️ Agent $BEAD_ID produced failing tests in worktree"
  echo "Keeping worktree for investigation"
  status="failed_tests"
fi
```

### 5. Parallel vs Sequential Trade-offs

**Current mistake:** Batched merges (2-4, 5-7) without testing
**Why dangerous:** Can't identify which merge caused regression

**Options:**
- Option A: Test after EVERY merge (slow but safe) ← **RECOMMENDED**
- Option B: Test after batches (fast but risky)
- Option C: Test after each bead from a logical batch (compromise)

**Recommendation:** Always test after each merge. The ~30s test time is worth catching regressions immediately.

### 6. Output Verification Standards

**Don't rely on:**
- Tail output (may show passing tests while summary shows failures)
- Memory/assumptions ("I think tests passed")
- Agent reports (verify independently)

**Do rely on:**
- Grep'ing for "Ran X tests, Y unexpected"
- Diff'ing test-results.txt against baseline
- Explicit pass/fail criteria

### 7. State File as Source of Truth

Update state file immediately after verification:
```bash
# After successful merge
jq '.beads[] | select(.bead_id == "$BEAD_ID") | .tests_passing = true' state.json

# Before next merge, check:
PREV_TESTS_PASSED=$(jq '.beads[-1].tests_passing' state.json)
if [ "$PREV_TESTS_PASSED" != "true" ]; then
  echo "⚠️ Previous merge had test failures, aborting"
  exit 1
fi
```

## Implementation Priority

1. **Critical (P0):** Add mandatory test verification after each merge
2. **High (P1):** Add test count tracking to state file
3. **High (P1):** Add conflict resolution re-testing
4. **Medium (P2):** Add agent worktree testing
5. **Medium (P2):** Improve output verification (grep, not tail)

## Lessons Learned

1. **Never skip gates during conflict resolution** - that's when bugs hide
2. **Don't batch merges** - test after each one
3. **Verify, don't assume** - even when agents report success
4. **Automate verification** - humans miss things under time pressure

## Next Steps

1. Update bead-orchestrator skill with these gates
2. Add test verification code blocks
3. Update state file schema to track test results
4. Add regression detection to conflict resolution path
