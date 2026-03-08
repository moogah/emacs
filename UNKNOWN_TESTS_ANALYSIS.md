# Analysis: UNKNOWN Test Cases in `make test`

## Summary

When running `make test`, you see:
- **627 passed tests**
- **19 failed tests** (with `void-function` errors)
- **1 aborted test** (`test-scope-file-paths-absolute-path-unchanged`)
- **~257 UNKNOWN tests** (all tests after the abort)

## Root Cause

**The UNKNOWN tests are caused by a test abort that stops ERT from processing remaining test files.**

### What Happens:

1. **Test Discovery**: All test files are found and loaded successfully
2. **Tests Run**: 646 tests pass successfully
3. **Test 647 Aborts**: `test-scope-file-paths-absolute-path-unchanged` at `config/gptel/tools/test/test-scope-validation-file-paths.el:152` aborts with "non-local exit"
4. **ERT Stops**: When a test aborts in batch mode, ERT stops processing
5. **Remaining Tests**: All tests defined after the abort point show as UNKNOWN because they never got registered

### The Abort Location:

```
   passed  646/904  test-resolve-word-boundary (0.000039 sec)
Test test-scope-file-paths-absolute-path-unchanged aborted with non-local exit
  ABORTED  647/904  test-scope-file-paths-absolute-path-unchanged (0.000044 sec)
  UNKNOWN  test-scope-file-paths-case-sensitive-patterns
  UNKNOWN  test-scope-file-paths-copy-both-in-scope
  ...257 more UNKNOWN tests...
```

## Secondary Issue: Failed Tests

19 tests are failing with `(void-function jf/gptel-bash--parse-command)`:
- All "legacy" tests in `config/gptel/tools/test/test-scope-shell-tools-legacy.el`
- These tests call a function that either:
  - Doesn't exist
  - Isn't loaded during test initialization
  - Was renamed/removed

## Why "Non-Local Exit"?

"Non-local exit" in ERT typically means:
1. **Keyboard quit** (`C-g` / `SIGINT`) - unlikely in batch mode
2. **Unhandled signal** - error that escapes all condition handlers
3. **Stack overflow** - deeply nested recursion
4. **Missing dependency** - a critical function that causes Emacs to bail out

**No backtrace is generated**, which suggests the error occurs at a low level that ERT can't capture.

## Investigation Steps

### 1. Check for Missing Functions

The test at line 152 calls:
```elisp
(jf/gptel-scope--validate-file-operation file-op directory paths-config)
```

This function exists in `config/gptel/tools/scope-shell-tools.el`, but may depend on other functions that don't exist or aren't loaded.

### 2. Verify Dependencies

The test file requires:
```elisp
(require 'jf-gptel-scope-shell-tools
         (expand-file-name "scope-shell-tools.el" tools-dir))
```

Need to verify:
- Is `scope-shell-tools.el` properly loaded in batch mode?
- Are all its dependencies available?
- Does it have any initialization code that fails in batch mode?

### 3. Check for Initialization Issues

The abort happens on the FIRST test in that file, suggesting:
- The test file loads successfully (no syntax errors)
- The abort occurs when actually running the test
- Likely a missing function or uninitialized state

## Recommended Fixes

### Option 1: Fix the Abort (Recommended)

Debug `test-scope-file-paths-absolute-path-unchanged` to find why it aborts:

```bash
# Run just this test file
make emacs-test-eval EVAL_CMD='(progn
  (load-file "config/gptel/tools/test/test-scope-validation-file-paths.el")
  (ert-run-tests-batch-and-exit "test-scope-file-paths-absolute-path-unchanged"))'
```

### Option 2: Skip Problematic Tests Temporarily

Modify the test to be skipped:
```elisp
(ert-deftest test-scope-file-paths-absolute-path-unchanged ()
  :expected-result :failed  ; Mark as expected failure
  ...)
```

### Option 3: Load Order Fix

The issue might be that `scope-shell-tools.el` needs to be loaded before the test file, but some of its dependencies aren't available in batch mode.

## Quick Stats

- **Total tests discovered**: 904
- **Tests that ran**: 647
- **Tests passed**: 627
- **Tests failed**: 19 (void-function errors)
- **Tests aborted**: 1 (the blocker)
- **Tests unknown**: ~257 (never ran due to abort)

## Conclusion

The UNKNOWN tests are not actually broken - they're **never executed** because an earlier test aborts the entire test run. Fix the abort in `test-scope-file-paths-absolute-path-unchanged`, and all those UNKNOWN tests will either pass, fail properly, or reveal their own issues.

The real problem is test #647, not tests #648-904.
