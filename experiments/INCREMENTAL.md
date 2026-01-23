# Incremental Testing Approach

Goal: Add complexity step-by-step until we reproduce the error.

## Test Increments

### Increment 1: Baseline
- Simple callback
- Simple string result
- **Expected:** PASS (proven by test-transient-async.el)

### Increment 2: JSON Result
- Callback expects JSON string
- Result is JSON-encoded alist
- Matches question-tools.el format: `[{"id":"q1","answer":"test"}]`
- **Expected:** PASS

### Increment 3: Complex Choices in Closure
- Question definition with `:choices` array captured in closure
- Choices are long strings like `"Spring - Fresh blooms and mild weather"`
- Callback has access to question (like real gptel callback captures tool call)
- Callback tries to access/iterate over choices
- **Expected:** May FAIL - this might reproduce the error

### Increment 4: Real Tool Registration
- Register actual gptel tool with simple schema
- Test tool registration/serialization
- **Expected:** PASS if schema is simple

### Increment 5: Real Tool with Complex Schema
- Register tool with nested array schema
- Schema matches question-tools.el structure
- **Expected:** May FAIL if schema serialization is the issue

## Running Tests

Load in interactive Emacs (not batch mode - we need to inspect results):

```elisp
M-x load-file RET experiments/test-incremental.el RET
```

Run tests one at a time:

```elisp
M-x test-inc-1-baseline       ; Should PASS
M-x test-inc-2-json-result    ; Should PASS
M-x test-inc-3-complex-choices ; May reproduce error
M-x test-inc-4-real-tool      ; Requires gptel loaded
M-x test-inc-5-complex-schema ; May reproduce error
```

Or run all:

```elisp
M-x test-inc-run-all
```

Each test displays results in `*test-incremental-log*` buffer.

## What to Look For

### If Increment 3 PASSES
- Error is NOT in callback closure structure
- Continue to Increment 4/5

### If Increment 3 FAILS
- We've reproduced it!
- Error is in how we access choices in callback
- Check error message - does it match real error?

### If Increment 5 FAILS
- Error is in tool schema serialization
- Check when it fails (registration or invocation?)

## Next Steps After Reproducing

Once we find the increment that fails:
1. Note exact error message
2. Compare with real error from question-tools.el
3. Isolate the specific line/operation that fails
4. Fix that specific issue
5. Verify fix works in both test and real implementation
