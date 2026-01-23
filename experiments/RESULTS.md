# Test Results: gptel-transient Async Integration

## Summary

**All three experiments PASSED** ✓

The integration pattern CAN work. Real transient with mock gptel FSM successfully transitions state via callback.

## Experiment Results

### Experiment 1: Baseline (matches question-tools.el) - ✓ PASSED

Pattern tested:
1. Quit transient FIRST (`transient-quit-one`)
2. Invoke callback via timer (`run-at-time 0.05`)
3. Callback switches to origin buffer and modifies FSM

**Result:** FSM successfully transitioned from TOOL → WAIT

### Experiment 2: Callback Before Quit - ✓ PASSED

Pattern tested:
1. Call callback FIRST (while transient still active)
2. Then quit transient

**Result:** FSM successfully transitioned. Transient state does NOT interfere with callback.

**Implication:** The order doesn't matter - callback can run before or after quit.

### Experiment 3: Immediate Callback (No Timer) - ✓ PASSED

Pattern tested:
1. Quit transient
2. Call callback IMMEDIATELY (no timer delay)

**Result:** FSM successfully transitioned.

**Implication:** The 0.05s timer delay is NOT necessary - callback can run immediately.

## Key Findings

### What Works

1. **Buffer-local callback storage** - Storing callback outside transient scope works correctly
2. **Buffer context preservation** - `with-current-buffer` correctly switches to origin buffer
3. **FSM state modification** - Callback can successfully modify buffer-local FSM state
4. **Transient doesn't interfere** - Transient state/scope doesn't block callback execution

### Simplified Pattern

Based on experiment results, the simplest working pattern is:

```elisp
(defun submit ()
  (let ((result (build-result))
        (callback callback-var)
        (origin origin-var))

    (transient-quit-one)

    ;; No timer needed - call immediately
    (with-current-buffer origin
      (funcall callback result))))
```

Even simpler - callback can run BEFORE quit:

```elisp
(defun submit ()
  (let ((result (build-result)))
    (funcall callback result)  ; Call first
    (transient-quit-one)))      ; Then quit
```

## Critical Question

**If our tests pass, why does question-tools.el fail?**

Possible differences between test and real implementation:

1. **Real gptel FSM structure** - Mock FSM might be simpler than real gptel--fsm
2. **Real gptel callback** - The actual `process-tool-result` closure might have additional logic
3. **Buffer context** - Real gptel might have different buffer-local variables
4. **Error type** - "Wrong type argument: symbolp" suggests type coercion issue, not FSM transition failure
5. **Tool schema validation** - Error might occur BEFORE FSM transition during result processing

## Next Steps

### Investigate Real Implementation

1. **Compare callback closures**
   - Test: `(lambda (result) ...simple FSM modification...)`
   - Real: `process-tool-result` from `gptel--handle-tool-use`

2. **Check FSM structure**
   - Test: `(list :state 'TOOL :pending-tool-results '())`
   - Real: `gptel--fsm` - what's the actual structure?

3. **Trace callback execution**
   - Add detailed logging to `jf/gptel-questions--submit`
   - Check what `funcall callback answer-json` does internally

4. **Reproduce real error**
   - Use REAL gptel callback in test (if possible)
   - Or create test that matches real gptel structure more closely

### If Error is Elsewhere

The "Wrong type argument: symbolp" error might not be in FSM transition at all:

- Could be in JSON encoding (`json-encode`)
- Could be in gptel's result processing
- Could be in tool schema validation

**Action:** Add error tracing to identify exact line where error occurs.

## Conclusion

The core integration pattern WORKS. The problem is likely:

1. A specific detail of real gptel's implementation
2. An error in result formatting/encoding
3. Something that happens AFTER callback is invoked

The experiments eliminate "fundamental incompatibility" - transient and async callbacks CAN work together.
