# gptel-transient Async Integration Tests

This directory contains tests to isolate and understand the incompatibility between gptel's FSM-based async tool callbacks and transient's command-driven menu system.

## Approach

- **Use REAL transient code** (from runtime/straight/build/transient)
- **Simulate ONLY gptel FSM** (minimal mock to avoid external dependencies)
- **No user interaction required** (programmatically set answers and call submit)
- **Runnable in batch mode** (automated testing via script)

## Quick Start

### Run all tests (batch mode)

```bash
./experiments/run-tests.sh
```

### Run individual experiments (interactive)

```elisp
emacs -Q -l experiments/test-transient-async.el -f test-run-experiment-1
```

## Experiments

### Experiment 1: Baseline (matches question-tools.el pattern)

Tests the exact pattern used in `config/gptel/tools/question-tools.el`:

1. Create gptel buffer with FSM
2. Create callback closure
3. Set up transient with test data
4. Programmatically fill in answers
5. Call `transient-quit-one` FIRST
6. Invoke callback via `run-at-time` timer (0.05s delay)
7. Verify FSM transitions correctly

**Expected:** FSM transitions from TOOL → WAIT after callback.

### Experiment 2: Callback Before Quit

Tests calling the callback BEFORE `transient-quit-one`:

1. Set up transient
2. Fill in answers
3. Call callback FIRST
4. Then quit transient

**Tests:** Does transient state interfere with callback execution?

### Experiment 3: Immediate Callback (No Timer)

Tests if the timer is necessary:

1. Set up transient
2. Fill in answers
3. Quit transient
4. Call callback IMMEDIATELY (no `run-at-time`)

**Tests:** Is the 0.05s delay required, or can callback run immediately?

## Test Structure

### Mock gptel FSM

Minimal simulation of gptel's FSM:

```elisp
(defvar-local test-gptel--fsm
  (list :state 'TOOL
        :pending-tool-results '()))
```

Callback closure captures buffer and modifies FSM state:

```elisp
(lambda (result)
  (with-current-buffer buffer
    ;; Transition FSM
    (test-gptel-fsm-transition 'WAIT)
    ;; Store result
    (push result pending-tool-results)))
```

### Real Transient Menu

Uses actual transient code with simplified menu matching question-tools.el structure:

- Stores callback in buffer-local variable (NOT in transient scope)
- Stores origin buffer for callback invocation
- `test-submit` function matches `jf/gptel-questions--submit` pattern

## Key Patterns Being Tested

### Pattern 1: Buffer-Local Callback Storage

```elisp
(defvar-local test-callback nil)
(defvar-local test-origin-buffer nil)

;; Store callback outside transient scope
(setq-local test-callback callback)
(setq-local test-origin-buffer origin-buffer)
```

**Why:** Avoids transient scope serialization issues.

### Pattern 2: Quit-Then-Callback

```elisp
(defun test-submit ()
  (let ((result (test-build-result)))
    ;; Close transient FIRST
    (transient-quit-one)

    ;; Invoke callback AFTER transient cleanup
    (run-at-time 0.05 nil
      (lambda ()
        (funcall callback result)))))
```

**Why:** Ensures transient state doesn't interfere with callback.

### Pattern 3: Buffer Context Preservation

```elisp
(lambda (result)
  (with-current-buffer origin-buffer
    ;; Modify FSM state
    (test-gptel-fsm-transition 'WAIT)))
```

**Why:** FSM state is buffer-local, callback must run in correct buffer.

## Expected Outcomes

### Success Scenario

All three experiments pass:
- Experiment 1: Timer-based callback works
- Experiment 2: Callback-before-quit works OR fails with clear error
- Experiment 3: Immediate callback works OR timer is necessary

→ Identifies working integration pattern.

### Failure Scenario

One or more experiments fail:
- Document specific failure mode
- Identify which pattern element is problematic
- Propose alternative approaches

## Analyzing Results

Look for these in the output:

- `[FSM] Transition: TOOL -> WAIT` - FSM transitioning correctly
- `[CALLBACK] FSM state after: WAIT` - Callback successfully modified FSM
- `✓ EXPERIMENT N PASSED` - Test succeeded
- `✗ EXPERIMENT N FAILED` - Test failed with details

## Files

- `test-transient-async.el` - Test suite (runnable in batch or interactive)
- `run-tests.sh` - Batch mode test runner
- `gptel-transient-async.el` - Old mock-based experiments (deprecated)
- `gptel-transient-async.org` - Org-babel notebook (deprecated)
- `README.md` - This file

## Dependencies

- Emacs 29.1+ (for transient compatibility)
- transient.el (loaded from runtime/straight/build/transient)
- No other dependencies (gptel is mocked)

## Next Steps After Testing

1. **If all tests pass:** Update `question-tools.el` with any refinements discovered
2. **If tests fail:** Document the specific incompatibility and explore alternatives:
   - Use `completing-read-multiple` instead of transient
   - Use dedicated buffer UI instead of transient
   - Use synchronous tool (less user-friendly but simpler)
