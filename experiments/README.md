# gptel-transient Async Integration Experiments

This directory contains experiments to isolate and understand the incompatibility between gptel's FSM-based async tool callbacks and transient's command-driven menu system.

## Usage

### Load the experiments

```elisp
M-x load-file RET experiments/gptel-transient-async.el RET
```

### Run individual experiments

```elisp
M-x gptel-exp-run-experiment-1  ; Baseline async pattern
M-x gptel-exp-run-experiment-2  ; Transient basics
M-x gptel-exp-run-experiment-3  ; Naive integration
M-x gptel-exp-run-experiment-4  ; Scope isolation
M-x gptel-exp-run-experiment-5  ; Execution context
M-x gptel-exp-run-experiment-6  ; FSM state access
```

### Run all experiments

```elisp
M-x gptel-exp-run-all
```

### View results

Results are displayed in the `*gptel-exp-log*` buffer, which opens automatically after each experiment.

You can also view the log manually:

```elisp
M-x gptel-exp-show-log
```

## Experiments

### Experiment 1: Baseline Async Pattern

Tests that gptel's closure-based async coordination works without transient.

**Expected:** FSM transitions correctly via callback.

### Experiment 2: Transient Basics

Understand transient's execution model and lifecycle timing.

**Expected:** Understand when commands return and hooks run.

### Experiment 3: Naive Integration

Attempt straightforward integration of callback with transient suffix using three strategies:
- Immediate invocation
- Post-command hook invocation
- Timer-based invocation

**Expected:** May reproduce the original error.

### Experiment 4: Transient Scope Isolation

Tests if transient's scope interferes with callback execution.

**Expected:** Callbacks work regardless of transient state.

### Experiment 5: Callback Execution Context

Tests if buffer context affects callback execution.

**Expected:** Closures work across buffers (lexical scoping).

### Experiment 6: FSM State Access Pattern

Simulates gptel's actual FSM transition mechanism with buffer-local state.

**Expected:** Callbacks can modify buffer-local state from any buffer.

## Analysis

After running experiments, document findings in the log buffer and determine:

1. **Root cause** of the incompatibility
2. **Working patterns** (if any)
3. **Recommendations** for implementation

## Files

- `gptel-transient-async.el` - All experiment code (lexical binding enabled)
- `gptel-transient-async.org` - Original org-babel notebook (deprecated due to binding issues)
- `README.md` - This file
