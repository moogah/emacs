# Plan: Reproduce Error After Transient Completion

## Objective

Reproduce the error "Wrong type argument: symbolp, 'Spring - Fresh beginnings...'" that occurs AFTER the transient menu completes and returns its result to gptel.

## What We Know

1. **Error location**: Happens in gptel's tool result processing, NOT in question-tools
2. **Error target**: First choice string from `:choices` array ("Spring"), not selected answer ("Summer")
3. **Error timing**: After async callback invokes `funcall callback answer-json`
4. **Error context**: Something in gptel tries to intern/symbolize the choices array

## Investigation Strategy

### Phase 1: Examine gptel's Tool Processing Code

**Goal**: Understand what gptel does with tool arguments after async callback returns.

**Files to examine**:
- `gptel-request.el:1700-1750` - Tool result processing
- `gptel-anthropic.el:300-320` - Parse tool results for Anthropic backend
- Any code that validates answers against choices

**Key questions**:
- Does gptel validate that the answer is in the original choices?
- Does gptel try to build completion candidates from choices?
- Does gptel intern choice strings anywhere?
- Is there a bug in how gptel handles string vs symbol in choices?

### Phase 2: Create Minimal gptel Simulation

**Test 9: Simulate gptel's tool-use flow**

Create a test that mimics gptel's complete workflow:
1. Parse tool spec (with `:choices` array)
2. Store tool-use arguments (including choices)
3. Invoke async tool function with callback
4. Process callback result
5. **Validate result or process original arguments** ← Error likely here

**File**: `experiments/test-9-gptel-simulation.el`

**Components to simulate**:
```elisp
;; 1. Tool-use plist (from LLM)
(:name "ask_questions"
 :args (:questions [(:id "q1"
                     :type "multiple-choice"
                     :choices ("Spring..." "Summer..." "Fall..." "Winter..."))]))

;; 2. Store tool-use in info plist
(plist-put info :tool-use (list tool-call))

;; 3. Invoke async tool
(apply (gptel-tool-function tool-spec) process-tool-result arg-values)

;; 4. Callback invoked
(funcall process-tool-result answer-json)

;; 5. Process result - THIS IS WHERE ERROR OCCURS
;; Need to find what gptel does here with the original :choices
```

### Phase 3: Identify Exact Error Location

**Method**: Add instrumentation to gptel code or use edebug

**Candidate locations**:
- Validation: Check if answer is in choices (may try to intern for comparison)
- Logging: Format choices for debug output (may try to symbolize)
- Caching: Store choices for history (may use symbols as keys)
- Completion: Build candidates list (may intern strings)

### Phase 4: Reproduce in Isolated Test

**Test 10: Minimal reproduction**

Once we find the exact code path, create a minimal test that:
1. Simulates only the problematic code path
2. Uses the actual :choices array format
3. Demonstrates the symbolp error
4. Provides clear fix guidance

## Expected Workflow

```
Test 9: Simulate gptel flow
├─ IF error reproduces: Identify exact line
│  └─ Test 10: Minimal reproduction of that line
│     └─ Propose fix to gptel
│
└─ IF error doesn't reproduce:
   ├─ Examine actual gptel source more carefully
   ├─ Check if backend-specific (Anthropic vs OpenAI)
   └─ Look for version-specific issues
```

## Success Criteria

1. **Reproduce the error** in a controlled test
2. **Identify the exact line** of gptel code that fails
3. **Understand the root cause** (why it expects symbol, gets string)
4. **Propose a fix** (either to gptel or question-tools)

## Implementation Notes

### Simulating Tool-Use Arguments

```elisp
;; Tool-use structure from LLM
(defvar test-tool-use
  '(:id "call_123"
    :name "ask_questions"
    :args (:questions [(:id "q1"
                        :type "multiple-choice"
                        :prompt "Favorite season?"
                        :choices ["Spring - Fresh..."
                                 "Summer - Warm..."
                                 "Fall - Colorful..."
                                 "Winter - Snow..."])])))
```

### Simulating Callback Pattern

```elisp
(letrec ((process-tool-result
          (lambda (result)
            ;; 1. Store result
            (plist-put tool-call :result result)

            ;; 2. Parse result (this is where error might occur)
            ;; gptel might try to validate result against :choices here

            ;; 3. Format for next API call
            (gptel--parse-tool-results backend (list tool-call)))))

  ;; Invoke async tool
  (apply tool-function process-tool-result arg-values))
```

### Key Insight

The error happens AFTER our code succeeds. We need to simulate what gptel does with the tool-use data structure after receiving our result, not just test our own code in isolation.
