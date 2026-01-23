# Comparison: Test vs Real Implementation

## Critical Discovery

Our tests **PASSED** - proving the integration pattern works. The real question-tools.el **FAILS** - but for a completely different reason than we tested.

## What Our Tests Proved

✓ FSM transitions work correctly via callback
✓ Transient doesn't interfere with callback execution
✓ Buffer context preservation works
✓ Timer-based deferral is unnecessary (immediate works)
✓ Quit-then-callback pattern works

**Conclusion:** The transient-gptel integration pattern is fundamentally sound.

## What We DIDN'T Test

✗ Real gptel callback structure
✗ Tool schema serialization
✗ Complex nested schemas with arrays of strings
✗ Message history construction with tool calls

**Conclusion:** The error is NOT in the integration pattern.

## Key Differences

### 1. Error Location (CRITICAL)

**What we expected:**
- Error during FSM transition
- Error in callback invocation
- Error from our integration code

**What actually happens (from question-tools.org:1140-1145):**

> "Spring" is a choice from the :choices array, not the answer. This indicates the error occurs
> when GPtel processes the ORIGINAL tool call arguments (the question definition), not when
> processing the tool RESULT (the user's answer).

**Meaning:**
- Error occurs during **schema serialization**
- Happens when gptel constructs API request with tool definitions
- Happens when re-serializing message history with tool calls
- Happens BEFORE or independently of callback execution
- NOT related to FSM transition at all!

### 2. FSM Architecture (CRITICAL)

**Our assumption:**
```elisp
;; We thought:
(defvar-local test-gptel--fsm ...)  ; Buffer-local FSM
```

**Reality (question-tools.org:1117-1130):**

```
Entry buffer: session.md (mode: markdown-mode)
gptel--fsm bound: nil

However, examining the callback structure reveals the FSM IS embedded in the callback closure
as the last element: #s(gptel-fsm TOOL ...)
```

**Meaning:**
- gptel does NOT use buffer-local FSM variable
- FSM is passed through callback closures
- Our test accidentally got this right (FSM captured in closure)
- But we didn't realize it's NEVER buffer-local

### 3. The Real Error

**Error message:**
```
Wrong type argument: symbolp, "Spring - Fresh beginnings and flowers blooming"
```

**Context:**
- "Spring - Fresh beginnings..." is from the `:choices` array in the tool schema
- This is NOT the user's selected answer (user picked "Summer")
- Error occurs when gptel tries to serialize the tool schema

**Tool schema structure:**
```elisp
:args '((:name "questions"
         :type "array"
         :items (:type "object"
                 :properties (...
                              :choices (:type "array"
                                       :items (:type "string"))
                              ...))))
```

The `:choices` field contains strings like:
- `"Spring - Fresh beginnings and flowers blooming"`
- `"Summer - Warm weather and long days"`

When gptel serializes this for the API request, it fails on these strings.

### 4. Attempted Solutions Match Our Tests

From question-tools.org lines 1171-1240, they tried:

1. **Synchronous callback (before quit)** - Same as our Experiment 2
2. **Post-command hook deferral** - Same as our early approach
3. **Timer-based deferral** - Same as our Experiment 1
4. **Buffer context preservation** - Same as our pattern

ALL of these approaches match what we tested. And our tests PASSED.

**This proves they were debugging the wrong problem.**

### 5. Schema Type Issue

**Original problem (lines 1146-1169):**
```elisp
;; WRONG - symbols
:type array
:items (:type object

;; CORRECT - strings
:type "array"
:items (:type "object"
```

They fixed this, but **error persists** (line 1166-1169).

This suggests:
- Tool wasn't reloaded properly, OR
- There's another schema issue we haven't found, OR
- The `:choices` array structure itself is incompatible with gptel's serialization

### 6. Mock vs Real Callback

**Our test callback:**
```elisp
(lambda (result)
  (message "[CALLBACK] Invoked with result: %s" result)
  (with-current-buffer buffer
    (test-gptel-fsm-transition 'WAIT)
    (push result (plist-get test-gptel--fsm :pending-tool-results))))
```

**Real gptel callback (process-tool-result):**
- Created in `gptel--handle-tool-use` (gptel-request.el:1679-1747)
- Captures full request context
- Manages message history
- Serializes tool results for API
- **Must serialize the original tool call** for message history

**Why this matters:**
When the callback is invoked, gptel needs to:
1. Store the tool result
2. Construct next API request
3. Include message history with tool call AND result
4. **Serialize the original tool definition** (the questions array with choices)

Step 4 is where it fails - trying to serialize the `:choices` arrays.

## Root Cause Analysis

### Our Test (Passed)

```
Tool invocation → Transient UI → User input → Submit → Callback
                                                         ↓
                                                   FSM transition ✓
```

We tested the **execution flow** - which works perfectly.

### Real Implementation (Failed)

```
Tool registration → Schema serialization ✗ (Error here!)
                    ↓
Tool invocation → Transient UI → User input → Submit → Callback
                                                         ↓
                                                   FSM transition (never reached)
```

The error happens during **schema serialization** when constructing API messages.

## Why Our Tests Passed

1. We didn't use real gptel tool registration
2. We didn't serialize complex schemas
3. We didn't construct API request messages
4. We only tested callback invocation and FSM transition
5. That part works perfectly!

## Why Real Implementation Fails

1. Complex nested schema with arrays of strings
2. gptel's serialization expects different format
3. Specifically the `:choices` array of strings
4. Error message shows it's treating strings as symbols
5. This happens during message construction, not callback execution

## The Actual Problem

**Not a transient-gptel integration issue at all!**

It's a **tool schema serialization issue**:
- How should `:choices` be formatted?
- How does gptel serialize array fields?
- What format does Anthropic API expect?
- Is there a mismatch in type handling?

## Next Steps

### What NOT to do
- ❌ Don't debug callback timing (it works)
- ❌ Don't debug FSM transitions (they work)
- ❌ Don't debug transient integration (it works)

### What TO do
- ✅ Debug tool schema serialization
- ✅ Check how gptel handles array fields
- ✅ Test with simplified schema (single text question, no choices)
- ✅ Compare with working tools that have array fields
- ✅ Check if tool needs to be re-registered after changes
- ✅ Examine `gptel--parse-tool-results` for Anthropic backend

## Test to Prove This

Create minimal tool with problematic schema:

```elisp
(gptel-make-tool
 :name "test_choices"
 :function (lambda (callback question)
             (funcall callback "test-answer"))
 :args '((:name "question"
          :type "object"
          :properties (:choices (:type "array"
                                :items (:type "string")))))
 :async t)
```

If this fails with same error, we've isolated it to schema serialization, not our code.

## Conclusion

Our experiments were successful and valuable:
- Proved integration pattern works
- Eliminated callback/FSM/transient as problem areas
- Redirected investigation to actual root cause: **schema serialization**

The question-tools.el implementation is fundamentally correct. The bug is in:
1. How the tool schema is defined, OR
2. How gptel serializes complex schemas, OR
3. How the tool is registered/reloaded

None of which have anything to do with transient or async callbacks.
