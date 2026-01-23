# Transient Question Tool Error Investigation

## Error Description

```
→ ERROR: Wrong type argument: symbolp, "Spring - Fresh beginnings and blooming flowers"
```

This error occurs when using the gptel question tool with transient menus. The string is one of the choices from a multiple-choice question.

## Investigation Progress

### Tests Completed (All Passing)

1. **Test 1: Basic Transient Menu** ✓
   - Basic transient structure definition works
   - Simple value storage works

2. **Test 2: Hash Table Storage** ✓
   - Storing values in hash tables works correctly
   - Multiple key-value pairs handled properly

3. **Test 3: Callback Invocation** ✓
   - Buffer-local callback storage works
   - Callback invocation with data works

4. **Test 4: Custom Infix Class** ✓
   - Custom transient infix classes work
   - EIEIO object creation and property access works

5. **Test 5: Custom Infix with Plist Data** ✓
   - Plist with :choices containing strings works fine
   - No symbol/string confusion in plist handling

6. **Test 6: JSON Building with Backquote** ✓
   - Backquote/unquote pattern for alist building works
   - JSON encoding/decoding works correctly
   - String values properly encoded and decoded

7. **Test 7: Dynamic Suffix Generation** ✓
   - Using :setup-children to generate suffixes works
   - String arguments (question IDs) work correctly
   - No symbol/string confusion in suffix specs

### Key Findings

1. **No issues in isolation**: All individual components work correctly:
   - Transient infrastructure
   - Hash table storage
   - Custom infix classes
   - Plist handling with string choices
   - JSON encoding/decoding
   - Dynamic suffix generation with string arguments

2. **The error is NOT in**:
   - The plist structure with :choices
   - The JSON building with backquote
   - The suffix generation with string IDs
   - The basic transient/callback pattern

3. **Where the error likely occurs**:
   - The error message "Wrong type argument: symbolp" suggests something is expecting a symbol but receiving a string
   - The error happens during callback invocation (based on user report)
   - The specific string "Spring - Fresh beginnings and blooming flowers" is from the answer choices

### Next Steps

Need to investigate:

1. **Test 8: Timer-Based Async Callback** (planned)
   - Test the exact pattern: quit transient → timer → callback in different buffer
   - This mirrors the question-tools async pattern

2. **Test 9: Buffer Context Switching** (planned)
   - Test invoking callback in origin buffer with `with-current-buffer`
   - This could reveal buffer-local variable issues

3. **Test 10: Full Integration** (planned)
   - Combine all patterns into one comprehensive test
   - Should reproduce the actual error if it's in our code

4. **Examine gptel-agent infrastructure**
   - The error might be in how gptel-agent processes tool results
   - Need to check if gptel-agent expects tool results in a specific format

### Hypothesis

Based on tests so far, the error is likely NOT in the question-tools code itself, but rather in:
- How gptel-agent processes the async tool callback result
- Some interaction between transient state and gptel's FSM
- Buffer-local variable handling across async boundaries

The string "Spring - Fresh beginnings and blooming flowers" successfully:
- Stores in hash table
- Encodes to JSON
- Decodes from JSON
- Passes through backquote pattern

But something downstream is trying to treat it as a symbol.
