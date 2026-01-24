# Critical Finding: Error is with Choices, Not Answer

## Key Observation

From the transcript logs:

```
Answers: [{"id":"favorite_season","answer":"Summer - Warm weather and long days","comment":"","skipped":null}]
→ About to funcall callback...
→ ERROR: Wrong type argument: symbolp, "Spring - Fresh beginnings and blooming flowers"
```

**The user selected "Summer" but the error mentions "Spring" (the first choice).**

## Hypothesis

The error is NOT happening with:
- The answer the user selected ("Summer")
- The JSON encoding/decoding of the answer
- The transient callback mechanism
- The hash table storage

The error IS likely happening with:
- The original tool-use `:choices` array passed from the LLM
- Something in gptel trying to process the tool call arguments AFTER the tool returns
- Possibly gptel trying to intern or symbolize strings from the choices array

## Why All Our Tests Passed

All tests 1-8 passed because we tested:
1. Transient infrastructure
2. Hash tables and storage
3. Callback invocation
4. JSON encoding/decoding
5. Custom infix classes
6. Dynamic suffix generation
7. Plist handling
8. Plain string arguments

But we NEVER tested:
- How gptel processes the original tool-use arguments after the callback returns
- What gptel does with the `:choices` array after receiving the tool result

## Next Steps

1. **Examine gptel's tool result processing** - Look at what happens AFTER the async callback is invoked with the result string
2. **Check if gptel validates tool arguments** - Does it try to intern or process the choices array?
3. **Test with gptel's actual tool infrastructure** - Need to simulate gptel's tool-use handling, not just the callback

## Code Locations to Investigate

- `gptel-request.el`: Tool result processing after callback (around line 1702)
- `gptel-anthropic.el` (or relevant backend): How tool-use arguments are stored/processed
- Any code that processes `:choices` arrays from tool arguments after execution

## Potential Root Cause

gptel might be trying to:
- Validate that the answer is in the choices array
- Build a completion candidates list from choices
- Intern choice strings as symbols for some internal processing
- Store or cache choices using symbols as keys

The fact that it's "Spring" (first choice) rather than "Summer" (selected) strongly suggests it's iterating over the choices array.
