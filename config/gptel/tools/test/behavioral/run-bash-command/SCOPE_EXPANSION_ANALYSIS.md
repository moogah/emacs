# Scope Expansion Behavioral Testing - Gap Analysis

## Executive Summary

After reviewing the scope expansion specification, implementation, and existing tests, I've identified gaps in behavioral test coverage and potential specification incompleteness. I've created comprehensive behavioral tests to cover the complete scope expansion workflow.

## Current State

### What Exists

1. **Spec Coverage** (`openspec/specs/gptel/scope-expansion.md`)
   - ✅ request_scope_expansion meta-tool requirements
   - ✅ Transient menu UI with 3 choices
   - ✅ Add to scope updates scope.yml
   - ✅ Allow-once list management
   - ✅ Expansion callback integration
   - ✅ Transient scope data passing
   - ✅ Validation type routing

2. **Implementation** (`config/gptel/tools/scope-shell-tools.org`, `config/gptel/scope/scope-expansion.org`)
   - ✅ request_scope_expansion tool (async, meta-categorized)
   - ✅ jf/gptel-scope-prompt-expansion entry point
   - ✅ Transient menu with deny/add-to-scope/allow-once actions
   - ✅ YAML updater functions for paths, patterns, bash commands
   - ✅ Allow-once buffer-local list management

3. **Existing Behavioral Tests** (`config/gptel/tools/test/behavioral/run-bash-command/`)
   - ✅ Parse completeness (stage 2)
   - ✅ Deny list validation (stage 3)
   - ✅ No-op allowance (stage 5)
   - ✅ Operation-specific paths (stage 6)
   - ✅ Cloud authentication (stage 7)
   - ✅ Error messages
   - ✅ Integration scenarios
   - ✅ Path resolution
   - ✅ Resource limits

### What Was Missing

**Behavioral test coverage for the complete scope expansion workflow:**

1. ❌ Validation failure → request_scope_expansion flow
2. ❌ request_scope_expansion → transient UI triggering
3. ❌ User choice workflows (deny, add to scope, allow once)
4. ❌ Allow-once consumption and retry behavior
5. ❌ Multiple expansion requests in same turn
6. ❌ Scope.yml updates and validation retry

## Implementation Flow Analysis

### Current Workflow (Two-Step Process)

```
┌─────────────────────────────────────────────────────────────┐
│ Step 1: Initial Command Fails Validation                   │
└─────────────────────────────────────────────────────────────┘
   LLM calls: run_bash_command("cat /tmp/file.txt")
   │
   ├─ Stage 1: Parse (bash-parser)
   ├─ Stage 2: Parse completeness check
   ├─ Stage 3: Deny list validation
   ├─ Stage 4: Semantic extraction
   ├─ Stage 5: No-op allowance
   ├─ Stage 6: File operation validation ← FAILS HERE
   │   └─ Error: path_out_of_scope
   │       {
   │         "error": "path_out_of_scope",
   │         "path": "/tmp/file.txt",
   │         "operation": "read",
   │         "message": "Use request_scope_expansion to request access"
   │       }
   └─ Returns error JSON to LLM

┌─────────────────────────────────────────────────────────────┐
│ Step 2: LLM Requests Scope Expansion (Separate Tool Call)  │
└─────────────────────────────────────────────────────────────┘
   LLM calls: request_scope_expansion(
     tool_name="run_bash_command",
     patterns=["/tmp/**"],
     justification="Need to read temporary files"
   )
   │
   ├─ Builds violation info from arguments
   ├─ Calls jf/gptel-scope-prompt-expansion
   │   └─ Opens transient menu with 3 choices
   │
   └─ User chooses:
       ├─ Deny → {success: false, user_denied: true}
       ├─ Add to scope → Updates scope.yml → {success: true, patterns_added: [...]}
       └─ Allow once → Adds to buffer-local list → {success: true, allowed_once: true}

┌─────────────────────────────────────────────────────────────┐
│ Step 3: (Optional) Retry Command with New Permission       │
└─────────────────────────────────────────────────────────────┘
   LLM calls: run_bash_command("cat /tmp/file.txt")  # retry
   │
   ├─ Stages 1-5: Same as before
   ├─ Stage 6: File operation validation
   │   └─ Check allow-once list FIRST
   │       └─ Match found → Permission CONSUMED → Validation succeeds
   │
   └─ Command executes successfully
```

### Key Design Points

1. **LLM Mediation Required**
   - Validation failures do NOT auto-trigger UI
   - LLM must make explicit second tool call to request_scope_expansion
   - This gives LLM control over when/how to request permissions

2. **Allow-Once Consumption Timing**
   - Permission consumed **during validation** (stage 6), not after execution
   - If command fails after validation, permission already consumed
   - Cannot retry with same allow-once permission

3. **Error Message Guidance**
   - Validation errors include structured info to guide LLM
   - Messages suggest using request_scope_expansion
   - **Exception:** Deny list errors do NOT suggest expansion (intentionally blocked)

## New Behavioral Tests Created

**File:** `scope-expansion-spec.el`

### Test Categories

1. **Validation Failure to Expansion Request Flow**
   - ✅ Returns structured error guiding LLM when path denied
   - ✅ Returns appropriate error when command in deny list (no expansion suggestion)

2. **request_scope_expansion Tool Behavior**
   - ✅ Builds violation info and calls jf/gptel-scope-prompt-expansion
   - ✅ Converts vector patterns to list for Elisp processing

3. **User Choice Workflows**
   - ✅ Deny choice returns error to LLM
   - ✅ Allow once choice grants temporary permission
   - ✅ Add to scope choice updates scope.yml for path-based resource

4. **Allow-Once Consumption and Retry**
   - ✅ Consumes allow-once permission before command execution
   - ✅ Second identical command fails after allow-once consumed
   - ✅ Multiple allow-once permissions tracked independently

### Test Strategy

- **Mock transient interaction:** Use `spy-on 'transient-scope` to simulate user choices
- **Mock bash execution:** Use helpers-spec mocks for parse/semantics
- **Test complete workflows:** Each test exercises multiple integration points
- **Verify state changes:** Check allow-once list, scope.yml updates, callback invocations

## Potential Specification Gaps

### 1. Auto-Triggering Scope Expansion UI

**Current:** LLM must explicitly call request_scope_expansion (two-step process)

**Possible Alternative:** Auto-trigger UI on validation failure (one-step process)

```
# Option A: Current (LLM-mediated)
run_bash_command → fails → LLM decides → request_scope_expansion → UI

# Option B: Auto-trigger (direct)
run_bash_command → fails → UI shown immediately → result to LLM
```

**Spec Question:** Should scope expansion UI trigger automatically, or require LLM mediation?

**Current Spec Says:** "Expansion UI SHALL integrate with gptel's async callback system" but doesn't specify if triggering is automatic or LLM-mediated.

**Recommendation:** Clarify in spec that LLM mediation is intentional design (gives LLM control over permission requests).

### 2. Allow-Once Consumption Timing Edge Cases

**Spec Says:** "Permission is consumed BEFORE tool body executes, not after"

**Edge Case Not Covered:**
- What if validation succeeds but tool execution fails?
- What if multiple file operations in same command?
- What if command retried due to execution error (not validation)?

**Example:**
```bash
# Command: cat file1.txt file2.txt
# Allow-once granted for file1.txt only
# What happens? Current impl would fail at validation for file2.txt
```

**Recommendation:** Add spec scenarios for multi-file commands and allow-once granularity.

### 3. Scope.yml Update Failure Handling

**Current Implementation:** `jf/gptel-scope--validate-scope-file-writable` signals `user-error`

**Spec Coverage:** Basic validation exists but doesn't cover:
- Concurrent modifications to scope.yml
- YAML parse errors during update
- Disk full / permission errors during write
- Callback behavior when update fails

**Recommendation:** Add spec requirements for update failure modes and error recovery.

### 4. Edit Scope Manual Option

**Spec Says:** "Edit scope opens file" and "Edit does not invoke callback"

**Unclear:**
- What should LLM do while user manually edits?
- Should LLM poll for changes?
- Should user signal completion somehow?
- What if user makes syntax errors in YAML?

**Current Implementation:** Opens file, quits transient, no callback invocation

**Recommendation:** Add spec scenario for edit-manual workflow completion and LLM coordination.

## Implementation Completeness

### ✅ Fully Implemented (Matches Spec)

- request_scope_expansion meta-tool
- Transient menu 3-choice UI
- Add to scope YAML updates (paths, patterns, bash)
- Allow-once buffer-local list
- Callback integration
- Transient scope data passing
- Validation type routing

### ⚠️ Partially Implemented (Spec Gaps)

- **Auto-trigger vs LLM-mediated:** Implementation uses LLM-mediated, spec unclear
- **Edit manual workflow:** Implementation basic, spec incomplete on coordination
- **Multi-file commands:** Implementation may have edge cases, spec doesn't cover

### ❌ Not Implemented (Missing from Spec)

- Concurrent scope.yml modification detection
- Allow-once granularity for multi-file commands
- Update failure recovery strategies

## Recommendations

### 1. Spec Updates Needed

1. **Add explicit requirement:** "Scope expansion SHALL require LLM to explicitly call request_scope_expansion (no auto-triggering)"
   - Rationale: LLM mediation gives control over permission flow

2. **Add scenario group:** "Multi-resource command handling"
   - Cover commands operating on multiple files
   - Define allow-once granularity (per-file vs per-command)

3. **Add scenario group:** "Update failure modes"
   - scope.yml write failures
   - Concurrent modification handling
   - Callback behavior on update errors

4. **Expand edit manual workflow:**
   - Define completion signal
   - LLM polling/waiting behavior
   - YAML syntax error handling

### 2. Implementation Enhancements

1. **Add YAML write error handling:**
   ```elisp
   (condition-case err
       (write-region ...)
     (file-error
      (funcall callback
               (json-serialize
                (list :success nil
                      :error "write_failed"
                      :message (error-message-string err))))))
   ```

2. **Add concurrent modification detection:**
   - Read file mtime before update
   - Check mtime after read, before write
   - Signal error if changed

3. **Consider multi-file allow-once:**
   - Current: `(tool . resource)` - single resource
   - Enhanced: `(tool . resource-list)` - multiple resources
   - Or: Pattern-based allow-once (e.g., "/tmp/*.txt")

### 3. Additional Behavioral Tests

**Already Created:**
- ✅ Basic scope expansion workflows (scope-expansion-spec.el)

**Should Add Later:**
- Concurrent scope.yml modification scenarios
- Multi-file command scenarios
- Update failure handling scenarios
- Edit manual workflow coordination

## Testing the New Tests

```bash
# Run new scope expansion tests
./bin/run-tests.sh -f buttercup -d config/gptel/tools/test/behavioral/run-bash-command -p 'scope-expansion'

# Or run all behavioral tests
./bin/run-tests.sh -f buttercup -d config/gptel/tools/test/behavioral
```

## Conclusion

The scope expansion implementation is **functionally complete** for the current spec, but both spec and implementation have gaps around:
- Auto-trigger vs LLM-mediation design intent
- Multi-file command handling
- Update failure modes
- Edit manual workflow coordination

The new behavioral tests cover the **core workflow** comprehensively. Additional tests should be added as spec is clarified and edge cases are addressed.
