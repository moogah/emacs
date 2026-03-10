## Context

The current scope expansion workflow requires the LLM to make three separate tool calls when encountering a scope violation:

1. Call `run_bash_command` → validation fails → returns error suggesting `request_scope_expansion`
2. Call `request_scope_expansion` → UI shown → user approves → returns success
3. Retry `run_bash_command` → validation succeeds via allow-once → command executes

This creates friction for both the LLM (context switching between tools) and the user (waiting through multiple tool calls). The validation failure already has all the information needed to trigger the expansion UI directly.

**Current implementation**:
- `gptel-make-scoped-tool` macro wraps tools with synchronous validation
- Validation failures return error plists immediately
- `request_scope_expansion` is a separate async tool that shows expansion UI

**Constraint**: We cannot change `run_bash_command` signature or make it a breaking change for existing code.

**Stakeholders**: LLM interaction patterns, user approval workflows, scope validation system

## Goals / Non-Goals

**Goals:**

1. Enable inline expansion: Validation failures in `run_bash_command` automatically trigger expansion UI
2. Reduce tool calls: "Allow once" flow goes from 3 calls → 1 call
3. Maintain backward compatibility: `request_scope_expansion` continues to work for pre-emptive requests
4. No signature changes: `run_bash_command(command, directory)` stays the same
5. Preserve sync tool behavior: Non-async scope-aware tools unchanged

**Non-Goals:**

1. Modify expansion UI itself (already works via callback)
2. Change allow-once list semantics (existing behavior preserved)
3. Update scope.yml updaters (existing implementation reused)
4. Enable inline expansion for all scope-aware tools (only `run_bash_command` for now)
5. Auto-approve expansions (user interaction still required)

## Decisions

### Decision 1: Make validation wrapper async-aware instead of changing tool signatures

**Rationale**: The validation wrapper (`gptel-make-scoped-tool`) already intercepts tool calls. Making it detect `:async t` and handle expansion inline requires no tool signature changes.

**Alternatives considered**:
- Add `request_expansion` parameter to `run_bash_command` → Rejected: breaks signature, adds complexity
- Create new `run_bash_command_interactive` tool → Rejected: tool proliferation, unclear to LLM when to use each
- Always show expansion UI (no `:async` flag) → Rejected: breaks sync tools, forces all tools async

**Implementation**: Check if tool has `:async t` in `gptel-make-scoped-tool`. If async, wrap in callback-based expansion flow. If sync, maintain current behavior.

### Decision 2: Build violation info from validation error plists

**Rationale**: Validation errors already contain all the information expansion UI needs (tool, resource, operation, error type). We just need to transform the plist format.

**Alternatives considered**:
- Have validation pipeline return violation-info directly → Rejected: couples validation to expansion, breaks sync tools
- Store validation errors in buffer-local variables → Rejected: state management complexity, race conditions
- Pass validation errors through gptel callback → Rejected: leaks implementation details to gptel layer

**Implementation**: New function `jf/gptel-scope--build-violation-info` maps error plists to violation-info plists:
- `:error "path_out_of_scope"` → `:resource` is `:path` from error, `:validation-type` is `'bash`
- `:error "command_denied"` → `:resource` is `:command` from error
- `:error "cloud_auth_denied"` → `:resource` is `:provider` from error

### Decision 3: Retry validation after approval rather than bypassing it

**Rationale**: After user approves (allow-once or add-to-scope), the permission is stored in allow-once list or scope.yml. Retrying validation ensures consistency with the normal validation path and catches edge cases.

**Alternatives considered**:
- Skip validation retry, execute tool body directly → Rejected: bypasses allow-once consumption timing, breaks invariants
- Store "approved" flag and check it in validator → Rejected: adds state, couples expansion to validation logic
- Clone validation context and modify → Rejected: fragile, hard to maintain

**Implementation**: After user approves, call validation function again. Allow-once check happens first, so retry succeeds. Tool body executes normally.

### Decision 4: Use callback-based coordination between wrapper and expansion UI

**Rationale**: Expansion UI already uses callbacks (for `request_scope_expansion`). Reusing this pattern avoids creating a new coordination mechanism.

**Alternatives considered**:
- Promise/future-based async → Rejected: Emacs Lisp doesn't have native promises, would need polyfill
- Blocking wait for transient menu → Rejected: transient already blocks, but we need callback for gptel async tools
- Event-based signaling → Rejected: more complex, harder to test

**Implementation**:
1. Validation wrapper calls `jf/gptel-scope--trigger-inline-expansion` with callback
2. Expansion trigger calls `jf/gptel-scope-prompt-expansion` (existing) with nested callback
3. User choice invokes nested callback → expansion trigger's callback → validation wrapper's callback
4. Callback chain: User action → Expansion UI → Trigger → Wrapper → Tool body → gptel callback

### Decision 5: Mark only `run_bash_command` as async, leave other scope-aware tools sync

**Rationale**: `run_bash_command` is the highest-friction tool (most common scope violations). Other tools (read_file, write_file_in_scope) have simpler patterns and less frequent violations.

**Alternatives considered**:
- Make all scope-aware tools async → Rejected: unnecessary churn, testing burden
- Make no tools async, add global flag → Rejected: loses per-tool granularity
- Auto-detect which tools would benefit → Rejected: hard to measure, premature optimization

**Implementation**: Add `:async t` only to `run_bash_command` tool registration in scope-shell-tools.org. Validation wrapper detects this and enables inline expansion. Other tools unchanged.

### Decision 6: Preserve error message format when user denies expansion

**Rationale**: If user denies expansion, the error returned to LLM should match the existing validation error format. This maintains LLM's understanding of scope violations.

**Alternatives considered**:
- Add "user_denied" flag to error → Rejected: changes error schema, confuses LLM
- Return different error when denied vs. initial failure → Rejected: inconsistent LLM experience
- Remove suggestion to use request_scope_expansion → Accepted: user already saw UI, no need to suggest tool

**Implementation**: When user denies, return original validation error plist, but remove any suggestion to use `request_scope_expansion` (since user just interacted with expansion UI).

## Implementation Approach

### Phase 1: Violation Info Construction

**File**: `config/gptel/scope/scope-core.org`

**Add function**: `jf/gptel-scope--build-violation-info`

```elisp
(defun jf/gptel-scope--build-violation-info (validation-error tool-name)
  "Build violation info plist from VALIDATION-ERROR for TOOL-NAME.
Maps error types to resources for expansion UI."
  (let* ((error-type (plist-get validation-error :error))
         (resource (pcase error-type
                    ("path_out_of_scope" (plist-get validation-error :path))
                    ("command_denied" (plist-get validation-error :command))
                    ("cloud_auth_denied" (plist-get validation-error :provider))
                    ("incomplete_parse" (plist-get validation-error :command))
                    (_ (plist-get validation-error :path))))
         (operation (plist-get validation-error :operation))
         (validation-type (jf/gptel-scope--infer-validation-type tool-name)))
    (list :tool tool-name
          :resource resource
          :operation operation
          :reason (plist-get validation-error :message)
          :validation-type validation-type)))
```

**Testing**: Unit test that validates plist transformation for each error type.

### Phase 2: Inline Expansion Trigger

**File**: `config/gptel/scope/scope-core.org`

**Add function**: `jf/gptel-scope--trigger-inline-expansion`

```elisp
(defun jf/gptel-scope--trigger-inline-expansion (validation-error tool-name callback)
  "Trigger inline expansion UI for VALIDATION-ERROR from TOOL-NAME.
CALLBACK is invoked with (:approved t/nil) when user makes choice."
  (let* ((violation-info (jf/gptel-scope--build-violation-info
                          validation-error tool-name))
         (resource (plist-get violation-info :resource))
         (patterns (list resource))  ; Single pattern for inline expansion
         (expansion-callback (lambda (result-json)
                              (let ((result (json-parse-string result-json
                                                              :object-type 'plist)))
                                (if (plist-get result :success)
                                    (funcall callback (list :approved t))
                                  (funcall callback (list :approved nil
                                                         :reason "user_denied")))))))
    ;; Invoke expansion UI (existing function)
    (jf/gptel-scope-prompt-expansion violation-info
                                     expansion-callback
                                     patterns
                                     tool-name)))
```

**Testing**: Behavioral test that mocks expansion UI and verifies callback invocation.

### Phase 3: Async-Capable Validation Wrapper

**File**: `config/gptel/scope/scope-core.org`

**Modify macro**: `gptel-make-scoped-tool`

**Current signature**:
```elisp
(gptel-make-scoped-tool tool-name description args validation-type tool-body)
```

**Add `:async` keyword detection**:

Look for `:async t` in the calling context (tool registration) and branch on it:

```elisp
(defmacro gptel-make-scoped-tool (tool-name description args validation-type &rest tool-body)
  "Wrap TOOL-BODY with scope validation.
If tool is registered with :async t, validation failures trigger inline expansion.
Otherwise validation failures return error immediately (current behavior)."
  (let* ((is-async (plist-get (symbol-plist (intern tool-name)) :gptel-async))
         (arg-names (mapcar (lambda (arg) (plist-get arg :name)) args))
         (first-arg (car arg-names)))
    (if is-async
        ;; ASYNC BRANCH: Wrap in callback-based expansion flow
        `(lambda (callback ,@arg-names)
           (let* ((result (jf/gptel-scope--validate-tool
                          ,tool-name ,first-arg ,validation-type)))
             (if result
                 ;; Validation failed - trigger inline expansion
                 (jf/gptel-scope--trigger-inline-expansion
                  result
                  ,tool-name
                  (lambda (expansion-result)
                    (if (plist-get expansion-result :approved)
                        ;; User approved - retry validation and execute
                        (let ((retry-result (jf/gptel-scope--validate-tool
                                             ,tool-name ,first-arg ,validation-type)))
                          (if retry-result
                              ;; Still failed - return error
                              (funcall callback (json-serialize retry-result))
                            ;; Validation passed - execute tool body
                            (let ((tool-result (progn ,@tool-body)))
                              (funcall callback (json-serialize tool-result)))))
                      ;; User denied - return original error
                      (funcall callback (json-serialize result)))))
               ;; Validation passed - execute tool body
               (let ((tool-result (progn ,@tool-body)))
                 (funcall callback (json-serialize tool-result))))))

      ;; SYNC BRANCH: Current behavior (unchanged)
      `(lambda (,@arg-names)
         (let ((result (jf/gptel-scope--validate-tool
                        ,tool-name ,first-arg ,validation-type)))
           (if result
               ;; Validation failed - return error
               result
             ;; Validation passed - execute tool body
             (progn ,@tool-body)))))))
```

**Key changes**:
- Detect `:async t` property on tool symbol
- Branch on async vs sync
- Async branch: validation failure → trigger expansion → callback with retry or error
- Sync branch: validation failure → return error immediately (unchanged)

**Testing**: Behavioral tests for both async and sync branches.

### Phase 4: Mark run_bash_command as Async

**File**: `config/gptel/tools/scope-shell-tools.org`

**Modify tool registration**: Add `:async t` to tool definition

**Before**:
```elisp
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command..."
 '((:name "command" :type string ...)
   (:name "directory" :type string ...))
 "bash"
 ;; Tool body
 ...)
```

**After**:
```elisp
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command..."
 '((:name "command" :type string ...)
   (:name "directory" :type string ...))
 "bash"
 :async t  ; NEW: Enable inline expansion
 ;; Tool body
 ...)
```

**Implementation note**: The `:async t` needs to be stored on the tool symbol's plist before `gptel-make-scoped-tool` expansion, so the macro can detect it.

**Alternative approach** (if plist doesn't work): Pass `:async` as explicit parameter to macro:

```elisp
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command..."
 '((:name "command" :type string ...)
   (:name "directory" :type string ...))
 "bash"
 :async  ; Keyword parameter
 ;; Tool body
 ...)
```

Then modify macro signature:
```elisp
(defmacro gptel-make-scoped-tool (tool-name description args validation-type
                                  &optional async-keyword &rest tool-body)
  (let ((is-async (eq async-keyword :async)))
    ...))
```

**Testing**: Behavioral test that verifies async expansion flow for run_bash_command.

### Phase 5: Behavioral Tests

**File**: `config/gptel/tools/test/behavioral/run-bash-command/inline-expansion-spec.el`

**Test scenarios** (from architecture.md):

```elisp
(describe "run_bash_command: Inline scope expansion"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Validation failure triggers expansion UI"

    (it "shows transient menu when path out of scope"
      ;; Mock validation to fail
      ;; Spy on jf/gptel-scope-prompt-expansion
      ;; Verify expansion UI called with correct violation-info
      ))

  (describe "User approval flows"

    (it "executes command when allow-once granted"
      ;; Mock expansion UI to approve with allow-once
      ;; Verify command executes in same tool call
      ;; Verify success response returned
      )

    (it "executes command when added to scope"
      ;; Mock expansion UI to approve with add-to-scope
      ;; Verify scope.yml updated
      ;; Verify command executes
      ))

  (describe "User denial flows"

    (it "returns error when expansion denied"
      ;; Mock expansion UI to deny
      ;; Verify error returned
      ;; Verify command not executed
      ))

  (describe "Backward compatibility"

    (it "request_scope_expansion still works for pre-emptive requests"
      ;; Call request_scope_expansion
      ;; Approve with allow-once
      ;; Call run_bash_command
      ;; Verify command succeeds
      )))
```

**Test helpers** (reuse from scope-expansion-spec.el):
- `helpers-spec-mock-bash-parse` - Mock bash parser
- `helpers-spec-mock-bash-semantics` - Mock semantic extraction
- `helpers-spec-setup-session` - Session context
- `helpers-spec-make-scope-yml` - Temporary scope.yml

**New mocking pattern**:
```elisp
;; Mock expansion UI to simulate user approval
(spy-on 'jf/gptel-scope-prompt-expansion
        :and-call-fake (lambda (violation-info callback patterns tool-name)
                        ;; Simulate allow-once approval
                        (funcall callback
                                 (json-serialize
                                  (list :success t
                                        :allowed_once t)))))
```

## Risks / Trade-offs

### Risk 1: Async callback coordination complexity

**Description**: Nested callbacks (validation wrapper → expansion trigger → expansion UI → user action) create complex control flow that's hard to debug.

**Mitigation**:
- Comprehensive behavioral tests with mocked expansion UI
- Logging at each callback boundary (if debug mode enabled)
- Unit tests for violation info construction (isolated from async flow)

### Risk 2: Breaking existing sync tool behavior

**Description**: Changes to `gptel-make-scoped-tool` macro could break non-async tools.

**Mitigation**:
- Explicit branching on `:async` flag (default is sync behavior)
- Existing tests for sync tools continue to pass
- Only mark `run_bash_command` as async initially (incremental rollout)

### Risk 3: Transient menu blocks Emacs UI during expansion

**Description**: User must respond to expansion UI before other work can proceed.

**Mitigation**:
- This is existing behavior (request_scope_expansion already blocks)
- User can quit transient menu (q key) to abort
- No new blocking introduced by this change

**Trade-off accepted**: Interactive approval inherently blocks.

### Risk 4: Validation retry could fail due to race conditions

**Description**: Between user approval and validation retry, allow-once list or scope.yml could be modified by concurrent operations.

**Mitigation**:
- Allow-once list is buffer-local (no cross-buffer races)
- Scope.yml updates are single-threaded (Emacs is single-threaded)
- Validation retry happens immediately after approval (minimal window)

**Trade-off accepted**: Edge case is unlikely and self-correcting (user sees error, can retry).

### Risk 5: Error message changes could confuse LLM

**Description**: If denied expansion errors differ from initial validation errors, LLM might not understand scope violations.

**Mitigation**:
- Return original validation error on denial
- Only remove "suggest request_scope_expansion" message (user already interacted)
- Error structure identical to current format

### Risk 6: Testing complexity with async mocking

**Description**: Testing callback-based async flows requires sophisticated mocking.

**Mitigation**:
- Use Buttercup spy system (built-in async support)
- Reuse existing test helpers from scope-expansion-spec.el
- Test violation info construction in isolation (pure function)

## Migration Plan

### Deployment Steps

1. **Phase 1**: Implement violation info construction and test in isolation
2. **Phase 2**: Implement inline expansion trigger and test with mocked expansion UI
3. **Phase 3**: Modify `gptel-make-scoped-tool` macro with async branching
4. **Phase 4**: Mark `run_bash_command` as async
5. **Phase 5**: Add behavioral tests for inline expansion
6. **Phase 6**: Update existing behavioral tests to handle async expansion (if needed)

### Rollback Strategy

If issues discovered:

1. **Immediate rollback**: Remove `:async t` from `run_bash_command` tool registration
   - Tool reverts to sync behavior (current state)
   - No data loss, no broken sessions

2. **Full rollback**: Revert macro changes to `gptel-make-scoped-tool`
   - All tools revert to sync behavior
   - `request_scope_expansion` still works

3. **No data migration needed**: Allow-once list and scope.yml format unchanged

### Compatibility

- **Backward compatible**: Existing sync tools unchanged
- **Forward compatible**: New tools can opt-in to async with `:async t`
- **No breaking changes**: `run_bash_command` signature unchanged

## Open Questions

None - design is complete and ready for implementation.
