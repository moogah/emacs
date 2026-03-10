## Components

### Async-Capable Scope Validation Wrapper

**Responsibility**: Wrap scope-aware tools with validation that can either return errors synchronously or trigger async expansion UI.

**Location**: `config/gptel/scope/scope-core.org` - `gptel-make-scoped-tool` macro

**Key behaviors**:
- Detect if tool is registered with `:async t`
- For async tools: On validation failure, trigger expansion UI and wait for user response
- For sync tools: On validation failure, return error immediately (current behavior)
- After approval: Retry validation and execute tool body in same tool call
- After denial: Return structured error to LLM

### Inline Expansion Trigger

**Responsibility**: Build violation info from validation errors and invoke expansion UI.

**Location**: `config/gptel/scope/scope-core.org` - new function `jf/gptel-scope--trigger-inline-expansion`

**Key behaviors**:
- Accept validation error plist as input
- Extract tool name, resource, operation, and validation type
- Build violation-info plist compatible with `jf/gptel-scope-prompt-expansion`
- Invoke expansion UI with callback that signals completion to validation wrapper

### Expansion UI (Existing)

**Responsibility**: Display transient menu and handle user choices.

**Location**: `config/gptel/scope/scope-expansion.org` - `jf/gptel-scope-prompt-expansion`

**No changes needed**: This component already supports the callback pattern and will work for both inline expansion and pre-emptive `request_scope_expansion`.

### Tool Registration (Modified)

**Responsibility**: Register `run_bash_command` as async tool.

**Location**: `config/gptel/tools/scope-shell-tools.org` - tool definition

**Changes**:
- Add `:async t` to `gptel-make-scoped-tool` call for `run_bash_command`
- No signature changes - still `(command, directory)`

### Request Scope Expansion Tool (Existing)

**Responsibility**: Allow LLM to pre-emptively request permissions.

**Location**: `config/gptel/tools/scope-shell-tools.org` - `request_scope_expansion` tool definition

**No changes needed**: This tool remains as-is for pre-emptive permission requests.

## Interfaces

### Scope Validation Wrapper API

**Input**: Tool definition (name, args, body, validation-type)

**Output**: Wrapped tool function that validates before executing

**Async contract**:
```elisp
;; For async tools (:async t):
(lambda (callback arg1 arg2 ...)
  ;; Validation fails → trigger-inline-expansion → wait for user
  ;; User approves → retry validation → execute tool body → funcall callback
  ;; User denies → funcall callback with error JSON
  )

;; For sync tools (default):
(lambda (arg1 arg2 ...)
  ;; Validation fails → return error immediately
  ;; Validation succeeds → execute tool body
  )
```

### Inline Expansion Trigger API

**Function**: `jf/gptel-scope--trigger-inline-expansion`

**Input**:
- `validation-error` - plist with `:error`, `:path`, `:operation`, etc.
- `tool-name` - string name of tool that failed validation
- `callback` - function to call with expansion result

**Output**: None (calls callback asynchronously)

**Callback contract**:
```elisp
;; User approves (allow-once or add-to-scope):
(funcall callback '(:approved t))

;; User denies:
(funcall callback '(:approved nil :reason "user_denied"))
```

### Violation Info Construction

**Function**: `jf/gptel-scope--build-violation-info`

**Input**: Validation error plist

**Output**: Violation info plist for expansion UI

**Mapping rules**:
- `:error "path_out_of_scope"` → `:resource` is `:path` from error
- `:error "command_denied"` → `:resource` is `:command` from error
- `:error "cloud_auth_denied"` → `:resource` is `:provider` from error
- `:error "incomplete_parse"` → `:resource` is original command string

## Boundaries

### In Scope

- Making `gptel-make-scoped-tool` wrapper async-capable
- Building violation info from validation errors
- Triggering expansion UI inline on validation failure
- Retrying validation after user approval
- Marking `run_bash_command` as async tool
- Behavioral tests for inline expansion flow

### Out of Scope

- Changes to expansion UI itself (already works via callback)
- Changes to `request_scope_expansion` tool (remains as-is)
- Changes to allow-once list management (existing implementation used)
- Changes to scope.yml update logic (existing implementation used)
- Other scope-aware tools (only `run_bash_command` for now)

### Internal vs External

**Internal**:
- Validation wrapper implementation
- Violation info construction
- Async callback coordination

**External**:
- Expansion UI (`jf/gptel-scope-prompt-expansion`)
- Allow-once list (`jf/gptel-scope--allow-once-list`)
- Scope.yml updaters (`jf/gptel-scope--add-*-to-scope`)

## Testing Approach

### Test Framework

**Buttercup** (BDD style) - Preferred for new behavioral tests.

**Rationale**:
- Behavioral scenarios in specs map naturally to `describe`/`it` structure
- Built-in `before-each`/`after-each` for setup/teardown
- Spy system for mocking transient menu interactions
- Async test support for callback-based expansion flow

### Test Organization

**Location**: `config/gptel/tools/test/behavioral/run-bash-command/`

**Files**:
- `inline-expansion-spec.el` - New file for inline expansion behavioral tests
- Extends existing behavioral test suite (co-located with other run_bash_command tests)

**Why co-locate**: Inline expansion is a behavioral change to `run_bash_command`, so tests belong with other behavioral tests for that tool.

### Naming Conventions

**File naming**: `*-spec.el` suffix for Buttercup tests

**Test structure**:
```elisp
(describe "run_bash_command: Inline scope expansion"
  (describe "Validation failure triggers expansion UI"
    (it "shows transient menu when path out of scope"
      ...))
  (describe "User approval flows"
    (it "executes command when allow-once granted"
      ...))
  (describe "User denial flows"
    (it "returns error when expansion denied"
      ...)))
```

**Scenario mapping**: Each spec scenario becomes one `(it "behavior" ...)` test case.

### Running Tests

**All inline expansion tests**:
```bash
./bin/run-tests.sh -f buttercup -d config/gptel/tools/test/behavioral/run-bash-command -p 'inline-expansion'
```

**All run_bash_command behavioral tests**:
```bash
./bin/run-tests.sh -f buttercup -d config/gptel/tools/test/behavioral/run-bash-command
```

**Interactive**:
```elisp
C-c t  ;; Open test transient menu
       ;; Select Buttercup → Directory → config/gptel/tools/test/behavioral
```

### Test Patterns

**Setup/teardown**:
```elisp
(before-each
  (helpers-spec-setup-session)       ;; Session context
  (helpers-spec-setup-bash-mocks)    ;; Bash parser mocks
  (setq expansion-ui-called nil))    ;; Reset test state

(after-each
  (helpers-spec-teardown-bash-mocks)
  (helpers-spec-teardown-session))
```

**Mocking expansion UI**:
```elisp
;; Spy on expansion UI to simulate user choice
(spy-on 'jf/gptel-scope-prompt-expansion
        :and-call-fake (lambda (violation-info callback patterns tool-name)
                        (setq expansion-ui-called t)
                        ;; Simulate user approving with allow-once
                        (funcall callback
                                 (json-serialize
                                  (list :success t
                                        :allowed_once t)))))
```

**Mocking bash execution**:
```elisp
;; Use existing helpers from scope-expansion-spec.el
(helpers-spec-mock-bash-parse "cat /tmp/file.txt" '("cat") t)
(helpers-spec-mock-bash-semantics
 '((:operation :read :path "/tmp/file.txt"))
 nil
 '(:ratio 1.0))
```

**Async callback testing**:
```elisp
;; Capture callback invocation
(let ((callback-result nil))
  (lambda (result)
    (setq callback-result result))
  ;; ... trigger async flow ...
  ;; Verify callback was invoked with expected result
  (expect callback-result :to-be t))
```

### Scenario Mapping

**From spec scenarios to test cases**:

1. **Validation failure triggers UI** (`inline-scope-expansion/spec.md`)
   - Scenario: "Validation failure triggers expansion UI automatically"
   - Test: `(it "shows transient menu when validation fails")`
   - Setup: Mock validation to fail with path_out_of_scope
   - Action: Call tool
   - Verify: Expansion UI was called with correct violation info

2. **Allow-once approval** (`inline-scope-expansion/spec.md`)
   - Scenario: "User approves with allow-once in inline flow"
   - Test: `(it "executes command when allow-once granted")`
   - Setup: Mock expansion UI to return allow-once approval
   - Action: Call tool with out-of-scope path
   - Verify: Command executes successfully, returns success to LLM

3. **Add-to-scope approval** (`inline-scope-expansion/spec.md`)
   - Scenario: "User approves with add-to-scope in inline flow"
   - Test: `(it "executes command when added to scope")`
   - Setup: Mock expansion UI to return add-to-scope approval
   - Action: Call tool with out-of-scope path
   - Verify: scope.yml updated, command executes successfully

4. **Denial flow** (`inline-scope-expansion/spec.md`)
   - Scenario: "User denies inline expansion request"
   - Test: `(it "returns error when expansion denied")`
   - Setup: Mock expansion UI to return denial
   - Action: Call tool with out-of-scope path
   - Verify: Error returned to LLM, command not executed

5. **Backward compat with request_scope_expansion** (`inline-scope-expansion/spec.md`)
   - Scenario: "request_scope_expansion still works for pre-emptive requests"
   - Test: `(it "supports pre-emptive expansion via request_scope_expansion")`
   - Action: Call request_scope_expansion, then call run_bash_command
   - Verify: Permission granted, command succeeds

## Dependencies

**Required packages**:
- `gptel` - Tool registration and async callback system
- `transient` - Expansion UI menu framework
- `yaml` - Scope.yml parsing and writing
- `bash-parser-core` - Command parsing and semantic extraction
- `bash-parser-plugins` - File operations and cloud auth detection
- `buttercup` - Test framework (dev dependency)

**Internal modules**:
- `jf-gptel-scope-core` - Validation wrapper and tool categories
- `jf-gptel-scope-expansion` - Expansion UI and scope updaters
- `jf-gptel-sessions-constants` - Session directory constants

## Constraints

### Technical Constraints

1. **Async callback coordination**: Must properly coordinate between validation failure, UI interaction, and tool execution within gptel's async tool system
2. **Allow-once timing**: Permission consumed during validation retry, not after tool execution (existing constraint)
3. **Transient menu blocking**: Expansion UI blocks until user makes choice (existing constraint)

### Performance Constraints

1. **UI responsiveness**: Expansion UI must appear within 100ms of validation failure
2. **Validation retry**: Retry validation should be <50ms (in-memory allow-once check)

### Compatibility Constraints

1. **Backward compatibility**: `request_scope_expansion` must continue to work unchanged
2. **Sync tools unchanged**: Non-async scope-aware tools must maintain current behavior
3. **Error format**: Denied expansion errors must match existing error structure for LLM consistency

### Testing Constraints

1. **Mock bash parser**: Tests use mocked bash-parser to avoid spawning actual bash processes
2. **Mock transient UI**: Tests spy on expansion UI to avoid blocking on user interaction
3. **Isolated sessions**: Each test uses isolated session context (existing pattern)
