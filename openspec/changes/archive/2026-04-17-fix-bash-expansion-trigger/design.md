## Context

During the v4 bash-parser migration, `validate-bash-tool` was changed to return `:allowed t` unconditionally, deferring real validation to the `run_bash_command` tool body. This broke the expansion UI trigger because the macro only triggers expansion when `check-tool-permission` returns `:allowed nil`. The tool body's validation errors are returned as normal JSON payloads that the LLM sees as data — no user prompt ever appears.

**Current flow (broken):**
```
macro → validate-bash-tool → (:allowed t) → tool body executes
  tool body → validate-command-semantics → error → returned as JSON data
  (expansion UI never triggered)
```

**Target flow (fixed):**
```
macro → validate-bash-tool → validate-command-semantics → (:allowed nil)
  macro → trigger-inline-expansion → user approves → tool body executes
```

## Goals / Non-Goals

**Goals:**
- Bash validation failures trigger the expansion UI (the core bug fix)
- All validation happens at macro level before tool body runs
- Specs clearly document that validation/expansion is a macro responsibility
- Integration test coverage for the bash denial → expansion flow

**Non-Goals:**
- Restructuring the macro itself
- Changing path or pattern validator behavior
- Resolving the scope-core ↔ scope-shell-tools circular dependency
- Changing the expansion UI appearance or behavior

## Decisions

### Decision 1: Keep `validate-bash-tool` in scope-core.el

**Choice**: Keep `validate-bash-tool` in scope-core.el alongside `validate-path-tool` and `validate-pattern-tool`. All validators live in scope-core.el because the macro owns ALL validation dispatch.

**Rationale**: The macro in scope-core.el is the single point of validation for all tool types. Moving `validate-bash-tool` to scope-shell-tools.el would make the shell tool appear to own its own validation — the same anti-pattern that caused this bug. Validators belong with the macro that calls them.

**How it works**: `validate-bash-tool` calls `jf/gptel-scope--validate-command-semantics` (defined in scope-shell-tools.el) at runtime. Since scope-shell-tools.el loads during init before any bash tool is invoked, the function is available at call time. No `require` is needed — this is a runtime call, not a load-time dependency.

**Alternative considered**: Move `validate-bash-tool` to scope-shell-tools.el to keep it near `validate-command-semantics`. Rejected because it makes the shell tool module appear responsible for its own validation, violating the principle that the macro owns all validation dispatch.

### Decision 2: `validate-bash-tool` wraps `validate-command-semantics` result

**Choice**: `validate-bash-tool` calls `validate-command-semantics` and wraps the result in the standard `(:allowed t/nil ...)` format.

**Implementation**:
```elisp
(defun jf/gptel-scope--validate-bash-tool (tool-name args config metadata)
  (cl-block jf/gptel-scope--validate-bash-tool
    (let* ((command (car args))
           (directory (cadr args))
           (bash-config (plist-get config :bash-tools)))
      ;; Guard: missing bash_tools config
      (unless bash-config
        (cl-return-from jf/gptel-scope--validate-bash-tool
          (list :allowed nil :error "command-not-allowed" ...)))
      ;; Guard: legacy categories
      (when (plist-get bash-config :categories)
        (cl-return-from jf/gptel-scope--validate-bash-tool
          (list :allowed nil :error "malformed-config" ...)))
      ;; Run semantic validation
      (let ((error (jf/gptel-scope--validate-command-semantics command directory config)))
        (if error
            ;; Propagate all error fields + :allowed nil
            (append (list :allowed nil) error)
          ;; Pass
          (list :allowed t))))))
```

Key: `(append (list :allowed nil) error)` propagates all error plist fields (`:error`, `:message`, `:path`, `:command`, `:provider`, etc.) so `build-violation-info` can extract them.

### Decision 3: Move tool definitions to `config/gptel/tools/bash-tools.el`

**Choice**: Move `run_bash_command` and `request_scope_expansion` tool definitions from `config/gptel/scope/scope-shell-tools.el` to a new `config/gptel/tools/bash-tools.el` (with corresponding `.org` file). Strip validation from the tool body.

**Rationale**: Every other tool lives in `config/gptel/tools/`. Having tool definitions in the scope directory conflates validation infrastructure with tool definitions — which is exactly the confusion that led to this bug. scope-shell-tools.el becomes a pure validation library.

**What moves to `config/gptel/tools/bash-tools.el`**:
- `run_bash_command` tool definition (gptel-make-scoped-tool call)
- `request_scope_expansion` meta-tool definition (gptel-make-tool call)
- `jf/gptel-bash--execute-command` (execution logic)
- `jf/gptel-bash--check-absolute-paths` (execution helper)
- `jf/gptel-bash--max-output-chars` and `jf/gptel-bash--command-timeout` (constants)

**What stays in `config/gptel/scope/scope-shell-tools.el`**:
- Schema defaults and loading (`load-schema`, `normalize-keys`, `validate-schema`)
- Cloud and security config loading/validation
- Validation pipeline (`validate-command-semantics`, all 7 stages)
- File operation validation (`validate-file-operations`, `validate-operation`)
- Pipeline command extraction and validation
- Glob matching (`glob-match-p`, `glob-to-regex`)

**Tool body simplification**:

Before (current):
```elisp
(let* ((config (jf/gptel-scope--load-config))
       (validation-error (when config
                          (jf/gptel-scope--validate-command-semantics command directory config))))
  (if validation-error
      (list :success nil :error ...)   ;; ← This path bypasses expansion
    (let* ((result (jf/gptel-bash--execute-command command directory))
           ...)))
```

After:
```elisp
;; No validation here — macro already validated before reaching this body
(let* ((result (jf/gptel-bash--execute-command command directory))
       ...)
```

### Decision 4: Update expansion "add to scope" for bash v4

**Choice**: For bash denials where the resource is a file path (path_out_of_scope), route to `add-path-to-scope` with the operation type from violation-info. For command_denied, don't offer "add to scope" (only allow-once/deny).

**Implementation approach**: In `jf/gptel-scope--add-to-scope`, the `'bash` case examines the `:error` type from violation-info:
- `path_out_of_scope` → delegate to path updater with operation type
- `command_denied` → not addable (the command is on the deny list for a reason)
- `cloud_auth_denied` → update `cloud.allowed_providers`

### Decision 5: Three-layer macro contract tests

**Problem**: Every existing test calls validators and expansion helpers directly — none invoke a tool through the macro. The tests prove each piece works in isolation but never verify the macro connects them. Additionally, the routing dispatch (`check-tool-permission`'s `pcase`) is never tested — nothing verifies that a bash-category tool actually calls `validate-bash-tool` rather than `validate-path-tool`.

**Choice**: Create `macro-expansion-contract-spec.el` with three test layers using **minimal test tools** created via the macro. Tool bodies are one-liners (`(list :success t :result "executed")`) — no bash-parser, no filesystem, no mocking overhead.

**Layer 1: Routing contract** — verify correct validator dispatched:
```elisp
(it "dispatches bash-category tool to validate-bash-tool"
  (let ((test-tool (gptel-make-scoped-tool "test-bash" "test"
                     (list '(:name "cmd" :type string)) "bash" :async
                     (list :success t))))
    ;; Spy on validators
    (spy-on 'jf/gptel-scope--validate-bash-tool :and-return-value '(:allowed t))
    (spy-on 'jf/gptel-scope--validate-path-tool)
    (spy-on 'jf/gptel-scope--load-config :and-return-value test-config)
    ;; Invoke through macro
    (funcall (gptel-tool-function test-tool) test-callback "ls")
    ;; Assert routing
    (expect 'jf/gptel-scope--validate-bash-tool :to-have-been-called)
    (expect 'jf/gptel-scope--validate-path-tool :not :to-have-been-called)))
```

**Layer 2: Expansion contract** — mock `check-tool-permission`, verify macro handles denial:
```elisp
(it "triggers expansion UI when validation fails for async tool"
  (let ((test-tool (gptel-make-scoped-tool "test-denied" "test"
                     (list '(:name "arg" :type string)) "bash" :async
                     (list :success t))))
    ;; Mock validation to deny
    (spy-on 'jf/gptel-scope--check-tool-permission
            :and-return-value '(:allowed nil :error "path_out_of_scope"
                                :message "Path not in scope" :path "/etc/foo"))
    (spy-on 'jf/gptel-scope--load-config :and-return-value test-config)
    ;; Mock expansion to auto-approve
    (spy-on 'jf/gptel-scope-prompt-expansion :and-call-fake
            (lambda (vi cb patterns tool)
              (funcall cb (json-serialize '(:success t :allowed_once t)))))
    ;; Invoke
    (funcall (gptel-tool-function test-tool) test-callback "arg")
    ;; Assert expansion was triggered
    (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)))
```

**Layer 3: Validator unit tests** — test `validate-bash-tool` directly:
```elisp
(it "returns :allowed nil for out-of-scope path"
  (helpers-spec-mock-bash-parse "cat /etc/passwd" '("cat") t)
  (helpers-spec-mock-bash-semantics '((:read "/etc/passwd")) nil 1.0)
  (let ((result (jf/gptel-scope--validate-bash-tool
                  "run_bash_command" '("cat /etc/passwd" "/workspace")
                  test-config nil)))
    (expect (plist-get result :allowed) :to-be nil)
    (expect (plist-get result :error) :to-equal "path_out_of_scope")))
```

**Transient automation**: Mock `jf/gptel-scope-prompt-expansion` with `:and-call-fake` that immediately invokes the callback. No transient UI rendered — this is already proven in existing tests and transient cannot display in batch Emacs anyway.

## Risks / Trade-offs

**[Risk] Double config loading**: The macro loads config at line 166, and `validate-command-semantics` may also expect config. Since we pass config to `validate-bash-tool` which passes it to `validate-command-semantics`, there's no double loading.
→ **Mitigation**: `validate-command-semantics` already takes config as a parameter. No change needed.

**[Risk] validate-bash-tool calls a function defined in another module**: `validate-command-semantics` is defined in scope-shell-tools.el, but `validate-bash-tool` stays in scope-core.el.
→ **Mitigation**: This is a runtime call, not a load-time dependency. scope-shell-tools.el loads during init (it defines the `run_bash_command` tool), so `validate-command-semantics` is always available by the time any bash tool is invoked. No `require` needed.

**[Risk] Existing tests may assume validate-bash-tool returns `:allowed t`**: Tool contract tests in `run-bash-command-spec.el` may mock or spy on `validate-bash-tool` with the old behavior.
→ **Mitigation**: Update those tests to expect the new behavior. The spy-based tests should now verify that expansion is triggered on denial.

**[Risk] Tool body still references config loading**: The current tool body calls `jf/gptel-scope--load-config` independently.
→ **Mitigation**: Remove this from the tool body. The macro loads config and passes it through the validation chain. The tool body doesn't need config at all.

**[Risk] Moving tool definitions creates a new file and changes load order**: `config/gptel/tools/bash-tools.el` is a new module that must be loaded after scope-core and scope-shell-tools.
→ **Mitigation**: Tool files in `config/gptel/tools/` are loaded after scope modules in gptel.org's load order. Add `bash-tools` to the tools loading section. The tool definition depends on `gptel-make-scoped-tool` (from scope-core) and `jf/gptel-bash--execute-command` (moved with it), both available at load time.

## Open Questions

None — the approach is straightforward and well-bounded.
