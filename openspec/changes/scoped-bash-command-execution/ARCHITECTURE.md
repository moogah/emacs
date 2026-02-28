# Architecture: scope-shell-tools Bypass of gptel-make-scoped-tool Macro

## Decision

The `run_bash_command` tool in `config/gptel/tools/scope-shell-tools.org` **does use** the `gptel-make-scoped-tool` macro, contrary to what bead emacs-6mp suggests. This document clarifies the architecture.

## Current Implementation

```elisp
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command in specified directory with scope validation..."

 (list '(:name "command"
         :type string
         :description "Shell command to execute (pipes and redirects allowed)")
       '(:name "directory"
         :type string
         :description "Working directory (must be in scope for command category)"))

 "bash"  ; Validation type

 ;; Tool body - executed only if validation passes
 (let* ((result (jf/gptel-bash--execute-command command directory))
        ...)
   (list :success success ...)))
```

## Why Use gptel-make-scoped-tool

The macro provides essential infrastructure:

### 1. Consistent Tool Registration

The macro handles:
- Tool definition in `gptel--known-tools`
- Argument specification format
- Integration with gptel's tool calling system

### 2. Scope Validation Dispatcher

The macro automatically:
- Loads scope.yml configuration
- Calls validation dispatcher with tool name and args
- Routes to appropriate validator based on validation type

### 3. Error Handling and Formatting

The macro provides:
- Structured error responses for scope violations
- Integration with scope expansion system
- Consistent error message format across all tools

### 4. Allow-Once Support

The macro implements:
- Buffer-local allow-once tracking
- Bypass validation when resource in allow-once list
- Integration with transient menu approval flow

## Bash Validation Type

The key architectural feature is the **validation type parameter**: `"bash"`

```elisp
(gptel-make-scoped-tool
 "run_bash_command"
 ...
 "bash"  ; <- This is the validation type
 ...)
```

### How It Works

1. **Tool invocation:** LLM calls `run_bash_command(command, directory)`
2. **Macro wrapper:** `gptel-make-scoped-tool` intercepts the call
3. **Load config:** Macro loads `scope.yml` from buffer's branch directory
4. **Dispatcher:** Macro looks up validation type "bash" in `jf/gptel-scope--tool-categories`
5. **Route to validator:** Dispatcher calls `jf/gptel-scope--validate-bash-tool`
6. **Bash validation:**
   - Parse command to extract base command
   - Categorize command (read_only, safe_write, dangerous, denied)
   - Validate directory matches category's path scope requirement
7. **Result handling:**
   - If allowed: Execute tool body, return result
   - If denied: Return structured error with expansion guidance

### Validator Implementation

In `config/gptel/scope/scope-core.el`:

```elisp
(defun jf/gptel-scope--validate-bash-tool (tool-name args config)
  "Validate bash tool: categorize command, validate directory.

  ARGS format: (command directory)

  Returns (:allowed t) or (:allowed nil :reason ... :allowed-patterns ...)."
  (let* ((command (car args))
         (directory (cadr args))
         (bash-config (plist-get config :bash-tools))
         (paths-config (plist-get config :paths)))

    ;; Parse base command from complex shell string
    (let ((base-cmd (jf/gptel-bash--parse-command command)))

      ;; Categorize command
      (let ((category (jf/gptel-bash--categorize-command base-cmd bash-config)))

        ;; Check if denied
        (when (eq category 'denied)
          (cl-return-from jf/gptel-scope--validate-bash-tool
            (list :allowed nil
                  :reason "denied"
                  :message (format "Command '%s' is in deny list and cannot be used." base-cmd))))

        ;; Validate directory for category
        (jf/gptel-bash--validate-directory-for-category directory category paths-config)))))
```

## Why Not Bypass?

Bypassing the macro would require:

1. **Reimplementing tool registration:**
   ```elisp
   (gptel-make-tool
    :name "run_bash_command"
    :function (lambda ...)
    ...)
   ```

2. **Manual scope validation:**
   ```elisp
   (let ((config (jf/gptel-scope--load-config)))
     (unless (jf/gptel-bash--validate command directory config)
       (error "Validation failed")))
   ```

3. **Manual error formatting:**
   ```elisp
   (list :success nil
         :error "command_not_allowed"
         :tool "run_bash_command"
         :message "...")
   ```

4. **Manual allow-once handling:**
   ```elisp
   (unless (member resource jf/gptel-scope--allow-once-resources)
     ;; Perform validation
     ...)
   ```

**Result:** Duplicates macro's functionality with no benefit.

## Related Tools

All scope-aware tools use the same pattern:

### File Tools

```elisp
(gptel-make-scoped-tool "read_file" ... "path" ...)
(gptel-make-scoped-tool "write_file_in_scope" ... "path" ...)
```

Validation type `"path"` routes to `jf/gptel-scope--validate-path-tool`.

### Org-Roam Tools

```elisp
(gptel-make-scoped-tool "query_org_roam" ... "pattern" ...)
```

Validation type `"pattern"` routes to `jf/gptel-scope--validate-pattern-tool`.

### Bash Tools

```elisp
(gptel-make-scoped-tool "run_bash_command" ... "bash" ...)
```

Validation type `"bash"` routes to `jf/gptel-scope--validate-bash-tool`.

## Tool Categories Registry

In `scope-core.el`:

```elisp
(defvar jf/gptel-scope--tool-categories
  '(("read_file" . (:validation path :operation read))
    ("write_file_in_scope" . (:validation path :operation write))
    ("edit_file_in_scope" . (:validation path :operation write))
    ("query_org_roam" . (:validation pattern :operation read))
    ("run_bash_command" . (:validation bash :operation write))
    ...)
  "Tool categorization for scope validation routing.")
```

The `(:validation bash :operation write)` entry tells the dispatcher:
- Use bash validator for this tool
- Consider it a write operation (for safety)

## Validation Dispatcher

In `scope-core.el`:

```elisp
(defun jf/gptel-scope--validate-tool (tool-name args config)
  "Dispatch to appropriate validator based on tool category."
  (let* ((category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
         (validation-type (plist-get category :validation)))
    (pcase validation-type
      ('path (jf/gptel-scope--validate-path-tool tool-name args config))
      ('pattern (jf/gptel-scope--validate-pattern-tool tool-name args config))
      ('bash (jf/gptel-scope--validate-bash-tool tool-name args config))  ; <-- Bash routing
      ('command (jf/gptel-scope--validate-command-tool tool-name args config))
      (_ (list :allowed nil
               :reason (format "Unknown validation type: %s" validation-type))))))
```

## Benefits of Using the Macro

1. **Consistency:** All tools follow same validation pattern
2. **Maintainability:** Validation logic centralized in scope-core
3. **Extensibility:** Adding new validation types requires only:
   - New validator function
   - Entry in tool categories registry
   - Case in validation dispatcher
4. **Integration:** Automatic integration with scope expansion and allow-once
5. **Error handling:** Standardized error format across all tools

## Conclusion

The `run_bash_command` tool **correctly uses** `gptel-make-scoped-tool` with validation type `"bash"`. There is no bypass, and no bypass is needed. The architecture is consistent with other scope-aware tools and provides all necessary functionality through the macro's validation dispatcher system.

The bead emacs-6mp appears to be based on a misunderstanding. The tool follows the established pattern and integrates properly with the scope system.

## See Also

- `config/gptel/scope/scope-core.org` - Scope validation dispatcher implementation
- `config/gptel/tools/scope-shell-tools.org` - Bash tool implementation using macro
- `openspec/changes/scoped-bash-command-execution/design.md` - Original design document describing integration with scope-core
