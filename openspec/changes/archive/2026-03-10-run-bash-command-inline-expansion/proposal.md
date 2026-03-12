## Why

The current scope expansion flow requires three separate tool calls: (1) LLM calls `run_bash_command` which fails validation, (2) LLM calls `request_scope_expansion` to request permission, (3) LLM retries `run_bash_command` which succeeds via allow-once. This is unnecessarily complex and creates poor UX. The validation failure should directly trigger the expansion UI inline, allowing the command to execute immediately upon user approval within a single tool call.

## What Changes

- Integrate scope expansion UI directly into `run_bash_command` validation pipeline
- When validation fails, automatically trigger the same expansion transient menu used by `request_scope_expansion`
- If user approves (allow-once or add-to-scope), retry validation and execute command immediately (same tool call)
- If user denies, return error to LLM as before
- Keep `request_scope_expansion` tool for pre-emptive permission requests
- No changes to `run_bash_command` signature - remains `(command, directory)`

## Capabilities

### New Capabilities
- `inline-scope-expansion`: Validation failures in scope-aware tools automatically trigger expansion UI and can execute upon approval within the same tool call

### Modified Capabilities
- `bash-tools`: Update `run_bash_command` to support inline expansion (behavioral change - validation failures now trigger UI instead of just returning error)

## Impact

- **Files Modified**:
  - `config/gptel/tools/scope-shell-tools.org` - Add inline expansion to validation pipeline
  - `config/gptel/scope/scope-core.org` - Make scope validation wrapper async-capable
- **Behavioral Change**: Validation failures in `run_bash_command` now trigger interactive UI instead of immediate error return
- **User Experience**: "Allow once" flow reduces from 3 tool calls to 1 tool call
- **Backward Compatibility**: `request_scope_expansion` tool remains available for pre-emptive requests
- **Testing**: Requires updates to behavioral tests in `config/gptel/tools/test/behavioral/run-bash-command/` to handle async expansion flow
