## Why

The scope expansion UI never triggers for bash tool denials. During the v4 bash-parser migration, semantic validation was moved into the `run_bash_command` tool body, but the expansion UI trigger lives in the `gptel-make-scoped-tool` macro wrapper. The macro-level `validate-bash-tool` always returns `:allowed t`, so the macro's expansion branch is never reached. Denied commands return error JSON as normal tool output — the user is never prompted to expand scope.

This was discovered during a real session (test-scope-control-2026-03-24) where `ls -la /usr/local/Cellar` and `find /opt/homebrew/Cellar` were denied but no expansion UI appeared. The LLM simply received error JSON and tried alternative commands.

## What Changes

- **Move bash semantic validation from tool body to macro level**: `validate-bash-tool` in scope-core will call the 7-stage semantic validation pipeline instead of returning `:allowed t`. This lets the macro's existing expansion trigger fire on bash denials.
- **Remove duplicate validation from `run_bash_command` tool body**: The tool body will only contain execution logic, not validation. By the time it runs, the macro has already validated.
- **Move `run_bash_command` and `request_scope_expansion` tool definitions to `config/gptel/tools/`**: Tool definitions belong with other tools, not in the scope validation infrastructure directory. The validation pipeline and helpers remain in `config/gptel/scope/`.
- **Update specs to clarify macro-level validation responsibility**: All validation and expansion triggering is a macro responsibility, not something individual tools implement. Tool bodies assume validation has passed.
- **Update scope-expansion spec for v4 schema**: The expansion spec still references v3 `bash_tools.categories` for "add to scope" actions. These need updating to match v4's operation-first model (path-based expansion for bash denials, not category-based).

## Capabilities

### New Capabilities

_(none — this is a bug fix restoring intended behavior)_

### Modified Capabilities

- `gptel/scope`: Clarify that validation and expansion are macro-level responsibilities. `validate-bash-tool` performs real validation instead of deferring. Tool bodies execute only after validation passes.
- `gptel/inline-scope-expansion`: Clarify that inline expansion applies to ALL validation types including bash. Update violation-info construction to handle bash-specific error types.
- `gptel/scope-expansion`: Update "add to scope" actions from v3 category-based to v4 path-based model for bash tools. Remove references to `bash_tools.categories`.

## Impact

- **scope-core.el**: `validate-bash-tool` function rewritten to call semantic validation pipeline
- **scope-shell-tools.el → split**: Validation pipeline infrastructure stays in scope/. Tool definitions (`run_bash_command`, `request_scope_expansion`) and execution logic move to `config/gptel/tools/bash-tools.el`
- **scope-expansion.el**: Bash "add to scope" action updated for v4 schema (paths, not categories)
- **Specs**: scope.md, scope-expansion.md, inline-scope-expansion/spec.md updated
- **Tests**: Integration tests needed for macro-level bash validation → expansion trigger flow
