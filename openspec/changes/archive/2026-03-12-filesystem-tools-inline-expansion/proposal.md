## Why

Scoped filesystem tools (read_file, write_file_in_scope, edit_file_in_scope) currently lack inline expansion support, creating an inconsistent user experience compared to run_bash_command. When scope validation fails, users must manually call request_scope_expansion and retry the operation across multiple LLM turns. Additionally, these tools have zero behavioral test coverage, and git-tracked status is checked after scope validation, preventing its use in scope policies or expansion UI.

## What Changes

- **Add `:async` keyword** to all three scoped filesystem tools to enable inline expansion workflow
- **Add metadata gathering** to collect file context (git status, existence, type) before validation
- **Pass metadata to validation** functions to enable future git-based policies and richer expansion UI
- **Create comprehensive behavioral tests** for all three filesystem tools covering scope expansion workflows
- **Update validation function signatures** to accept metadata parameter (**BREAKING**)
- **Enhance expansion UI** to display file metadata (git status) during scope violations

## Capabilities

### New Capabilities
- `filesystem-inline-expansion`: Inline expansion support for scoped filesystem tools (read_file, write_file_in_scope, edit_file_in_scope)
- `filesystem-metadata`: File metadata gathering and validation integration (git status, existence, type)
- `filesystem-behavioral-tests`: Comprehensive behavioral test coverage for filesystem tools

### Modified Capabilities
- `scope`: Core scope validation to accept and propagate file metadata through validation pipeline

## Impact

**Breaking Changes:**
- Validation function signatures change to require metadata parameter
- `jf/gptel-scope--validate-path-tool` signature: `(tool-name args category config metadata)` (adds metadata)
- `jf/gptel-scope--check-tool-permission` signature: `(config tool-name args metadata)` (adds metadata)

**Files Modified:**
- `config/gptel/scope/scope-core.el` - validation function signatures, metadata propagation
- `config/gptel/scope/scope-filesystem-tools.el` - add :async, add metadata gathering
- `config/gptel/scope/scope-expansion.el` - display metadata in expansion UI

**Files Created:**
- `config/gptel/scope/scope-metadata.el` - metadata gathering functions
- `config/gptel/tools/test/behavioral/filesystem-tools-scope-expansion-spec.el` - behavioral tests

**Dependencies:**
- Reuses existing `helpers-spec.el` test infrastructure from run_bash_command tests
- No new external dependencies
