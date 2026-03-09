## Why

The bash_tools command categories (read_only, safe_write, dangerous) were originally created as a workaround for limited semantic understanding of bash commands. Now that bash-parser provides reliable operation extraction (distinguishing `python3 --version` from `python3 script.py`), we can replace category-based validation with operation-first validation. This simplifies the security model, eliminates manual command allowlists, and leverages semantic analysis as the primary security boundary.

## What Changes

- **BREAKING**: Remove bash_tools categories (read_only, safe_write, dangerous) from scope profile schema
- Add no-op command allowance: commands with no file operations are allowed by default (enables version checks without explicit allowlisting)
- Refactor validation pipeline from category-first to operation-first model
- Simplify deny list to edge cases only (sudo, commands with parser limitations)
- Update all scope profiles to remove categories section
- Update validation pipeline stages to use semantics-first approach

## Capabilities

### New Capabilities
- `no-op-command-allowance`: Commands that extract no file operations are allowed by default, enabling safe commands like version checks (`python3 --version`, `node --version`) without requiring explicit command allowlists

### Modified Capabilities
- `scope-validation-pipelines`: Validation pipeline changes from category-based command checking to operation-based validation with no-op allowance
- `scope-profiles`: Scope profile schema removes categories section, keeps minimal deny list for edge cases
- `bash-tools`: Command validation behavior changes from category membership to semantic operation extraction

## Impact

**Code Changes:**
- `config/gptel/tools/scope-shell-tools.org`: Refactor validation pipeline (remove Stage 3 category validation, add Stage 4 no-op check)
- `config/gptel/scope-profiles/*.yml`: Remove categories sections from all profiles (system-explorer, bash-enabled, coding, research, restricted)

**Spec Changes:**
- `openspec/specs/gptel/scope-validation-pipelines/spec.md`: Update validation stages and flow
- `openspec/specs/gptel/scope-profiles.md`: Update schema documentation (remove categories)
- `openspec/specs/gptel/bash-tools.md`: Update command validation behavior

**Test Changes:**
- `config/gptel/tools/test/`: Remove category-based validation tests, add no-op allowance tests

**User-Facing Changes:**
- Version check commands work immediately without configuration
- Unknown commands allowed if they have no file operations
- Deny list becomes minimal (only dangerous edge cases)
- Clearer error messages (always based on operations, not category membership)
