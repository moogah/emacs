## Why

The bash-parser experiment has proven successful with 683 tests demonstrating production-ready parsing, semantic extraction, and security validation. Integrating this sophisticated parser into gptel's scope control system enables operation-specific path scoping (read/write/execute/modify), cloud authentication detection, full pipeline validation, and rich error reporting. The current regex-based command validation is too coarse-grained to support fine-grained security policies, leaving gaps where dangerous commands can bypass validation in pipelines and file paths are not validated against scope.

## What Changes

- **BREAKING**: Graduate bash-parser from `config/experiments/bash-parser/` to production module at `config/bash-parser/`
- **BREAKING**: Replace regex-based command parsing in `scope-shell-tools.org` with tree-sitter-based bash-parser
- **BREAKING**: Extend `scope.yml` schema with operation-specific path scoping: `paths.read`, `paths.write`, `paths.execute`, `paths.modify`
- **BREAKING**: Add cloud authentication detection to scope.yml schema: `cloud.auth_detection`, `cloud.allowed_providers`
- **BREAKING**: Add security configuration to scope.yml: `security.enforce_parse_complete`, `security.max_coverage_threshold`
- **NEW**: Validate all file paths extracted from commands against operation-specific scope patterns (not just working directory)
- **NEW**: Validate all commands in pipelines (close `ls | xargs rm` bypass)
- **NEW**: Detect cloud authentication commands (AWS, GCP, Azure) and enforce policy
- **NEW**: Provide rich error messages with coverage metrics, token analysis, and actionable suggestions
- **REMOVED**: Backward compatibility with old scope.yml schema (no migration path)
- **REMOVED**: Simple regex-based `jf/gptel-bash--parse-command` function
- Migrate 683 bash-parser tests from experiments to production location
- Update gptel module loader to initialize bash-parser subsystem
- Update example presets (`system-explorer.md`, `bash-tools-example.md`) to use new scope.yml schema

## Capabilities

### New Capabilities

- `scope-validation-file-paths`: Operation-specific file path validation using bash-parser to extract file operations and match paths against scope.yml patterns
- `scope-validation-pipelines`: Full pipeline command validation using bash-parser to validate all commands in chains, not just base command
- `scope-validation-cloud-auth`: Cloud authentication policy enforcement using bash-parser cloud-auth plugin with allow/warn/deny modes
- `scope-schema-v4`: Enhanced scope.yml schema with operation-specific paths (read/write/execute/modify), cloud configuration, and security settings

### Modified Capabilities

- `gptel/bash-tools`: Changes from directory-only validation to file-path-and-operation validation, adds pipeline validation, adds cloud auth detection, adds parse completeness enforcement, removes backward compatibility with old schema

## Impact

**Code changes:**
- `config/experiments/bash-parser/` → `config/bash-parser/` (move entire directory structure)
- `config/gptel/tools/scope-shell-tools.org` - Replace validation logic with bash-parser integration
- `config/gptel/gptel.org` - Add bash-parser to module load order
- `config/gptel/presets/*.md` - Update example scope.yml documents to v4 schema
- All user `scope.yml` files - Must migrate to v4 schema (breaking change, no automatic migration)

**Dependencies:**
- Add `config/bash-parser` to gptel module dependencies
- bash-parser already depends on `treesit` (built-in Emacs 29+)
- No new external dependencies

**Test migration:**
- `config/experiments/bash-parser/test/` → `config/bash-parser/test/` (683 tests)
- Add integration tests in `config/gptel/test/` for scope-shell-tools validation

**Documentation:**
- Update `CLAUDE.md` section on gptel scope system
- Update `openspec/specs/gptel/bash-tools.md` spec with new requirements
- Move `openspec/specs/bash-parser/` from experiments to production (specs don't change, just location awareness)

**Breaking changes for users:**
- All `scope.yml` files must be updated to v4 schema manually
- Commands previously allowed may now be blocked (stricter file path validation)
- Pipeline commands previously ignored now validated (closes security gap)
- Cloud commands may require explicit approval (new detection capability)
- No rollback - once integrated, old validation system is removed
