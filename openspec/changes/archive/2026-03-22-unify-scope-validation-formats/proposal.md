## Why

Scope validation for gptel tools is conceptually a two-step process: (1) extract the operations and paths from tool arguments, (2) validate each (operation, path) pair against scope patterns. The extraction step varies by tool type (trivial for filesystem tools, bash-parser for bash tools), but the validation step should be identical. Instead, two validation systems evolved independently — the tool dispatch validators in scope-core and the 7-stage pipeline validators in scope-shell-tools — with divergent return formats, duplicated glob matching, and an impedance mismatch at `build-violation-info` that produces incorrect user-facing messages in edge cases.

## What Changes

- **BREAKING**: Unify validator return format across dispatch validators (scope-core) and pipeline validators (scope-shell-tools) to use `:error` for machine codes and `:message` for human-readable text consistently
- Remove the overloaded `:reason` field from validator returns — currently used as a machine code in dispatch validators but consumed as human-readable text by `build-violation-info`
- Consolidate path-vs-pattern glob matching so dispatch validators and pipeline validators share the same validation logic after extraction
- Simplify `build-violation-info` by removing fallback chains (`(or :reason :message)`) that paper over format inconsistencies
- Update `build-expansion-info` to use the unified format
- Remove stale tests that assert against formats no real validator produces

## Capabilities

### New Capabilities

None — this is a consolidation of existing capabilities, not new functionality.

### Modified Capabilities

- `gptel/scope`: Validator return format changes (dispatch validators adopt `:error` + `:message` instead of `:reason`), `build-violation-info` simplified, shared validation path after extraction
- `gptel/inline-scope-expansion`: Expansion UI consumes unified violation-info format — no more format-dependent fallback logic

## Impact

- **Code**: `scope-core.org` (validate-path-tool, validate-pattern-tool, validate-bash-tool, build-violation-info, build-expansion-info, check-tool-permission), `scope-shell-tools.org` (validate-operation may become the shared validation function), `scope-expansion.org` (UI field consumption)
- **Tests**: `test-violation-info-spec.el`, `expansion-integration-spec.el`, `expansion-ui-handlers-spec.el`, `expansion-ui-spec.el`, `path-validation-spec.el` — tests that construct hand-built validator plists need updating to use the unified format
- **API**: Internal only — no changes to scope.yml format or tool signatures. The `gptel-make-scoped-tool` macro interface is unchanged.
- **Downstream**: Expansion UI behavior unchanged from user perspective — denial reasons will be more consistent and always human-readable.
