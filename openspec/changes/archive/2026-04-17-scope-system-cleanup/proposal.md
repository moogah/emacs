## Why

The gptel scope system has accumulated architectural drift across three development cycles (v2 path validation, v3 categories, v4 bash-parser integration). Each cycle explored new capabilities without reconciling the prior architecture, leaving duplicate implementations (two glob-to-regex functions), dead concepts (org-roam tools, bash_tools.categories), a monolithic scope-core.el that mixes concerns (macro, dispatch, validation, error formatting, pattern matching), and divergent error code schemes between validators that do the same thing. The system works but is fragile — changes risk subtle breakage because the same logic lives in multiple places with inconsistent interfaces.

## What Changes

- **BREAKING**: Remove org-roam tools and pattern validation type entirely (no specs, only adding complexity)
- **BREAKING**: Remove tool categorization const — tools declare their own operation type at the macro call site
- **BREAKING**: Remove scope-commands.el (no longer used)
- Split scope-core.el into two focused modules: a thin tool wrapper (macro + dispatch) and a validation module
- Merge bash validation (scope-shell-tools.el) into the validation module alongside path validation
- Unify path validation: single `validate-path-operation(path, operation, scope-config)` function used by both filesystem tools and bash pipeline stage 6
- Single glob-to-regex implementation shared by all validators
- Move schema loading/defaults into scope-yaml.el (canonical config boundary)
- Consolidate error codes to one scheme (resolve divergence between path validators)
- Fix allow-once double-check (single check point in validation, not macro + validator)
- Remove add-bash-to-scope categories logic (writes invalid config)
- Update interfaces.el contracts to match new architecture

## Capabilities

### New Capabilities

- `scope-validation`: Unified validation module — single path operation validator shared by filesystem tools and bash pipeline, single glob implementation, consolidated error codes, allow-once mechanism
- `scope-tool-wrapper`: Thin macro that loads config, calls validation, handles errors/expansion — tools declare operation type at definition site

### Modified Capabilities

- `gptel/scope`: Core scope spec updated to reflect removed org-roam, removed tool categories, new module boundaries (scope-core split into wrapper + validation, scope-shell-tools merged into validation)

## Impact

- **Files deleted**: scope-core.el/org, scope-shell-tools.el/org, scope-org-roam-tools.el/org, scope-commands.el/org
- **Files created**: scope-validation.el/org, scope-tool-wrapper.el/org
- **Files modified**: scope-expansion.el/org (remove org-roam and categories handling), scope-yaml.el/org (absorb schema loading), scope-filesystem-tools.el/org (macro call site declares operation), interfaces.el/org (updated contracts)
- **Tests**: Existing test suites need updating to match new module boundaries; contract tests in interfaces.el remain the source of truth
- **Scope documents**: No changes to scope.yml format — this is purely internal restructuring
- **Dependencies**: bash-parser integration preserved as-is within validation module
